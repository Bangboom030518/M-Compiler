use super::builder::{self, VariableId};
use super::TypedExpression;
use crate::declarations::{self, Declarations, FuncReference, ScopeId};
use crate::layout::{self, Layout};
use crate::{hir, SemanticError};
use declarations::TypeReference;
use parser::expression::IntrinsicOperator;
use std::collections::HashMap;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum EnvironmentState {
    #[default]
    NonMutated,
    Mutated,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct EnvironmentStateMonad<T> {
    environment_state: EnvironmentState,
    value: T,
}

impl<T> EnvironmentStateMonad<T> {
    fn new(environment_state: EnvironmentState, value: T) -> Self {
        Self {
            environment_state,
            value,
        }
    }

    fn unwrap_merge(self, environment_state: &mut EnvironmentState) -> T {
        environment_state.merge_mut(self.environment_state);
        self.value
    }

    fn bind<U>(self, f: impl FnOnce(T) -> EnvironmentStateMonad<U>) -> EnvironmentStateMonad<U> {
        let EnvironmentStateMonad {
            environment_state,
            value,
        } = f(self.value);
        EnvironmentStateMonad {
            value,
            environment_state: self.environment_state.merge(environment_state),
        }
    }
}

impl EnvironmentState {
    const fn merge(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::NonMutated, Self::NonMutated) => Self::NonMutated,
            _ => Self::Mutated,
        }
    }

    fn merge_mut(&mut self, rhs: Self) {
        *self = match (*self, rhs) {
            (Self::NonMutated, Self::NonMutated) => Self::NonMutated,
            _ => Self::Mutated,
        };
    }
}

pub struct Inferer<'a, M> {
    variables: &'a mut HashMap<VariableId, Option<TypeReference>>,
    return_type: TypeReference,
    declarations: &'a mut Declarations,
    module: &'a mut M,
    scope: ScopeId,
}

impl<'a, M> Inferer<'a, M>
where
    M: cranelift_module::Module,
{
    pub fn function(
        function: &mut builder::Function,
        declarations: &mut declarations::Declarations,
        context: &mut M,
        scope: ScopeId,
    ) -> Result<(), SemanticError> {
        let mut inferer = Inferer {
            variables: &mut function.variables,
            return_type: function.return_type.clone(),
            declarations,
            module: context,
            scope,
        };

        'a: loop {
            for statement in &mut function.body {
                if inferer.statement(statement)? == EnvironmentState::Mutated {
                    continue 'a;
                }
            }
            return Ok(());
        }
    }

    fn statement(
        &mut self,
        statement: hir::Statement,
    ) -> Result<EnvironmentStateMonad<hir::Statement>, SemanticError> {
        match statement {
            hir::Statement::Assignment(hir::Assignment { left, right }) => {
                let mut environment_state = EnvironmentState::default();

                let right = self
                    .expression(right, left.type_ref.clone())?
                    .unwrap_merge(&mut environment_state);
                let left = self
                    .expression(left, right.type_ref.clone())?
                    .unwrap_merge(&mut environment_state);
                let right = self
                    .expression(right, left.type_ref.clone())?
                    .unwrap_merge(&mut environment_state);

                Ok(EnvironmentStateMonad {
                    value: hir::Statement::Assignment(hir::Assignment { left, right }),
                    environment_state,
                })
            }
            hir::Statement::Let(variable, expression) => {
                let variable_type = self
                    .variables
                    .get(&variable)
                    .expect("variable doesn't exist!");

                let environment_state = self.expression(expression, variable_type.clone())?;

                self.variables
                    .insert(variable, expression.type_ref.clone())
                    .expect("variable doesn't exist!");

                Ok(environment_state)
            }
            hir::Statement::Expression(expression) => self.expression(expression, None),
        }
    }

    fn block(
        &mut self,
        block: hir::Block,
        expected_type: Option<TypeReference>,
    ) -> Result<EnvironmentStateMonad<hir::Block>, SemanticError> {
        let mut environment_state = EnvironmentState::default();

        let statements = block
            .statements
            .into_iter()
            .map(|statement| {
                self.statement(statement)
                    .map(|statement| statement.unwrap_merge(&mut environment_state))
            })
            .collect::<Result<Vec<_>, _>>()?;

        if let Some(expression) = &mut block.expression {
            environment_state.merge_mut(self.expression(expression, expected_type)?);
        }

        Ok(EnvironmentStateMonad::new(
            environment_state,
            hir::Block {
                expression: block.expression.map(|expression| {
                    self.expression(expression, expected_type)
                        .map(|expression| expression.unwrap_merge(&mut environment_state))
                }),
                statements,
            },
        ))
    }

    fn if_expression(
        &mut self,
        expression: &mut hir::If,
        expected_type: Option<TypeReference>,
    ) -> Result<EnvironmentState, SemanticError> {
        let hir::If {
            condition,
            then_branch,
            else_branch,
        } = expression;
        let mut environment_state = self.expression(condition, None)?;
        if let Some(type_ref) = &condition.type_ref {
            let layout = self.declarations.insert_layout(type_ref, self.scope)?;
            if Layout::Primitive(layout::Primitive::U8) != layout {
                // todo!("expected bool, found something else");
            }
        }

        environment_state.merge_mut(self.block(then_branch, expected_type.clone())?);
        let mut expected_type = expected_type.or_else(|| {
            then_branch
                .expression
                .as_ref()
                .and_then(|expression| expression.type_ref.clone())
        });

        environment_state.merge_mut(self.block(else_branch, expected_type.clone())?);

        if let Some(expression) = &else_branch.expression {
            if let Some(type_ref) = expression.type_ref.clone() {
                if expected_type.is_none() {
                    expected_type = Some(type_ref);
                    environment_state.merge_mut(self.block(then_branch, expected_type)?);
                }
            }
        }
        Ok(environment_state)
    }

    fn field_access(
        &mut self,
        type_ref: &mut Option<TypeReference>,
        field_access: &mut hir::FieldAccess,
    ) -> Result<EnvironmentState, SemanticError> {
        let environment_state = self.expression(&mut field_access.expression, None)?;
        let struct_type_id = field_access.expression.type_ref.clone();
        match struct_type_id {
            None => Ok(environment_state),
            Some(struct_type_id) => match self
                .declarations
                .insert_layout(&struct_type_id, self.scope)?
            {
                Layout::Struct(layout) => {
                    let fields = layout.fields;
                    let field = fields
                        .get(field_access.field.as_ref())
                        .ok_or(SemanticError::NonExistentField)?;

                    if let Some(type_ref) = type_ref {
                        type_ref.assert_equivalent(
                            &field.type_id,
                            self.declarations,
                            self.scope,
                            &hir::Expression::FieldAccess(Box::new(field_access.clone())),
                        )?;
                    } else {
                        *type_ref = Some(field.type_id.clone());
                    };
                    Ok(environment_state)
                }
                layout => Err(SemanticError::InvalidFieldAccess(layout)),
            },
        }
    }

    fn expression(
        &mut self,
        expression: hir::TypedExpression,
        expected_type: Option<TypeReference>,
    ) -> Result<EnvironmentStateMonad<TypedExpression>, SemanticError> {
        if let (Some(expected), Some(found)) = (expected_type.clone(), expression.type_ref.clone())
        {
            expected.assert_equivalent(
                &found,
                self.declarations,
                self.scope,
                &expression.expression,
            )?;
        }

        expression.type_ref = expression
            .type_ref
            .as_ref()
            .or(expected_type.as_ref())
            .cloned();

        let mut inferred_type = None;
        let environment_state = match &mut expression.expression {
            hir::Expression::FloatConst(_)
            | hir::Expression::IntegerConst(_)
            | hir::Expression::StringConst(_) => {
                // TODO: assert type here
                EnvironmentState::NonMutated
            }
            hir::Expression::LocalAccess(variable) => {
                let variable_type = self
                    .variables
                    .get_mut(variable)
                    .expect("variable doesn't exist!");
                if variable_type.is_none() {
                    if let expected_type @ Some(_) = expected_type.clone() {
                        inferred_type = expected_type.clone();
                        *variable_type = expected_type;
                        EnvironmentState::Mutated
                    } else {
                        EnvironmentState::NonMutated
                    }
                } else {
                    inferred_type = variable_type.clone();
                    EnvironmentState::NonMutated
                }
            }
            hir::Expression::FieldAccess(field_access) => {
                self.field_access(&mut expression.type_ref, field_access)?
            }
            hir::Expression::Return(inner) => {
                inner.type_ref = Some(self.return_type.clone());
                self.expression(inner, Some(self.return_type.clone()))?
            }
            hir::Expression::If(if_expression) => {
                self.if_expression(if_expression, expected_type.clone())?
            }
            hir::Expression::Call(call) => {
                let (callable, generics) =
                    if let hir::Expression::Generixed(generixed) = &call.callable.expression {
                        let hir::Generixed {
                            expression,
                            generics,
                        } = generixed.as_ref();
                        (expression.expression.clone(), generics.clone())
                    } else {
                        (call.callable.expression.clone(), Vec::new())
                    };

                let hir::Expression::GlobalAccess(declaration) = callable else {
                    todo!("func refs!")
                };
                let signature = self
                    .declarations
                    .insert_function(
                        FuncReference {
                            id: declaration,
                            generics,
                        },
                        self.module,
                        self.scope,
                    )?
                    .signature()
                    .clone();

                if signature.parameters.len() != call.arguments.len() {
                    return Err(SemanticError::InvalidNumberOfArguments);
                }
                let mut environment_state = EnvironmentState::NonMutated;

                for (type_id, argument) in signature
                    .parameters
                    .into_iter()
                    .zip(call.arguments.iter_mut())
                {
                    environment_state.merge_mut(self.expression(argument, Some(type_id))?);
                }

                inferred_type = Some(signature.return_type);

                environment_state
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                let mut environment_state = self
                    .expression(&mut binary.left, binary.right.type_ref.clone())?
                    .merge(self.expression(&mut binary.right, binary.left.type_ref.clone())?)
                    .merge(self.expression(&mut binary.left, binary.right.type_ref.clone())?);

                if !matches!(binary.operator, IntrinsicOperator::Cmp(_))
                    && expression.type_ref.is_none()
                    && binary.left.type_ref.is_some()
                {
                    let type_ref = binary.left.type_ref.clone();
                    environment_state =
                        environment_state.merge(self.expression(expression, type_ref)?);
                }

                environment_state
            }
            hir::Expression::Constructor(constructor) => constructor.0.iter_mut().try_fold(
                EnvironmentState::default(),
                |environment_state, (_, field)| {
                    self.expression(field, None)
                        .map(|new_environment_state| environment_state.merge(new_environment_state))
                },
            )?,
            hir::Expression::Addr(pointer) => {
                // TODO: move to translate
                if let Some(type_ref) = expression.type_ref.clone() {
                    let layout = self.declarations.insert_layout(&type_ref, self.scope)?;
                    if layout == Layout::Primitive(layout::Primitive::USize) {
                        self.expression(pointer, None)?
                    } else {
                        return Err(SemanticError::InvalidAddr {
                            found: layout,
                            expression: expression.expression.clone(),
                        });
                    }
                } else {
                    EnvironmentState::default()
                }
            }
            hir::Expression::GlobalAccess(_) => {
                // TODO: exprs
                EnvironmentState::default()
            }
            hir::Expression::Load(inner) => self.expression(inner, None)?,
            hir::Expression::Generixed(generixed) => {
                self.expression(&mut generixed.expression, None)?
            }
            hir::Expression::Store(store) => EnvironmentState::default()
                .merge(self.expression(&mut store.expression, None)?)
                .merge(self.expression(&mut store.pointer, None)?),
            hir::Expression::AssertType(inner) => self.expression(
                inner,
                Some(inner.type_ref.clone().expect("assert_type to have a type")),
            )?,
        };

        if let (Some(expected), Some(found)) = (expected_type, expression.type_ref.clone()) {
            if self.declarations.insert_layout(&expected, self.scope)? == Layout::Void {
                expression.type_ref = Some(expected);
            } else {
                expected.assert_equivalent(
                    &found,
                    self.declarations,
                    self.scope,
                    &expression.expression,
                )?;
            }
        }

        Ok(environment_state)
    }
}
