use super::builder::{self, VariableId};
use crate::declarations::{self, Declarations, FuncReference, ScopeId};
use crate::layout::{self, Layout};
use crate::{hir, SemanticError};
use declarations::TypeReference;
use parser::expression::IntrinsicOperator;
use std::collections::{HashMap, VecDeque};

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
    const fn new(environment_state: EnvironmentState, value: T) -> Self {
        Self {
            environment_state,
            value,
        }
    }

    fn unwrap_merge(self, environment_state: &mut EnvironmentState) -> T {
        *environment_state = environment_state.merge(self.environment_state);
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

    fn boxed(self) -> EnvironmentStateMonad<Box<T>> {
        self.map(Box::new)
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> EnvironmentStateMonad<U> {
        self.bind(|value| EnvironmentStateMonad::new(EnvironmentState::default(), f(value)))
    }
}

impl EnvironmentState {
    const fn merge(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::NonMutated, Self::NonMutated) => Self::NonMutated,
            _ => Self::Mutated,
        }
    }
}

pub struct Inferer<'a, M> {
    variables: HashMap<VariableId, Option<TypeReference>>,
    return_type: TypeReference,
    declarations: &'a mut Declarations,
    module: &'a mut M,
    scope: ScopeId,
}

impl<M> Inferer<'_, M>
where
    M: cranelift_module::Module,
{
    pub fn function(
        function: builder::Function,
        declarations: &mut declarations::Declarations,
        context: &mut M,
        scope: ScopeId,
    ) -> Result<builder::Function, SemanticError> {
        let mut inferer = Inferer {
            variables: function.variables,
            return_type: function.return_type,
            declarations,
            module: context,
            scope,
        };

        let mut body = VecDeque::from(function.body);
        let mut state = EnvironmentState::NonMutated;
        let mut inferred = Vec::new();
        while let Some(statement) = body.pop_front() {
            let statement = inferer.statement(statement)?.unwrap_merge(&mut state);
            inferred.push(statement);
            if state == EnvironmentState::Mutated {
                body = inferred.drain(..).chain(body.drain(..)).collect();
            }

            state = EnvironmentState::NonMutated;
        }

        Ok(builder::Function {
            return_type: inferer.return_type,
            variables: inferer.variables,
            body: inferred,
        })
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

                let mut type_ref = None;
                let statement =
                    self.expression(expression, variable_type.clone())?
                        .map(|expression| {
                            type_ref = expression.type_ref.clone();
                            hir::Statement::Let(variable, expression)
                        });

                self.variables
                    .insert(variable, type_ref)
                    .expect("variable doesn't exist!");

                Ok(statement)
            }
            hir::Statement::Expression(expression) => Ok(self
                .expression(expression, None)?
                .map(hir::Statement::Expression)),
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

        Ok(EnvironmentStateMonad::new(
            environment_state,
            hir::Block {
                expression: block
                    .expression
                    .map(|expression| {
                        self.expression(expression, expected_type)
                            .map(|expression| expression.unwrap_merge(&mut environment_state))
                    })
                    .transpose()?,
                statements,
            },
        ))
    }

    fn if_expression(
        &mut self,
        expression: hir::Typed<hir::If>,
    ) -> Result<EnvironmentStateMonad<hir::Typed<hir::If>>, SemanticError> {
        let hir::If {
            condition,
            then_branch,
            else_branch,
        } = expression.value;
        let type_ref = expression.type_ref;
        let mut state = EnvironmentState::default();
        let condition = self.expression(condition, None)?.unwrap_merge(&mut state);
        if let Some(type_ref) = &condition.type_ref {
            let layout = self.declarations.insert_layout(type_ref, self.scope)?;
            if Layout::Primitive(layout::Primitive::U8) != layout {
                todo!("expected bool, found something else");
            }
        }

        let mut then_branch = self
            .block(then_branch, type_ref.clone())?
            .unwrap_merge(&mut state);
        let mut type_ref = type_ref.or_else(|| {
            then_branch
                .expression
                .as_ref()
                .and_then(|expression| expression.type_ref.clone())
        });

        let else_branch = self
            .block(else_branch, type_ref.clone())?
            .unwrap_merge(&mut state);

        if let Some(expression) = &else_branch.expression {
            if let Some(else_type_ref) = expression.type_ref.clone() {
                if type_ref.is_none() {
                    type_ref = Some(else_type_ref);
                    then_branch = self
                        .block(then_branch, type_ref.clone())?
                        .unwrap_merge(&mut state);
                }
            }
        }

        Ok(EnvironmentStateMonad::new(
            state,
            hir::Typed::new(
                hir::If {
                    condition,
                    then_branch,
                    else_branch,
                },
                type_ref,
            ),
        ))
    }

    fn field_access(
        &mut self,
        field_access: hir::Typed<hir::FieldAccess>,
    ) -> Result<EnvironmentStateMonad<hir::Typed<hir::FieldAccess>>, SemanticError> {
        let hir::Typed {
            value: mut field_access,
            mut type_ref,
        } = field_access;
        let mut state = EnvironmentState::default();
        field_access.expression = self
            .expression(field_access.expression, None)?
            .unwrap_merge(&mut state);

        let struct_type_id = field_access.expression.type_ref.clone();
        match struct_type_id {
            None => Ok(EnvironmentStateMonad::new(
                state,
                hir::Typed {
                    value: field_access,
                    type_ref,
                },
            )),
            Some(struct_type_id) => match self
                .declarations
                .insert_layout(&struct_type_id, self.scope)?
            {
                Layout::Struct(layout) => {
                    let fields = layout.fields;
                    let field = fields
                        .get(field_access.field.as_ref())
                        .ok_or(SemanticError::NonExistentField)?;

                    if let Some(ref type_ref) = type_ref {
                        type_ref.assert_equivalent(
                            &field.type_id,
                            self.declarations,
                            self.scope,
                            &hir::Expression::FieldAccess(Box::new(field_access.clone())),
                        )?;
                    } else {
                        type_ref = Some(field.type_id.clone());
                    };

                    Ok(EnvironmentStateMonad::new(
                        state,
                        hir::Typed {
                            value: field_access,
                            type_ref,
                        },
                    ))
                }
                layout => Err(SemanticError::InvalidFieldAccess(layout)),
            },
        }
    }

    fn expression(
        &mut self,
        mut expression: hir::Typed<hir::Expression>,
        expected_type: Option<TypeReference>,
    ) -> Result<EnvironmentStateMonad<hir::Typed<hir::Expression>>, SemanticError> {
        if let (Some(expected), Some(found)) = (expected_type.clone(), expression.type_ref.clone())
        {
            expected.assert_equivalent(&found, self.declarations, self.scope, &expression.value)?;
        }

        expression.type_ref = expression
            .type_ref
            .as_ref()
            .or(expected_type.as_ref())
            .cloned();

        // if let Some(type_ref) = expression.type_ref {
        // if type_ref != expected_type.unwrap() {
        // todo!("Mismatched inferences (internal)")
        // }
        // }

        let mut inferred_type = None;

        let mut expression = match expression.value {
            hir::Expression::FloatConst(_)
            | hir::Expression::IntegerConst(_)
            | hir::Expression::StringConst(_) => {
                // TODO: assert type here
                EnvironmentStateMonad::new(EnvironmentState::default(), expression)
            }
            hir::Expression::LocalAccess(variable) => {
                let variable_type = self
                    .variables
                    .get_mut(&variable)
                    .expect("variable doesn't exist!");

                let state = if variable_type.is_none() {
                    if let expected_type @ Some(_) = expected_type.clone() {
                        // TODO: is this line necessary??
                        inferred_type = expected_type.clone();
                        *variable_type = expected_type;
                        EnvironmentState::Mutated
                    } else {
                        EnvironmentState::NonMutated
                    }
                } else {
                    inferred_type = variable_type.clone();
                    EnvironmentState::NonMutated
                };

                EnvironmentStateMonad::new(state, expression)
            }
            hir::Expression::FieldAccess(field_access) => self
                .field_access(hir::Typed::new(*field_access, expression.type_ref))?
                .map(|field_access| {
                    field_access
                        .map(Box::new)
                        .map(hir::Expression::FieldAccess)
                        .into()
                }),
            hir::Expression::Return(mut inner) => {
                inner.type_ref = Some(self.return_type.clone());
                self.expression(*inner, Some(self.return_type.clone()))?
            }
            hir::Expression::If(if_expression) => self
                .if_expression(hir::Typed::new(*if_expression, expression.type_ref))?
                .map(|if_expression| if_expression.map(Box::new).map(hir::Expression::If)),
            hir::Expression::Call(mut call) => {
                let (callable, generics) =
                    if let hir::Expression::Generixed(generixed) = &call.callable.value {
                        let hir::Generixed {
                            expression,
                            generics,
                        } = generixed.as_ref();
                        (expression.value.clone(), generics.clone())
                    } else {
                        (call.callable.value.clone(), Vec::new())
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

                call.arguments = signature
                    .parameters
                    .into_iter()
                    .zip(call.arguments.into_iter())
                    .map(|(type_ref, argument)| {
                        self.expression(argument, Some(type_ref))
                            .map(|expression| expression.unwrap_merge(&mut environment_state))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                inferred_type = Some(signature.return_type);

                EnvironmentStateMonad::new(
                    environment_state,
                    hir::Typed::new(hir::Expression::Call(call), expression.type_ref).into(),
                )
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                let mut state = EnvironmentState::default();
                let hir::BinaryIntrinsic {
                    mut left,
                    mut right,
                    operator,
                } = *binary;
                left = self
                    .expression(left, right.type_ref.clone())?
                    .unwrap_merge(&mut state);
                right = self
                    .expression(right, left.type_ref.clone())?
                    .unwrap_merge(&mut state);
                left = self
                    .expression(left, right.type_ref.clone())?
                    .unwrap_merge(&mut state);

                if !matches!(binary.operator, IntrinsicOperator::Cmp(_))
                    && expression.type_ref.is_none()
                    && left.type_ref.is_some()
                {
                    inferred_type = left.type_ref.clone();
                    todo!("can we assign to expression?");
                }
                EnvironmentStateMonad::new(
                    state,
                    hir::Typed::new(
                        hir::Expression::BinaryIntrinsic(Box::new(hir::BinaryIntrinsic {
                            left,
                            right,
                            operator,
                        })),
                        expression.type_ref,
                    )
                    .into(),
                )
            }
            hir::Expression::Constructor(constructor) => {
                let mut state = EnvironmentState::default();
                let fields = constructor
                    .0
                    .into_iter()
                    .map(|(offset, expression)| {
                        self.expression(expression, None)
                            .map(|expression| (offset, { expression.unwrap_merge(&mut state) }))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                EnvironmentStateMonad::new(
                    state,
                    hir::Typed::new(
                        hir::Expression::Constructor(hir::Constructor(fields)),
                        expression.type_ref,
                    )
                    .into(),
                )
            }
            hir::Expression::Addr(pointer) => {
                // TODO: move to translate
                if let Some(type_ref) = expression.type_ref.clone() {
                    let layout = self.declarations.insert_layout(&type_ref, self.scope)?;
                    if layout == Layout::Primitive(layout::Primitive::USize) {
                        self.expression(*pointer, None)?.boxed().map(|addr| {
                            hir::Typed::new(hir::Expression::Addr(addr), expression.type_ref)
                        })
                    } else {
                        return Err(SemanticError::InvalidAddr {
                            found: layout,
                            expression: hir::Expression::Addr(pointer),
                        });
                    }
                } else {
                    EnvironmentStateMonad::new(
                        EnvironmentState::default(),
                        hir::Typed::new(hir::Expression::Addr(pointer), expression.type_ref).into(),
                    )
                }
            }
            hir::Expression::GlobalAccess(_) => {
                todo!("global access expressions")
            }
            hir::Expression::Load(inner) => self
                .expression(*inner, None)?
                .map(Box::new)
                .map(hir::Expression::Load)
                .map(From::from),
            hir::Expression::Generixed(generixed) => {
                let mut state = EnvironmentState::default();
                let hir::Generixed {
                    expression: mut inner,
                    generics,
                } = *generixed;
                inner = self.expression(inner, None)?.unwrap_merge(&mut state);
                EnvironmentStateMonad::new(
                    state,
                    hir::Typed::new(
                        hir::Expression::Generixed(Box::new(hir::Generixed {
                            expression: inner,
                            generics,
                        })),
                        expression.type_ref,
                    ),
                )
            }
            hir::Expression::Store(store) => {
                let hir::Store {
                    expression: mut inner,
                    mut pointer,
                } = *store;
                let mut state = EnvironmentState::default();
                inner = self.expression(inner, None)?.unwrap_merge(&mut state);
                pointer = self.expression(pointer, None)?.unwrap_merge(&mut state);
                EnvironmentStateMonad::new(
                    state,
                    hir::Typed::new(
                        hir::Expression::Store(Box::new(hir::Store {
                            expression: inner,
                            pointer,
                        })),
                        expression.type_ref,
                    )
                    .into(),
                )
            }
            hir::Expression::AssertType(inner) => {
                let type_ref = Some(inner.type_ref.clone().expect("assert_type to have a type"));
                self.expression(*inner, type_ref)?
                    .map(Box::new)
                    .map(hir::Expression::AssertType)
                    .map(|assert_type| hir::Typed::new(assert_type, expression.type_ref).into())
            }
        };
        if let (Some(inferred), Some(type_ref)) = (&inferred_type, &expression.value.type_ref) {
            if type_ref != inferred {
                dbg!(type_ref, inferred);
                todo!("Mismatched inferences (internal?)")
            }
        }
        if expression.value.type_ref.is_none() {
            expression.value.type_ref = inferred_type;
        }
        if let (Some(expected), Some(found)) = (expected_type, expression.value.type_ref.clone()) {
            if self.declarations.insert_layout(&expected, self.scope)? == Layout::Void {
                expression.value.type_ref = Some(expected);
            } else {
                expected.assert_equivalent(
                    &found,
                    self.declarations,
                    self.scope,
                    &expression.value.value,
                )?;
            }
        }

        Ok(expression)
    }
}
