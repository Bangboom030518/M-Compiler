use parser::expression::IntrinsicOperator;

use super::builder::{self, VariableId};
use crate::declarations::{self, Declarations};
use crate::layout::{self, Layout};
use crate::{hir, SemanticError};
use std::collections::HashMap;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum EnvironmentState {
    #[default]
    NonMutated,
    Mutated,
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

pub struct Inferer<'a> {
    variables: &'a mut HashMap<VariableId, Option<declarations::Id>>,
    return_type: declarations::Id,
    declarations: &'a Declarations,
}

impl<'a> Inferer<'a> {
    pub fn function(
        function: &mut builder::Function,
        declarations: &declarations::Declarations,
    ) -> Result<(), SemanticError> {
        let mut inferer = Inferer {
            variables: &mut function.variables,
            return_type: function.return_type,
            declarations,
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
        statement: &mut hir::Statement,
    ) -> Result<EnvironmentState, SemanticError> {
        match statement {
            hir::Statement::Assignment(hir::Assignment { left, right }) => {
                let environment_state = EnvironmentState::default()
                    .merge(self.expression(right, left.type_id)?)
                    .merge(self.expression(left, right.type_id)?)
                    .merge(self.expression(right, left.type_id)?);

                Ok(environment_state)
            }
            hir::Statement::Let(variable, expression) => {
                let variable_type = self
                    .variables
                    .get(variable)
                    .expect("variable doesn't exist!");

                let environment_state = self.expression(expression, *variable_type)?;

                self.variables
                    .insert(*variable, expression.type_id)
                    .expect("variable doesn't exist!");

                Ok(environment_state)
            }
            hir::Statement::Expression(expression) => self.expression(expression, None),
        }
    }

    fn block(
        &mut self,
        block: &mut hir::Block,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        let mut environment_state = EnvironmentState::default();

        for statement in &mut block.statements {
            environment_state.merge_mut(self.statement(statement)?);
        }

        if let Some(expression) = &mut block.expression {
            environment_state.merge_mut(self.expression(expression, expected_type)?);
        }

        Ok(environment_state)
    }

    fn if_expression(
        &mut self,
        expression: &mut hir::If,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        let hir::If {
            condition,
            then_branch,
            else_branch,
        } = expression;
        let mut environment_state = self.expression(condition, None)?;

        environment_state.merge_mut(self.block(then_branch, expected_type)?);
        let mut expected_type = expected_type.or_else(|| {
            then_branch
                .expression
                .as_ref()
                .and_then(|expression| expression.type_id)
        });

        environment_state.merge_mut(self.block(else_branch, expected_type)?);

        if let Some(expression) = &else_branch.expression {
            if let Some(type_id) = expression.type_id {
                if expected_type.is_none() {
                    expected_type = Some(type_id);
                    environment_state.merge_mut(self.block(then_branch, expected_type)?);
                }
            }
        }
        Ok(environment_state)
    }

    fn field_access(
        &mut self,
        type_id: &mut Option<declarations::Id>,
        field_access: &mut hir::FieldAccess,
    ) -> Result<EnvironmentState, SemanticError> {
        let environment_state = self.expression(&mut field_access.expression, None)?;
        let struct_type_id = field_access.expression.type_id;
        match struct_type_id {
            None => Ok(environment_state),
            Some(struct_type_id) => match self.declarations.get_layout(struct_type_id) {
                Layout::Struct(layout) => {
                    let field = layout
                        .fields
                        .get(field_access.field.as_ref())
                        .ok_or(SemanticError::NonExistentField)?;

                    if let Some(type_id) = type_id {
                        if *type_id != field.type_id {
                            let expected = self.declarations.get_layout(*type_id).clone();
                            let found = self.declarations.get_layout(field.type_id).clone();
                            return Err(SemanticError::MismatchedTypes {
                                expected,
                                found,
                                expression: hir::Expression::FieldAccess(Box::new(
                                    field_access.clone(),
                                )),
                            });
                        }
                    } else {
                        *type_id = Some(field.type_id);
                    };
                    Ok(environment_state)
                }
                layout => Err(SemanticError::InvalidFieldAccess(layout.clone())),
            },
        }
    }

    fn expression(
        &mut self,
        expression: &mut hir::TypedExpression,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        expression.type_id = expression.type_id.or(expected_type);

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
                    if let expected_type @ Some(_) = expected_type {
                        expression.type_id = expected_type;
                        *variable_type = expected_type;
                        EnvironmentState::Mutated
                    } else {
                        EnvironmentState::NonMutated
                    }
                } else {
                    expression.type_id = *variable_type;
                    EnvironmentState::NonMutated
                }
            }
            hir::Expression::FieldAccess(field_access) => {
                self.field_access(&mut expression.type_id, field_access)?
            }
            hir::Expression::Return(inner) => {
                inner.type_id = Some(self.return_type);
                self.expression(inner, Some(self.return_type))?
            }
            hir::Expression::If(if_expression) => {
                self.if_expression(if_expression, expected_type)?
            }
            hir::Expression::Call(call) => {
                let hir::Expression::GlobalAccess(declaration) = call.callable.expression else {
                    todo!("closures!")
                };
                let signature = self.declarations.get_function(declaration)?.signature();

                if signature.parameters.len() != call.arguments.len() {
                    return Err(SemanticError::InvalidNumberOfArguments);
                }
                let mut environment_state = EnvironmentState::NonMutated;

                for (type_id, argument) in signature
                    .parameters
                    .iter()
                    .copied()
                    .zip(call.arguments.iter_mut())
                {
                    environment_state.merge_mut(self.expression(argument, Some(type_id))?);
                }

                expression.type_id = Some(signature.return_type);

                environment_state
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                let mut environment_state = self
                    .expression(&mut binary.left, binary.right.type_id)?
                    .merge(self.expression(&mut binary.right, binary.left.type_id)?)
                    .merge(self.expression(&mut binary.left, binary.right.type_id)?);
                if !matches!(binary.operator, IntrinsicOperator::Cmp(_))
                    && expression.type_id.is_none()
                    && binary.left.type_id.is_some()
                {
                    let type_id = binary.left.type_id;
                    environment_state =
                        environment_state.merge(self.expression(expression, type_id)?);
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
                if let Some(type_id) = expression.type_id {
                    let layout = self.declarations.get_layout(type_id);
                    if layout == &Layout::Primitive(layout::Primitive::USize) {
                        self.expression(pointer, None)?
                    } else {
                        return Err(SemanticError::InvalidAddr {
                            found: layout.clone(),
                            expression: expression.expression.clone(),
                        });
                    }
                } else {
                    EnvironmentState::default()
                }
            }
            hir::Expression::GlobalAccess(_) => todo!("global access!"),
            hir::Expression::Load(inner) => self.expression(inner, None)?,
            hir::Expression::Store(store) => EnvironmentState::default()
                .merge(self.expression(&mut store.expression, None)?)
                .merge(self.expression(&mut store.pointer, None)?),
        };

        if let (Some(expected), Some(found)) = (expected_type, expression.type_id) {
            if expected != found {
                return Err(SemanticError::MismatchedTypes {
                    expected: self.declarations.get_layout(expected).clone(),
                    found: self.declarations.get_layout(found).clone(),
                    expression: expression.expression.clone(),
                });
            }
        }

        Ok(environment_state)
    }
}
