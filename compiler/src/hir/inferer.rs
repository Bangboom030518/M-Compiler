use super::builder::{self, VariableId};
use super::TypedExpression;
use crate::declarations::{self, Declarations};
use crate::layout::Layout;
use crate::{hir, SemanticError};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EnvironmentState {
    Mutated,
    NonMutated,
}

impl EnvironmentState {
    fn merge(&mut self, rhs: Self) {
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
            hir::Statement::Assignment(
                TypedExpression {
                    expression: hir::Expression::LocalAccess(var),
                    ..
                },
                expression,
            ) => todo!(),
            hir::Statement::Let(_, _) => todo!(),
            hir::Statement::Expression(expression) => self.expression(expression, None),
            hir::Statement::Assignment(_, _) => todo!(),
        }
    }

    fn block(
        &mut self,
        block: &mut hir::Block,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        let mut environment_state = EnvironmentState::NonMutated;

        for statement in &mut block.statements {
            environment_state.merge(self.statement(statement)?);
        }

        if let Some(expression) = &mut block.expression {
            environment_state.merge(self.expression(expression, expected_type)?);
        }

        Ok(environment_state)
    }

    fn expression(
        &mut self,
        expression: &mut hir::TypedExpression,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        if let (Some(expected), Some(found)) = (expected_type, expression.type_id) {
            if expected != found {
                return Err(SemanticError::MismatchedTypes {
                    expected: self.declarations.get_layout(expected).clone(),
                    found: self.declarations.get_layout(found).clone(),
                });
            }
        }

        let environment_state = match &mut expression.expression {
            hir::Expression::FloatConst(_) | hir::Expression::IntegerConst(_) => {
                EnvironmentState::NonMutated
            }
            hir::Expression::LocalAccess(variable) => {
                expression.type_id = *self
                    .variables
                    .get(variable)
                    .expect("Variable doesn't exist in hir!");

                EnvironmentState::NonMutated
            }
            hir::Expression::FieldAccess(left, field) => {
                let environment_state = self.expression(left, None)?;
                let type_id = left.type_id;
                match type_id {
                    None => environment_state,
                    Some(type_id) => {
                        let Layout::Struct { fields, .. } = self.declarations.get_layout(type_id)
                        else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };
                        let field = fields.get(field).ok_or(SemanticError::NonExistentField)?;

                        if expression.type_id.is_none() {
                            expression.type_id = Some(field.type_id);
                        } else {
                            // TODO: assert same
                        };
                        environment_state
                    }
                }
            }
            hir::Expression::Return(inner) => {
                inner.type_id = Some(self.return_type);
                let environment_state = self.expression(inner, Some(self.return_type))?;
                expression.type_id = expression.type_id.or(expected_type);
                environment_state
            }
            hir::Expression::If(if_expression) => {
                let hir::If {
                    condition,
                    then_branch,
                    else_branch,
                } = &mut **if_expression;
                let mut environment_state = self.expression(condition, None)?;

                environment_state.merge(self.block(then_branch, expected_type)?);
                let mut expected_type = expected_type.or_else(|| {
                    then_branch
                        .expression
                        .as_ref()
                        .and_then(|expression| expression.type_id)
                });

                environment_state.merge(self.block(else_branch, expected_type)?);

                if let Some(expression) = &else_branch.expression {
                    if let Some(type_id) = expression.type_id {
                        if expected_type.is_none() {
                            expected_type = Some(type_id);
                            environment_state.merge(self.block(then_branch, expected_type)?);
                        }
                    }
                }
                expression.type_id = expression.type_id.or(expected_type);
                environment_state
            }
            _ => todo!(),
        };

        Ok(environment_state)
    }
}
