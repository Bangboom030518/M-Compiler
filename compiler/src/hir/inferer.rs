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

    fn expression(
        &mut self,
        expression: &mut hir::TypedExpression,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        match expression.type_id {
            id @ Some(_) if id != expected_type => return Err(SemanticError::MismatchedTypes),
            Some(_) => return Ok(EnvironmentState::NonMutated),
            None => {}
        };

        let (type_id, environment_state) = match &mut expression.expression {
            hir::Expression::FloatConst(_) | hir::Expression::IntegerConst(_) => {
                (expected_type, EnvironmentState::NonMutated)
            }
            hir::Expression::LocalAccess(variable) => (
                *self
                    .variables
                    .get(variable)
                    .expect("Variable doesn't exist in hir!"),
                EnvironmentState::NonMutated,
            ),
            hir::Expression::FieldAccess(expression, field) => {
                let environment_state = self.expression(expression, None)?;
                let type_id = expression.type_id;
                match type_id {
                    None => (None, environment_state),
                    Some(type_id) => {
                        let Layout::Struct { fields, .. } = self.declarations.get_layout(type_id)
                        else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };
                        let field = fields.get(field).ok_or(SemanticError::NonExistentField)?;
                        (Some(field.type_id), environment_state)
                    }
                }
            }
            hir::Expression::Return(expression) => {
                expression.type_id = Some(self.return_type);
                (
                    expected_type,
                    self.expression(expression, Some(self.return_type))?,
                )
            }
            _ => todo!(),
        };
        expression.type_id = type_id;

        Ok(environment_state)
    }
}
