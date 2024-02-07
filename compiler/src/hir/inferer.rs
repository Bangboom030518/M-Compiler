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
            // TODO: distinct variant?
            hir::Statement::Assignment(hir::Assignment { left, right }) => {
                let mut environment_state = EnvironmentState::default();
                
                environment_state.merge(self.expression(
                    right,
                    left.type_id,
                )?);
                
                environment_state.merge(self.expression(
                    left,
                    right.type_id,
                )?);
                
                environment_state.merge(self.expression(
                    right,
                    left.type_id,
                )?);
                
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
        let environment_state = match &mut expression.expression {
            hir::Expression::FloatConst(_) | hir::Expression::IntegerConst(_) => {
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
                let environment_state = self.expression(&mut field_access.expression, None)?;
                let type_id = field_access.expression.type_id;
                match type_id {
                    None => environment_state,
                    Some(type_id) => {
                        let Layout::Struct(layout) = self
                            .declarations
                            .get_layout(type_id)
                            .deref_pointers(self.declarations)
                        else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };

                        let field = layout
                            .fields
                            .get(&field_access.field)
                            .ok_or(SemanticError::NonExistentField)?;

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
                self.expression(inner, Some(self.return_type))?
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
                environment_state
            }
            hir::Expression::Call(call) => {
                let hir::Expression::GlobalAccess(declaration) = call.callable.expression else {
                    todo!("closures!")
                };
                let function = self.declarations.get_function(declaration)?;

                if function.parameters.len() != call.arguments.len() {
                    return Err(SemanticError::InvalidNumberOfArguments);
                }
                let mut environment_state = EnvironmentState::NonMutated;

                for (type_id, argument) in function
                    .parameters
                    .iter()
                    .map(|parameter| parameter.1)
                    .zip(call.arguments.iter_mut())
                {
                    argument.type_id = Some(type_id);
                    environment_state.merge(self.expression(argument, Some(type_id))?);
                }

                expression.type_id = Some(function.return_type);

                environment_state
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                // TODO: `right -> left` inference!
                let mut environment_state = EnvironmentState::default();
                environment_state.merge(self.expression(&mut binary.left, None)?);
                environment_state.merge(self.expression(&mut binary.right, binary.left.type_id)?);
                environment_state
            }
            hir::Expression::Constructor(constructor) => {
                let mut environment_state = EnvironmentState::default();
                for (_, field) in &mut constructor.0 {
                    environment_state.merge(self.expression(field, None)?);
                }
                environment_state
            }
            hir::Expression::MutablePointer(pointer) => {
                if let Some(type_id) = expression.type_id {
                    let Layout::Primitive(layout::Primitive::MutablePointer(inner)) =
                        self.declarations.get_layout(type_id)
                    else {
                        return Err(SemanticError::InvalidMutRef);
                    };
                    self.expression(pointer, Some(*inner))?
                } else {
                    EnvironmentState::default()
                }
            }
            hir::Expression::GlobalAccess(_) => todo!("global access!"),
        };

        if let (Some(expected), Some(found)) = (expected_type, expression.type_id) {
            if expected != found {
                return Err(SemanticError::MismatchedTypes {
                    expected: self.declarations.get_layout(expected).clone(),
                    found: self.declarations.get_layout(found).clone(),
                });
            }
        }

        expression.type_id = expression.type_id.or(expected_type);

        Ok(environment_state)
    }
}
