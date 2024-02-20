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
        type_id2: &mut Option<declarations::Id>,
        field_access: &mut hir::FieldAccess,
    ) -> Result<EnvironmentState, SemanticError> {
        let environment_state = self.expression(&mut field_access.expression, None)?;
        let type_id = field_access.expression.type_id;
        match type_id {
            None => Ok(environment_state),
            Some(type_id) => {
                match self
                    .declarations
                    .get_layout(type_id)
                    .deref_pointers(self.declarations)
                {
                    Layout::Struct(layout) => {
                        let field = layout
                            .fields
                            .get(&field_access.field)
                            .ok_or(SemanticError::NonExistentField)?;

                        if type_id2.is_none() {
                            *type_id2 = Some(field.type_id);
                        } else {
                            // TODO: assert same
                        };
                        Ok(environment_state)
                    }
                    layout => Err(SemanticError::InvalidFieldAccess(layout.clone())),
                }
            }
        }
    }

    fn expression(
        &mut self,
        expression: &mut hir::TypedExpression,
        expected_type: Option<declarations::Id>,
    ) -> Result<EnvironmentState, SemanticError> {
        expression.type_id = expression.type_id.or(expected_type);

        let environment_state = match &mut expression.expression {
            hir::Expression::FloatConst(_) | hir::Expression::IntegerConst(_) | hir::Expression::StringConst(_) => {
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
                    argument.type_id = Some(type_id);
                    environment_state.merge_mut(self.expression(argument, Some(type_id))?);
                }

                expression.type_id = Some(signature.return_type);

                environment_state
            }
            hir::Expression::BinaryIntrinsic(binary) => EnvironmentState::default()
                .merge(self.expression(&mut binary.left, binary.right.type_id)?)
                .merge(self.expression(&mut binary.right, binary.left.type_id)?)
                .merge(self.expression(&mut binary.left, binary.right.type_id)?),
            hir::Expression::Constructor(constructor) => constructor.0.iter_mut().try_fold(
                EnvironmentState::default(),
                |environment_state, (_, field)| {
                    self.expression(field, None)
                        .map(|new_environment_state| environment_state.merge(new_environment_state))
                },
            )?,
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
            hir::Expression::Deref(inner) => {
                let environment_state = self.expression(inner, None)?;
                expression.type_id = expression.type_id.or_else(|| {
                    inner
                        .type_id
                        .map(|id| self.declarations.get_layout(id))
                        .and_then(|layout| match layout {
                            layout::Layout::Primitive(layout::Primitive::MutablePointer(inner)) => {
                                Some(*inner)
                            }
                            _ => None,
                        })
                });

                environment_state
            }
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
