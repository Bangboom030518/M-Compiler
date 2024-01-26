use crate::declarations::Declarations;
use crate::hir::{BinaryIntrinsic, Expression};
use crate::layout::Layout;
use crate::{declarations, function, hir, SemanticError};
use cranelift::prelude::*;
use itertools::Itertools;
use parser::expression::control_flow::If;
use parser::expression::IntrinsicCall;
use parser::prelude::Literal;
use std::collections::HashMap;

use super::TypedExpression;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct VariableId(usize);

impl From<Variable> for VariableId {
    fn from(value: Variable) -> Self {
        Self(value.as_u32() as usize)
    }
}

impl From<VariableId> for Variable {
    fn from(value: VariableId) -> Self {
        Self::new(value.0)
    }
}

#[derive(Clone)]
pub struct Function {
    pub return_type: declarations::Id,
    pub variables: HashMap<VariableId, Option<declarations::Id>>,
    pub body: Vec<hir::Statement>,
}

impl Function {
    fn infer(&mut self, declarations: &declarations::Declarations) -> Result<(), SemanticError> {
        let mut inferring = true;
        while inferring {
            inferring = false;
            for statement in &mut self.body {
                let has_changed_var = statement.infer(self, declarations)?;
                inferring |= has_changed_var;
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct Builder<'a> {
    declarations: &'a declarations::Declarations,
    top_level_scope: parser::scope::Id,
    return_type: declarations::Id,
    variables: HashMap<VariableId, Option<declarations::Id>>,
    local_scopes: Vec<HashMap<parser::Ident, Variable>>,
    new_variable_index: usize,
    body: &'a [parser::Statement],
}

impl<'a> Builder<'a> {
    pub fn new(
        declarations: &'a Declarations,
        function: &'a function::Function,
        parameters: Vec<(parser::Ident, Variable, declarations::Id)>,
    ) -> Self {
        let mut variables = HashMap::new();
        let mut scope = HashMap::new();

        for (ident, variable, type_id) in parameters {
            variables.insert(variable.into(), Some(type_id));
            scope.insert(ident, variable);
        }

        Self {
            declarations,
            top_level_scope: function.scope,
            variables,
            new_variable_index: parameters.len(),
            return_type: function.return_type,
            local_scopes: vec![scope],
            body: &function.body,
        }
    }

    pub fn build(mut self) -> Result<Function, SemanticError> {
        let body = self
            .body
            .into_iter()
            .map(|statement| self.statement(statement))
            .collect::<Result<_, _>>()?;

        Ok(Function {
            return_type: self.return_type,
            variables: self.variables,
            body,
        })
    }

    fn create_variable(&mut self) -> Variable {
        let variable = Variable::new(self.new_variable_index);
        self.new_variable_index += 1;
        variable
    }

    pub fn statement(
        &mut self,
        statement: &parser::Statement,
    ) -> Result<hir::Statement, SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(left, right)) => Ok(
                hir::Statement::Assignment(self.expression(left)?, self.expression(right)?),
            ),
            parser::Statement::Let(ident, expression) => {
                let variable = self.create_variable();
                self.local_scopes
                    .last_mut()
                    .expect("Must always have a scope")
                    .insert(ident.clone(), variable);

                Ok(hir::Statement::Let(variable, self.expression(expression)?))
            }
            parser::Statement::Expression(expression) => {
                Ok(hir::Statement::Ignore(self.expression(expression)?))
            }
        }
    }

    pub fn lookup(&self, ident: &parser::Ident) -> Result<Expression, SemanticError> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(variable) = scope.get(ident) {
                return Ok(Expression::LocalAccess((*variable).into()));
            }
        }

        self.declarations
            .lookup(&ident, self.top_level_scope)
            .map(Expression::GlobalAccess)
            .ok_or(SemanticError::DeclarationNotFound)
    }

    pub fn expression(
        &mut self,
        expression: &parser::Expression,
    ) -> Result<hir::TypedExpression, SemanticError> {
        match expression {
            parser::Expression::Literal(literal) => match literal {
                Literal::Integer(int) => Ok(Expression::IntegerConst(*int).into()),
                Literal::Float(float) => Ok(Expression::FloatConst(*float).into()),
                _ => todo!("non-numeric literal"),
            },
            parser::Expression::IntrinsicCall(intrinsic) => match intrinsic {
                IntrinsicCall::Binary(left, right, operator) => {
                    Ok(Expression::BinaryIntrinsic(Box::new(BinaryIntrinsic {
                        left: self.expression(left)?,
                        right: self.expression(right)?,
                        operator: *operator,
                    }))
                    .into())
                }
                IntrinsicCall::AssertType(expression, r#type) => {
                    let type_id = self
                        .declarations
                        .lookup(&r#type.ident(), self.top_level_scope)
                        .ok_or(SemanticError::DeclarationNotFound)?;

                    self.expression(&expression)
                        .map(|expression| expression.with_type(type_id))
                }
                IntrinsicCall::MutablePointer(expression) => {
                    Ok(Expression::MutablePointer(Box::new(self.expression(expression)?)).into())
                }
            },
            parser::Expression::Return(expression) => Ok(hir::Expression::Return(Box::new(
                self.expression(expression)?.with_type(self.return_type),
            ))
            .into()),
            parser::Expression::Ident(ident) => Ok(self.lookup(&ident)?.into()),
            parser::Expression::Call(call) => Ok(hir::Expression::Call(Box::new(hir::Call {
                callable: self.expression(&call.callable)?,
                arguments: call
                    .arguments
                    .into_iter()
                    .map(|argument| self.expression(&argument))
                    .map_ok(super::TypedExpression::from)
                    .collect::<Result<_, _>>()?,
            }))
            .into()),
            parser::Expression::If(If {
                condition,
                then_branch,
                else_branch,
            }) => Ok(Expression::If(Box::new(hir::If {
                condition: self.expression(condition)?,
                then_branch: then_branch
                    .into_iter()
                    .map(|statement| self.statement(statement))
                    .collect::<Result<_, _>>()?,
                else_branch: else_branch
                    .into_iter()
                    .flatten()
                    .map(|statement| self.statement(statement))
                    .collect::<Result<_, _>>()?,
            }))
            .into()),
            parser::Expression::Constructor(parser::expression::Constructor { r#type, fields }) => {
                let type_id = self
                    .declarations
                    .lookup(
                        &match r#type {
                            parser::Type::Ident(ident) => ident.clone(),
                        },
                        self.top_level_scope,
                    )
                    .ok_or(SemanticError::DeclarationNotFound)?;

                let Layout::Struct {
                    fields: layout_fields,
                    ..
                } = self.declarations.get_layout(type_id)
                else {
                    return Err(SemanticError::InvalidConstructor);
                };

                // TODO: Make sure all fields are present
                Ok(TypedExpression::new(
                    Expression::Constructor(hir::Constructor(
                        fields
                            .into_iter()
                            .map(|(ident, expression)| {
                                let field = *layout_fields
                                    .get(&ident)
                                    .ok_or(SemanticError::NonExistentField)?;

                                Ok((
                                    field.offset,
                                    self.expression(expression)?.with_type(field.type_id),
                                ))
                            })
                            .collect::<Result<_, SemanticError>>()?,
                    )),
                    Some(type_id),
                ))
            }
            parser::Expression::FieldAccess(expression, field) => Ok(Expression::FieldAccess(
                Box::new(self.expression(expression)?),
                field.clone(),
            )
            .into()),
            parser::Expression::Binary(_) => todo!(),
            parser::Expression::UnaryPrefix(_, _) => todo!(),
        }
    }
}
