use super::TypedExpression;
use crate::declarations::Declarations;
use crate::hir::{BinaryIntrinsic, Expression};
use crate::layout::Layout;
use crate::{declarations, function, hir, SemanticError};
use cranelift::prelude::*;
use itertools::Itertools;
use parser::expression::control_flow::If;
use parser::expression::IntrinsicCall;
use std::collections::HashMap;
use tokenizer::Spanned;

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

#[derive(Clone)]
pub struct Builder<'a> {
    declarations: &'a declarations::Declarations,
    top_level_scope: parser::scope::Id,
    return_type: declarations::Id,
    variables: HashMap<VariableId, Option<declarations::Id>>,
    local_scopes: Vec<HashMap<String, Variable>>,
    new_variable_index: usize,
    body: &'a [Spanned<parser::Statement>],
}

impl<'a> Builder<'a> {
    pub fn new(
        declarations: &'a Declarations,
        function: &'a function::Internal,
        parameters: Vec<(parser::Ident, Variable, declarations::Id)>,
    ) -> Self {
        let mut variables = HashMap::new();
        let mut scope = HashMap::new();
        let new_variable_index = parameters.len() + crate::function::SPECIAL_VARIABLES.len();
        for (ident, variable, type_id) in parameters {
            variables.insert(variable.into(), Some(type_id));
            scope.insert(ident.0, variable);
        }

        Self {
            declarations,
            top_level_scope: function.scope_id,
            variables,
            new_variable_index,
            return_type: function.signature.return_type,
            local_scopes: vec![scope],
            body: &function.body,
        }
    }

    pub fn build(mut self) -> Result<Function, SemanticError> {
        let body = self
            .body
            .iter()
            .map(|statement| self.statement(&statement.value))
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
            parser::Statement::Assignment(parser::Assignment { left, right }) => {
                Ok(hir::Statement::Assignment(hir::Assignment::new(
                    self.expression(&left.value)?,
                    self.expression(&right.value)?,
                )))
            }
            parser::Statement::Let(statement) => {
                let variable = self.create_variable();
                self.local_scopes
                    .last_mut()
                    .expect("Must always have a scope")
                    .insert(statement.ident.value.0, variable);
                self.variables.insert(variable.into(), None);

                Ok(hir::Statement::Let(
                    variable.into(),
                    self.expression(&statement.expression.value)?,
                ))
            }
            parser::Statement::Expression(expression) => {
                Ok(hir::Statement::Expression(self.expression(expression)?))
            }
        }
    }

    fn lookup(&self, ident: &Spanned<parser::Ident>) -> Result<Expression, SemanticError> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(variable) = scope.get(ident.value.as_ref()) {
                return Ok(Expression::LocalAccess((*variable).into()));
            }
        }

        self.declarations
            .lookup(ident.value.as_ref(), self.top_level_scope)
            .map(Expression::GlobalAccess)
            .ok_or_else(|| SemanticError::DeclarationNotFound(ident.clone()))
    }

    fn block(&mut self, statements: &[parser::Statement]) -> Result<hir::Block, SemanticError> {
        self.local_scopes.push(HashMap::new());

        let Some((last, statements)) = statements.split_last() else {
            return Ok(hir::Block {
                statements: Vec::new(),
                expression: None,
            });
        };

        let mut statements = statements
            .iter()
            .map(|statement| self.statement(statement))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        let expression = if let parser::Statement::Expression(expression) = last {
            Some(self.expression(&expression)?)
        } else {
            statements.push(self.statement(&last)?);
            None
        };

        self.local_scopes.pop();

        Ok(hir::Block {
            statements,
            expression,
        })
    }

    fn constructor(
        &mut self,
        constructor: &parser::expression::Constructor,
    ) -> Result<TypedExpression, SemanticError> {
        let type_id = self.lookup_type(&constructor.r#type.value)?;

        let Layout::Struct(layout) = self.declarations.get_layout(type_id) else {
            return Err(SemanticError::InvalidConstructor);
        };

        // TODO: Make sure all fields are present
        Ok(TypedExpression::new(
            Expression::Constructor(hir::Constructor(
                constructor
                    .fields
                    .iter()
                    .map(|(ident, expression)| {
                        layout
                            .fields
                            .get(ident.value.as_ref())
                            .ok_or(SemanticError::NonExistentField)
                            .and_then(|field| {
                                self.expression(&expression.value).map(|expression| {
                                    (field.offset, expression.with_type(field.type_id))
                                })
                            })
                    })
                    .collect::<Result<_, SemanticError>>()?,
            )),
            Some(type_id),
        ))
    }

    fn lookup_type(&self, path: Spanned<&parser::Type>) -> Result<declarations::Id, SemanticError> {
        let ident = match path.value {
            parser::Type::Ident(ident) => ident,
        };
        self.declarations
            .lookup(ident.as_ref(), self.top_level_scope)
            .ok_or_else(|| SemanticError::DeclarationNotFound(ident.clone()))
    }

    fn expression(
        &mut self,
        expression: Spanned<&parser::Expression>,
    ) -> Result<hir::TypedExpression, SemanticError> {
        match expression.value {
            parser::Expression::Literal(literal) => match literal {
                parser::Literal::Integer(int) => Ok(Expression::IntegerConst(*int).into()),
                parser::Literal::Float(float) => Ok(Expression::FloatConst(*float).into()),
                parser::Literal::String(string) => {
                    Ok(Expression::StringConst(string.clone()).into())
                }
                parser::Literal::Char(_) => todo!("char literals"),
            },
            parser::Expression::IntrinsicCall(intrinsic) => match intrinsic {
                IntrinsicCall::Binary(left, right, operator) => {
                    Ok(Expression::BinaryIntrinsic(Box::new(BinaryIntrinsic {
                        left: self.expression(&left.value)?,
                        right: self.expression(&right.value)?,
                        operator: *operator,
                    }))
                    .into())
                }
                IntrinsicCall::AssertType(expression, r#type) => {
                    let type_id = self.lookup_type(&r#type.value)?;

                    self.expression(&expression.value)
                        .map(|expression| expression.with_type(type_id))
                }
                IntrinsicCall::MutablePointer(expression) => Ok(Expression::MutablePointer(
                    Box::new(self.expression(&expression.value)?),
                )
                .into()),
                IntrinsicCall::Deref(expression) => {
                    Ok(Expression::Deref(Box::new(self.expression(&expression.value)?)).into())
                }
            },
            parser::Expression::Return(expression) => Ok(hir::Expression::Return(Box::new(
                self.expression(&expression.expression.value)?
                    .with_type(self.return_type),
            ))
            .into()),
            parser::Expression::Ident(ident) => Ok(self.lookup(ident)?.into()),
            parser::Expression::Call(call) => Ok(hir::Expression::Call(Box::new(hir::Call {
                callable: self.expression(&call.callable.value)?,
                arguments: call
                    .arguments
                    .iter()
                    .map(|argument| self.expression(&argument.value))
                    .map_ok(super::TypedExpression::from)
                    .collect::<Result<_, _>>()?,
            }))
            .into()),
            parser::Expression::If(If {
                condition,
                then_branch,
                else_branch,
            }) => Ok(Expression::If(Box::new(hir::If {
                condition: self.expression(&condition.value)?,
                then_branch: self.block(
                    then_branch
                        .iter()
                        .map(|value| value.value)
                        .collect_vec()
                        .as_slice(),
                )?,
                else_branch: self.block(else_branch.as_ref().map_or(&[], |else_branch| {
                    else_branch
                        .iter()
                        .map(|value| value.value)
                        .collect_vec()
                        .as_slice()
                }))?,
            }))
            .into()),
            parser::Expression::Constructor(constructor) => self.constructor(constructor),
            parser::Expression::FieldAccess(field_access) => {
                Ok(Expression::FieldAccess(Box::new(hir::FieldAccess {
                    expression: self.expression(&field_access.expression.value)?,
                    field: field_access.ident.value,
                }))
                .into())
            }
            parser::Expression::Binary(_) => todo!("binary"),
            parser::Expression::UnaryPrefix(_) => todo!("unary"),
        }
    }
}
