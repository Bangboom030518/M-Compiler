use super::{Store, TypedExpression};
use crate::declarations::{Declarations, GenericArgument, Length, ScopeId, TypeReference};
use crate::hir::{BinaryIntrinsic, Expression};
use crate::layout::Layout;
use crate::{declarations, function, hir, SemanticError};
use cranelift::prelude::*;
use itertools::Itertools;
use parser::expression::control_flow::If;
use parser::expression::IntrinsicCall;
use std::collections::HashMap;
use tokenizer::{AsSpanned, Spanned};

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
    pub return_type: declarations::TypeReference,
    pub variables: HashMap<VariableId, Option<TypeReference>>,
    pub body: Vec<hir::Statement>,
}

pub struct Builder<'a> {
    declarations: &'a mut declarations::Declarations,
    top_level_scope: ScopeId,
    return_type: TypeReference,
    variables: HashMap<VariableId, Option<TypeReference>>,
    local_scopes: Vec<HashMap<String, Variable>>,
    new_variable_index: usize,
    body: &'a [Spanned<parser::Statement>],
    generics: &'a [GenericArgument], // generic_arguments: HashMap<String, GenericArgument>,
}

impl<'a> Builder<'a> {
    pub fn new(
        declarations: &'a mut Declarations,
        function: &'a function::Internal,
        parameters: Vec<(Spanned<parser::Ident>, Variable, TypeReference)>,
    ) -> Self {
        dbg!();
        let mut variables = HashMap::new();
        let mut scope = HashMap::new();
        let new_variable_index = parameters.len() + crate::function::SPECIAL_VARIABLES.len();
        for (ident, variable, type_id) in parameters {
            variables.insert(variable.into(), Some(type_id));
            scope.insert(ident.value.0, variable);
        }

        Self {
            declarations,
            top_level_scope: function.scope_id,
            variables,
            new_variable_index,
            return_type: function.signature.return_type.clone(),
            local_scopes: vec![scope],
            body: &function.body,
            generics: &function.generics,
            // generic_arguments: function.generic_arguments.clone(),
        }
    }

    pub fn build(mut self) -> Result<Function, SemanticError> {
        dbg!();
        let body = self
            .body
            .iter()
            .map(|statement| self.statement(statement.as_ref()))
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
        statement: Spanned<&parser::Statement>,
    ) -> Result<hir::Statement, SemanticError> {
        match statement.value {
            parser::Statement::Assignment(parser::Assignment { left, right }) => {
                Ok(hir::Statement::Assignment(hir::Assignment::new(
                    self.expression(left.as_ref())?,
                    self.expression(right.as_ref())?,
                )))
            }
            parser::Statement::Let(statement) => {
                let variable = self.create_variable();
                self.local_scopes
                    .last_mut()
                    .expect("Must always have a scope")
                    .insert(statement.ident.value.0.clone(), variable);
                self.variables.insert(variable.into(), None);

                Ok(hir::Statement::Let(
                    variable.into(),
                    self.expression(statement.expression.as_ref())?,
                ))
            }
            parser::Statement::Expression(expression) => Ok(hir::Statement::Expression(
                self.expression(expression.spanned(statement.span))?,
            )),
        }
    }

    fn lookup(&self, ident: Spanned<&parser::Ident>) -> Result<Expression, SemanticError> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(variable) = scope.get(ident.value.as_ref()) {
                return Ok(Expression::LocalAccess((*variable).into()));
            }
        }

        self.declarations
            .lookup(ident.value.as_ref(), self.top_level_scope)
            .map(Expression::GlobalAccess)
            .ok_or_else(|| SemanticError::DeclarationNotFound(ident.map(Clone::clone)))
    }

    fn block(
        &mut self,
        statements: &[Spanned<parser::Statement>],
    ) -> Result<hir::Block, SemanticError> {
        self.local_scopes.push(HashMap::new());

        let Some((last, statements)) = statements.split_last() else {
            return Ok(hir::Block {
                statements: Vec::new(),
                expression: None,
            });
        };

        let mut statements = statements
            .iter()
            .map(|statement| self.statement(statement.as_ref()))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        let expression = if let parser::Statement::Expression(expression) = &last.value {
            Some(self.expression(expression.spanned(last.span.clone()))?)
        } else {
            statements.push(self.statement(last.as_ref())?);
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
        let type_ref = self
            .declarations
            .lookup_type(&constructor.r#type.value, self.top_level_scope)?;

        let Layout::Struct(layout) = self.declarations.insert_layout(&type_ref)?
        else {
            return Err(SemanticError::InvalidConstructor);
        };

        // TODO: Make sure all fields are present
        Ok(Expression::Constructor(hir::Constructor(
            constructor
                .fields
                .iter()
                .map(|(ident, expression)| {
                    layout
                        .fields
                        .get(ident.value.as_ref())
                        .ok_or(SemanticError::NonExistentField)
                        .and_then(|field| {
                            self.expression(expression.as_ref()).map(|expression| {
                                (field.offset, expression.with_type(field.type_id.clone()))
                            })
                        })
                })
                .collect::<Result<_, SemanticError>>()?,
        ))
        .with_type(type_ref))
    }

    // fn resolve_generic_argument(
    //     &mut self,
    //     argument: parser::GenericArgument,
    // ) -> Result<GenericArgument, SemanticError> {
    //     let ty = match argument {
    //         parser::GenericArgument::Literal(value) => {
    //             return Ok(GenericArgument::Length(Length::Literal(value)));
    //         }
    //         parser::GenericArgument::Type(ty) => ty,
    //     };

    //     if let Some(value) = self.generic_arguments.get(&ty.name.value.0) {
    //         return if ty.generics.value.0.is_empty() {
    //             Ok(value.clone())
    //         } else {
    //             Err(SemanticError::UnexpectedGenerics)
    //         };
    //     };

    //     if let Some(id) = self
    //         .declarations
    //         .lookup(&ty.name.value.0, self.top_level_scope)
    //     {
    //         return self
    //             .declarations
    //             .resolve_generics(&ty.generics.value.0, self.top_level_scope)
    //             .map(|generics| GenericArgument::Type(TypeReference { generics, id }));
    //     }

    //     Err(SemanticError::DeclarationNotFound(ty.name))
    // }

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
                        left: self.expression(left.as_ref().as_ref())?,
                        right: self.expression(right.as_ref().as_ref())?,
                        operator: *operator,
                    }))
                    .into())
                }
                IntrinsicCall::AssertType(expression, r#type) => {
                    let type_id = self
                        .declarations
                        .lookup_type(&r#type.value, self.top_level_scope)?;

                    self.expression(expression.as_ref().as_ref())
                        .map(|expression| expression.with_type(type_id))
                }
                IntrinsicCall::Addr(expression) => Ok(Expression::Addr(Box::new(
                    self.expression(expression.as_ref().as_ref())?,
                ))
                .into()),
                IntrinsicCall::Load(expression) => Ok(Expression::Load(Box::new(
                    self.expression(expression.as_ref().as_ref())?,
                ))
                .into()),
                IntrinsicCall::Store {
                    pointer,
                    expression,
                } => Ok(Expression::Store(Box::new(Store {
                    pointer: self.expression(pointer.as_ref().as_ref())?,
                    expression: self.expression(expression.as_ref().as_ref())?,
                }))
                .into()),
            },
            parser::Expression::Return(expression) => Ok(hir::Expression::Return(Box::new(
                self.expression(expression.expression.as_ref())?
                    .with_type(self.return_type.clone()),
            ))
            .into()),
            parser::Expression::Ident(ident) => {
                Ok(self.lookup(ident.spanned(expression.span))?.into())
            }
            parser::Expression::Call(call) => Ok(hir::Expression::Call(Box::new(hir::Call {
                callable: self.expression(call.callable.as_ref().as_ref())?,
                arguments: call
                    .arguments
                    .iter()
                    .map(|argument| self.expression(argument.as_ref()))
                    .map_ok(super::TypedExpression::from)
                    .collect::<Result<_, _>>()?,
            }))
            .into()),
            parser::Expression::If(If {
                condition,
                then_branch,
                else_branch,
            }) => Ok(Expression::If(Box::new(hir::If {
                condition: self.expression(condition.as_ref().as_ref())?,
                then_branch: self.block(then_branch.iter().cloned().collect_vec().as_slice())?,
                else_branch: self.block(
                    &else_branch.as_ref().map_or(Vec::new(), |else_branch| {
                        else_branch.iter().cloned().collect_vec()
                    }),
                )?,
            }))
            .into()),
            parser::Expression::Constructor(constructor) => self.constructor(constructor),
            parser::Expression::FieldAccess(field_access) => {
                Ok(Expression::FieldAccess(Box::new(hir::FieldAccess {
                    expression: self.expression(field_access.expression.as_ref())?,
                    field: field_access.ident.value.clone(),
                }))
                .into())
            }
            parser::Expression::Generixed(generixed) => {
                Ok(Expression::Generixed(Box::new(hir::Generixed {
                    expression: self.expression(generixed.expression.as_ref())?,
                    generics: self
                        .declarations
                        .resolve_generics(&generixed.generics.value.0, self.top_level_scope)?, // .value
                                                                                               // .0
                                                                                               // .iter()
                                                                                               // .enumerate()
                                                                                               // .map(|generic| match generic.1.value.clone() {
                                                                                               //     parser::GenericArgument::Literal(literal) => todo!(),
                                                                                               //     parser::GenericArgument::Type(_) => self.declarations.lookup(, self.top_level_scope)
                                                                                               // })
                                                                                               // .collect::<Result<_, _>>()?,
                }))
                .into())
            }
            parser::Expression::Binary(_) => todo!("binary"),
            parser::Expression::UnaryPrefix(_) => todo!("unary"),
        }
    }
}
