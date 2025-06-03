use super::{Store, Typed};
use crate::declarations::{Declarations, Reference, ScopeId};
use crate::hir::{BinaryIntrinsic, Expression};
use crate::{declarations, errors, function, hir, Error};
use cranelift::prelude::*;
use itertools::Itertools;
use parser::expression::control_flow::If;
use parser::expression::{IntrinsicCall, IntrinsicOperator};
use std::collections::HashMap;
use std::iter;
use tokenizer::{AsSpanned, Span, Spanned};

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

enum Constraint {
    StructField {
        field: Spanned<parser::Ident>,
        expression: hir::Typed<hir::Expression>,
        struct_type: Reference,
    },
    ArrayLength {
        expected_length: u32,
        array_type: Reference,
    },
    VoidExpression {
        expression: hir::Typed<hir::Expression>,
    },
}

impl Constraint {
    /// Checks against constraint, returning a bool indicating whether or not the condition was
    /// able to be verified.
    fn check(&self, declarations: &mut Declarations) -> Result<bool, Error> {
        match self {
            Self::StructField {
                struct_type: struct_type_ref,
                expression,
                field,
            } => {
                let struct_type = declarations.insert_layout(struct_type_ref)?;
                let Some(struct_type) = struct_type else {
                    return Ok(false);
                };
                let struct_type = struct_type.expect_struct()?;
                let field = struct_type
                    .fields
                    .get(&field.value.0)
                    .ok_or_else(|| Error {
                        span: field.span.clone(),
                        kind: errors::Kind::FieldNotFound {
                            parent_struct: struct_type_ref.clone(),
                            field: field.value.0.clone(),
                        },
                    })?;

                declarations.check_expression_type(expression, &field.type_ref)?;
            }
            Self::ArrayLength {
                expected_length,
                array_type,
            } => {
                let array_type = declarations.insert_layout(array_type)?;
                let Some(array_type) = array_type else {
                    return Ok(false);
                };
                let array_type = array_type.expect_array()?;
                declarations
                    .unresolved
                    .check_length(*expected_length, array_type.length)?;
            }
            Self::VoidExpression { expression } => {
                let void = declarations.unresolved.void();
                declarations.check_expression_type(expression, &void)?;
            }
        }
        Ok(true)
    }
}

pub struct Builder<'a> {
    declarations: &'a mut declarations::Declarations,
    scope: ScopeId,
    return_type: Reference,
    variables: HashMap<VariableId, Reference>,
    local_scopes: Vec<HashMap<String, Variable>>,
    constraints: Vec<Constraint>,
    new_variable_index: usize,
    body: &'a [Spanned<parser::Statement>],
}

impl<'a> Builder<'a> {
    pub fn new(
        declarations: &'a mut Declarations,
        function: &'a function::Internal,
        parameters: Vec<(Spanned<parser::Ident>, Variable, Reference)>,
    ) -> Self {
        let mut variables = HashMap::new();
        let mut scope = HashMap::new();
        let new_variable_index = parameters.len() + crate::function::SPECIAL_VARIABLES.len();
        for (ident, variable, type_ref) in parameters {
            variables.insert(variable.into(), type_ref);
            scope.insert(ident.value.0, variable);
        }

        Self {
            declarations,
            scope: function.signature.scope,
            variables,
            new_variable_index,
            return_type: function.signature.return_type.clone(),
            local_scopes: vec![scope],
            body: &function.body,
            constraints: Vec::new(),
        }
    }

    pub fn build_body(mut self) -> Result<Vec<hir::Statement>, Error> {
        let body = self
            .body
            .iter()
            .map(|statement| self.statement(statement.as_ref()))
            .collect::<Result<_, _>>()?;
        let mut constraints = self.constraints;
        while !constraints.is_empty() {
            let mut checked = Vec::with_capacity(constraints.len());
            for (index, constraint) in constraints.iter().enumerate() {
                if constraint.check(self.declarations)? {
                    checked.push(index);
                }
            }
            if checked.is_empty() {
                break;
            }
            for index in checked.into_iter().rev() {
                constraints.remove(index);
            }
        }

        Ok(body)
    }

    fn create_variable(&mut self) -> Variable {
        let variable = Variable::new(self.new_variable_index);
        self.new_variable_index += 1;
        variable
    }

    fn statement(
        &mut self,
        statement: Spanned<&parser::Statement>,
    ) -> Result<hir::Statement, Error> {
        match statement.value {
            parser::Statement::Assignment(parser::Assignment { left, right }) => {
                let left = self.expression(left.as_ref())?;
                let right = self.expression(right.as_ref())?;
                self.declarations
                    .check_expression_type(&right, &left.type_ref)?;
                Ok(hir::Statement::Assignment(hir::Assignment::new(
                    left, right,
                )))
            }
            parser::Statement::Let(statement) => {
                let variable = self.create_variable();
                let type_ref = self.declarations.unresolved.create_type_ref();
                self.local_scopes
                    .last_mut()
                    .expect("Must always have a scope")
                    .insert(statement.ident.value.0.clone(), variable);
                let expression = self.expression(statement.expression.as_ref())?;
                self.declarations
                    .check_expression_type(&expression, &type_ref)?;
                self.variables.insert(variable.into(), type_ref);
                Ok(hir::Statement::Let(variable.into(), expression))
            }
            parser::Statement::Expression(expression) => Ok(hir::Statement::Expression(
                self.expression(expression.spanned(statement.span))?,
            )),
        }
    }

    fn lookup_ident(&mut self, ident: &Spanned<parser::Ident>) -> Result<Typed<Expression>, Error> {
        for scope in self.local_scopes.iter().rev() {
            let Some(variable) = scope.get(&ident.value.0).copied() else {
                continue;
            };
            let variable = variable.into();
            let type_ref = self
                .variables
                .get(&variable)
                .expect("variable doesn't exist");
            return Ok(Typed::new(
                Expression::LocalAccess(variable),
                type_ref.clone(),
                ident.span.clone(),
            ));
        }

        let global = self.declarations.unresolved.lookup(ident, self.scope)?;
        Ok(Expression::GlobalAccess(global).typed(self.declarations, ident.span.clone()))
    }

    fn block(&mut self, statements: &[Spanned<parser::Statement>]) -> Result<hir::Block, Error> {
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
            .collect::<Result<Vec<_>, Error>>()?;

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
        span: Span,
    ) -> Result<hir::Typed<Expression>, Error> {
        let struct_type = self
            .declarations
            .unresolved
            .lookup_type(&constructor.r#type.value, self.scope)?;

        let fields = constructor
            .fields
            .iter()
            .map(|(ident, expression)| {
                let expression = self.expression(expression.as_ref())?;
                self.constraints.push(Constraint::StructField {
                    struct_type: struct_type.clone(),
                    field: ident.clone(),
                    expression: expression.clone(),
                });
                Ok((ident.value.0.clone(), expression))
            })
            .collect::<Result<_, Error>>()?;

        Ok(Typed::new(
            Expression::Constructor(hir::Constructor(fields)),
            struct_type,
            span,
        ))
    }

    fn intrinsic_call(
        &mut self,
        intrinsic: &IntrinsicCall,
        span: Span,
    ) -> Result<Typed<Expression>, Error> {
        match intrinsic {
            IntrinsicCall::True => Ok(Expression::BoolConst(true).typed(self.declarations, span)),
            IntrinsicCall::False => Ok(Expression::BoolConst(false).typed(self.declarations, span)),
            IntrinsicCall::Binary(left, right, operator) => {
                let left = self.expression(left.as_ref().as_ref())?;
                let right = self.expression(right.as_ref().as_ref())?;
                self.declarations
                    .check_expression_type(&right, &left.type_ref)?;
                let type_ref = if matches!(operator, IntrinsicOperator::Cmp(_)) {
                    self.declarations.unresolved.create_type_ref()
                } else {
                    left.type_ref.clone()
                };
                Ok(Typed::new(
                    Expression::BinaryIntrinsic(Box::new(BinaryIntrinsic {
                        left,
                        right,
                        operator: *operator,
                    })),
                    type_ref,
                    span,
                ))
            }
            IntrinsicCall::AssertType(expression, r#type) => {
                let expected = self
                    .declarations
                    .unresolved
                    .lookup_type(&r#type.value, self.scope)?;

                let expression = self.expression(expression.as_ref().as_ref())?;
                self.declarations
                    .check_expression_type(&expression, &expected)?;
                Ok(expression)
            }
            IntrinsicCall::Addr(expression) => Ok(Expression::Addr(Box::new(
                self.expression(expression.as_ref().as_ref())?,
            ))
            .typed(self.declarations, span)),

            IntrinsicCall::Load(expression) => Ok(Expression::Load(Box::new(
                self.expression(expression.as_ref().as_ref())?,
            ))
            .typed(self.declarations, span)),
            IntrinsicCall::SizeOf(r#type) => {
                let reference = self
                    .declarations
                    .unresolved
                    .lookup_type(&r#type.value, self.scope)?;

                Ok(Expression::SizeOf(reference).typed(self.declarations, span))
            }
            IntrinsicCall::Store {
                pointer,
                expression,
            } => Ok(Expression::Store(Box::new(Store {
                pointer: self.expression(pointer.as_ref().as_ref())?,
                expression: self.expression(expression.as_ref().as_ref())?,
            }))
            .typed(self.declarations, span)),
        }
    }

    fn call(
        &mut self,
        call: &parser::expression::Call,
        span: Span,
    ) -> Result<Typed<Expression>, Error> {
        let mut callable = self.expression(call.callable.as_ref().as_ref())?;
        let arguments: Vec<_> = call
            .arguments
            .iter()
            .map(|argument| self.expression(argument.as_ref()))
            .map_ok(super::Typed::<Expression>::from)
            .collect::<Result<_, _>>()?;

        let (declaration, generics) = match callable.value {
            hir::Expression::GlobalAccess(declaration) => {
                let generics = self.declarations.make_generic_arguments(declaration);
                let inner = callable.clone();
                callable.value = hir::Expression::Generixed(Box::new(hir::Generixed {
                    expression: inner,
                    generics: generics.clone(),
                }));
                (declaration, generics)
            }
            hir::Expression::Generixed(ref generixed) => {
                let hir::Expression::GlobalAccess(declaration) = generixed.expression.value else {
                    todo!("func ref");
                };
                (declaration, generixed.generics.clone())
            }
            _ => todo!("func ref"),
        };

        let call_expression = hir::Expression::Call(Box::new(hir::Call {
            callable,
            arguments: arguments.clone(),
        }))
        .typed(self.declarations, span);

        let func_reference = Reference {
            id: declaration,
            generics,
        };

        self.declarations.insert_function(&func_reference)?;
        let signature = self
            .declarations
            .get_function(&func_reference)
            .expect("function not inserted")
            .signature()
            .clone();

        if signature.parameters.len() != call.arguments.len() {
            return Err(Error {
                span: todo!(),
                kind: errors::Kind::MismatchedArguments {
                    arguments: call.arguments.len(),
                    parameters: signature.parameters.len(),
                    declaration: todo!(),
                },
            });
        }
        for (parameter, argument) in iter::zip(&signature.parameters, &arguments) {
            self.declarations
                .check_expression_type(argument, parameter)?;
        }

        self.declarations
            .check_expression_type(&call_expression, &signature.return_type)?;
        Ok(call_expression)
    }

    fn expression(
        &mut self,
        expression: Spanned<&parser::Expression>,
    ) -> Result<Typed<Expression>, Error> {
        match expression.value {
            parser::Expression::Literal(literal) => match literal {
                parser::Literal::Integer(int) => {
                    Ok(Expression::IntegerConst(*int).typed(self.declarations, expression.span))
                }
                parser::Literal::Float(float) => {
                    Ok(Expression::FloatConst(*float).typed(self.declarations, expression.span))
                }
                parser::Literal::String(string) => {
                    let expression = Expression::StringConst(string.clone())
                        .typed(self.declarations, expression.span);
                    self.constraints.push(Constraint::ArrayLength {
                        expected_length: string.len() as u32,
                        array_type: expression.type_ref.clone(),
                    });
                    Ok(expression)
                }
                parser::Literal::Char(_) => todo!("char literals"),
            },
            parser::Expression::IntrinsicCall(intrinsic) => {
                self.intrinsic_call(intrinsic, expression.span)
            }
            parser::Expression::Return(inner) => {
                let inner = self.expression(inner.expression.as_ref())?;
                self.declarations
                    .check_expression_type(&inner, &self.return_type)?;
                Ok(hir::Expression::Return(Box::new(inner))
                    .typed(self.declarations, expression.span))
            }
            parser::Expression::Ident(ident) => {
                self.lookup_ident(&ident.clone().spanned(expression.span))
            }
            parser::Expression::Call(call) => self.call(call, expression.span),
            parser::Expression::If(If {
                condition,
                then_branch,
                else_branch,
            }) => {
                let then_branch =
                    self.block(then_branch.iter().cloned().collect_vec().as_slice())?;
                let else_branch =
                    self.block(&else_branch.as_ref().map_or(Vec::new(), |else_branch| {
                        else_branch.iter().cloned().collect_vec()
                    }))?;
                let expression = Expression::If(Box::new(hir::If {
                    condition: self.expression(condition.as_ref().as_ref())?,
                    then_branch: then_branch.clone(),
                    else_branch: else_branch.clone(),
                }))
                .typed(self.declarations, expression.span);

                if let Some(sub_expression) = then_branch.expression {
                    self.declarations
                        .check_expression_type(&expression, &sub_expression.type_ref)?;
                } else {
                    self.constraints.push(Constraint::VoidExpression {
                        expression: expression.clone(),
                    });
                }

                if let Some(sub_expression) = else_branch.expression {
                    self.declarations
                        .check_expression_type(&expression, &sub_expression.type_ref)?;
                } else {
                    self.constraints.push(Constraint::VoidExpression {
                        expression: expression.clone(),
                    });
                }
                Ok(expression)
            }
            parser::Expression::Constructor(constructor) => {
                self.constructor(constructor, expression.span)
            }
            parser::Expression::FieldAccess(field_access) => {
                let struct_expression = self.expression(field_access.expression.as_ref())?;
                let field = &field_access.ident;
                let field_access = Expression::FieldAccess(Box::new(hir::FieldAccess {
                    expression: struct_expression.clone(),
                    field: field.clone(),
                }))
                .typed(self.declarations, expression.span);
                self.constraints.push(Constraint::StructField {
                    struct_type: struct_expression.type_ref,
                    field: field.clone(),
                    expression: field_access.clone(),
                });
                Ok(field_access)
            }
            parser::Expression::Generixed(generixed) => {
                Ok(Expression::Generixed(Box::new(hir::Generixed {
                    expression: self.expression(generixed.expression.as_ref())?,
                    generics: self
                        .declarations
                        .unresolved
                        .build_generics(&generixed.generics.value.0, self.scope)?,
                }))
                .typed(self.declarations, expression.span))
            }
            parser::Expression::Binary(_) => todo!("binary"),
            parser::Expression::UnaryPrefix(_) => todo!("unary"),
        }
    }
}
