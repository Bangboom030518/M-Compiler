use super::{Store, Typed};
use crate::declarations::{Declarations, Reference, ScopeId};
use crate::hir::{BinaryIntrinsic, Expression};
use crate::{declarations, function, hir, SemanticError};
use cranelift::prelude::*;
use itertools::Itertools;
use parser::expression::control_flow::If;
use parser::expression::{IntrinsicCall, IntrinsicOperator};
use std::collections::HashMap;
use std::iter;
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

enum Constraint {
    StructField {
        field: String,
        expression: hir::Typed<hir::Expression>,
        struct_type: Reference,
    },
    ArrayLength {
        expected_length: u32,
        array_type: Reference,
    },
}

impl Constraint {
    /// Checks against constraint, returning a bool indicating whether or not the condition was
    /// able to be verified.
    fn check(&self, declarations: &mut Declarations) -> Result<bool, SemanticError> {
        match self {
            Self::StructField {
                struct_type,
                expression,
                field,
            } => {
                let struct_type = declarations.insert_layout(struct_type)?;
                let Some(struct_type) = struct_type else {
                    return Ok(false);
                };
                let struct_type = struct_type.expect_struct()?;
                let field = struct_type
                    .fields
                    .get(field)
                    .ok_or(SemanticError::NonExistentField)?;

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
                declarations.check_length(*expected_length, array_type.length)?;
            }
        };
        Ok(true)
    }
}

pub struct Builder<'a> {
    declarations: &'a mut declarations::Declarations,
    scope: ScopeId,
    return_type: Reference,
    variables: HashMap<VariableId, Reference>,
    local_scopes: Vec<HashMap<String, Variable>>,
    struct_constraints: Vec<Constraint>,
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
            struct_constraints: Vec::new(),
        }
    }

    pub fn build_body(mut self) -> Result<Vec<hir::Statement>, SemanticError> {
        let body = self
            .body
            .iter()
            .map(|statement| self.statement(statement.as_ref()))
            .collect::<Result<_, _>>()?;
        let mut constraints = self.struct_constraints;
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
    ) -> Result<hir::Statement, SemanticError> {
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
                let type_ref = self.declarations.create_type_ref();
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

    fn lookup_ident(
        &mut self,
        ident: &Spanned<parser::Ident>,
    ) -> Result<Typed<Expression>, SemanticError> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(variable) = scope.get(&ident.value.0).copied() {
                let variable = variable.into();
                let type_ref = self
                    .variables
                    .get(&variable)
                    .expect("variable doesn't exist");
                return Ok(Typed::new(
                    Expression::LocalAccess(variable),
                    type_ref.clone(),
                ));
            }
        }

        let global = self.declarations.lookup(ident, self.scope)?;
        Ok(Expression::GlobalAccess(global).typed(self.declarations))
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
    ) -> Result<hir::Typed<Expression>, SemanticError> {
        let struct_type = self
            .declarations
            .lookup_type(&constructor.r#type.value, self.scope)?;

        let fields = constructor
            .fields
            .iter()
            .map(|(ident, expression)| {
                let expression = self.expression(expression.as_ref())?;
                self.struct_constraints.push(Constraint::StructField {
                    struct_type: struct_type.clone(),
                    field: ident.value.0.clone(),
                    expression: expression.clone(),
                });
                Ok((ident.value.0.clone(), expression))
            })
            .collect::<Result<_, SemanticError>>()?;

        Ok(Typed::new(
            Expression::Constructor(hir::Constructor(fields)),
            struct_type,
        ))
    }

    fn intrinsic_call(
        &mut self,
        intrinsic: &IntrinsicCall,
    ) -> Result<Typed<Expression>, SemanticError> {
        match intrinsic {
            IntrinsicCall::Binary(left, right, operator) => {
                let left = self.expression(left.as_ref().as_ref())?;
                let right = self.expression(right.as_ref().as_ref())?;
                self.declarations
                    .check_expression_type(&right, &left.type_ref)?;
                let type_ref = if matches!(operator, IntrinsicOperator::Cmp(_)) {
                    self.declarations.create_type_ref()
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
                ))
            }
            IntrinsicCall::AssertType(expression, r#type) => {
                let expected = self.declarations.lookup_type(&r#type.value, self.scope)?;

                let expression = self.expression(expression.as_ref().as_ref())?;
                self.declarations
                    .check_expression_type(&expression, &expected)?;
                Ok(expression)
            }
            IntrinsicCall::Addr(expression) => Ok(Expression::Addr(Box::new(
                self.expression(expression.as_ref().as_ref())?,
            ))
            .typed(self.declarations)),
            IntrinsicCall::Load(expression) => Ok(Expression::Load(Box::new(
                self.expression(expression.as_ref().as_ref())?,
            ))
            .typed(self.declarations)),
            IntrinsicCall::Store {
                pointer,
                expression,
            } => Ok(Expression::Store(Box::new(Store {
                pointer: self.expression(pointer.as_ref().as_ref())?,
                expression: self.expression(expression.as_ref().as_ref())?,
            }))
            .typed(self.declarations)),
        }
    }

    fn call(
        &mut self,
        call: &parser::expression::Call,
    ) -> Result<Typed<Expression>, SemanticError> {
        let callable = self.expression(call.callable.as_ref().as_ref())?;
        let arguments: Vec<_> = call
            .arguments
            .iter()
            .map(|argument| self.expression(argument.as_ref()))
            .map_ok(super::Typed::<Expression>::from)
            .collect::<Result<_, _>>()?;
        let call_expression = hir::Expression::Call(Box::new(hir::Call {
            callable: callable.clone(),
            arguments: arguments.clone(),
        }))
        .typed(self.declarations);

        let (callable, generics) = if let hir::Expression::Generixed(generixed) = &callable.value {
            let hir::Generixed {
                expression,
                generics,
            } = generixed.as_ref();
            (expression.value.clone(), generics.clone())
        } else {
            (callable.value.clone(), Vec::new())
        };

        let hir::Expression::GlobalAccess(declaration) = callable else {
            todo!("func refs!")
        };
        let func_reference = Reference {
            id: declaration,
            generics,
        };
        let signature = &self
            .declarations
            .insert_function(&func_reference)?
            .signature()
            .clone();

        if signature.parameters.len() != call.arguments.len() {
            return Err(SemanticError::InvalidNumberOfArguments);
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
    ) -> Result<Typed<Expression>, SemanticError> {
        match expression.value {
            parser::Expression::Literal(literal) => match literal {
                parser::Literal::Integer(int) => {
                    Ok(Expression::IntegerConst(*int).typed(self.declarations))
                }
                parser::Literal::Float(float) => {
                    Ok(Expression::FloatConst(*float).typed(self.declarations))
                }
                parser::Literal::String(string) => {
                    let expression =
                        Expression::StringConst(string.clone()).typed(self.declarations);
                    self.struct_constraints.push(Constraint::ArrayLength {
                        expected_length: string.len() as u32,
                        array_type: expression.type_ref.clone(),
                    });
                    Ok(expression)
                }
                parser::Literal::Char(_) => todo!("char literals"),
            },
            parser::Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            parser::Expression::Return(expression) => {
                let inner = self.expression(expression.expression.as_ref())?;
                self.declarations
                    .check_expression_type(&inner, &self.return_type)?;
                Ok(hir::Expression::Return(Box::new(inner)).typed(self.declarations))
            }
            parser::Expression::Ident(ident) => {
                self.lookup_ident(&ident.clone().spanned(expression.span))
            }
            parser::Expression::Call(call) => self.call(call),
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
                .typed(self.declarations);
                if let Some(sub_expression) = then_branch.expression {
                    self.declarations
                        .check_expression_type(&expression, &sub_expression.type_ref)?;
                }
                if let Some(sub_expression) = else_branch.expression {
                    self.declarations
                        .check_expression_type(&expression, &sub_expression.type_ref)?;
                }
                Ok(expression)
            }
            parser::Expression::Constructor(constructor) => self.constructor(constructor),
            parser::Expression::FieldAccess(field_access) => {
                let struct_expression = self.expression(field_access.expression.as_ref())?;
                let field = &field_access.ident.value;
                let field_access = Expression::FieldAccess(Box::new(hir::FieldAccess {
                    expression: struct_expression.clone(),
                    field: field.clone(),
                }))
                .typed(self.declarations);
                self.struct_constraints.push(Constraint::StructField {
                    struct_type: struct_expression.type_ref,
                    field: field.0.clone(),
                    expression: field_access.clone(),
                });
                Ok(field_access)
            }
            parser::Expression::Generixed(generixed) => {
                Ok(Expression::Generixed(Box::new(hir::Generixed {
                    expression: self.expression(generixed.expression.as_ref())?,
                    generics: self
                        .declarations
                        .build_generics(&generixed.generics.value.0, self.scope)?,
                }))
                .typed(self.declarations))
            }
            parser::Expression::Binary(_) => todo!("binary"),
            parser::Expression::UnaryPrefix(_) => todo!("unary"),
        }
    }
}
