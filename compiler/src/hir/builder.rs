use super::{Store, Typed};
use crate::declarations::{Declarations, FuncReference, ScopeId, TypeReference};
use crate::hir::{BinaryIntrinsic, Expression};
use crate::layout::Layout;
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

#[derive(Clone)]
pub struct Function {
    pub return_type: declarations::TypeReference,
    pub variables: HashMap<VariableId, TypeReference>,
    pub body: Vec<hir::Statement>,
}

pub struct Builder<'a, M> {
    declarations: &'a mut declarations::Declarations,
    top_level_scope: ScopeId,
    return_type: TypeReference,
    variables: HashMap<VariableId, TypeReference>,
    local_scopes: Vec<HashMap<String, Variable>>,
    new_variable_index: usize,
    body: &'a [Spanned<parser::Statement>],
    module: &'a mut M,
}

impl<'a, M> Builder<'a, M>
where
    M: cranelift_module::Module,
{
    pub fn new(
        declarations: &'a mut Declarations,
        function: &'a function::Internal,
        parameters: Vec<(Spanned<parser::Ident>, Variable, TypeReference)>,
        module: &'a mut M,
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
            top_level_scope: function.scope_id,
            variables,
            new_variable_index,
            return_type: function.signature.return_type.clone(),
            local_scopes: vec![scope],
            body: &function.body,
            module,
        }
    }

    pub fn build(mut self) -> Result<Function, SemanticError> {
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
                let left = self.expression(left.as_ref())?;
                let right = self.expression(right.as_ref())?;
                self.declarations.check_expression_type(
                    &right,
                    &left.type_ref,
                    self.top_level_scope,
                )?;
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
                self.declarations.check_expression_type(
                    &expression,
                    &type_ref,
                    self.top_level_scope,
                )?;
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
        ident: Spanned<&parser::Ident>,
    ) -> Result<Typed<Expression>, SemanticError> {
        for scope in self.local_scopes.iter().rev() {
            if let Some(variable) = scope.get(ident.value.as_ref()).copied() {
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

        let global = self
            .declarations
            .lookup(ident.value.as_ref(), self.top_level_scope)
            .ok_or_else(|| SemanticError::DeclarationNotFound(ident.map(Clone::clone)))?;

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
        let type_ref = self
            .declarations
            .lookup_type(&constructor.r#type.value, self.top_level_scope)?;

        let Layout::Struct(layout) = self
            .declarations
            .insert_layout(&type_ref, self.top_level_scope)?
            .unwrap_or_else(|| todo!("uninitialised layout"))
        else {
            return Err(SemanticError::InvalidConstructor);
        };

        // TODO: Make sure all fields are present
        Ok(Typed::new(
            Expression::Constructor(hir::Constructor(
                constructor
                    .fields
                    .iter()
                    .map(|(ident, expression)| {
                        let field = layout
                            .fields
                            .get(&ident.value.0)
                            .ok_or(SemanticError::NonExistentField)?;

                        let expression = self.expression(expression.as_ref())?;
                        self.declarations.check_expression_type(
                            &expression,
                            &field.type_ref,
                            self.top_level_scope,
                        )?;
                        Ok((ident.value.0.clone(), expression))
                    })
                    .collect::<Result<_, SemanticError>>()?,
            )),
            type_ref,
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
                self.declarations.check_expression_type(
                    &right,
                    &left.type_ref,
                    self.top_level_scope,
                )?;
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
                let expected = self
                    .declarations
                    .lookup_type(&r#type.value, self.top_level_scope)?;

                let expression = self.expression(expression.as_ref().as_ref())?;
                self.declarations.check_expression_type(
                    &expression,
                    &expected,
                    self.top_level_scope,
                )?;
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
        // TODO: do we need `CallContext`?
        let call_context = function::CallContext {
            arguments: &arguments,
            call_expression: call_expression.clone(),
        };
        let func_reference = FuncReference {
            id: declaration,
            generics,
        };
        let signature = self
            .declarations
            .insert_function(
                func_reference,
                self.module,
                Some(call_context),
                self.top_level_scope,
            )?
            .signature()
            .clone();

        if signature.parameters.len() != call.arguments.len() {
            return Err(SemanticError::InvalidNumberOfArguments);
        }
        for (parameter, argument) in iter::zip(signature.parameters, &arguments) {
            self.declarations
                .check_expression_type(argument, &parameter, self.top_level_scope)?;
        }
        self.declarations.check_expression_type(
            &call_expression,
            &signature.return_type,
            self.top_level_scope,
        )?;
        Ok(call_expression)
    }

    fn expression(
        &mut self,
        expression: Spanned<&parser::Expression>,
    ) -> Result<Typed<Expression>, SemanticError> {
        match expression.value {
            parser::Expression::Literal(literal) => {
                let expression = match literal {
                    parser::Literal::Integer(int) => Expression::IntegerConst(*int),
                    parser::Literal::Float(float) => Expression::FloatConst(*float),
                    parser::Literal::String(string) => Expression::StringConst(string.clone()),
                    parser::Literal::Char(_) => todo!("char literals"),
                };
                Ok(Typed::new(expression, self.declarations.create_type_ref()))
            }
            parser::Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            parser::Expression::Return(expression) => {
                let inner = self.expression(expression.expression.as_ref())?;
                self.declarations.check_expression_type(
                    &inner,
                    &self.return_type,
                    self.top_level_scope,
                )?;
                Ok(hir::Expression::Return(Box::new(inner)).typed(self.declarations))
            }
            parser::Expression::Ident(ident) => self.lookup_ident(ident.spanned(expression.span)),
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
                    self.declarations.check_expression_type(
                        &expression,
                        &sub_expression.type_ref,
                        self.top_level_scope,
                    )?;
                }
                if let Some(sub_expression) = else_branch.expression {
                    self.declarations.check_expression_type(
                        &expression,
                        &sub_expression.type_ref,
                        self.top_level_scope,
                    )?;
                }
                Ok(expression)
            }
            parser::Expression::Constructor(constructor) => self.constructor(constructor),
            parser::Expression::FieldAccess(field_access) => {
                let expression = self.expression(field_access.expression.as_ref())?;
                let field = &field_access.ident.value;
                let field_access = Expression::FieldAccess(Box::new(hir::FieldAccess {
                    expression: expression.clone(),
                    field: field.clone(),
                }))
                .typed(self.declarations);
                let struct_layout = self
                    .declarations
                    .insert_layout(&expression.type_ref, self.top_level_scope)?;
                match struct_layout {
                    Some(layout) => match layout {
                        Layout::Struct(layout) => {
                            let fields = layout.fields;
                            let field = fields
                                .get(&field.0)
                                .ok_or(SemanticError::NonExistentField)?;

                            self.declarations.check_expression_type(
                                &field_access,
                                &field.type_ref,
                                self.top_level_scope,
                            )?;
                        }
                        layout => {
                            return Err(SemanticError::InvalidFieldAccess(layout));
                        }
                    },
                    None => todo!("lazy resolve structs"),
                }
                Ok(field_access)
            }
            parser::Expression::Generixed(generixed) => {
                Ok(Expression::Generixed(Box::new(hir::Generixed {
                    expression: self.expression(generixed.expression.as_ref())?,
                    generics: self
                        .declarations
                        .resolve_generics(&generixed.generics.value.0, self.top_level_scope)?,
                }))
                .typed(self.declarations))
            }
            parser::Expression::Binary(_) => todo!("binary"),
            parser::Expression::UnaryPrefix(_) => todo!("unary"),
        }
    }
}
