use crate::hir::Expression;
use crate::layout::{Layout, Primitive};
use crate::{declarations, hir, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::control_flow::If;
use parser::expression::{Call, Constructor, IntrinsicCall};
use parser::prelude::Literal;
use std::collections::HashMap;

pub struct FunctionBuilder<'a, M> {
    pub declarations: &'a declarations::Declarations,
    pub module: &'a mut M,
    pub scope: parser::scope::Id,
    pub r#return: declarations::Id,
    pub variables: HashMap<Variable, Option<declarations::Id>>,
    pub scopes: Vec<HashMap<parser::Ident, Variable>>,
    pub builder: cranelift::prelude::FunctionBuilder<'a>,
    pub new_variable_index: usize,
}

impl<'a, M> FunctionBuilder<'a, M>
where
    M: Module,
{
    fn create_variable(&mut self) -> Variable {
        let variable = Variable::new(self.new_variable_index);
        self.new_variable_index += 1;
        variable
    }

    pub fn translate_statement(
        &mut self,
        statement: parser::Statement,
    ) -> Result<hir::Statement, SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(left, right)) => Ok(
                hir::Statement::Assignment(self.expression(left)?, self.expression(right)?),
            ),
            parser::Statement::Let(ident, expression) => {
                Ok(hir::Statement::Let(ident, self.expression(expression)?))
            }
            parser::Statement::Expression(expression) => {
                Ok(hir::Statement::Ignore(self.expression(expression)?))
            }
        }
    }

    pub fn handle_statement(&mut self, statement: parser::Statement) -> Result<(), SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                match name {
                    parser::Expression::Ident(name) => {
                        let (variable, r#type) = *self
                            .names
                            .get(&name)
                            .ok_or(SemanticError::DeclarationNotFound)?;

                        let value = self.expression(expression, r#type)?;
                        if r#type != value.r#type(self.declarations) {
                            return Err(SemanticError::InvalidAssignment);
                        };
                        // TODO: inference
                        let value = value.unwrap(r#type.unwrap(), self)?;

                        self.builder.def_var(variable, value);
                    }
                    parser::Expression::FieldAccess(left, field) => {
                        let left = self.expression(*left, None)?;
                        let struct_id = left
                            .r#type(self.declarations)
                            .ok_or(SemanticError::UnknownType)?;
                        let pointer = left.unwrap(struct_id, self)?;

                        let fields = match self.declarations.get_layout(struct_id) {
                            Layout::Struct { fields, .. } => fields,
                            Layout::Primitive(Primitive::MutablePointer(inner_type)) => {
                                let layout = self.declarations.get_layout(*inner_type);
                                // TODO: what about if recursive?!
                                dbg!(layout);
                                let Layout::Struct { fields, .. } = layout else {
                                    return Err(SemanticError::NonStructFieldAccess);
                                };
                                fields
                            }
                            Layout::Primitive(_) => {
                                return Err(SemanticError::NonStructFieldAccess)
                            }
                        };

                        let field = fields.get(&field).ok_or(SemanticError::NonExistentField)?;

                        let value = self
                            .expression(expression, Some(field.r#type))?
                            .unwrap(field.r#type, self)?;

                        self.builder
                            .ins()
                            .store(MemFlags::new(), value, pointer, field.offset);
                    }
                    _ => todo!("assign to arbitirary expression"),
                }
            }
            parser::Statement::Let(ident, expression) => {
                let value = self.expression(expression, None)?;
                let variable = self.create_variable();

                let layout = match value.r#type(self.declarations) {
                    Some(r#type) => Some(self.declarations.get_layout(r#type).clone()),
                    None => None,
                };

                let cranelift_type = layout.map_or_else(
                    || todo!("type inference"),
                    |layout| layout.cranelift_type(&self.declarations.isa),
                );

                self.builder.declare_var(variable, cranelift_type);
                self.names
                    .insert(ident, (variable, value.r#type(self.declarations)));

                // TODO: `.unwrap()`
                let r#type = value.r#type(self.declarations).unwrap();
                let value = value.unwrap(r#type, self)?;

                self.builder.def_var(variable, value);
            }
            parser::Statement::Expression(expression) => {
                // TODO: add(return 1, 2)
                if let parser::Expression::Return(expression) = expression {
                    let value = self
                        .expression(*expression, Some(self.r#return))?
                        .unwrap(self.r#return, self)?;
                    let layout = self.declarations.get_layout(self.r#return);
                    if layout.is_aggregate() {
                        let dest = self
                            .builder
                            .use_var(Variable::new(crate::function::AGGREGATE_PARAM_VARIABLE));

                        let addr = self.builder.ins().iconst(
                            self.declarations.isa.pointer_type(),
                            i64::from(layout.size(&self.declarations.isa)),
                        );

                        self.builder.call_memcpy(
                            self.declarations.isa.frontend_config(),
                            dest,
                            value,
                            addr,
                        );

                        let dest = self
                            .builder
                            .use_var(Variable::new(crate::function::AGGREGATE_PARAM_VARIABLE));

                        self.builder.ins().return_(&[dest]);
                    } else {
                        self.builder.ins().return_(&[value]);
                    }
                } else {
                    self.expression(expression, None)?;
                }
            }
        };

        Ok(())
    }

    fn intrinsic_call(&mut self, intrinsic: IntrinsicCall) -> Result<Expression, SemanticError> {
        match intrinsic {
            IntrinsicCall::Binary(left, right, operator) => {
                let mut left = self.expression(*left, None)?;
                let right = self.expression(*right, left.r#type(self.declarations))?;
                if let Some(r#type) = right.r#type(self.declarations) {
                    // TODO: `.clone()`
                    left = Expression::Cranelift(left.unwrap(r#type, self)?, r#type);
                }
                Ok(Expression::Binary(
                    Box::new(left),
                    Box::new(right),
                    operator,
                ))
            }
            IntrinsicCall::AssertType(expression, r#type) => {
                let r#type = self
                    .declarations
                    .lookup(
                        &match r#type {
                            parser::Type::Ident(identifier) => identifier,
                        },
                        self.scope,
                    )
                    .ok_or(SemanticError::DeclarationNotFound)?;
                let value = self.expression(*expression, Some(r#type))?;
                Ok(Expression::Cranelift(value.unwrap(r#type, self)?, r#type))
            }
            IntrinsicCall::MutablePointer(expression) => {
                let value = self.expression(*expression, None)?;
                Ok(Expression::MutablePointer(Box::new(value)))
            }
        }
    }

    fn call(
        &mut self,
        Call {
            callable,
            arguments,
            ..
        }: Call,
    ) -> Result<Expression, SemanticError> {
        // TODO: what about if not ident
        let callable = match callable.as_ref() {
            parser::Expression::Ident(ident) => ident,
            _ => todo!(),
        };

        let function = self
            .declarations
            .lookup(callable, self.scope)
            .ok_or(SemanticError::DeclarationNotFound)?;
        let function = self.declarations.get_function(function)?;

        if arguments.len() != function.parameters.len() {
            return Err(SemanticError::InvalidNumberOfArguments);
        }

        let mut arguments = arguments
            .into_iter()
            .zip(&function.parameters)
            .map(|(expression, (_, r#type))| {
                self.expression(expression, Some(*r#type))
                    .and_then(|value| value.unwrap(*r#type, self))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let func_ref = self
            .module
            .declare_func_in_func(function.id, self.builder.func);

        if matches!(
            self.declarations.get_layout(function.r#return),
            Layout::Struct { .. }
        ) {
            let stack_slot = self
                .builder
                .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, 12));

            let addr = self.builder.ins().stack_addr(
                self.declarations.isa.pointer_type(),
                stack_slot,
                Offset32::new(0),
            );

            arguments.push(addr);
        }

        let call = self.builder.ins().call(func_ref, arguments.as_slice());
        let value = self.builder.inst_results(call)[0];

        Ok(Expression::Cranelift(value, function.r#return))
    }

    pub fn lookup(&self, ident: &parser::Ident) -> Result<Variable, SemanticError> {
        for scope in self.scopes.iter().rev() {
            if let Some(variable) = scope.get(ident) {
                return Ok(*variable);
            }
        }
        Err(SemanticError::DeclarationNotFound)
    }

    pub fn expression(
        &mut self,
        expression: parser::Expression,
    ) -> Result<hir::Expression, SemanticError> {
        match expression {
            parser::Expression::Literal(literal) => match literal {
                Literal::Integer(integer) => Ok(Expression::IntegerConst(int)),
                Literal::Float(float) => Ok(Expression::FloatConst(float)),
                _ => todo!(),
            },
            parser::Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            parser::Expression::Return(expression) => Ok(hir::Expression::Return(Box::new(
                self.expression(*expression)?,
            ))),
            parser::Expression::Ident(ident) => {
                Ok(hir::Expression::VariableAccess(self.lookup(&ident)?))
            }
            parser::Expression::Call(call) => Ok(hir::Expression::Call(Box::new(hir::Call {
                callable: self.expression(*call.callable)?,
                arguments: call
                    .arguments
                    .into_iter()
                    .map(|argument| self.expression(argument))
                    .collect::<Result<_, _>>()?,
            }))),
            parser::Expression::If(If {
                condition,
                then_branch,
                else_branch,
            }) => Ok(Expression::If(Box::new(hir::If {
                condition: self.expression(*condition)?,
                then_branch: then_branch
                    .into_iter()
                    .map(|statement| self.translate_statement(statement))
                    .collect::<Result<_, _>>()?,
                else_branch: else_branch
                    .into_iter()
                    .flatten()
                    .map(|statement| self.translate_statement(statement))
                    .collect::<Result<_, _>>()?,
            }))),
            parser::Expression::Constructor(Constructor { r#type, fields }) => {
                let type_id = self
                    .declarations
                    .lookup(
                        &match r#type {
                            parser::Type::Ident(ident) => ident,
                        },
                        self.scope,
                    )
                    .ok_or(SemanticError::DeclarationNotFound)?;

                let Layout::Struct {
                    fields: layout_fields,
                    ..
                } = self.declarations.get_layout(type_id)
                else {
                    return Err(SemanticError::InvalidConstructor);
                };

                Ok(Expression::Constructor {
                    type_id,
                    fields: fields
                        .into_iter()
                        .map(|(ident, expression)| {
                            let value = self.expression(
                                *expression,
                                Some(
                                    layout_fields
                                        .iter()
                                        .find(|(name, _)| *name == &ident)
                                        .ok_or(SemanticError::NonExistentField)?
                                        .1
                                        .r#type,
                                ),
                            )?;
                            Ok((ident, value))
                        })
                        .collect::<Result<_, SemanticError>>()?,
                })
            }
            parser::Expression::FieldAccess(expression, field) => Ok(Expression::FieldAccess(
                Box::new(self.expression(*expression, None)?),
                field,
            )),
            parser::Expression::Binary(_) => todo!(),
            parser::Expression::UnaryPrefix(_) => todo!(),
        }
    }
}
