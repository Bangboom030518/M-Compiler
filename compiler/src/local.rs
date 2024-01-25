use crate::declarations::{self, Declarations};
use crate::layout::{Layout, Primitive};
use crate::SemanticError;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::control_flow::If;
use parser::expression::{
    Call, CmpOperator, Constructor, IntrinsicCall, IntrinsicOperator, UnaryOperator,
};
use parser::prelude::Literal;
use parser::Expression;
use std::collections::HashMap;

// TODO: primitive as variant, remove `Cranelift` variant
#[derive(Debug, Clone)]
pub enum Value {
    Cranelift(cranelift::prelude::Value, declarations::Id),
    UnknownSignedIntegerConst(i128),
    UnknownIntegerConst(u128),
    UnknownFloatConst(f64),
    Binary(Box<Value>, Box<Value>, IntrinsicOperator),
    If {
        condition: Box<Value>,
        then_branch: Vec<parser::Statement>,
        else_branch: Vec<parser::Statement>,
    },
    FieldAccess(Box<Value>, parser::Ident),
    Constructor {
        fields: Vec<(parser::Ident, Value)>,
        type_id: declarations::Id,
    },
    MutablePointer(Box<Value>),
}

impl Value {
    /// remove unknowns
    fn unwrap(
        self,
        type_id: declarations::Id,
        builder: &mut FunctionBuilder<impl Module>,
    ) -> Result<cranelift::prelude::Value, SemanticError> {
        let layout = builder.declarations.get_layout(type_id);
        // TODO: support unsigned integers
        match self {
            Self::UnknownIntegerConst(int) => {
                let Layout::Primitive(r#type) = layout else {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                let r#type = match r#type {
                    Primitive::I8 | Primitive::U8 => types::I8,
                    Primitive::I16 | Primitive::U16 => types::I16,
                    Primitive::I32 | Primitive::U32 => types::I32,
                    Primitive::I64 | Primitive::U64 => types::I64,
                    Primitive::I128 | Primitive::U128 => todo!("chonky intz"),
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };
                Ok(builder.builder.ins().iconst(r#type, i64::try_from(int)?))
            }
            Self::UnknownSignedIntegerConst(int) => {
                let Layout::Primitive(r#type) = layout else {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                let r#type = match r#type {
                    Primitive::I8 => types::I8,
                    Primitive::I16 => types::I16,
                    Primitive::I32 => types::I32,
                    Primitive::I64 => types::I64,
                    Primitive::I128 => todo!("chonky intz"),
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };
                Ok(builder.builder.ins().iconst(r#type, i64::try_from(int)?))
            }
            Self::UnknownFloatConst(float) => match layout {
                // TODO: as
                Layout::Primitive(Primitive::F32) => {
                    Ok(builder.builder.ins().f32const(Ieee32::from(float as f32)))
                }
                Layout::Primitive(Primitive::F64) => {
                    Ok(builder.builder.ins().f64const(Ieee64::from(float)))
                }
                _ => Err(SemanticError::UnexpectedNumberLiteral),
            },
            Self::Cranelift(value, value_type) => {
                if value_type == type_id {
                    Ok(value)
                } else {
                    Err(SemanticError::MismatchedTypes)
                }
            }
            Self::MutablePointer(value) => {
                let pointer_id = type_id;
                let Layout::Primitive(Primitive::MutablePointer(inner_id)) =
                    builder.declarations.get_layout(pointer_id)
                else {
                    return Err(SemanticError::InvalidMutRef);
                };
                let layout = builder.declarations.get_layout(*inner_id);
                let value = value.unwrap(*inner_id, builder)?;
                if layout.is_aggregate() {
                    Ok(value)
                } else {
                    todo!("primitive mut refs")
                }
            }
            Self::Binary(left, right, operator) => {
                let type_id = left
                    .r#type(builder.declarations)
                    .or_else(|| right.r#type(builder.declarations))
                    .ok_or(SemanticError::UnknownType)?;
                let left = left.unwrap(type_id, builder)?;
                let right = right.unwrap(type_id, builder)?;
                let layout = builder.declarations.get_layout(type_id);

                match operator {
                    IntrinsicOperator::Add => Ok(builder.builder.ins().iadd(left, right)),
                    IntrinsicOperator::Sub => Ok(builder.builder.ins().isub(left, right)),
                    IntrinsicOperator::Cmp(operator) => {
                        let Layout::Primitive(r#type) = layout else {
                            todo!()
                        };
                        if r#type.is_signed_integer() {
                            let cc = match operator {
                                CmpOperator::Eq => IntCC::Equal,
                                CmpOperator::Ne => IntCC::NotEqual,
                                CmpOperator::Gt => IntCC::SignedGreaterThan,
                                CmpOperator::Gte => IntCC::SignedGreaterThanOrEqual,
                                CmpOperator::Lt => IntCC::SignedLessThan,
                                CmpOperator::Lte => IntCC::SignedLessThanOrEqual,
                            };
                            Ok(builder.builder.ins().icmp(cc, left, right))
                        } else if r#type.is_integer() {
                            let cc = match operator {
                                CmpOperator::Eq => IntCC::Equal,
                                CmpOperator::Ne => IntCC::NotEqual,
                                CmpOperator::Gt => IntCC::UnsignedGreaterThan,
                                CmpOperator::Gte => IntCC::UnsignedGreaterThanOrEqual,
                                CmpOperator::Lt => IntCC::UnsignedLessThan,
                                CmpOperator::Lte => IntCC::UnsignedLessThanOrEqual,
                            };
                            Ok(builder.builder.ins().icmp(cc, left, right))
                        } else {
                            let cc = match operator {
                                CmpOperator::Eq => FloatCC::Equal,
                                CmpOperator::Ne => FloatCC::NotEqual,
                                CmpOperator::Gt => todo!(),
                                CmpOperator::Gte => todo!(),
                                CmpOperator::Lt => todo!(),
                                CmpOperator::Lte => todo!(),
                            };
                            Ok(builder.builder.ins().fcmp(cc, left, right))
                        }
                    }
                }
            }
            Self::If {
                condition,
                mut then_branch,
                mut else_branch,
            } => {
                let condition_type = condition
                    .r#type(builder.declarations)
                    .ok_or(SemanticError::UnknownType)?;
                let condition = condition.unwrap(condition_type, builder)?;

                let then_block = builder.builder.create_block();
                let else_block = builder.builder.create_block();
                let merge_block = builder.builder.create_block();

                builder.builder.append_block_param(
                    merge_block,
                    match layout {
                        Layout::Primitive(r#type) => {
                            r#type.cranelift_type(builder.declarations.isa.pointer_type())
                        }
                        _ => todo!(),
                    },
                );

                builder
                    .builder
                    .ins()
                    .brif(condition, then_block, &[], else_block, &[]);

                builder.builder.switch_to_block(then_block);
                builder.builder.seal_block(then_block);
                let then_return = then_branch.pop();
                for statement in then_branch {
                    builder.handle_statement(statement)?;
                }

                if let Some(parser::Statement::Expression(then_return)) = then_return {
                    let value = builder
                        .expression(then_return, Some(type_id))?
                        .unwrap(type_id, builder)?;
                    builder.builder.ins().jump(merge_block, &[value]);
                } else {
                    then_return.map(|then_return| builder.handle_statement(then_return));
                    builder.builder.ins().jump(merge_block, &[]);
                }

                builder.builder.switch_to_block(else_block);
                builder.builder.seal_block(else_block);
                let else_return = else_branch.pop();
                for statement in else_branch {
                    builder.handle_statement(statement)?;
                }

                if let Some(parser::Statement::Expression(else_return)) = else_return {
                    let value = builder
                        .expression(else_return, Some(type_id))?
                        .unwrap(type_id, builder)?;
                    builder.builder.ins().jump(merge_block, &[value]);
                } else {
                    else_return.map(|else_return| builder.handle_statement(else_return));
                    builder.builder.ins().jump(merge_block, &[]);
                }

                builder.builder.switch_to_block(merge_block);
                builder.builder.seal_block(merge_block);

                Ok(builder.builder.block_params(merge_block)[0])
            }
            Self::FieldAccess(value, field) => {
                let struct_id = value
                    .r#type(builder.declarations)
                    .ok_or(SemanticError::UnknownType)?;
                let value = value.unwrap(struct_id, builder)?;
                let Layout::Struct { fields, .. } = builder.declarations.get_layout(struct_id)
                else {
                    return Err(SemanticError::NonStructFieldAccess);
                };

                let offset = fields
                    .get(&field)
                    .ok_or(SemanticError::NonExistentField)?
                    .offset;

                Ok(builder.builder.ins().load(
                    builder
                        .declarations
                        .get_layout(type_id)
                        .cranelift_type(&builder.declarations.isa),
                    MemFlags::new(),
                    value,
                    offset,
                ))
            }
            Self::Constructor { fields, type_id } => {
                // AbiParam::special(vt, codegen::ir::ArgumentPurpose::StructArgument(()));
                let Layout::Struct {
                    fields: type_fields,
                    size,
                } = builder.declarations.get_layout(type_id)
                else {
                    return Err(SemanticError::InvalidConstructor);
                };

                let stack_slot = builder.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: *size,
                });

                let addr = builder.builder.ins().stack_addr(
                    builder.declarations.isa.pointer_type(),
                    stack_slot,
                    Offset32::new(0),
                );

                for (name, field) in type_fields {
                    let value = fields
                        .iter()
                        .find(|field| &field.0 == name)
                        .ok_or(SemanticError::NonExistentField)?
                        .clone()
                        .1
                        .unwrap(field.r#type, builder)?;

                    builder
                        .builder
                        .ins()
                        .stack_store(value, stack_slot, field.offset);
                }

                Ok(addr)
            }
        }
    }

    // TODO: This does not infer deeply nested values types
    pub fn r#type(&self, declarations: &Declarations) -> Option<declarations::Id> {
        match self {
            Self::Cranelift(_, type_id) | Self::Constructor { type_id, .. } => Some(*type_id),
            Self::FieldAccess(r#struct, field) => {
                match declarations.get_layout(r#struct.r#type(declarations)?) {
                    Layout::Primitive(_) => None,
                    Layout::Struct { fields, .. } => fields.get(field).map(|field| field.r#type),
                }
            }
            _ => None,
        }
    }
}

pub struct FunctionBuilder<'a, M> {
    pub declarations: &'a declarations::Declarations,
    pub module: &'a mut M,
    pub scope: parser::scope::Id,
    pub r#return: declarations::Id,
    pub names: HashMap<parser::Ident, (Variable, Option<declarations::Id>)>,
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

    pub fn handle_statement(&mut self, statement: parser::Statement) -> Result<(), SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                match name {
                    Expression::Ident(name) => {
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
                    Expression::FieldAccess(left, field) => {
                        let left = self.expression(*left, None)?;
                        let struct_id = left
                            .r#type(self.declarations)
                            .ok_or(SemanticError::UnknownType)?;
                        let pointer = left.unwrap(struct_id, self)?;
                        let Layout::Struct { fields, .. } = self.declarations.get_layout(struct_id)
                        else {
                            return Err(SemanticError::NonStructFieldAccess);
                        };

                        let field = fields.get(&field).ok_or(SemanticError::NonExistentField)?;

                        let value = self
                            .expression(expression, Some(field.r#type))?
                            .unwrap(field.r#type, self)?;

                        self.builder
                            .ins()
                            .store(MemFlags::new(), value, pointer, field.offset);
                    },
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
                if let Expression::Return(expression) = expression {
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

    fn integer(
        &mut self,
        int: u128,
        type_id: Option<declarations::Id>,
    ) -> Result<Value, SemanticError> {
        let Some(r#type) = type_id.map(|id| self.declarations.get_layout(id)) else {
            return Ok(Value::UnknownIntegerConst(int));
        };

        let Layout::Primitive(r#type) = r#type else {
            return Err(SemanticError::UnexpectedNumberLiteral);
        };

        if !r#type.is_integer() {
            return Err(SemanticError::UnexpectedNumberLiteral);
        };

        // TODO: `.unwrap()`
        // TODO: phatt intz
        // TODO: subset of `Type` for `Primitive`?
        Ok(Value::Cranelift(
            self.builder.ins().iconst(
                r#type.cranelift_type(self.declarations.isa.pointer_type()),
                i64::try_from(int)?,
            ),
            type_id.unwrap(),
        ))
    }

    fn float(
        &mut self,
        float: f64,
        type_id: Option<declarations::Id>,
    ) -> Result<Value, SemanticError> {
        // TODO: `.unwrap()`
        // TODO: refactor
        let Some(layout) = type_id.map(|type_id| self.declarations.get_layout(type_id)) else {
            return Ok(Value::UnknownFloatConst(float));
        };

        let value = match layout {
            // TODO: `as`
            Layout::Primitive(Primitive::F32) => Value::Cranelift(
                self.builder.ins().f32const(Ieee32::from(float as f32)),
                type_id.unwrap(),
            ),
            Layout::Primitive(Primitive::F64) => Value::Cranelift(
                self.builder.ins().f64const(Ieee64::from(float)),
                type_id.unwrap(),
            ),
            _ => return Err(SemanticError::UnexpectedNumberLiteral),
        };
        Ok(value)
    }

    fn literal(
        &mut self,
        literal: &Literal,
        r#type: Option<declarations::Id>,
    ) -> Result<Value, SemanticError> {
        match literal {
            Literal::Integer(integer) => Ok(self.integer(*integer, r#type)?),
            Literal::Float(float) => Ok(self.float(*float, r#type)?),
            _ => todo!(),
        }
    }

    fn intrinsic_call(&mut self, intrinsic: IntrinsicCall) -> Result<Value, SemanticError> {
        match intrinsic {
            IntrinsicCall::Binary(left, right, operator) => {
                let mut left = self.expression(*left, None)?;
                let right = self.expression(*right, left.r#type(self.declarations))?;
                if let Some(r#type) = right.r#type(self.declarations) {
                    // TODO: `.clone()`
                    left = Value::Cranelift(left.unwrap(r#type, self)?, r#type);
                }
                Ok(Value::Binary(Box::new(left), Box::new(right), operator))
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
                Ok(Value::Cranelift(value.unwrap(r#type, self)?, r#type))
            }
            IntrinsicCall::MutablePointer(expression) => {
                let value = self.expression(*expression, None)?;
                Ok(Value::MutablePointer(Box::new(value)))
            }
        }
    }

    fn unary_prefix(
        &mut self,
        operator: UnaryOperator,
        expression: Expression,
        type_id: Option<declarations::Id>,
    ) -> Result<Value, SemanticError> {
        match (operator, expression) {
            (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                let Some(layout) = type_id.map(|type_id| self.declarations.get_layout(type_id))
                else {
                    return Ok(Value::UnknownSignedIntegerConst(-i128::try_from(integer)?));
                };

                let Layout::Primitive(layout) = layout else {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                if !layout.is_signed_integer() {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                Ok(Value::Cranelift(
                    self.builder.ins().iconst(
                        layout.cranelift_type(self.declarations.isa.pointer_type()),
                        -i64::try_from(integer)?,
                    ),
                    type_id.unwrap(),
                ))
            }
            (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                self.float(-float, type_id)
            }
            _ => todo!(),
        }
    }

    fn call(
        &mut self,
        Call {
            callable,
            arguments,
            ..
        }: Call,
    ) -> Result<Value, SemanticError> {
        // TODO: what about if not ident
        let callable = match callable.as_ref() {
            Expression::Ident(ident) => ident,
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

        Ok(Value::Cranelift(value, function.r#return))
    }

    fn expression(
        &mut self,
        expression: Expression,
        r#type: Option<declarations::Id>,
    ) -> Result<Value, SemanticError> {
        match expression {
            Expression::Literal(literal) => self.literal(&literal, r#type),
            Expression::UnaryPrefix(operator, expression) => {
                self.unary_prefix(operator, *expression, r#type)
            }
            Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            Expression::Return(expression) => self.expression(*expression, Some(self.r#return)),
            Expression::Ident(ident) => {
                let variable = *self
                    .names
                    .get(&ident)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                if let (Some(r#type), Some(variable)) = (r#type, variable.1) {
                    if r#type != variable {
                        dbg!(&self.declarations.get_layout(r#type));
                        dbg!(&self.declarations.get_layout(variable));
                        return Err(SemanticError::MismatchedTypes);
                    }
                };

                Ok(Value::Cranelift(
                    self.builder.use_var(variable.0),
                    variable.1.unwrap_or_else(|| todo!()),
                ))
            }
            Expression::Call(call) => {
                let value = self.call(call)?;
                if let r#type @ Some(_) = r#type {
                    if r#type != value.r#type(self.declarations) {
                        dbg!();
                        return Err(SemanticError::MismatchedTypes);
                    }
                };
                Ok(value)
            }
            Expression::If(If {
                condition,
                then_branch: true_branch,
                else_branch: false_branch,
            }) => Ok(Value::If {
                condition: Box::new(self.expression(*condition, None)?),
                then_branch: true_branch,
                else_branch: false_branch.into_iter().flatten().collect(),
            }),
            Expression::Constructor(Constructor { r#type, fields }) => {
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

                Ok(Value::Constructor {
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
            Expression::FieldAccess(expression, field) => Ok(Value::FieldAccess(
                Box::new(self.expression(*expression, None)?),
                field,
            )),
            Expression::Binary(_) => todo!(),
        }
    }
}
