use crate::declarations::{self, Declarations};
use crate::layout::{Layout, Primitive};
use crate::local::FunctionBuilder;
use crate::SemanticError;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::{CmpOperator, IntrinsicOperator};

#[derive(Debug, Clone)]
pub enum Statement {
    Ignore(Expression),
    Assignment(Expression, Expression),
    Let(parser::Ident, Expression),
}

#[derive(Debug, Clone)]
pub struct BinaryIntrinsic {
    pub left: Expression,
    pub right: Expression,
    pub operator: IntrinsicOperator,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expression,
    pub then_branch: Vec<Statement>,
    pub else_branch: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callable: Expression,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Expression {
    SignedIntegerConst(i128),
    IntegerConst(u128),
    FloatConst(f64),
    BinaryIntrinsic(Box<BinaryIntrinsic>),
    If(Box<If>),
    FieldAccess(Box<Expression>, parser::Ident),
    Constructor {
        fields: Vec<(parser::Ident, Expression)>,
        type_id: declarations::Id,
    },
    MutablePointer(Box<Expression>),
    Call(Box<Call>),
    VariableAccess(Variable),
    Return(Box<Expression>),
}

impl Expression {
    /// remove unknowns
    pub fn unwrap(
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
