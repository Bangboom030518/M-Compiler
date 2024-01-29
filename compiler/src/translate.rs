use crate::declarations::Declarations;
use crate::layout::{Layout, Primitive};
use crate::{hir, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::{CmpOperator, IntrinsicOperator};

pub struct Translator<'a, M> {
    builder: cranelift::prelude::FunctionBuilder<'a>,
    declarations: &'a Declarations,
    module: &'a M,
}

impl<'a, M> Translator<'a, M>
where
    M: Module,
{
    pub fn finalize(self) {
        self.builder.finalize();
    }

    pub fn new(
        builder: cranelift::prelude::FunctionBuilder<'a>,
        declarations: &'a Declarations,
        module: &'a M,
    ) -> Self {
        Self {
            builder,
            declarations,
            module,
        }
    }

    pub fn statement(&mut self, statement: hir::Statement) -> Result<(), SemanticError> {
        match statement {
            hir::Statement::Assignment(left, right) => todo!("assignment"),
            hir::Statement::Expression(expression) => {
                self.expression(expression)?;
            }
            hir::Statement::Let(variable, expression) => {
                self.builder.declare_var(
                    variable,
                    self.declarations
                        .get_layout(expression.type_id.ok_or(SemanticError::UnknownType)?)
                        .cranelift_type(&self.declarations.isa),
                );
                let expression = self.expression(expression)?;
                self.builder.def_var(variable, expression);
            }
        };

        Ok(())
    }

    fn translate_if(
        &mut self,
        hir::If {
            condition,
            mut then_branch,
            mut else_branch,
        }: hir::If,
    ) -> Result<Value, SemanticError> {
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder.append_block_param(
            merge_block,
            match self
                .declarations
                .get_layout(condition.type_id.ok_or(SemanticError::UnknownType)?)
            {
                Layout::Primitive(primitive) => {
                    primitive.cranelift_type(self.declarations.isa.pointer_type())
                }
                _ => todo!(),
            },
        );

        let condition = self.expression(condition)?;

        self.builder
            .ins()
            .brif(condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let then_return = then_branch.pop();
        for statement in then_branch {
            todo!("handle statement!")
        }

        if let Some(hir::Statement::Expression(then_return)) = then_return {
            let expression = self.expression(then_return)?;
            self.builder.ins().jump(merge_block, &[expression]);
        } else {
            todo!("non-expr ifs: `void`?")
        }

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        let else_return = else_branch.pop();
        for statement in else_branch {
            todo!("handle statement!")
        }

        if let Some(hir::Statement::Expression(else_return)) = else_return {
            let expression = self.expression(else_return)?;
            self.builder.ins().jump(merge_block, &[expression]);
        } else {
            todo!("non-expr ifs: `void`?")
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        Ok(self.builder.block_params(merge_block)[0])
    }

    fn expression(
        &mut self,
        hir::TypedExpression {
            expression,
            type_id,
        }: hir::TypedExpression,
    ) -> Result<Value, SemanticError> {
        let type_id = type_id.ok_or(SemanticError::UnknownType)?;
        let layout = self.declarations.get_layout(type_id);

        match expression {
            hir::Expression::IntegerConst(int) => {
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

                Ok(self.builder.ins().iconst(r#type, i64::try_from(int)?))
            }
            hir::Expression::FloatConst(float) => {
                match layout {
                    // TODO: as
                    Layout::Primitive(Primitive::F32) => {
                        Ok(self.builder.ins().f32const(Ieee32::from(float as f32)))
                    }
                    Layout::Primitive(Primitive::F64) => {
                        Ok(self.builder.ins().f64const(Ieee64::from(float)))
                    }
                    _ => Err(SemanticError::UnexpectedNumberLiteral),
                }
            }
            hir::Expression::MutablePointer(value) => {
                let pointer_id = type_id;
                let Layout::Primitive(Primitive::MutablePointer(inner_id)) =
                    self.declarations.get_layout(pointer_id)
                else {
                    return Err(SemanticError::InvalidMutRef);
                };

                let layout = self.declarations.get_layout(*inner_id);
                let value = self.expression(*value)?;
                if layout.is_aggregate() {
                    Ok(value)
                } else {
                    todo!("primitive mut refs")
                }
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                let left = self.expression(binary.left)?;
                let right = self.expression(binary.right)?;

                match binary.operator {
                    IntrinsicOperator::Add => Ok(self.builder.ins().iadd(left, right)),
                    IntrinsicOperator::Sub => Ok(self.builder.ins().isub(left, right)),
                    IntrinsicOperator::Cmp(operator) => {
                        let Layout::Primitive(r#type) = layout else {
                            return Err(SemanticError::InvalidIntrinsic);
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
                            Ok(self.builder.ins().icmp(cc, left, right))
                        } else if r#type.is_integer() {
                            let cc = match operator {
                                CmpOperator::Eq => IntCC::Equal,
                                CmpOperator::Ne => IntCC::NotEqual,
                                CmpOperator::Gt => IntCC::UnsignedGreaterThan,
                                CmpOperator::Gte => IntCC::UnsignedGreaterThanOrEqual,
                                CmpOperator::Lt => IntCC::UnsignedLessThan,
                                CmpOperator::Lte => IntCC::UnsignedLessThanOrEqual,
                            };
                            Ok(self.builder.ins().icmp(cc, left, right))
                        } else {
                            let cc = match operator {
                                CmpOperator::Eq => FloatCC::Equal,
                                CmpOperator::Ne => FloatCC::NotEqual,
                                CmpOperator::Gt => todo!(),
                                CmpOperator::Gte => todo!(),
                                CmpOperator::Lt => todo!(),
                                CmpOperator::Lte => todo!(),
                            };
                            Ok(self.builder.ins().fcmp(cc, left, right))
                        }
                    }
                }
            }
            hir::Expression::If(r#if) => self.translate_if(*r#if),
            hir::Expression::FieldAccess(expression, field) => {
                let struct_id = expression.type_id.ok_or(SemanticError::UnknownType)?;
                let value = self.expression(*expression)?;
                let Layout::Struct { fields, .. } = self.declarations.get_layout(struct_id) else {
                    return Err(SemanticError::NonStructFieldAccess);
                };

                let offset = fields
                    .get(&field)
                    .ok_or(SemanticError::NonExistentField)?
                    .offset;

                Ok(self.builder.ins().load(
                    layout.cranelift_type(&self.declarations.isa),
                    MemFlags::new(),
                    value,
                    offset,
                ))
            }
            hir::Expression::Constructor(constructor) => {
                let Layout::Struct {
                    fields: type_fields,
                    size,
                } = layout
                else {
                    return Err(SemanticError::InvalidConstructor);
                };

                let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: *size,
                });

                let addr = self.builder.ins().stack_addr(
                    self.declarations.isa.pointer_type(),
                    stack_slot,
                    Offset32::new(0),
                );

                for (offset, expression) in constructor.0 {
                    let expression = self.expression(expression)?;
                    self.builder
                        .ins()
                        .stack_store(expression, stack_slot, offset);
                }

                Ok(addr)
            }
            hir::Expression::Return(expression) => {
                let expression = self.expression(*expression)?;
                self.builder.ins().return_(&[expression]);
                todo!("void!")
            }
            hir::Expression::LocalAccess(variable) => Ok(self.builder.use_var(variable.into())),
            hir::Expression::GlobalAccess(_) => todo!(),
            hir::Expression::Call(_) => todo!(),
        }
    }
}
