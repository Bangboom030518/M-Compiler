use crate::declarations::Declarations;
use crate::layout::{Layout, Primitive};
use crate::{hir, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::{CmpOperator, IntrinsicOperator};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchStatus<T> {
    Finished,
    Continue(T),
}

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

    pub const fn new(
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

    pub fn statement(
        &mut self,
        statement: hir::Statement,
    ) -> Result<BranchStatus<()>, SemanticError> {
        match statement {
            hir::Statement::Assignment(left, right) => todo!("assignment"),
            hir::Statement::Expression(expression) => {
                if self.expression(expression)? == BranchStatus::Finished {
                    return Ok(BranchStatus::Finished);
                }
            }
            hir::Statement::Let(variable, expression) => {
                self.builder.declare_var(
                    variable,
                    self.declarations
                        .get_layout(expression.type_id.ok_or_else(|| {
                            dbg!(&expression);
                            SemanticError::UnknownType
                        })?)
                        .cranelift_type(&self.declarations.isa),
                );
                let BranchStatus::Continue(expression) = self.expression(expression)? else {
                    return Ok(BranchStatus::Finished);
                };
                self.builder.def_var(variable, expression);
            }
        };

        Ok(BranchStatus::Continue(()))
    }

    fn block(
        &mut self,
        hir::Block {
            statements,
            expression,
        }: hir::Block,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        for statement in statements {
            if self.statement(statement)? == BranchStatus::Finished {
                return Ok(BranchStatus::Finished);
            }
        }
        self.expression(expression.unwrap_or_else(|| todo!("void blocks")))
    }

    fn translate_if(
        &mut self,
        hir::If {
            condition,
            then_branch,
            else_branch,
        }: hir::If,
    ) -> Result<BranchStatus<Value>, SemanticError> {
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

        let BranchStatus::Continue(condition) = self.expression(condition)? else {
            return Ok(BranchStatus::Finished);
        };

        self.builder
            .ins()
            .brif(condition, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        match self.block(then_branch)? {
            BranchStatus::Finished => {}
            BranchStatus::Continue(value) => {
                self.builder.ins().jump(merge_block, &[value]);
            }
        };

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        match self.block(else_branch)? {
            BranchStatus::Finished => {}
            BranchStatus::Continue(value) => {
                self.builder.ins().jump(merge_block, &[value]);
            }
        };

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        Ok(BranchStatus::Continue(
            self.builder.block_params(merge_block)[0],
        ))
    }

    fn expression(
        &mut self,
        hir::TypedExpression {
            expression,
            type_id,
        }: hir::TypedExpression,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let layout = type_id
            .map(|type_id| self.declarations.get_layout(type_id))
            .ok_or(SemanticError::UnknownType);

        let value = match expression {
            hir::Expression::IntegerConst(int) => {
                let Layout::Primitive(r#type) = layout? else {
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

                self.builder.ins().iconst(r#type, i64::try_from(int)?)
            }
            hir::Expression::FloatConst(float) => {
                match layout? {
                    // TODO: as
                    Layout::Primitive(Primitive::F32) => {
                        self.builder.ins().f32const(Ieee32::from(float as f32))
                    }
                    Layout::Primitive(Primitive::F64) => {
                        self.builder.ins().f64const(Ieee64::from(float))
                    }
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                }
            }
            hir::Expression::MutablePointer(expression) => {
                let Layout::Primitive(Primitive::MutablePointer(inner_id)) = layout? else {
                    return Err(SemanticError::InvalidMutRef);
                };

                let layout = self.declarations.get_layout(*inner_id);
                let value = match self.expression(*expression)? {
                    BranchStatus::Continue(value) => value,
                    BranchStatus::Finished => return Ok(BranchStatus::Finished),
                };
                if layout.is_aggregate() {
                    value
                } else {
                    todo!("primitive mut refs")
                }
            }
            hir::Expression::BinaryIntrinsic(binary) => {
                let left = match self.expression(binary.left)? {
                    BranchStatus::Continue(value) => value,
                    BranchStatus::Finished => return Ok(BranchStatus::Finished),
                };
                let right = match self.expression(binary.right)? {
                    BranchStatus::Continue(value) => value,
                    BranchStatus::Finished => return Ok(BranchStatus::Finished),
                };

                match binary.operator {
                    IntrinsicOperator::Add => self.builder.ins().iadd(left, right),
                    IntrinsicOperator::Sub => self.builder.ins().isub(left, right),
                    IntrinsicOperator::Cmp(operator) => {
                        let Layout::Primitive(r#type) = layout? else {
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
                            self.builder.ins().icmp(cc, left, right)
                        } else if r#type.is_integer() {
                            let cc = match operator {
                                CmpOperator::Eq => IntCC::Equal,
                                CmpOperator::Ne => IntCC::NotEqual,
                                CmpOperator::Gt => IntCC::UnsignedGreaterThan,
                                CmpOperator::Gte => IntCC::UnsignedGreaterThanOrEqual,
                                CmpOperator::Lt => IntCC::UnsignedLessThan,
                                CmpOperator::Lte => IntCC::UnsignedLessThanOrEqual,
                            };
                            self.builder.ins().icmp(cc, left, right)
                        } else {
                            let cc = match operator {
                                CmpOperator::Eq => FloatCC::Equal,
                                CmpOperator::Ne => FloatCC::NotEqual,
                                CmpOperator::Gt => todo!(),
                                CmpOperator::Gte => todo!(),
                                CmpOperator::Lt => todo!(),
                                CmpOperator::Lte => todo!(),
                            };
                            self.builder.ins().fcmp(cc, left, right)
                        }
                    }
                }
            }
            hir::Expression::If(r#if) => match self.translate_if(*r#if)? {
                BranchStatus::Continue(value) => value,
                BranchStatus::Finished => return Ok(BranchStatus::Finished),
            },
            hir::Expression::FieldAccess(expression, field) => {
                let struct_id = expression.type_id.ok_or(SemanticError::UnknownType)?;
                let value = match self.expression(*expression)? {
                    BranchStatus::Continue(value) => value,
                    BranchStatus::Finished => return Ok(BranchStatus::Finished),
                };
                let Layout::Struct { fields, .. } = self.declarations.get_layout(struct_id) else {
                    return Err(SemanticError::NonStructFieldAccess);
                };

                let offset = fields
                    .get(&field)
                    .ok_or(SemanticError::NonExistentField)?
                    .offset;

                self.builder.ins().load(
                    layout?.cranelift_type(&self.declarations.isa),
                    MemFlags::new(),
                    value,
                    offset,
                )
            }
            hir::Expression::Constructor(constructor) => {
                let Layout::Struct {
                    fields: type_fields,
                    size,
                } = layout?
                else {
                    return Err(SemanticError::InvalidConstructor.into());
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
                    let expression = match self.expression(expression)? {
                        BranchStatus::Continue(value) => value,
                        BranchStatus::Finished => return Ok(BranchStatus::Finished),
                    };
                    self.builder
                        .ins()
                        .stack_store(expression, stack_slot, offset);
                }

                addr
            }
            hir::Expression::Return(expression) => {
                let expression = match self.expression(*expression)? {
                    BranchStatus::Continue(value) => value,
                    BranchStatus::Finished => return Ok(BranchStatus::Finished),
                };
                self.builder.ins().return_(&[expression]);
                return Ok(BranchStatus::Finished);
            }
            hir::Expression::LocalAccess(variable) => self.builder.use_var(variable.into()),
            hir::Expression::GlobalAccess(_) => todo!(),
            hir::Expression::Call(_) => todo!(),
        };

        Ok(BranchStatus::Continue(value))
    }
}
