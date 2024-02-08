use crate::declarations::Declarations;
use crate::layout::{Layout, Primitive};
use crate::{hir, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::expression::IntrinsicOperator;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchStatus<T> {
    Finished,
    Continue(T),
}

pub struct Translator<'a, M> {
    builder: cranelift::prelude::FunctionBuilder<'a>,
    declarations: &'a Declarations,
    module: &'a mut M,
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
        module: &'a mut M,
    ) -> Self {
        Self {
            builder,
            declarations,
            module,
        }
    }

    pub fn iconst(&mut self, value: u32) -> Value {
        self.builder.ins().iconst(
            self.declarations.isa.pointer_type(),
            Imm64::from(i64::from(value)),
        )
    }

    pub fn statement(
        &mut self,
        statement: hir::Statement,
    ) -> Result<BranchStatus<()>, SemanticError> {
        match statement {
            hir::Statement::Assignment(assignment) => {
                let BranchStatus::Continue(left) = self.expression(assignment.left)? else {
                    return Ok(BranchStatus::Finished);
                };

                let layout = self
                    .declarations
                    .get_layout(assignment.right.expect_type()?);

                let size = self.iconst(layout.size(&self.declarations.isa));

                let BranchStatus::Continue(right) = self.expression(assignment.right)? else {
                    return Ok(BranchStatus::Finished);
                };

                self.builder.call_memmove(
                    self.declarations.isa.frontend_config(),
                    left,
                    right,
                    size,
                );
            }
            hir::Statement::Expression(expression) => {
                if self.expression(expression)? == BranchStatus::Finished {
                    return Ok(BranchStatus::Finished);
                }
            }
            hir::Statement::Let(variable, expression) => {
                self.builder.declare_var(
                    variable.into(),
                    self.declarations
                        .get_layout(expression.expect_type()?)
                        .cranelift_type(&self.declarations.isa),
                );
                let BranchStatus::Continue(expression) = self.expression(expression)? else {
                    return Ok(BranchStatus::Finished);
                };
                self.builder.def_var(variable.into(), expression);
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

        if let Some(then_return) = &then_branch.expression {
            self.builder.append_block_param(
                merge_block,
                match self.declarations.get_layout(then_return.expect_type()?) {
                    Layout::Primitive(primitive) => {
                        primitive.cranelift_type(self.declarations.isa.pointer_type())
                    }
                    Layout::Struct { .. } => todo!("structs in ifs!"),
                },
            );
        } else {
            todo!("void ifs")
        }

        let BranchStatus::Continue(condition) = self.load_primitive(condition)? else {
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

    fn binary_intrinsic(
        &mut self,
        binary: hir::BinaryIntrinsic,
        layout: &Layout,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let BranchStatus::Continue(left) = self.load_primitive(binary.left)? else {
            return Ok(BranchStatus::Finished);
        };

        let BranchStatus::Continue(right) = self.load_primitive(binary.right)? else {
            return Ok(BranchStatus::Finished);
        };

        let value = match binary.operator {
            IntrinsicOperator::Add => self.builder.ins().iadd(left, right),
            IntrinsicOperator::Sub => self.builder.ins().isub(left, right),
            IntrinsicOperator::Cmp(operator) => {
                let Layout::Primitive(primitive) = layout else {
                    return Err(SemanticError::InvalidIntrinsic);
                };

                if primitive.is_signed_integer() {
                    self.builder
                        .ins()
                        .icmp(operator.signed_intcc(), left, right)
                } else if primitive.is_integer() {
                    self.builder
                        .ins()
                        .icmp(operator.unsigned_intcc(), left, right)
                } else {
                    self.builder.ins().fcmp(operator.floatcc(), left, right)
                }
            }
        };

        Ok(BranchStatus::Continue(self.put_in_stack_slot(
            value,
            layout.size(&self.declarations.isa),
        )))
    }

    fn field_access(
        &mut self,
        access: hir::FieldAccess,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let layout = self
            .declarations
            .get_layout(access.expression.expect_type()?)
            .deref_pointers(self.declarations);

        let BranchStatus::Continue(value) = self.expression(access.expression)? else {
            return Ok(BranchStatus::Finished);
        };

        let struct_layout = match layout {
            Layout::Struct(struct_layout) => struct_layout,
            // Layout::Primitive(Primitive::MutablePointer(inner)) => {}
            layout => return Err(SemanticError::InvalidFieldAccess(layout.clone())),
        };

        let offset = struct_layout
            .fields
            .get(&access.field)
            .ok_or(SemanticError::NonExistentField)?
            .offset;

        let offset = self.builder.ins().iconst(
            self.declarations.isa.pointer_type(),
            Imm64::new(offset.into()),
        );

        Ok(BranchStatus::Continue(
            self.builder.ins().iadd(value, offset),
        ))
    }

    fn constructor(
        &mut self,
        constructor: hir::Constructor,
        layout: &Layout,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let Layout::Struct(layout) = layout else {
            return Err(SemanticError::InvalidConstructor);
        };

        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size: layout.size,
        });

        let addr = self.builder.ins().stack_addr(
            self.declarations.isa.pointer_type(),
            stack_slot,
            Offset32::new(0),
        );

        for (offset, expression) in constructor.0 {
            let layout = self.declarations.get_layout(expression.expect_type()?);

            let size = self.iconst(layout.size(&self.declarations.isa));

            let addr = self.builder.ins().iadd_imm(addr, i64::from(offset));

            let value = match self.expression(expression)? {
                BranchStatus::Continue(value) => value,
                BranchStatus::Finished => return Ok(BranchStatus::Finished),
            };

            self.builder
                .call_memmove(self.declarations.isa.frontend_config(), addr, value, size);
        }

        Ok(BranchStatus::Continue(addr))
    }

    fn call(&mut self, call: hir::Call) -> Result<BranchStatus<Value>, SemanticError> {
        let hir::Expression::GlobalAccess(declaration) = call.callable.expression else {
            todo!("closures!")
        };
        let function = self.declarations.get_function(declaration)?;

        let mut arguments = Vec::new();
        for expression in call.arguments {
            let BranchStatus::Continue(value) = self.load_primitive(expression)? else {
                return Ok(BranchStatus::Finished);
            };
            arguments.push(value);
        }

        let return_layout = self.declarations.get_layout(function.return_type);

        if return_layout.is_aggregate() {
            let stack_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                return_layout.size(&self.declarations.isa),
            ));

            let addr = self.builder.ins().stack_addr(
                self.declarations.isa.pointer_type(),
                stack_slot,
                Offset32::new(0),
            );

            arguments.push(addr);
        }

        let func_ref = self
            .module
            .declare_func_in_func(function.id, self.builder.func);

        let call = self.builder.ins().call(func_ref, arguments.as_slice());
        let value = self.builder.inst_results(call)[0];
        let value = if return_layout.is_aggregate() {
            value
        } else {
            self.put_in_stack_slot(value, return_layout.size(&self.declarations.isa))
        };
        Ok(BranchStatus::Continue(value))
    }

    fn put_in_stack_slot(&mut self, value: Value, size: u32) -> Value {
        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size,
        });

        self.builder
            .ins()
            .stack_store(value, stack_slot, Offset32::new(0));

        self.builder.ins().stack_addr(
            self.declarations.isa.pointer_type(),
            stack_slot,
            Offset32::new(0),
        )
    }

    fn load_primitive(
        &mut self,
        expression: hir::TypedExpression,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let layout = self.declarations.get_layout(expression.expect_type()?);

        let BranchStatus::Continue(value) = self.expression(expression)? else {
            return Ok(BranchStatus::Finished);
        };

        let value = if layout.is_aggregate() {
            value
        } else {
            self.builder.ins().load(
                layout.cranelift_type(&self.declarations.isa),
                MemFlags::new(),
                value,
                Offset32::new(0),
            )
        };

        Ok(BranchStatus::Continue(value))
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
            .ok_or_else(|| SemanticError::UnknownType(expression.clone()));

        let value = match expression {
            hir::Expression::IntegerConst(int) => {
                let Layout::Primitive(primitive) = layout? else {
                    return Err(SemanticError::UnexpectedNumberLiteral);
                };

                let cranelift_type = match primitive {
                    Primitive::I8 | Primitive::U8 => types::I8,
                    Primitive::I16 | Primitive::U16 => types::I16,
                    Primitive::I32 | Primitive::U32 => types::I32,
                    Primitive::I64 | Primitive::U64 => types::I64,
                    Primitive::I128 | Primitive::U128 => todo!("chonky intz"),
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };

                let value = self
                    .builder
                    .ins()
                    .iconst(cranelift_type, i64::try_from(int)?);

                BranchStatus::Continue(self.put_in_stack_slot(
                    value,
                    primitive.size(self.declarations.isa.pointer_bytes().into()),
                ))
            }
            hir::Expression::FloatConst(float) => {
                let value = match layout? {
                    Layout::Primitive(Primitive::F32) => {
                        #[allow(clippy::cast_possible_truncation)]
                        let value = self.builder.ins().f32const(Ieee32::from(float as f32));
                        self.put_in_stack_slot(value, 4)
                    }
                    Layout::Primitive(Primitive::F64) => {
                        let value = self.builder.ins().f64const(Ieee64::from(float));
                        self.put_in_stack_slot(value, 8)
                    }
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };

                BranchStatus::Continue(value)
            }
            hir::Expression::MutablePointer(inner) | hir::Expression::Deref(inner) => self.expression(*inner)?,
            hir::Expression::BinaryIntrinsic(binary) => self.binary_intrinsic(*binary, layout?)?,
            hir::Expression::If(r#if) => self.translate_if(*r#if)?,
            hir::Expression::FieldAccess(access) => self.field_access(*access)?,
            hir::Expression::Constructor(constructor) => self.constructor(constructor, layout?)?,
            hir::Expression::Return(expression) => {
                let BranchStatus::Continue(value) = self.load_primitive(*expression)? else {
                    return Ok(BranchStatus::Finished);
                };

                self.builder.ins().return_(&[value]);

                return Ok(BranchStatus::Finished);
            }
            hir::Expression::LocalAccess(variable) => {
                BranchStatus::Continue(self.builder.use_var(variable.into()))
            }
            hir::Expression::GlobalAccess(_) => todo!(),
            hir::Expression::Call(call) => self.call(*call)?,
        };

        Ok(value)
    }
}
