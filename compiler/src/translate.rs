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

    pub fn statement(
        &mut self,
        statement: hir::Statement,
    ) -> Result<BranchStatus<()>, SemanticError> {
        match statement {
            hir::Statement::Assignment(_, _) => todo!("assignment"),
            hir::Statement::Expression(expression) => {
                if self.expression(expression)? == BranchStatus::Finished {
                    return Ok(BranchStatus::Finished);
                }
            }
            hir::Statement::Let(variable, expression) => {
                self.builder.declare_var(
                    variable.into(),
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
                match self
                    .declarations
                    .get_layout(then_return.type_id.ok_or(SemanticError::UnknownType)?)
                {
                    Layout::Primitive(primitive) => {
                        primitive.cranelift_type(self.declarations.isa.pointer_type())
                    }
                    Layout::Struct { .. } => todo!("structs in ifs!"),
                },
            );
        } else {
            todo!("void ifs")
        }

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

    fn binary_intrinsic(
        &mut self,
        binary: hir::BinaryIntrinsic,
        layout: Result<&Layout, SemanticError>,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let BranchStatus::Continue(left) = self.expression(binary.left)? else {
            return Ok(BranchStatus::Finished);
        };
        let right = match self.expression(binary.right)? {
            BranchStatus::Continue(value) => value,
            BranchStatus::Finished => return Ok(BranchStatus::Finished),
        };

        let value = match binary.operator {
            IntrinsicOperator::Add => self.builder.ins().iadd(left, right),
            IntrinsicOperator::Sub => self.builder.ins().isub(left, right),
            IntrinsicOperator::Cmp(operator) => {
                let Layout::Primitive(primitive) = layout? else {
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
        Ok(BranchStatus::Continue(value))
    }

    fn field_access(
        &mut self,
        access: hir::FieldAccess,
        layout: Result<&Layout, SemanticError>,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let Layout::Struct { fields, .. } = self.declarations.get_layout(
            access
                .expression
                .type_id
                .ok_or(SemanticError::UnknownType)?,
        ) else {
            return Err(SemanticError::NonStructFieldAccess);
        };
        let BranchStatus::Continue(value) = self.expression(access.expression)? else {
            return Ok(BranchStatus::Finished);
        };
        let value = self.builder.ins().load(
            layout?.cranelift_type(&self.declarations.isa),
            MemFlags::new(),
            value,
            fields
                .get(&access.field)
                .ok_or(SemanticError::NonExistentField)?
                .offset,
        );
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
            .ok_or(SemanticError::UnknownType);

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

                BranchStatus::Continue(
                    self.builder
                        .ins()
                        .iconst(cranelift_type, i64::try_from(int)?),
                )
            }
            hir::Expression::FloatConst(float) => {
                let value = match layout? {
                    // TODO: as
                    Layout::Primitive(Primitive::F32) =>
                    {
                        #[allow(clippy::cast_possible_truncation)]
                        self.builder.ins().f32const(Ieee32::from(float as f32))
                    }
                    Layout::Primitive(Primitive::F64) => {
                        self.builder.ins().f64const(Ieee64::from(float))
                    }
                    _ => return Err(SemanticError::UnexpectedNumberLiteral),
                };
                BranchStatus::Continue(value)
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
                    BranchStatus::Continue(value)
                } else {
                    todo!("primitive mut refs")
                }
            }
            hir::Expression::BinaryIntrinsic(binary) => self.binary_intrinsic(*binary, layout)?,
            hir::Expression::If(r#if) => self.translate_if(*r#if)?,
            hir::Expression::FieldAccess(access) => self.field_access(*access, layout)?,
            hir::Expression::Constructor(constructor) => {
                let Layout::Struct { size, .. } = layout? else {
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
                    let expression = match self.expression(expression)? {
                        BranchStatus::Continue(value) => value,
                        BranchStatus::Finished => return Ok(BranchStatus::Finished),
                    };
                    self.builder
                        .ins()
                        .stack_store(expression, stack_slot, offset);
                }

                BranchStatus::Continue(addr)
            }
            hir::Expression::Return(expression) => {
                let BranchStatus::Continue(expression) = self.expression(*expression)? else {
                    return Ok(BranchStatus::Finished);
                };
                self.builder.ins().return_(&[expression]);
                return Ok(BranchStatus::Finished);
            }
            hir::Expression::LocalAccess(variable) => {
                BranchStatus::Continue(self.builder.use_var(variable.into()))
            }
            hir::Expression::GlobalAccess(_) => todo!(),
            hir::Expression::Call(call) => {
                let hir::Expression::GlobalAccess(declaration) = call.callable.expression else {
                    todo!("closures!")
                };
                let function = self.declarations.get_function(declaration)?;

                let mut arguments = Vec::new();
                for expression in call.arguments {
                    let BranchStatus::Continue(value) = self.expression(expression)? else {
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

                BranchStatus::Continue(self.builder.inst_results(call)[0])
            }
        };

        Ok(value)
    }
}
