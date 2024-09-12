use std::collections::hash_map::Keys;

use crate::declarations::{Declarations, FuncReference, ScopeId};
use crate::function::{AGGREGATE_PARAM_VARIABLE, SPECIAL_VARIABLES};
use crate::hir::builder::VariableId;
use crate::layout::{Layout, Primitive};
use crate::{hir, FunctionCompiler, SemanticError};
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
    declarations: &'a mut Declarations,
    context: &'a mut M,
    scope: ScopeId,
    function_compiler: &'a mut crate::FunctionCompiler,
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
        declarations: &'a mut Declarations,
        context: &'a mut M,
        function_compiler: &'a mut FunctionCompiler,
        scope: ScopeId,
    ) -> Self {
        Self {
            builder,
            declarations,
            context,
            scope,
            function_compiler,
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
                    .insert_layout(assignment.right.expect_type()?, self.scope)?;

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
                    self.declarations.isa.pointer_type(),
                    // self.declarations
                    //     .insert_layout(expression.expect_type()?, self.scope)?
                    //     .cranelift_type(&self.declarations.isa),
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

        self.builder
            .append_block_param(merge_block, self.declarations.isa.pointer_type());

        // if let Some(then_return) = &then_branch.expression {
        //     self.builder.append_block_param(
        //         merge_block,
        //         match self
        //             .declarations
        //             .insert_layout(then_return.expect_type()?, self.scope)?
        //         {
        //             Layout::Primitive(primitive) => {
        //                 dbg!(&primitive);
        //                 dbg!(primitive.cranelift_type(self.declarations.isa.pointer_type()))
        //             }
        //             Layout::Struct(_) => todo!("structs in ifs!"),
        //             Layout::Array(_) => todo!("arrays in ifs!"),
        //         },
        //     );
        // } else {
        //     todo!("void ifs")
        // }

        let BranchStatus::Continue(condition) = self.load_primitive(condition)? else {
            return Ok(BranchStatus::Finished);
        };

        // let condition = self.builder.ins().load(
        //     condition_layout.cranelift_type(&self.declarations.isa),
        //     MemFlags::new(),
        //     condition,
        //     Offset32::new(0),
        // );

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
        layout: &Layout,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let struct_layout = self
            .declarations
            .insert_layout(access.expression.expect_type()?, self.scope)?;

        let BranchStatus::Continue(value) = self.expression(access.expression)? else {
            return Ok(BranchStatus::Finished);
        };

        let struct_layout = match struct_layout {
            Layout::Struct(struct_layout) => struct_layout,
            Layout::Primitive(_) | Layout::Void | Layout::Array(_) => {
                return Err(SemanticError::InvalidFieldAccess(struct_layout.clone()))
            }
        };

        let offset = struct_layout
            .fields
            .get(access.field.as_ref())
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
            align_shift: 0,
            size: layout.size,
        });

        let addr = self.builder.ins().stack_addr(
            self.declarations.isa.pointer_type(),
            stack_slot,
            Offset32::new(0),
        );

        for (offset, expression) in constructor.0 {
            let layout = self
                .declarations
                .insert_layout(expression.expect_type()?, self.scope)?;

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
        let (callable, generics) =
            if let hir::Expression::Generixed(generixed) = call.callable.expression {
                (generixed.expression.expression, generixed.generics)
            } else {
                (call.callable.expression, Vec::new())
            };

        let hir::Expression::GlobalAccess(declaration) = callable else {
            todo!("closures!")
        };

        let reference = FuncReference {
            id: declaration,
            generics,
        };

        let function =
            self.declarations
                .insert_function(reference.clone(), &mut self.context, self.scope)?;

        self.function_compiler.push(reference);

        let mut arguments = Vec::new();
        for expression in call.arguments {
            expression.expect_type()?;
            let BranchStatus::Continue(value) = self.load_primitive(expression)? else {
                return Ok(BranchStatus::Finished);
            };
            arguments.push(value);
        }

        let return_layout = self
            .declarations
            .insert_layout(&function.signature().return_type, self.scope)?;

        if return_layout.is_aggregate() {
            let stack_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                return_layout.size(&self.declarations.isa),
                0,
            ));

            let addr = self.builder.ins().stack_addr(
                self.declarations.isa.pointer_type(),
                stack_slot,
                Offset32::new(0),
            );

            arguments.push(addr);
        }

        let func_ref = self
            .context
            .declare_func_in_func(function.id(), self.builder.func);

        let call = self.builder.ins().call(func_ref, arguments.as_slice());
        if return_layout == Layout::Void {
            // TODO: What????
            let value = self
                .builder
                .ins()
                .iconst(self.context.isa().pointer_type(), 0);

            Ok(BranchStatus::Continue(value))
        } else {
            let value = self.builder.inst_results(call)[0];
            let value = if return_layout.is_aggregate() {
                value
            } else {
                self.put_in_stack_slot(value, return_layout.size(&self.declarations.isa))
            };
            Ok(BranchStatus::Continue(value))
        }
    }

    fn put_in_stack_slot(&mut self, value: Value, size: u32) -> Value {
        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            size,
            align_shift: 0,
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
        let layout = self
            .declarations
            .insert_layout(expression.expect_type()?, self.scope)?;

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

    fn store(&mut self, store: hir::Store) -> Result<BranchStatus<Value>, SemanticError> {
        let layout = self
            .declarations
            .insert_layout(store.pointer.expect_type()?, self.scope)?;
        if layout != Layout::Primitive(crate::layout::Primitive::USize) {
            return Err(SemanticError::InvalidAddr {
                found: layout,
                expression: store.pointer.expression,
            });
        }

        let layout = self
            .declarations
            .insert_layout(store.expression.expect_type()?, self.scope)?;

        if layout.is_aggregate() {
            todo!("store aggregates")
        } else {
            let BranchStatus::Continue(pointer) = self.load_primitive(store.pointer)? else {
                return Ok(BranchStatus::Finished);
            };
            let BranchStatus::Continue(expression) = self.load_primitive(store.expression)? else {
                return Ok(BranchStatus::Finished);
            };
            self.builder
                .ins()
                .store(MemFlags::new(), expression, pointer, Offset32::new(0));
            Ok(BranchStatus::Continue(self.iconst(0)))
        }
    }

    fn string_const(
        &mut self,
        string: &str,
        layout: &Layout,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let data = self
            .context
            .declare_anonymous_data(false, false)
            .expect("Internal Module Error");

        let Layout::Array(array) = layout else {
            return Err(SemanticError::InvalidStringConst {
                expected: layout.clone(),
            });
        };
        let bytes = string.as_bytes().to_vec();

        if array.length != bytes.len() as u128
            || !matches!(
                self.declarations.insert_layout(&array.item, self.scope)?,
                Layout::Primitive(Primitive::U8)
            )
        {
            return Err(SemanticError::InvalidStringConst {
                expected: layout.clone(),
            });
        }

        let mut desc = cranelift_module::DataDescription::new();
        desc.define(bytes.into_boxed_slice());
        self.context
            .define_data(data, &desc)
            .expect("Internal Module Error :(");
        let value = self.context.declare_data_in_func(data, self.builder.func);

        let value = self
            .builder
            .ins()
            .global_value(self.declarations.isa.pointer_type(), value);

        Ok(BranchStatus::Continue(
            // self.put_in_stack_slot(value, self.declarations.isa.pointer_bytes().into()),
            value,
        ))
    }

    fn expression(
        &mut self,
        hir::TypedExpression {
            expression,
            type_ref,
        }: hir::TypedExpression,
    ) -> Result<BranchStatus<Value>, SemanticError> {
        let layout =
            match type_ref.map(|type_id| self.declarations.insert_layout(&type_id, self.scope)) {
                Some(result) => Ok(result?),
                None => Err(SemanticError::UnknownType(expression.clone())),
            };

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
                    Primitive::USize => self.declarations.isa.pointer_type(),
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
            hir::Expression::StringConst(string) => self.string_const(&string, &layout?)?,
            hir::Expression::Addr(inner) => {
                layout?;
                let BranchStatus::Continue(value) = self.expression(*inner)? else {
                    return Ok(BranchStatus::Finished);
                };
                BranchStatus::Continue(
                    self.put_in_stack_slot(value, self.declarations.isa.pointer_bytes().into()),
                )
            }
            hir::Expression::Load(inner) => {
                if layout?.is_aggregate() {
                    todo!("loading structs")
                } else {
                    self.load_primitive(*inner)?
                }
            }
            hir::Expression::Store(store) => self.store(*store)?,
            hir::Expression::BinaryIntrinsic(binary) => self.binary_intrinsic(*binary, &layout?)?,
            hir::Expression::If(r#if) => self.translate_if(*r#if)?,
            hir::Expression::FieldAccess(access) => {
                // layout?;
                self.field_access(*access, &layout?)?
            }
            hir::Expression::Constructor(constructor) => self.constructor(constructor, &layout?)?,
            hir::Expression::Return(expression) => {
                let layout = expression
                    .type_ref
                    .clone()
                    .ok_or(SemanticError::UnknownType(hir::Expression::Return(
                        expression.clone(),
                    )))?;

                let layout = self.declarations.insert_layout(&layout, self.scope)?;

                let BranchStatus::Continue(value) = self.load_primitive(*expression)? else {
                    return Ok(BranchStatus::Finished);
                };

                if layout.is_aggregate() {
                    let return_param = self
                        .builder
                        .use_var(Variable::new(AGGREGATE_PARAM_VARIABLE));

                    let size = self.iconst(layout.size(&self.declarations.isa));
                    self.builder.call_memcpy(
                        self.declarations.isa.frontend_config(),
                        return_param,
                        value,
                        size,
                    );
                    self.builder.ins().return_(&[return_param]);
                } else {
                    self.builder.ins().return_(&[value]);
                }

                return Ok(BranchStatus::Finished);
            }
            hir::Expression::LocalAccess(variable) => {
                BranchStatus::Continue(self.builder.use_var(variable.into()))
            }
            hir::Expression::Call(call) => self.call(*call)?,
            hir::Expression::Generixed(_) => todo!("generixed"),
            hir::Expression::GlobalAccess(_) => todo!("global access"),
            hir::Expression::AssertType(inner) => self.expression(*inner)?,
        };

        Ok(value)
    }
}
