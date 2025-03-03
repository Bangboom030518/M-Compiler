use crate::declarations::{Declarations, Reference};
use crate::function::AGGREGATE_PARAM_VARIABLE;
use crate::layout::Layout;
use crate::{errors, hir, Error, FunctionCompiler};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::Module;
use itertools::Itertools;
use parser::expression::{self, IntrinsicOperator};
use parser::{Primitive, PrimitiveKind};
use std::sync::Arc;
use tokenizer::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BranchStatus<T> {
    Finished,
    Continue(T),
}

impl BranchStatus<()> {
    fn map_none<T>(self) -> BranchStatus<Option<T>> {
        match self {
            Self::Finished => BranchStatus::Finished,
            Self::Continue(_) => BranchStatus::Continue(None),
        }
    }
}

impl<T> BranchStatus<T> {
    fn map_some(self) -> BranchStatus<Option<T>> {
        match self {
            Self::Finished => BranchStatus::Finished,
            Self::Continue(value) => BranchStatus::Continue(Some(value)),
        }
    }
}

pub struct Translator<'a, M> {
    builder: cranelift::prelude::FunctionBuilder<'a>,
    declarations: &'a mut Declarations,
    module: &'a mut M,
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
    ) -> Self {
        Self {
            builder,
            declarations,
            module: context,
            function_compiler,
        }
    }

    pub fn iconst(&mut self, value: u32) -> Value {
        self.builder.ins().iconst(
            self.declarations.isa.pointer_type(),
            Imm64::from(i64::from(value)),
        )
    }

    pub fn statement(&mut self, statement: hir::Statement) -> Result<BranchStatus<()>, Error> {
        match statement {
            hir::Statement::Assignment(assignment) => {
                let type_ref = assignment.left.type_ref.clone();
                let BranchStatus::Continue(left) = self.expression(assignment.left)? else {
                    return Ok(BranchStatus::Finished);
                };

                let layout = self.declarations.insert_layout_initialised(&type_ref)?;
                let size = layout.size(self.declarations)?;
                let size = self.iconst(size);

                let BranchStatus::Continue(right) = self.expression(assignment.right)? else {
                    return Ok(BranchStatus::Finished);
                };

                if layout != Layout::Primitive(PrimitiveKind::Void) {
                    self.builder.call_memmove(
                        self.declarations.isa.frontend_config(),
                        left.expect("value should be non-void"),
                        right.expect("value should be non-void"),
                        size,
                    );
                }
            }
            hir::Statement::Expression(expression) => {
                if self.expression(expression)? == BranchStatus::Finished {
                    return Ok(BranchStatus::Finished);
                }
            }
            hir::Statement::Let(variable, expression) => {
                self.builder
                    .declare_var(variable.into(), self.declarations.isa.pointer_type());
                let layout = self
                    .declarations
                    .insert_layout_initialised(&expression.type_ref);
                let BranchStatus::Continue(value) = self.expression(expression)? else {
                    return Ok(BranchStatus::Finished);
                };
                if layout? != Layout::Primitive(PrimitiveKind::Void) {
                    self.builder
                        .def_var(variable.into(), value.expect("value should be non-void"));
                }
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
    ) -> Result<BranchStatus<Option<Value>>, Error> {
        for statement in statements {
            if self.statement(statement)? == BranchStatus::Finished {
                return Ok(BranchStatus::Finished);
            }
        }

        match expression {
            Some(expression) => self.expression(expression),
            None => Ok(BranchStatus::Continue(None)),
        }
    }

    fn translate_if(
        &mut self,
        hir::If {
            condition,
            then_branch,
            else_branch,
        }: hir::If,
        layout: &Layout,
        span: Span,
        found: &Reference,
    ) -> Result<BranchStatus<Option<Value>>, Error> {
        let condition_type = self
            .declarations
            .insert_layout_initialised(&condition.type_ref)?;

        if condition_type != Layout::Primitive(PrimitiveKind::Bool) {
            return Err(Error {
                span: condition.span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Bool,
                    found: condition.type_ref,
                },
            });
        }

        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .append_block_param(merge_block, self.declarations.isa.pointer_type());

        let BranchStatus::Continue(condition) = self.load_primitive(condition)? else {
            return Ok(BranchStatus::Finished);
        };

        self.builder.ins().brif(
            condition.expect("value should be non-void"),
            then_block,
            &[],
            else_block,
            &[],
        );

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        match self.block(then_branch)? {
            BranchStatus::Finished => {}
            BranchStatus::Continue(value) => {
                let result: &[_] = if layout == &Layout::Primitive(PrimitiveKind::Void) {
                    &[]
                } else {
                    &[value.expect("value should be non-void")]
                };

                self.builder.ins().jump(merge_block, result);
            }
        };

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);
        match self.block(else_branch)? {
            BranchStatus::Finished => {}
            BranchStatus::Continue(value) => {
                let result: &[_] = if layout == &Layout::Primitive(PrimitiveKind::Void) {
                    &[]
                } else {
                    &[value.expect("value should be non-void")]
                };

                self.builder.ins().jump(merge_block, result);
            }
        };

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);

        if layout == &Layout::Primitive(PrimitiveKind::Void) {
            Ok(BranchStatus::Continue(None))
        } else {
            Ok(BranchStatus::Continue(Some(
                self.builder.block_params(merge_block)[0],
            )))
        }
    }

    fn binary_intrinsic(
        &mut self,
        binary: hir::BinaryIntrinsic,
        layout: &Layout,
        type_ref: &Reference,
        span: Span,
    ) -> Result<BranchStatus<Value>, Error> {
        let input_type_ref = binary.left.type_ref.clone();
        let Layout::Primitive(input_type) = self
            .declarations
            .insert_layout_initialised(&input_type_ref)?
        else {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Number,
                    found: input_type_ref,
                },
            });
        };

        if input_type == PrimitiveKind::Void {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Number,
                    found: input_type_ref,
                },
            });
        };

        let BranchStatus::Continue(left) = self.load_primitive(binary.left)? else {
            return Ok(BranchStatus::Finished);
        };

        let BranchStatus::Continue(right) = self.load_primitive(binary.right)? else {
            return Ok(BranchStatus::Finished);
        };

        if matches!(binary.operator, IntrinsicOperator::Cmp(_))
            && layout != &Layout::Primitive(PrimitiveKind::Bool)
        {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Bool,
                    found: type_ref.clone(),
                },
            });
        }

        let left = left.expect("value should be non-void");
        let right = right.expect("value should be non-void");

        let value = match binary.operator {
            IntrinsicOperator::Add if input_type.is_integer() => {
                self.builder.ins().iadd(left, right)
            }
            IntrinsicOperator::Add if input_type.is_float() => self.builder.ins().fadd(left, right),
            IntrinsicOperator::Sub if input_type.is_integer() => {
                self.builder.ins().isub(left, right)
            }
            IntrinsicOperator::Sub if input_type.is_float() => self.builder.ins().fsub(left, right),
            IntrinsicOperator::Mul if input_type.is_integer() => {
                self.builder.ins().imul(left, right)
            }
            IntrinsicOperator::Mul if input_type.is_float() => self.builder.ins().fmul(left, right),
            IntrinsicOperator::Div if input_type.is_signed_integer() => {
                self.builder.ins().sdiv(left, right)
            }
            IntrinsicOperator::Div if input_type.is_integer() => {
                self.builder.ins().udiv(left, right)
            }
            IntrinsicOperator::Div if input_type.is_float() => self.builder.ins().fdiv(left, right),
            IntrinsicOperator::Cmp(operator) if input_type.is_signed_integer() => self
                .builder
                .ins()
                .icmp(operator.signed_intcc(), left, right),
            IntrinsicOperator::Cmp(operator) if input_type.is_integer() => {
                self.builder
                    .ins()
                    .icmp(operator.unsigned_intcc(), left, right)
            }
            IntrinsicOperator::Sub
            | IntrinsicOperator::Add
            | IntrinsicOperator::Mul
            | IntrinsicOperator::Div
            | IntrinsicOperator::Cmp(_) => {
                return Err(Error {
                    span: todo!("argument span"),
                    kind: errors::Kind::TypeConstraintViolation {
                        constraint: errors::TypeConstraint::Number,
                        found: input_type_ref,
                    },
                })
            }
        };

        let size = layout.size(self.declarations)?;
        Ok(BranchStatus::Continue(self.put_in_stack_slot(value, size)))
    }

    fn field_access(
        &mut self,
        access: hir::FieldAccess,
        layout: &Layout,
    ) -> Result<BranchStatus<Option<Value>>, Error> {
        let struct_layout = self
            .declarations
            .insert_layout_initialised(&access.expression.type_ref)?;

        let BranchStatus::Continue(value) = self.expression(access.expression.clone())? else {
            return Ok(BranchStatus::Finished);
        };

        let struct_layout = struct_layout.expect_struct()?;

        let offset = struct_layout
            .fields
            .get(&access.field.value.0)
            .ok_or_else(move || Error {
                span: access.field.span,
                kind: errors::Kind::FieldNotFound {
                    parent_struct: access.expression.type_ref,
                    field: access.field.value.0,
                },
            })?
            .offset;

        let offset = self.builder.ins().iconst(
            self.declarations.isa.pointer_type(),
            Imm64::new(offset.into()),
        );
        if layout == &Layout::Primitive(PrimitiveKind::Void) {
            Ok(BranchStatus::Continue(None))
        } else {
            Ok(BranchStatus::Continue(Some(
                self.builder
                    .ins()
                    .iadd(value.expect("value should be non-void"), offset),
            )))
        }
    }

    fn constructor(
        &mut self,
        constructor: hir::Constructor,
        layout: &Layout,
    ) -> Result<BranchStatus<Value>, Error> {
        let struct_layout = layout.expect_struct()?;

        let stack_slot = self.builder.create_sized_stack_slot(StackSlotData {
            kind: StackSlotKind::ExplicitSlot,
            align_shift: 0,
            size: struct_layout.size,
        });

        let addr = self.builder.ins().stack_addr(
            self.declarations.isa.pointer_type(),
            stack_slot,
            Offset32::new(0),
        );

        // TODO: check all fields are provided for struct

        for (field, expression) in constructor.0 {
            let offset = struct_layout
                .fields
                .get(&field)
                .expect("field should exist")
                .offset;
            let layout = self
                .declarations
                .insert_layout_initialised(&expression.type_ref)?;
            if layout == Layout::Primitive(PrimitiveKind::Void) {
                continue;
            }
            let size = layout.size(self.declarations)?;
            let size = self.iconst(size);

            let addr = self.builder.ins().iadd_imm(addr, i64::from(offset));

            let value = match self.expression(expression)? {
                BranchStatus::Continue(value) => value,
                BranchStatus::Finished => return Ok(BranchStatus::Finished),
            };

            self.builder.call_memmove(
                self.declarations.isa.frontend_config(),
                addr,
                value.expect("value should be non-void"),
                size,
            );
        }

        Ok(BranchStatus::Continue(addr))
    }

    fn call(&mut self, call: hir::Call) -> Result<BranchStatus<Option<Value>>, Error> {
        let (declaration, generics) =
            if let hir::Expression::Generixed(generixed) = call.callable.value {
                let hir::Expression::GlobalAccess(declaration) = generixed.expression.value else {
                    todo!("func refs")
                };
                (declaration, generixed.generics)
            } else {
                todo!("func ref")
            };

        let reference = Reference {
            id: declaration,
            generics,
        };

        self.declarations.insert_function(&reference)?;
        let function = self
            .declarations
            .get_function(&reference)
            .expect("function doesn't exist")
            .clone();

        self.function_compiler.push(reference.clone());

        let mut arguments = Vec::new();
        for expression in call.arguments {
            let layout = self
                .declarations
                .insert_layout_initialised(&expression.type_ref)?;

            let BranchStatus::Continue(value) = self.load_primitive(expression)? else {
                return Ok(BranchStatus::Finished);
            };

            if layout == Layout::Primitive(PrimitiveKind::Void) {
                continue;
            }

            arguments.push(value.expect("value should be non-void"));
        }

        let return_layout = self
            .declarations
            .insert_layout_initialised(&function.signature().return_type)?;

        if return_layout.is_aggregate() {
            let size = return_layout.size(self.declarations)?;
            let stack_slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                size,
                0,
            ));

            let addr = self.builder.ins().stack_addr(
                self.declarations.isa.pointer_type(),
                stack_slot,
                Offset32::new(0),
            );

            arguments.push(addr);
        }

        self.declarations.insert_function(&reference)?;
        let function = self
            .declarations
            .get_function(&reference)
            .expect("function not inserted");

        let func_id = Arc::clone(function)
            .signature()
            .cranelift_declaration(self.module, self.declarations)?
            .1;

        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);

        let call = self.builder.ins().call(func_ref, arguments.as_slice());

        if return_layout == Layout::Primitive(PrimitiveKind::Void) {
            Ok(BranchStatus::Continue(None))
        } else {
            let value = self.builder.inst_results(call)[0];
            let value = if return_layout.is_aggregate() {
                value
            } else {
                let size = return_layout.size(self.declarations)?;
                self.put_in_stack_slot(value, size)
            };
            Ok(BranchStatus::Continue(Some(value)))
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
        expression: hir::Typed<hir::Expression>,
    ) -> Result<BranchStatus<Option<Value>>, Error> {
        let layout = self
            .declarations
            .insert_layout_initialised(&expression.type_ref)?;

        let BranchStatus::Continue(value) = self.expression(expression)? else {
            return Ok(BranchStatus::Finished);
        };

        let Some(value) = value else {
            return Ok(BranchStatus::Continue(None));
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

        Ok(BranchStatus::Continue(Some(value)))
    }

    fn store(&mut self, store: hir::Store) -> Result<BranchStatus<()>, Error> {
        let layout = self
            .declarations
            .insert_layout_initialised(&store.pointer.type_ref)?;

        if layout != Layout::Primitive(PrimitiveKind::USize) {
            return Err(errors::Error {
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Address,
                    found: store.pointer.type_ref,
                },
                span: store.pointer.span,
            });
        }

        let layout = self
            .declarations
            .insert_layout_initialised(&store.expression.type_ref)?;

        if layout == Layout::Primitive(PrimitiveKind::Void) || layout.is_aggregate() {
            return Err(errors::Error {
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::StorableOrLoadable,
                    found: store.expression.type_ref,
                },
                span: store.expression.span,
            });
        }

        let BranchStatus::Continue(pointer) = self.load_primitive(store.pointer)? else {
            return Ok(BranchStatus::Finished);
        };

        let pointer = pointer.expect("pointer translated as void in store");

        let BranchStatus::Continue(expression) = self.load_primitive(store.expression)? else {
            return Ok(BranchStatus::Finished);
        };

        let expression = expression.expect("stored primitive translated as void");

        self.builder
            .ins()
            .store(MemFlags::new(), expression, pointer, Offset32::new(0));
        Ok(BranchStatus::Continue(()))
    }

    fn string_const(
        &mut self,
        string: &str,
        layout: &Layout,
        type_ref: &Reference,
        span: Span,
    ) -> Result<BranchStatus<Value>, Error> {
        let data = self
            .module
            .declare_anonymous_data(false, false)
            .expect("Internal Module Error");

        let Layout::Array(array) = layout else {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::String,
                    found: type_ref.clone(),
                },
            });
        };
        let bytes = string.as_bytes().to_vec();
        let length = self
            .declarations
            .unresolved
            .get_initialised_length(array.length)?;
        let element_type = self
            .declarations
            .insert_layout_initialised(&array.element_type)?;
        if length != bytes.len() as u32 {
            return Err(Error {
                span,
                kind: errors::Kind::MismatchedLengths {
                    expected: length,
                    found: bytes.len() as u32,
                },
            });
        }
        if element_type != Layout::Primitive(PrimitiveKind::U8) {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::String,
                    found: type_ref.clone(),
                },
            });
        }

        let mut desc = cranelift_module::DataDescription::new();
        desc.define(bytes.into_boxed_slice());
        self.module
            .define_data(data, &desc)
            .expect("Internal Module Error :(");
        let value = self.module.declare_data_in_func(data, self.builder.func);

        let value = self
            .builder
            .ins()
            .global_value(self.declarations.isa.pointer_type(), value);

        Ok(BranchStatus::Continue(value))
    }

    fn integer_const(
        &mut self,
        value: u64,
        layout: &Layout,
        type_ref: &Reference,
        span: Span,
    ) -> Result<BranchStatus<Value>, Error> {
        let Layout::Primitive(primitive) = layout else {
            return Err(Error {
                span,
                kind: errors::Kind::TypeConstraintViolation {
                    constraint: errors::TypeConstraint::Integer,
                    found: type_ref.clone(),
                },
            });
        };

        let cranelift_type = match primitive {
            PrimitiveKind::I8 | PrimitiveKind::U8 => types::I8,
            PrimitiveKind::I16 | PrimitiveKind::U16 => types::I16,
            PrimitiveKind::I32 | PrimitiveKind::U32 => types::I32,
            PrimitiveKind::I64 | PrimitiveKind::U64 => types::I64,
            PrimitiveKind::USize => self.declarations.isa.pointer_type(),
            _ => {
                return Err(Error {
                    span,
                    kind: errors::Kind::TypeConstraintViolation {
                        constraint: errors::TypeConstraint::Integer,
                        found: type_ref.clone(),
                    },
                })
            }
        };

        let value = self.builder.ins().iconst(
            cranelift_type,
            i64::try_from(value).map_err(|_| Error {
                span,
                kind: errors::Kind::IntegerLiteralTooBig,
            })?,
        );

        Ok(BranchStatus::Continue(self.put_in_stack_slot(
            value,
            primitive.size(self.declarations.isa.pointer_bytes().into()),
        )))
    }

    fn expression(
        &mut self,
        hir::Typed {
            value: expression,
            type_ref,
            span,
        }: hir::Typed<hir::Expression>,
    ) -> Result<BranchStatus<Option<Value>>, Error> {
        let layout = self.declarations.insert_layout_initialised(&type_ref);

        let value = match expression {
            hir::Expression::IntegerConst(int) => self
                .integer_const(int, &layout?, &type_ref, span)?
                .map_some(),
            hir::Expression::BoolConst(bool) => {
                let layout = layout?;
                if layout != Layout::Primitive(PrimitiveKind::Bool) {
                    return Err(Error {
                        span,
                        kind: errors::Kind::TypeConstraintViolation {
                            constraint: errors::TypeConstraint::Bool,
                            found: type_ref,
                        },
                    });
                }
                let value = self.iconst(bool.into());
                let size = layout.size(self.declarations)?;
                BranchStatus::Continue(Some(self.put_in_stack_slot(value, size)))
            }
            hir::Expression::FloatConst(float) => {
                let value = match layout? {
                    Layout::Primitive(PrimitiveKind::F32) => {
                        #[allow(clippy::cast_possible_truncation)]
                        let value = self.builder.ins().f32const(Ieee32::from(float as f32));
                        self.put_in_stack_slot(value, 4)
                    }
                    Layout::Primitive(PrimitiveKind::F64) => {
                        let value = self.builder.ins().f64const(Ieee64::from(float));
                        self.put_in_stack_slot(value, 8)
                    }
                    _ => {
                        return Err(Error {
                            span,
                            kind: errors::Kind::TypeConstraintViolation {
                                constraint: errors::TypeConstraint::Float,
                                found: type_ref,
                            },
                        })
                    }
                };

                BranchStatus::Continue(Some(value))
            }
            hir::Expression::StringConst(string) => self
                .string_const(&string, &layout?, &type_ref, span)?
                .map_some(),
            hir::Expression::Addr(inner) => {
                layout?;
                let BranchStatus::Continue(value) = self.expression(*inner)? else {
                    return Ok(BranchStatus::Finished);
                };

                let Some(value) = value else {
                    return Err(errors::Error {
                        span,
                        kind: errors::Kind::TypeConstraintViolation {
                            constraint: errors::TypeConstraint::NotVoid,
                            found: type_ref,
                        },
                    });
                };

                BranchStatus::Continue(Some(
                    self.put_in_stack_slot(value, self.declarations.isa.pointer_bytes().into()),
                ))
            }
            hir::Expression::Load(inner) => {
                let layout = layout?;
                if layout == Layout::Primitive(PrimitiveKind::Void) || layout.is_aggregate() {
                    return Err(errors::Error {
                        kind: errors::Kind::TypeConstraintViolation {
                            constraint: errors::TypeConstraint::StorableOrLoadable,
                            found: inner.type_ref,
                        },
                        span: inner.span,
                    });
                }
                self.load_primitive(*inner)?
            }
            hir::Expression::Store(store) => self.store(*store)?.map_none(),
            hir::Expression::BinaryIntrinsic(binary) => self
                .binary_intrinsic(*binary, &layout?, &type_ref, span)?
                .map_some(),
            hir::Expression::If(r#if) => self.translate_if(*r#if, &layout?, span, &type_ref)?,
            hir::Expression::FieldAccess(access) => {
                // TODO: ignore layout?
                self.field_access(*access, &layout?)?
            }
            hir::Expression::Constructor(constructor) => {
                self.constructor(constructor, &layout?)?.map_some()
            }
            hir::Expression::Return(expression) => {
                let layout = self
                    .declarations
                    .insert_layout_initialised(&expression.type_ref)?;

                let BranchStatus::Continue(value) = self.load_primitive(*expression)? else {
                    return Ok(BranchStatus::Finished);
                };

                if layout.is_aggregate() {
                    let return_param = self
                        .builder
                        .use_var(Variable::new(AGGREGATE_PARAM_VARIABLE));
                    let size = layout.size(self.declarations)?;
                    let size = self.iconst(size);
                    self.builder.call_memcpy(
                        self.declarations.isa.frontend_config(),
                        return_param,
                        value.expect("value should be non-void"),
                        size,
                    );
                    self.builder.ins().return_(&[return_param]);
                } else if layout == Layout::Primitive(PrimitiveKind::Void) {
                    self.builder.ins().return_(&[]);
                } else {
                    self.builder
                        .ins()
                        .return_(&[value.expect("value should be non-void")]);
                }

                return Ok(BranchStatus::Finished);
            }
            hir::Expression::LocalAccess(variable) => {
                if layout? == Layout::Primitive(PrimitiveKind::Void) {
                    BranchStatus::Continue(None)
                } else {
                    BranchStatus::Continue(Some(self.builder.use_var(variable.into())))
                }
            }
            hir::Expression::Call(call) => self.call(*call)?,
            hir::Expression::Generixed(_) => todo!("generixed"),
            hir::Expression::GlobalAccess(id) => {
                let length = self.declarations.unresolved.get_initialised_length(id)?;

                // TODO: strictness: lengths must be usize?
                self.integer_const(length.into(), &layout?, &type_ref, span)?
                    .map_some()
            }
            hir::Expression::SizeOf(reference) => {
                let size = self
                    .declarations
                    .insert_layout_initialised(&reference)?
                    .size(&mut self.declarations)?;

                // TODO: strictness: sizes must be usize?
                self.integer_const(size.into(), &layout?, &type_ref, span)?
                    .map_some()
            }
            hir::Expression::AssertType(inner) => self.expression(*inner)?,
        };

        Ok(value)
    }
}
