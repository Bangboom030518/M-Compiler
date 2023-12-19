use crate::type_resolution::{self, Type};
use cranelift::{
    codegen::{ir::Function, isa::CallConv},
    prelude::*,
};
use parser::{
    expression::{IntrinsicCall, UnaryOperator},
    prelude::Literal,
    Expression,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    U8Const(u8),
    U16Const(u16),
    U32Const(u32),
    U64Const(u64),
    U128Const(u128),
    I8Const(i8),
    I16Const(i16),
    I32Const(i32),
    I64Const(i64),
    I128Const(i128),
    F32Const(f32),
    F64Const(f64),
    UnknownSignedIntegerConst(i128),
    UnknownIntegerConst(u128),
    UnknownFloatConst(f64),
    IAdd(Box<Value>, Box<Value>),
    Variable(Variable),
}

impl Value {
    /// remove unknowns
    fn promote(self, r#type: &Type) -> Result<Self, SemanticError> {
        match self {
            Self::UnknownIntegerConst(int) => match r#type {
                Type::U8 => Ok(Self::U8Const(u8::try_from(int)?)),
                Type::U16 => Ok(Self::U16Const(u16::try_from(int)?)),
                Type::U32 => Ok(Self::U32Const(u32::try_from(int)?)),
                Type::U64 => Ok(Self::U64Const(u64::try_from(int)?)),
                Type::U128 => Ok(Self::U128Const(int)),
                Type::I8 => Ok(Self::I8Const(i8::try_from(int)?)),
                Type::I16 => Ok(Self::I16Const(i16::try_from(int)?)),
                Type::I32 => Ok(Self::I32Const(i32::try_from(int)?)),
                Type::I64 => Ok(Self::I64Const(i64::try_from(int)?)),
                Type::I128 => Ok(Self::I128Const(i128::try_from(int)?)),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            Self::UnknownSignedIntegerConst(int) => match r#type {
                Type::I8 => Ok(Self::I8Const(i8::try_from(int)?)),
                Type::I16 => Ok(Self::I16Const(i16::try_from(int)?)),
                Type::I32 => Ok(Self::I32Const(i32::try_from(int)?)),
                Type::I64 => Ok(Self::I64Const(i64::try_from(int)?)),
                Type::I128 => Ok(Self::I128Const(int)),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            Self::UnknownFloatConst(float) => match r#type {
                Type::F32 => Ok(Self::F32Const(float as f32)),
                Type::F64 => Ok(Self::F64Const(float)),
                _ => Err(SemanticError::UnexpectedIntegerLiteral),
            },
            _ => Ok(self),
        }
    }

    pub fn cranelift_value(
        &self,
        builder: &mut cranelift::prelude::FunctionBuilder,
    ) -> Result<cranelift::prelude::Value, SemanticError> {
        // use cranelift::codegen::ir::immediates::{Imm64, Uimm32, Uimm64, Uimm8};

        let value = match self {
            Self::I8Const(value) => builder.ins().iconst(types::I8, i64::from(*value)),
            Self::I16Const(value) => builder.ins().iconst(types::I16, i64::from(*value)),
            Self::I32Const(value) => builder.ins().iconst(types::I32, i64::from(*value)),
            Self::I64Const(value) => builder.ins().iconst(types::I64, *value),
            Self::I128Const(value) => todo!(),
            Self::U8Const(value) => todo!(),
            Self::U16Const(value) => todo!(),
            Self::U32Const(value) => todo!(),
            Self::U64Const(value) => todo!(),
            Self::U128Const(value) => todo!(),
            Self::F32Const(value) => builder.ins().f32const(Ieee32::from(*value)),
            Self::F64Const(value) => builder.ins().f64const(Ieee64::from(*value)),
            Self::UnknownSignedIntegerConst(_)
            | Self::UnknownIntegerConst(_)
            | Self::UnknownFloatConst(_) => return Err(SemanticError::UnknownType),
            Self::IAdd(left, right) => {
                let left = left.cranelift_value(builder)?;
                let right = right.cranelift_value(builder)?;

                builder.ins().iadd(left, right)
            }
            Self::Variable(variable) => builder.use_var(*variable),
        };
        Ok(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("{:?}", &self)]
// TODO: annotated results
pub enum SemanticError {
    UnexpectedIntegerLiteral,
    IntegerOverflow(#[from] std::num::TryFromIntError),
    UnknownType,
    InvalidAssignment,
    DeclarationNotFound,
}

pub struct FunctionBuilder<'a> {
    type_store: &'a type_resolution::TypeStore,
    scope_id: parser::scope::Id,
    return_type: type_resolution::Id,
    current_type: Option<type_resolution::Id>,
    names: HashMap<parser::Ident, (Variable, Option<type_resolution::Id>)>,
    builder: cranelift::prelude::FunctionBuilder<'a>,
    new_variable_index: usize,
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(
        type_store: &'a type_resolution::TypeStore,
        scope_id: parser::scope::Id,
        parameters: Vec<(parser::Ident, type_resolution::Id)>,
        return_type: type_resolution::Id,
        function_builder_context: &'a mut FunctionBuilderContext,
        function: &'a mut Function,
        call_convention: CallConv,
    ) -> Self {
        let mut signature = Signature::new(call_convention);
        signature.params = parameters
            .iter()
            .map(|(_, type_id)| {
                let r#type = type_store.get(*type_id).cranelift_type();
                AbiParam::new(r#type)
            })
            .collect();

        signature.returns = vec![AbiParam::new(type_store.get(return_type).cranelift_type())];

        let mut builder =
            cranelift::prelude::FunctionBuilder::new(function, function_builder_context);
        builder.func.signature = signature;

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // TODO: `to_vec()`?
        let block_params = builder.block_params(entry_block).to_vec();
        dbg!(&block_params);

        let names = parameters
            .into_iter()
            .zip(block_params)
            .enumerate()
            .map(|(index, ((name, type_id), value))| {
                let variable = Variable::new(index);
                // TODO: get type again?
                let r#type = type_store.get(type_id).cranelift_type();
                builder.declare_var(variable, r#type);
                builder.def_var(variable, value);
                (name, (variable, Some(type_id)))
            })
            .collect::<HashMap<_, _>>();

        Self {
            type_store,
            return_type,
            current_type: None,
            scope_id,
            new_variable_index: names.len(),
            names,
            builder,
        }
    }

    pub fn compile(mut self, statements: &[parser::Statement]) -> Result<(), SemanticError> {
        for statement in statements {
            self.handle_statement(statement)?;
        }
        self.builder.finalize();
        Ok(())
    }

    fn create_variable(&mut self) -> Variable {
        let variable = Variable::new(self.new_variable_index);
        self.new_variable_index += 1;
        variable
    }

    pub fn handle_statement(&mut self, statement: &parser::Statement) -> Result<(), SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let (variable, r#type) = *self
                    .names
                    .get(name)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                let value = self.expression(expression)?;
                if r#type == self.current_type {
                    return Err(SemanticError::InvalidAssignment);
                };
                let value = value
                    .cranelift_value(&mut self.builder)
                    .unwrap_or_else(|error| todo!("handle me :( {error}"));

                self.builder.def_var(variable, value);
            }
            parser::Statement::Let(ident, expression) => {
                let value = self.expression(expression)?;
                let variable = self.create_variable();
                let m_type = self
                    .current_type
                    .map(|r#type| self.type_store.get(r#type).clone());

                let cranelift_type = m_type.map_or(cranelift::prelude::types::INVALID, |r#type| {
                    r#type.cranelift_type()
                });

                self.builder.declare_var(variable, cranelift_type);

                self.names
                    .insert(ident.clone(), (variable, self.current_type));
                // TODO: lazily promote
                let value = value
                    .cranelift_value(&mut self.builder)
                    .unwrap_or_else(|error| todo!("handle me :( {error}"));

                self.builder.def_var(variable, value);
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    self.current_type = Some(self.return_type);
                    let value = self
                        .expression(&expression.clone())?
                        .cranelift_value(&mut self.builder)
                        .unwrap_or_else(|error| todo!("handle me :( {error}"));

                    self.builder.ins().return_(&[value]);
                } else {
                    self.expression(expression)?
                        .cranelift_value(&mut self.builder)
                        .unwrap_or_else(|error| todo!("handle me :) {error}"));
                }
            }
        };

        Ok(())
    }

    fn integer(&self, integer: u128) -> Result<Value, SemanticError> {
        let value = match self.current_type() {
            Some(Type::U8) => Value::U8Const(u8::try_from(integer)?),
            Some(Type::U16) => Value::U16Const(u16::try_from(integer)?),
            Some(Type::U32) => Value::U32Const(u32::try_from(integer)?),
            Some(Type::U64) => Value::U64Const(u64::try_from(integer)?),
            Some(Type::U128) => Value::U128Const(integer),
            Some(Type::I8) => Value::I8Const(i8::try_from(integer)?),
            Some(Type::I16) => Value::I16Const(i16::try_from(integer)?),
            Some(Type::I32) => Value::I32Const(i32::try_from(integer)?),
            Some(Type::I64) => Value::I64Const(i64::try_from(integer)?),
            Some(Type::I128) => Value::I128Const(i128::try_from(integer)?),
            None => Value::UnknownIntegerConst(integer),
            Some(_) => return Err(SemanticError::UnexpectedIntegerLiteral),
        };
        Ok(value)
    }

    fn float(&self, float: f64) -> Result<Value, SemanticError> {
        let value = match self.current_type() {
            Some(Type::F32) => Value::F32Const(float as f32),
            Some(Type::F64) => Value::F64Const(float),
            None => Value::UnknownFloatConst(float),
            Some(_) => return Err(SemanticError::UnexpectedIntegerLiteral),
        };
        Ok(value)
    }

    fn literal(&self, literal: &Literal) -> Result<Value, SemanticError> {
        match literal {
            Literal::Integer(integer) => Ok(self.integer(*integer)?),
            Literal::Float(float) => Ok(self.float(*float)?),
            _ => todo!(),
        }
    }

    fn current_type(&self) -> Option<&Type> {
        self.current_type
            .map(|type_id| self.type_store.get(type_id))
    }

    fn intrinsic_call(&mut self, intrinsic: &IntrinsicCall) -> Result<Value, SemanticError> {
        match intrinsic {
            IntrinsicCall::IAdd(left, right) => {
                let mut left = self.expression(left)?;
                let right = self.expression(right)?;
                if let Some(current_type) = self.current_type() {
                    left = left.promote(current_type)?;
                }
                Ok(Value::IAdd(Box::new(left), Box::new(right)))
            }
            IntrinsicCall::AssertType(expression, r#type) => {
                let r#type = self
                    .type_store
                    .lookup(
                        match r#type {
                            parser::Type::Identifier(identifier) => identifier,
                        },
                        self.scope_id,
                    )
                    .ok_or(SemanticError::DeclarationNotFound)?;
                self.current_type = Some(r#type);
                Ok(self.expression(expression)?)
            }
        }
    }

    fn unary_prefix(
        &mut self,
        operator: UnaryOperator,
        expression: &Expression,
    ) -> Result<Value, SemanticError> {
        match (operator, expression) {
            (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                let value = match self.current_type() {
                    Some(Type::I8) => Value::I8Const(-i8::try_from(*integer)?),
                    Some(Type::I16) => Value::I16Const(-i16::try_from(*integer)?),
                    Some(Type::I32) => Value::I32Const(-i32::try_from(*integer)?),
                    Some(Type::I64) => Value::I64Const(-i64::try_from(*integer)?),
                    Some(Type::I128) => Value::I128Const(-i128::try_from(*integer)?),
                    None => Value::UnknownSignedIntegerConst(-i128::try_from(*integer)?),
                    Some(_) => return Err(SemanticError::UnexpectedIntegerLiteral),
                };
                Ok(value)
            }
            (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                self.float(-float)
            }
            _ => todo!(),
        }
    }

    fn expression(&mut self, expression: &Expression) -> Result<Value, SemanticError> {
        match expression {
            Expression::Literal(literal) => self.literal(literal),
            Expression::UnaryPrefix(operator, expression) => {
                self.unary_prefix(*operator, expression)
            }
            Expression::IntrinsicCall(intrinsic) => self.intrinsic_call(intrinsic),
            Expression::Return(expression) => {
                self.current_type = Some(self.return_type);
                self.expression(expression)
            }
            Expression::Identifier(ident) => {
                dbg!(ident);
                dbg!(&self.names);
                let variable = *self
                    .names
                    .get(ident)
                    .ok_or(SemanticError::DeclarationNotFound)?;
                self.current_type = variable.1;
                Ok(Value::Variable(variable.0))
            }
            _ => todo!(),
        }
    }
}
