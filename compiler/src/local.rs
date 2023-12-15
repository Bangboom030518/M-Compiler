use crate::type_resolution::{self, Type};
use cranelift::{codegen::ir::Function, prelude::*};
use parser::{
    expression::{IntrinsicCall, UnaryOperator},
    prelude::Literal,
    Expression, Ident,
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Statement {
    Assignment(VariableId, Value),
    Ignore(Value),
    Return(Value),
}

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
    Variable(VariableId),
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
            Self::I8Const(value) => builder.ins().iconst(types::I8, *value as i64),
            Self::I16Const(value) => builder.ins().iconst(types::I16, *value as i64),
            Self::I32Const(value) => builder.ins().iconst(types::I32, *value as i64),
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
            Self::Variable(variable) => builder.use_var((*variable).into()),
        };
        Ok(value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type error :)")]
// TODO: annotated results
pub enum SemanticError {
    UnexpectedIntegerLiteral,
    IntegerOverflow(#[from] std::num::TryFromIntError),
    UnknownType,
    MismatchedType,
    InvalidAssignment,
    DeclarationNotFound,
    ExpectedReturnType,
    UndefinedVariable,
}

#[derive(Default, Debug)]
pub struct Scope(Vec<Option<type_resolution::Id>>);

impl Scope {
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: index operator
    pub fn get(&self, VariableId(index): VariableId) -> Option<type_resolution::Id> {
        *self.0.get(index).unwrap()
    }

    pub fn create_slot(&mut self, r#type: Option<type_resolution::Id>) -> VariableId {
        self.0.push(r#type);
        VariableId(self.0.len() - 1)
    }

    pub fn variables(
        &self,
    ) -> impl Iterator<Item = (VariableId, Option<type_resolution::Id>)> + '_ {
        self.0
            .iter()
            .enumerate()
            .map(|(index, r#type)| (VariableId(index), *r#type))
    }
}

pub struct FunctionBuilder<'a> {
    type_store: &'a type_resolution::TypeStore,
    scope_id: parser::scope::Id,
    return_type: type_resolution::Id,
    current_type: Option<type_resolution::Id>,
    names: HashMap<parser::Ident, Variable>,
    builder: cranelift::prelude::FunctionBuilder<'a>,
    local_scope: Scope,
    parameters: &'a [(parser::Ident, type_resolution::Id)],
}

impl<'a> FunctionBuilder<'a> {
    pub fn new(
        type_store: &'a type_resolution::TypeStore,
        scope_id: parser::scope::Id,
        parameters: &'a [(parser::Ident, type_resolution::Id)],
        return_type: type_resolution::Id,
        function_builder_context: &mut FunctionBuilderContext,
        function: &mut Function,
    ) -> Self {
        let mut builder =
            cranelift::prelude::FunctionBuilder::new(function, function_builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut signature = Signature::new(isa::CallConv::Fast);

        parameters
            .iter()
            .map(|(_, r#type)| {
                let r#type = type_store.get(*r#type).clone().into();
                AbiParam::new(r#type)
            })
            .collect_into(&mut signature.params);

        let mut index = 0;
        for (param, value) in signature
            .params
            .iter()
            .zip(builder.block_params(entry_block))
        {
            let var = Variable::new(index);
            builder.declare_var(var, param.value_type);
            // TODO: `.clone()`?
            builder.def_var(var, value.clone());
            index += 1;
        }

        signature.returns = vec![AbiParam::new(
            type_store
                .get(return_type)
                .clone()
                .into(),
        )];

        Self {
            type_store,
            return_type,
            current_type: None,
            local_scope: Scope::new(),
            scope_id,
            names: HashMap::new(),
            parameters,
            builder,
        }
    }

    // fn lookup_local(&self, ident: &Ident) -> Result<VariableId, SemanticError> {
    //     self.parameters
    //         .iter()
    //         .find_map(|(name, r#type)| (name == ident).then_some(r#type))
    //         .copied()
    //         .or_else(|| {
    //             self.names
    //                 .get(ident)
    //                 .and_then(|&stack_slot| self.local_scope.get(stack_slot))
    //         })
    //         .map_or_else(|| Err(SemanticError::UndefinedVariable), Ok)
    // }

    fn lookup_local_type(&self, ident: &Ident) -> Result<type_resolution::Id, SemanticError> {
        self.parameters
            .iter()
            .find_map(|(name, r#type)| (name == ident).then_some(r#type))
            .copied()
            .or_else(|| {
                self.names
                    .get(ident)
                    .and_then(|&stack_slot| self.local_scope.get(stack_slot))
            })
            .map_or_else(|| Err(SemanticError::UndefinedVariable), Ok)
    }

    pub fn with_return_type(self, r#type: type_resolution::Id) -> Self {
        Self {
            return_type: Some(r#type),
            ..self
        }
    }

    fn add_let_declaration(
        &mut self,
        identifier: &parser::Ident,
        expression: &Expression,
    ) -> Result<Statement, SemanticError> {
        let value = self.expression(expression)?;
        let stack_slot = self.local_scope.create_slot(self.current_type);
        self.names.insert(identifier.clone(), stack_slot);
        if self.current_type != self.local_scope.get(stack_slot) {
            return Err(SemanticError::InvalidAssignment);
        };
        Ok(Statement::Assignment(stack_slot, value))
    }

    pub fn handle_statement(
        &mut self,
        statement: &parser::Statement,
    ) -> Result<Statement, SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let variable_type = self.lookup_local_type(name)?;
                let value = self.expression(expression)?;
                if let Some(current_type) = self.current_type {
                    if current_type != variable_type {
                        return Err(SemanticError::InvalidAssignment);
                    }
                };
                Ok(Statement::Assignment(*self.names.get(name).unwrap(), value))
            }
            parser::Statement::Let(ident, expression) => {
                self.add_let_declaration(ident, expression)
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    self.current_type = self.return_type;
                    Ok(Statement::Return(self.expression(&expression.clone())?))
                } else {
                    Ok(Statement::Ignore(self.expression(expression)?))
                }
            }
        }
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
                dbg!("assert type");
                let r#type = self
                    .type_store
                    .lookup(
                        match r#type {
                            parser::Type::Identifier(identifier) => identifier,
                        },
                        self.scope_id,
                    )
                    .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
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
                self.current_type = self.return_type;
                self.expression(expression)
            }
            Expression::Identifier(ident) => {
                dbg!(ident);
                dbg!(self.names);
                let variable = *self
                    .names
                    .get(ident)
                    .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
                self.current_type = self.local_scope.get(variable);
                Ok(Value::Variable(variable))
            }
            _ => todo!(),
        }
    }
}
