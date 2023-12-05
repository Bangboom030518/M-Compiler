#![warn(clippy::pedantic, clippy::nursery)]

mod type_resolution;
use itertools::Itertools as _;
use std::collections::HashMap;
// use cranelift::prelude::*;
use parser::{
    expression::{IntrinsicCall, UnaryOperator},
    prelude::Literal,
    top_level::DeclarationKind,
    Expression,
};
use type_resolution::Type;

#[derive(Debug, Clone)]
enum Statement {
    Assignment(StackSlotId, Value),
    Ignore(Value),
    Return(Value),
}

#[derive(Debug, Clone)]
enum Value {
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
    StackSlot(StackSlotId),
}

impl Value {
    fn ensure_type(self, r#type: &Type) -> Result<Self, SemanticError> {
        macro_rules! int_assert_branch {
            ($type_name:ident) => {
                match r#type {
                    Type::$type_name => Ok(self),
                    _ => Err(SemanticError::MismatchedType),
                }
            };
        }

        match self {
            Self::U8Const(_) => int_assert_branch!(U8),
            Self::U16Const(_) => int_assert_branch!(U16),
            Self::U32Const(_) => int_assert_branch!(U32),
            Self::U64Const(_) => int_assert_branch!(U64),
            Self::U128Const(_) => int_assert_branch!(U128),
            Self::I8Const(_) => int_assert_branch!(I8),
            Self::I16Const(_) => int_assert_branch!(I16),
            Self::I32Const(_) => int_assert_branch!(I32),
            Self::I64Const(_) => int_assert_branch!(I64),
            Self::I128Const(_) => int_assert_branch!(I128),
            Self::F32Const(_) => int_assert_branch!(F32),
            Self::F64Const(_) => int_assert_branch!(F64),
        }

        match (self, r#type) {
            (Self::U8Const(_), Type::U8) => Ok(self),
            (Self::U16Const(_), Type::U16) => Ok(self),
            (Self::U32Const(_), Type::U32) => Ok(self),
            (Self::U64Const(_), Type::U64) => Ok(self),
            (Self::U128Const(_), Type::U128) => Ok(self),
            (Self::I8Const(_), Type::I8) => Ok(self),
            (Self::I16Const(_), Type::I16) => Ok(self),
            (Self::I32Const(_), Type::I32) => Ok(self),
            (Self::I64Const(_), Type::I64) => Ok(self),
            (Self::I128Const(_), Type::I128) => Ok(self),
            (Self::F32Const(_), Type::F32) => Ok(self),
            (Self::F64Const(_), Type::F64) => Ok(self),
            (Self::UnknownIntegerConst(int), Type::U8) => Ok(Value::U8Const(u8::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::U16) => Ok(Value::U16Const(u16::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::U32) => Ok(Value::U32Const(u32::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::U64) => Ok(Value::U64Const(u64::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::U128) => Ok(Value::U128Const(int)),
            (Self::UnknownIntegerConst(int), Type::I8) => Ok(Value::I8Const(i8::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::I16) => Ok(Value::I16Const(i16::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::I32) => Ok(Value::I32Const(i32::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::I64) => Ok(Value::I64Const(i64::try_from(int)?)),
            (Self::UnknownIntegerConst(int), Type::I128) => {
                Ok(Value::I128Const(i128::try_from(int)?))
            }
            (Self::UnknownSignedIntegerConst(int), Type::I8) => {
                Ok(Value::I8Const(i8::try_from(int)?))
            }
            (Self::UnknownSignedIntegerConst(int), Type::I16) => {
                Ok(Value::I16Const(i16::try_from(int)?))
            }
            (Self::UnknownSignedIntegerConst(int), Type::I32) => {
                Ok(Value::I32Const(i32::try_from(int)?))
            }
            (Self::UnknownSignedIntegerConst(int), Type::I64) => {
                Ok(Value::I64Const(i64::try_from(int)?))
            }
            (Self::UnknownSignedIntegerConst(int), Type::I128) => Ok(Value::I128Const(int)),
            (Self::UnknownFloatConst(float), Type::F32) => Ok(Value::F32Const(float as f32)),
            (Self::UnknownFloatConst(float), Type::F64) => Ok(Value::F64Const(float)),

            _ => Err(SemanticError::MismatchedType),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type error :)")]
// TODO: annotated results
enum SemanticError {
    InvalidIntegerLiteral,
    IntegerOverflow(#[from] std::num::TryFromIntError),
    UnknownType,
    MismatchedType,
    InvalidAssignment,
    DeclarationNotFound,
}

#[derive(Default, Debug)]
struct LocalScope(Vec<Option<type_resolution::Id>>);

impl LocalScope {
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: index operator
    pub fn get(&self, StackSlotId(index): StackSlotId) -> Option<type_resolution::Id> {
        *self.0.get(index).unwrap()
    }

    pub fn create_slot(&mut self, r#type: Option<type_resolution::Id>) -> StackSlotId {
        self.0.push(r#type);
        StackSlotId(self.0.len() - 1)
    }
}

#[derive(Clone, Copy, Debug)]
struct StackSlotId(usize);

#[derive(Debug)]
struct ExpressionContext<'a> {
    type_store: &'a type_resolution::TypeStore,
    return_type: Option<type_resolution::Id>,
    current_type: Option<type_resolution::Id>,
    names: HashMap<parser::Ident, StackSlotId>,
    local_scope: &'a mut LocalScope,
    scope_id: parser::scope::Id,
}

impl<'a> ExpressionContext<'a> {
    fn new(
        type_store: &'a type_resolution::TypeStore,
        local_scope: &'a mut LocalScope,
        scope_id: parser::scope::Id,
    ) -> Self {
        Self {
            type_store,
            return_type: None,
            current_type: None,
            local_scope,
            scope_id,
            names: HashMap::new(),
        }
    }

    fn with_return_type(self, r#type: type_resolution::Id) -> Self {
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
        let (value, r#type) = self.get_expression_information(expression)?;
        let stack_slot = self.local_scope.create_slot(r#type);
        self.names.insert(identifier.clone(), stack_slot);
        if r#type != self.local_scope.get(stack_slot) {
            return Err(SemanticError::InvalidAssignment);
        };
        Ok(Statement::Assignment(stack_slot, value))
    }

    fn handle_statement(
        &mut self,
        statement: &parser::Statement,
    ) -> Result<Statement, SemanticError> {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let stack_slot = *self.names.get(name).unwrap_or_else(|| todo!());
                let (value, r#type) = self.get_expression_information(expression)?;
                if r#type != self.local_scope.get(stack_slot) {
                    return Err(SemanticError::InvalidAssignment);
                };
                Ok(Statement::Assignment(stack_slot, value))
            }
            parser::Statement::Let(ident, expression) => {
                self.add_let_declaration(ident, expression)
            }
            parser::Statement::Expression(expression) => {
                if let Expression::Return(expression) = expression {
                    self.current_type = self.return_type;
                    Ok(Statement::Return(
                        self.get_expression_information(&expression.clone())?,
                    ))
                } else {
                    Ok(Statement::Ignore(
                        self.get_expression_information(expression)?,
                    ))
                }
            }
        }
    }

    fn get_integer_value(&self, integer: u128) -> Result<Value, SemanticError> {
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
            Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
        };
        Ok(value)
    }

    fn get_float_value(&self, float: f64) -> Result<Value, SemanticError> {
        let value = match self.current_type() {
            Some(Type::F32) => Value::F32Const(float as f32),
            Some(Type::F64) => Value::F64Const(float),
            None => Value::UnknownFloatConst(float),
            Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
        };
        Ok(value)
    }

    fn get_literal_value(&self, literal: &Literal) -> Result<Value, SemanticError> {
        match literal {
            Literal::Integer(integer) => Ok(self.get_integer_value(*integer)?),
            Literal::Float(float) => Ok(self.get_float_value(*float)?),
            _ => todo!(),
        }
    }

    fn current_type(&self) -> Option<&Type> {
        self.current_type
            .map(|type_id| self.type_store.get(type_id))
    }

    fn get_expression_information(
        &mut self,
        expression: &Expression,
    ) -> Result<Value, SemanticError> {
        match expression {
            Expression::Literal(literal) => self.get_literal_value(literal),
            Expression::UnaryPrefix(operator, expression) => match (*operator, expression.as_ref())
            {
                (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                    // TODO: use `self.get_integer_value()`?
                    let value = match self.current_type() {
                        Some(Type::I8) => Value::I8Const(-i8::try_from(*integer)?),
                        Some(Type::I16) => Value::I16Const(-i16::try_from(*integer)?),
                        Some(Type::I32) => Value::I32Const(-i32::try_from(*integer)?),
                        Some(Type::I64) => Value::I64Const(-i64::try_from(*integer)?),
                        Some(Type::I128) => Value::I128Const(-i128::try_from(*integer)?),
                        None => Value::UnknownSignedIntegerConst(-i128::try_from(*integer)?),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };
                    Ok(value)
                }
                (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                    // TODO: use `self.get_float_value()`?
                    let value = match self.current_type() {
                        Some(Type::F32) => Value::F32Const(-(*float as f32)),
                        Some(Type::F64) => Value::F64Const(-*float),
                        None => Value::UnknownFloatConst(-*float),
                        Some(_) => return Err(SemanticError::InvalidIntegerLiteral),
                    };
                    Ok(value)
                }
                _ => todo!(),
            },
            Expression::IntrinsicCall(intrinsic) => match intrinsic {
                IntrinsicCall::IAdd(left, right) => {
                    // TODO: infer left type from right
                    let mut left = self.get_expression_information(left)?;
                    let right = self.get_expression_information(right)?;
                    if let Some(current_type) = self.current_type() {
                        left = left.ensure_type(current_type)?
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
                        .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
                    self.current_type = Some(r#type);
                    Ok(self.get_expression_information(expression)?)
                }
            },
            Expression::Return(expression) => {
                self.current_type = self.return_type;
                self.get_expression_information(expression)
            }
            Expression::Identifier(ident) => {
                let stack_slot = *self
                    .names
                    .get(ident)
                    .map_or_else(|| Err(SemanticError::DeclarationNotFound), Ok)?;
                self.current_type = self.local_scope.get(stack_slot);
                let value = Value::StackSlot(stack_slot);
                Ok(Value::StackSlot(stack_slot))
            }
            _ => todo!(),
        }
    }
}

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root;
    let mut type_store = type_resolution::TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();
    for declaration in type_store.file_cache[root].declarations.values() {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };

        let Some((return_statement, statements)) = function.body.split_last() else {
            todo!("handle empty body")
        };

        // TODO: `.clone()`?
        let return_statement = if let parser::Statement::Expression(expression) = return_statement {
            parser::Statement::Expression(Expression::Return(Box::new(expression.clone())))
        } else {
            return_statement.clone()
        };

        let mut local_scope = LocalScope::new();
        let context = ExpressionContext::new(&type_store, &mut local_scope, root);
        // TODO: not root
        let mut context = context.with_return_type(
            type_store
                .lookup(
                    match function.return_type.as_ref().unwrap() {
                        parser::Type::Identifier(ident) => ident,
                    },
                    root,
                )
                .unwrap(),
        );
        let statements = statements
            .iter()
            .chain(std::iter::once(&return_statement))
            .map(|statement| context.handle_statement(statement))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        dbg!(&statements);
        dbg!(&context);
        dbg!(&type_store.types);
    }
}
