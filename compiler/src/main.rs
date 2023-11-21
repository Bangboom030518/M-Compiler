#![warn(clippy::pedantic, clippy::nursery)]

mod type_resolution;
use std::collections::HashMap;

// use cranelift::prelude::*;
use parser::{
    expression::{IntrinsicCall, UnaryOperator},
    prelude::Literal,
    top_level::DeclarationKind,
    Expression,
};
use type_resolution::Type;

enum Statement {
    Assignment(StackSlotId, Value),
    Ignore(Value),
    Return(Value),
}

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
    IAdd(Box<Value>, Box<Value>),
    StackSlot(StackSlotId),
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type error :)")]
enum TypeError {
    InvalidIntegerLiteral,
    IntegerOverflow(#[from] std::num::TryFromIntError),
    Unknown,
}

#[derive(Default)]
struct LocalScope(Vec<type_resolution::Id>);

impl LocalScope {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create_slot(&mut self, r#type: type_resolution::Id) -> StackSlotId {
        self.0.push(r#type);
        StackSlotId(self.0.len() - 1)
    }
}

#[derive(Clone, Copy)]
struct StackSlotId(usize);

struct ExpressionContext<'a> {
    type_store: &'a type_resolution::TypeStore,
    return_type: Option<type_resolution::Id>,
    context_type: Option<type_resolution::Id>,
    names: HashMap<parser::Ident, StackSlotId>,
    local_scope: &'a mut LocalScope,
}

impl<'a> ExpressionContext<'a> {
    fn new(type_store: &'a type_resolution::TypeStore, local_scope: &'a mut LocalScope) -> Self {
        Self {
            type_store,
            return_type: None,
            context_type: None,
            local_scope,
            names: HashMap::new(),
        }
    }

    fn return_type(self, r#type: type_resolution::Id) -> Self {
        Self {
            return_type: Some(r#type),
            ..self
        }
    }

    fn add_let_declaration(
        &mut self,
        identifier: parser::Ident,
        expression: &Expression,
    ) -> Result<(), TypeError> {
        let (value, r#type) = self.get_expression_information(&expression)?;
        self.local_scope.create_slot(r#type);
        todo!("add assignment")
    }

    // TODO: result
    fn handle_statement(&mut self, statement: parser::Statement) -> Statement {
        match statement {
            parser::Statement::Assignment(parser::Assignment(name, expression)) => {
                let stack_slot = self.names.get(&name).unwrap_or_else(|| todo!());
                let (value, r#type) = self.get_expression_information(&expression)?;
                // TODO: result
                assert!(r#type == self.local_scope.get(stack_slot));
                Statement::Assignment(*stack_slot, value)
            }
            _ => todo!(),
        }
    }

    fn get_expression_information(
        &mut self,
        expression: &Expression,
    ) -> Result<(Value, type_resolution::Id), TypeError> {
        let type_id = self
            .context_type
            .map_or_else(|| Err(TypeError::Unknown), Ok)?;
        let r#type = self.type_store.get(type_id);

        match expression {
            Expression::Literal(literal) => match literal {
                Literal::Integer(integer) => {
                    let value = match r#type {
                        Type::U8 => Value::U8Const(u8::try_from(*integer)?),
                        Type::U16 => Value::U16Const(u16::try_from(*integer)?),
                        Type::U32 => Value::U32Const(u32::try_from(*integer)?),
                        Type::U64 => Value::U64Const(u64::try_from(*integer)?),
                        Type::U128 => Value::U128Const(*integer),
                        Type::I8 => Value::I8Const(i8::try_from(*integer)?),
                        Type::I16 => Value::I16Const(i16::try_from(*integer)?),
                        Type::I32 => Value::I32Const(i32::try_from(*integer)?),
                        Type::I64 => Value::I64Const(i64::try_from(*integer)?),
                        Type::I128 => Value::I128Const(i128::try_from(*integer)?),
                        _ => return Err(TypeError::InvalidIntegerLiteral),
                    };

                    Ok((value, type_id))
                }
                Literal::Float(float) => {
                    let value = match r#type {
                        Type::F32 => Value::F32Const(*float as f32),
                        Type::F64 => Value::F64Const(*float),
                        _ => return Err(TypeError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                _ => todo!(),
            },
            Expression::UnaryPrefix(operator, expression) => match (*operator, expression.as_ref())
            {
                (UnaryOperator::Minus, Expression::Literal(Literal::Integer(integer))) => {
                    let value = match r#type {
                        Type::I8 => Value::I8Const(-i8::try_from(*integer)?),
                        Type::I16 => Value::I16Const(-i16::try_from(*integer)?),
                        Type::I32 => Value::I32Const(-i32::try_from(*integer)?),
                        Type::I64 => Value::I64Const(-i64::try_from(*integer)?),
                        Type::I128 => Value::I128Const(-i128::try_from(*integer)?),
                        _ => return Err(TypeError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                (UnaryOperator::Minus, Expression::Literal(Literal::Float(float))) => {
                    let value = match r#type {
                        Type::F32 => Value::F32Const(-(*float as f32)),
                        Type::F64 => Value::F64Const(-*float),
                        _ => return Err(TypeError::InvalidIntegerLiteral),
                    };
                    Ok((value, type_id))
                }
                _ => todo!(),
            },
            Expression::IntrinsicCall(intrinsic) => match intrinsic {
                IntrinsicCall::IAdd(left, right) => {
                    let left = self.get_expression_information(left)?;
                    self.context_type = Some(left.1);
                    let right = self.get_expression_information(right)?;
                    Ok((Value::IAdd(Box::new(left.0), Box::new(right.0)), left.1))
                }
            },
            Expression::Return(expression) => {
                self.context_type = self.return_type;
                self.get_expression_information(expression)
            }
            _ => todo!(),
        }
    }
}

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).unwrap();
    dbg!(&file);
    let root = file.root;
    let mut type_store = type_resolution::TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();

    for (ident, declaration) in &type_store.file_cache[root].declarations {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };

        let Some((return_statement, statements)) = function.body.split_last() else {
            todo!("handle empty body")
        };

        for statement in statements {
            match statement {
                parser::Statement::Expression(expression) => {
                    ExpressionContext::new(&type_store)
                        .get_expression_information(expression)
                        .unwrap_or_else(|_| todo!());
                }
                parser::Statement::Let(ident, expression) => {
                    let (value, r#type) = ExpressionContext::new(&type_store)
                        .get_expression_information(expression)
                        .unwrap_or_else(|_| todo!());
                }
            }
        }
    }

    dbg!(type_store.scopes);
    dbg!(type_store.types);
}
