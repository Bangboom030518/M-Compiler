#![warn(clippy::pedantic, clippy::nursery)]

mod type_resolution;
use std::collections::HashMap;

// use cranelift::prelude::*;
use parser::{
    expression::IntrinsicCall, prelude::Literal, top_level::DeclarationKind, Expression, Statement,
};
use type_resolution::Type;

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
    IAdd(Box<Value>, Box<Value>),
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type error :)")]
enum TypeError {
    InvalidIntegerLiteral,
    Overflow(#[from] std::num::TryFromIntError),
    Unknown,
}

fn get_expression_information(
    expression: &Expression,
    type_store: &type_resolution::TypeStore,
    return_type: Option<type_resolution::Id>,
    context_type: Option<type_resolution::Id>,
) -> Result<(Value, type_resolution::Id), TypeError> {
    match expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(integer) => {
                let type_id = context_type.map_or_else(|| Err(TypeError::Unknown), Ok)?;
                let r#type = type_store.get(type_id);

                let value = match r#type {
                    Type::UInt8 => Value::U8Const(u8::try_from(*integer)?),
                    Type::UInt16 => Value::U16Const(u16::try_from(*integer)?),
                    Type::UInt32 => Value::U32Const(u32::try_from(*integer)?),
                    Type::UInt64 => Value::U64Const(u64::try_from(*integer)?),
                    Type::UInt128 => Value::U128Const(*integer),
                    _ => return Err(TypeError::InvalidIntegerLiteral),
                };

                Ok((value, type_id))
            }
            _ => todo!(),
        },
        Expression::IntrinsicCall(intrinsic) => match intrinsic {
            IntrinsicCall::IAdd(left, right) => {
                let left = get_expression_information(left, type_store, return_type, context_type)?;
                let right =
                    get_expression_information(right, type_store, return_type, Some(left.1))?;
                Ok((Value::IAdd(Box::new(left.0), Box::new(right.0)), left.1))
            }
        },
        Expression::Return(expression) => {
            get_expression_information(expression, type_store, return_type, return_type)
        }
        _ => todo!(),
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

    dbg!(type_store.scopes);
    dbg!(type_store.types);

    for (ident, declaration) in &type_store.file_cache.get(root).declarations {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };
        let mut stack_slots = Vec::new();

        let Some((return_statement, statements)) = function.body.split_last() else {
            todo!("handle empty body")
        };

        for statement in statements {
            match statement {
                Statement::Expression(expression) => get_expression_information(expression, None),
                Statement::Let(ident, expression) => stack_slots.push((ident, expression)),
            }
        }
    }
}
