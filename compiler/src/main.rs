#![warn(clippy::pedantic, clippy::nursery)]

mod type_resolution;
use std::collections::HashMap;

use cranelift::prelude::*;
use parser::{top_level::DeclarationKind, Statement, prelude::Literal, Expression, expression::IntrinsicCall};

// mod translate;

// fn get_expression_information(expression: Expression) -> (Value, type_resolution::Id) {
//     match expression {
//         Expression::Literal(literal) => match literal {
//             _ => todo!("literal")
//         },
//         Expression::IntrinsicCall(intrinsic) => match intrinsic {
//             IntrinsicCall::I32(integer) =>
//         }
//     }
// }

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root;
    let mut type_store = type_resolution::TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    type_resolution::TypeScope::append_new(&mut type_store, file.root).unwrap();

    for (ident, declaration) in &type_store.file_cache.get(root).declarations {
        let DeclarationKind::Function(function) = declaration else {
            continue;
        };
        let mut stack_slots = Vec::new();
        for statement in &function.body {
            match statement {
                Statement::Expression(expression) => todo!("handle expr"),
                Statement::Let(ident, expression) => stack_slots.push((ident, expression)),
            }
        }
    }
}
