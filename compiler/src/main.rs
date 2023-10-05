#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::prelude::*;
use std::{
    collections::HashMap,
    sync::atomic::{AtomicU32, Ordering},
};

static ID: AtomicU32 = AtomicU32::new(0);

#[derive(Debug)]
enum TypeKind {
    Struct(Struct),
    Union(Union),
}

#[derive(Debug)]
struct Type {
    id: u32,
    kind: TypeKind,
}

fn main() {
    let ast = parse_file(include_str!("../../input.m")).unwrap();
    let mut map = HashMap::new();
    dbg!(&ast);
    for top_level::Declaration { name, kind } in ast {
        use top_level::DeclarationKind;

        let kind = match kind {
            DeclarationKind::Struct(r#struct) => TypeKind::Struct(r#struct),
            DeclarationKind::Union(r#union) => TypeKind::Union(r#union),
            _ => continue,
        };

        map.insert(
            name,
            Type {
                kind,
                id: ID.fetch_add(1, Ordering::Relaxed),
            },
        );
    }
    dbg!(map);
}
