#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::prelude::*;
use itertools::Itertools;
use std::collections::HashMap;

impl Declaration {
    pub fn new(
        declaration: top_level::Declaration,
        scope_id: ScopeId,
        scope_cache: &mut ScopeCache,
    ) -> Self {
        match declaration.kind {
            top_level::DeclarationKind::Struct(r#struct) => {
                Self::Struct(Struct::new(r#struct, scope_id, scope_cache))
            }
            kind => todo!("implement {kind:?}"),
        }
    }
}

#[derive(Debug)]
struct Function {
    scope: ScopeId,
    statements: Vec<Statement>,
}

#[derive(Debug)]
struct Struct {
    fields: Vec<(Identifier, ::parser::Type)>,
    scope: ScopeId,
}

impl Struct {
    fn new(
        top_level::Struct {
            fields,
            declarations,
        }: top_level::Struct,
        scope_id: ScopeId,
        scope_cache: &mut ScopeCache,
    ) -> Self {
        let scope = scope_cache.create_scope(scope_id, |id, scope_cache| {
            declarations
                .into_iter()
                .map(|declaration| {
                    (
                        declaration.name.clone(),
                        Declaration::new(declaration, id, scope_cache),
                    )
                })
                .collect()
        });

        Self {
            fields: fields
                .into_iter()
                .map(|field| (field.name, field.r#type))
                .collect(),
            scope,
        }
    }
}

#[derive(Debug)]
struct Union {
    variants: Vec<(Identifier, ::parser::Type)>,
    scope: ScopeId,
}

fn main() {
    let ast = parse_file(include_str!("../../input.m")).unwrap();
    dbg!(&ast);
    let mut scope_cache = ScopeCache::new();
    scope_cache.create_root_scope(|scope_id, scope_cache| {
        ast.into_iter()
            .map(|declaration| {
                (
                    declaration.name.clone(),
                    Declaration::new(declaration, scope_id, scope_cache),
                )
            })
            .collect()
    });
    dbg!(scope_cache);
}
