#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::prelude::*;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct ScopeId(usize);

#[derive(Debug)]
enum Declaration {
    Struct(Struct),
    Union,
    Function,
    Const,
    Let(),
}

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

#[derive(Debug, Default)]
struct ScopeCache(Vec<Scope>);

impl ScopeCache {
    fn new() -> Self {
        Self::default()
    }

    fn create_root_scope(
        &mut self,
        init: impl FnOnce(ScopeId, &mut Self) -> HashMap<Identifier, Declaration>,
    ) -> ScopeId {
        let id = ScopeId(self.0.len());
        let declarations = init(id, self);
        self.0.push(Scope {
            parent: None,
            declarations,
        });
        id
    }

    fn create_scope(
        &mut self,
        parent: ScopeId,
        init: impl FnOnce(ScopeId, &mut Self) -> HashMap<Identifier, Declaration>,
    ) -> ScopeId {
        let id = ScopeId(self.0.len());
        let declarations = init(id, self);
        self.0.push(Scope {
            parent: Some(parent),
            declarations,
        });
        id
    }

    // TODO: index operator instead?
    fn get(&mut self, ScopeId(id): ScopeId) -> &mut Scope {
        &mut self.0[id]
    }
}

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeId>,
    declarations: HashMap<Identifier, Declaration>,
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
