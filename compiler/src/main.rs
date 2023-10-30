#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::{prelude::*, top_level::DeclarationKind, scope::Cache};
use itertools::Itertools;
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct TypeId(usize);

pub enum Type {
    Struct { fields: Vec<(Ident, TypeId)> },
    Union { variants: Vec<(Ident, TypeId)> },
    Int32,
    Int64,
    Float32,
    Float64,
}

#[derive(Default)]
struct TypeStore {
    types: Vec<Type>,
}

// #[derive(Default)]
// struct TypeScope {
//     types: HashMap<Ident, TypeId>,
// }

// impl TypeScope {
//     pub fn get(&mut self, r#type: &::parser::Type, type_store: &mut TypeStore) -> TypeId {
//         self.types.get(match r#type {
//             ::parser::Type::Identifier(identifier) => identifier,
//         }).copied().
//     }
// }

impl TypeStore {
    pub fn create(&mut self, r#type: Type) -> TypeId {
        TypeId(self.types.len())
    }

    #[must_use]
    pub fn get(&self, TypeId(id): TypeId) -> &Type {
        &self.types[id]
    }
}

fn create_type(
    type_store: &mut TypeStore,
    // type_scope: &mut TypeScope,
    declaration: DeclarationKind,
    root: &Scope,
    scope_cache: &Cache
) -> TypeId {
    match declaration {
        DeclarationKind::Struct(r#struct) => {
            // TODO: create TypeId before types
            let id = type_store.create(Type::Struct {
                fields: r#struct
                    .fields
                    .into_iter()
                    .map(|top_level::Field { r#type, name }| {
                        let scope = scope_cache.get(r#struct.scope);
                        (
                            name,
                            scope_cache.lookup(r#struct.scope, match r#type {
                                ::parser::Type::Identifier(ident) => ident,
                            }).unwrap(),
                        )
                    })
                    .collect_vec(),
            });
            type_scope.types.insert(name.clone(), id);
            id
        }
    }
}

fn main() {
    let file = parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root();
    let file_cache = &file.cache;
    let mut type_store = TypeStore::default();
    // let mut type_scope = TypeScope::default();
    for (name, declaration) in root.declarations {
        create_type(&mut type_store, declaration, root, file_cache);
    }

    // let top_level::DeclarationKind::Function(main) = root
    //     .declarations
    //     .get(&Ident("main".to_string()))
    //     .expect("No main function!")
    // else {
    //     panic!("No main function")
    // };
}
