#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::{prelude::*, top_level::DeclarationKind};
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

#[derive(Default)]
struct TypeScope {
    types: HashMap<Ident, TypeId>,
}

impl TypeStore {
    pub fn create(&mut self, r#type: Type) -> TypeId {
        TypeId(self.types.len())
    }

    #[must_use]
    pub fn get(&self, TypeId(id): TypeId) -> &Type {
        &self.types[id]
    }
}

fn handle_declaration(
    type_store: &mut TypeStore,
    type_scope: &mut TypeScope,
    declaration: DeclarationKind,
) {
    match declaration {
        DeclarationKind::Struct(r#struct) => {
            // TODO: create TypeId before types
            let id = type_store.create(Type::Struct {
                fields: r#struct
                    .fields
                    .into_iter()
                    .map(|top_level::Field(r#type, ident)| {
                        (file.cache.get(r#struct.scope).declarations, ident)
                    }),
            });
            type_scope.types.insert(name.clone(), id);
        }
    }
}

fn main() {
    let file = parse_file(include_str!("../../input.m")).unwrap();
    let root = file.root();
    let mut type_store = TypeStore::default();
    let mut type_scope = TypeScope::default();
    for (name, declaration) in root.declarations {
        match declaration {
            DeclarationKind::Struct(r#struct) => {
                // TODO: create TypeId before types
                let id = type_store.create(Type::Struct {
                    fields: r#struct
                        .fields
                        .into_iter()
                        .map(|top_level::Field(r#type, ident)| {
                            (file.cache.get(r#struct.scope).declarations, ident)
                        }),
                });
                type_scope.types.insert(name.clone(), id);
            } // DeclarationKind::Union(union) => todo!(),
              // DeclarationKind::Function()
        }
    }

    // let top_level::DeclarationKind::Function(main) = root
    //     .declarations
    //     .get(&Ident("main".to_string()))
    //     .expect("No main function!")
    // else {
    //     panic!("No main function")
    // };
}
