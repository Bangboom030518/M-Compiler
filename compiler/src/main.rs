#![warn(clippy::pedantic, clippy::nursery)]

use ::parser::{prelude::*, top_level::DeclarationKind};
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct TypeId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Struct {
        fields: Vec<(Ident, TypeId)>,
        ident: Ident,
    },
    Union {
        variants: Vec<(Ident, TypeId)>,
        ident: Ident,
    },
    Int32,
    Int64,
    Float32,
    Float64,
}

#[derive(Clone, Debug, PartialEq)]
struct TypeStore {
    types: Vec<Option<Type>>,
    scopes: HashMap<scope::Id, TypeScope>,
    file_cache: scope::Cache,
}

impl TypeStore {
    pub fn create_empty(&mut self) -> TypeId {
        self.types.push(None);
        TypeId(self.types.len() - 1)
    }

    pub fn initialise(&mut self, TypeId(index): TypeId, r#type: Type) {
        self.types[index] = Some(r#type);
    }

    #[must_use]
    pub fn get(&self, TypeId(id): TypeId) -> &Type {
        self.types.get(id).unwrap().as_ref().unwrap()
    }

    pub fn lookup(&self, ident: Ident, scope_id: scope::Id) -> Option<TypeId> {
        self.scopes[&scope_id]
            .get(&ident, self)
            .or_else(|| self.lookup(ident, self.file_cache.get(scope_id).parent?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct TypeScope {
    types: HashMap<Ident, TypeId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type Not Found")]
struct TypeNotFound;

impl TypeScope {
    fn append_new(type_store: &mut TypeStore, scope_id: scope::Id) -> Result<(), TypeNotFound> {
        let declarations = type_store.file_cache.get(scope_id).declarations.clone();
        let types = declarations
            .iter()
            .filter_map(|(ident, declaration)| {
                if !matches!(
                    declaration,
                    DeclarationKind::Struct(_) | DeclarationKind::Union(_)
                ) {
                    return None;
                }

                let r#type = type_store.create_empty();
                Some((ident.clone(), r#type))
            })
            .collect();

        type_store.scopes.insert(scope_id, Self { types });
        // TODO: remove clone
        let type_scope = &type_store.scopes[&scope_id].clone();

        for (ident, type_id) in &type_scope.types {
            let declaration = &declarations[&ident];
            match declaration {
                DeclarationKind::Struct(r#struct) => {
                    Self::append_new(type_store, r#struct.scope)?;
                    let r#type = Type::Struct {
                        fields: r#struct
                            .fields
                            .iter()
                            .map(|top_level::Field { r#type, name }| {
                                // TODO: will need to handle generics
                                let type_id = type_store
                                    .lookup(
                                        match r#type {
                                            ::parser::Type::Identifier(identifier) => {
                                                identifier.clone()
                                            }
                                        },
                                        r#struct.scope,
                                    )
                                    .map_or_else(|| Err(TypeNotFound), Ok)?;
                                Ok((name.clone(), type_id))
                            })
                            .collect::<Result<Vec<_>, TypeNotFound>>()?,
                        ident: ident.clone(),
                    };
                    type_store.initialise(*type_id, r#type);
                }
                DeclarationKind::Union(_) => todo!(),
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    pub fn get(&self, ident: &Ident, type_store: &TypeStore) -> Option<TypeId> {
        self.types.get(ident).copied()
    }
}

fn main() {
    let file = parse_file(include_str!("../../input.m")).unwrap();

    let mut type_store = TypeStore {
        types: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    TypeScope::append_new(&mut type_store, file.root).unwrap();
    dbg!(type_store.scopes);
    dbg!(type_store.types);
    // let mut type_scope = TypeScope::default();

    // for (name, declaration) in root.declarations {
    //     create_type(&mut type_store, declaration, root, file_cache);
    // }

    // let top_level::DeclarationKind::Function(main) = root
    //     .declarations
    //     .get(&Ident("main".to_string()))
    //     .expect("No main function!")
    // else {
    //     panic!("No main function")
    // };
}
