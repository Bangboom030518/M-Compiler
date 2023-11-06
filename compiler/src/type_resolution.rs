use ::parser::{prelude::*, top_level::DeclarationKind};
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Struct {
        fields: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Union {
        variants: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Int32,
    Int64,
    Float32,
    Float64,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeStore {
    pub types: Vec<Option<Type>>,
    pub scopes: HashMap<scope::Id, TypeScope>,
    pub file_cache: scope::Cache,
}

impl TypeStore {
    pub fn create_empty(&mut self) -> Id {
        self.types.push(None);
        Id(self.types.len() - 1)
    }

    pub fn initialise(&mut self, Id(index): Id, r#type: Type) {
        self.types[index] = Some(r#type);
    }

    #[must_use]
    pub fn get(&self, Id(id): Id) -> &Type {
        self.types.get(id).unwrap().as_ref().unwrap()
    }

    pub fn lookup(&self, ident: Ident, scope_id: scope::Id) -> Option<Id> {
        self.scopes[&scope_id]
            .get(&ident)
            .or_else(|| self.lookup(ident, self.file_cache.get(scope_id).parent?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeScope {
    pub types: HashMap<Ident, Id>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, thiserror::Error)]
#[error("Type Not Found")]
pub struct TypeNotFound;

impl TypeScope {
    pub fn append_new(type_store: &mut TypeStore, scope_id: scope::Id) -> Result<(), TypeNotFound> {
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

                Some((ident.clone(), type_store.create_empty()))
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
                DeclarationKind::Union(_) => todo!("unions!"),
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    pub fn get(&self, ident: &Ident) -> Option<Id> {
        self.types.get(ident).copied()
    }
}
