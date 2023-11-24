use ::parser::{
    prelude::*,
    top_level::{DeclarationKind, PrimitiveKind},
};
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
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
}

impl Type {
    fn size(&self) -> usize {
        match self {
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::F32 | Self::U32 | Self::I32 => 4,
            Self::F64 | Self::U64 | Self::I64 => 8,
            Self::U128 | Self::I128 => 16,
            Self::Struct { fields, .. } => {
                todo!("handle struct")
            }
            Self::Union { variants, .. } => {
                todo!("handle union")
            }
        }
    }
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

    pub fn lookup(&self, ident: &Ident, scope_id: scope::Id) -> Option<Id> {
        self.scopes[&scope_id]
            .get(ident)
            .or_else(|| self.lookup(ident, self.file_cache[scope_id].parent?))
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
        let declarations = type_store.file_cache[scope_id].declarations.clone();
        let types = declarations
            .iter()
            .filter_map(|(ident, declaration)| {
                if !matches!(
                    declaration,
                    DeclarationKind::Struct(_)
                        | DeclarationKind::Union(_)
                        | DeclarationKind::Primitive(_)
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
                                            ::parser::Type::Identifier(identifier) => identifier,
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
                DeclarationKind::Primitive(primitive) => {
                    let r#type = match primitive.kind {
                        PrimitiveKind::I8 => Type::I8,
                        PrimitiveKind::I16 => Type::I16,
                        PrimitiveKind::I32 => Type::I32,
                        PrimitiveKind::I64 => Type::I64,
                        PrimitiveKind::I128 => Type::I128,
                        PrimitiveKind::U8 => Type::U8,
                        PrimitiveKind::U16 => Type::U16,
                        PrimitiveKind::U32 => Type::U32,
                        PrimitiveKind::U64 => Type::U64,
                        PrimitiveKind::U128 => Type::U128,
                        PrimitiveKind::F32 => Type::F32,
                        PrimitiveKind::F64 => Type::F64,
                    };
                    type_store.initialise(*type_id, r#type);
                }
                _ => unimplemented!(),
            }
        }

        Ok(())
    }

    pub fn get(&self, ident: &Ident) -> Option<Id> {
        self.types.get(ident).copied()
    }
}
