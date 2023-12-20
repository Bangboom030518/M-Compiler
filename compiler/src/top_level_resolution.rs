use crate::SemanticError;
use ::parser::prelude::*;
use ::parser::top_level::{DeclarationKind, Parameter, PrimitiveKind, TypeBinding};
use cranelift::codegen::ir::{AbiParam, Signature};
use cranelift::codegen::isa::CallConv;
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
    pub fn cranelift_type(&self) -> cranelift::prelude::Type {
        use cranelift::prelude::types;
        match self {
            Self::U8 | Self::I8 => types::I8,
            Self::U16 | Self::I16 => types::I16,
            Self::F32 => types::F32,
            Self::U32 | Self::I32 => types::I32,
            Self::F64 => types::F64,
            Self::U64 | Self::I64 => types::I64,
            Self::U128 | Self::I128 => types::I128,
            _ => todo!("handle complex data structures"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct FunctionId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<(Ident, Id)>,
    pub r#return: Id,
    pub signature: Signature,
    pub name: String,
}

impl Function {
    fn new(
        function: &::parser::top_level::Function,
        call_conv: CallConv,
        declarations: &TopLevelDeclarations,
        scope_id: ::parser::scope::Id,
        name: &Ident,
    ) -> Result<Self, SemanticError> {
        let mut signature = Signature::new(call_conv);

        let parameters = function
            .parameters
            .into_iter()
            .map(|Parameter(TypeBinding { r#type, name })| {
                let r#type = r#type
                    .as_ref()
                    .map(|r#type| match r#type {
                        ::parser::Type::Identifier(ident) => ident,
                    })
                    .ok_or(SemanticError::UntypedParameter)?;

                let r#type = declarations
                    .lookup(r#type, scope_id)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                Ok((name, r#type))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        signature.params = parameters
            .iter()
            .map(|r#type| {
                Ok(AbiParam::new(
                    declarations.get_type(r#type.1)?.cranelift_type(),
                ))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        let return_type = function
            .return_type
            .as_ref()
            .map(|r#type| match r#type {
                ::parser::Type::Identifier(ident) => ident,
            })
            .ok_or(SemanticError::MissingReturnType)?;

        let return_type_id = declarations
            .lookup(return_type, scope_id)
            .ok_or(SemanticError::DeclarationNotFound)?;

        let return_type = declarations.get_type(return_type_id)?.cranelift_type();

        signature.returns = vec![AbiParam::new(return_type)];
        Ok(Self {
            signature,
            parameters,
            r#return: return_type_id,
            name: name.to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Declaration {
    Type(Type),
    Function(Function),
}

impl Declaration {
    pub const fn expect_type(&self) -> Result<&Type, SemanticError> {
        match self {
            Self::Type(r#type) => Ok(r#type),
            Self::Function(_) => Err(SemanticError::FunctionUsedAsType),
        }
    }

    pub const fn expect_function(&self) -> Result<&Function, SemanticError> {
        match self {
            Self::Function(function) => Ok(function),
            Self::Type(_) => Err(SemanticError::TypeUsedAsFunction),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TopLevelDeclarations {
    pub declarations: Vec<Option<Declaration>>,
    pub scopes: HashMap<scope::Id, TopLevelScope>,
    pub file_cache: scope::Cache,
}

impl TopLevelDeclarations {
    pub fn create_empty(&mut self) -> Id {
        self.declarations.push(None);
        Id(self.declarations.len() - 1)
    }

    pub fn initialise(&mut self, Id(index): Id, declaration: Declaration) {
        self.declarations[index] = Some(declaration);
    }

    #[must_use]
    pub fn get(&self, Id(id): Id) -> &Declaration {
        self.declarations.get(id).unwrap().as_ref().unwrap()
    }

    pub fn get_type(&self, id: Id) -> Result<&Type, SemanticError> {
        self.get(id).expect_type()
    }

    pub fn get_function(&self, id: Id) -> Result<&Function, SemanticError> {
        self.get(id).expect_function()
    }

    pub fn lookup(&self, ident: &Ident, scope_id: scope::Id) -> Option<Id> {
        self.scopes[&scope_id]
            .get(ident)
            .or_else(|| self.lookup(ident, self.file_cache[scope_id].parent?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelScope {
    pub declarations: HashMap<Ident, Id>,
}

impl TopLevelScope {
    pub fn append_new(
        type_store: &mut TopLevelDeclarations,
        scope_id: scope::Id,
        call_conv: CallConv,
    ) -> Result<(), SemanticError> {
        let declarations = type_store.file_cache[scope_id].declarations.clone();

        {
            let declarations = declarations
                .iter()
                .map(|(ident, ..)| (ident.clone(), type_store.create_empty()))
                .collect();
            type_store.scopes.insert(scope_id, Self { declarations });
        }

        // TODO: clone?
        let type_scope = type_store.scopes[&scope_id].clone();

        for (ident, id) in &type_scope.declarations {
            let declaration = &declarations[&ident];
            match declaration {
                DeclarationKind::Struct(r#struct) => {
                    Self::append_new(type_store, r#struct.scope, call_conv)?;
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
                                    .ok_or(SemanticError::DeclarationNotFound)?;
                                Ok((name.clone(), type_id))
                            })
                            .collect::<Result<Vec<_>, SemanticError>>()?,
                        ident: ident.clone(),
                    };
                    type_store.initialise(*id, Declaration::Type(r#type));
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
                    type_store.initialise(*id, Declaration::Type(r#type));
                }
                DeclarationKind::Function(function) => {
                    type_store.initialise(
                        *id,
                        Declaration::Function(Function::new(
                            function, call_conv, type_store, scope_id, ident,
                        )?),
                    );
                }
                DeclarationKind::Const(_) => todo!(),
            }
        }

        Ok(())
    }

    pub fn get(&self, ident: &Ident) -> Option<Id> {
        self.declarations.get(ident).copied()
    }
}
