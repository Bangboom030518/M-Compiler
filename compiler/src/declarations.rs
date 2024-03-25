use crate::layout::{self, Array, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use parser::{scope, Ident};
use std::collections::HashMap;
use std::sync::Arc;
use tokenizer::{AsSpanned, Spanned};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeReference {
    pub id: Id,
    pub generics: Vec<GenericArgument>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Primitive {
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
    USize,
    Array(Length, TypeReference),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Struct {
        fields: Vec<(Spanned<Ident>, TypeReference)>,
        // ident: String,
    },
    Primitive(Primitive),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    Internal(function::Internal),
    External(function::External),
}

// TODO: refactor VV
impl Function {
    pub const fn signature(&self) -> &function::MSignature {
        match self {
            Self::External(func) => &func.signature,
            Self::Internal(func) => &func.signature,
        }
    }

    pub const fn id(&self) -> FuncId {
        match self {
            Self::External(func) => func.id,
            Self::Internal(func) => func.id,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Type(Type),
    Function(Function),
    LengthGeneric,
    TypeGeneric,
}

impl Declaration {
    pub const fn expect_function(&self) -> Result<&Function, SemanticError> {
        match self {
            Self::Function(function) => Ok(function),
            _ => Err(SemanticError::InvalidFunction),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Length {
    Literal(u128),
    Reference(Id),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericArgument {
    Type(TypeReference),
    Length(Length),
}

pub struct Declarations {
    pub declarations: Vec<Option<Declaration>>,
    pub scopes: HashMap<scope::Id, TopLevelScope>,
    pub isa: Arc<dyn TargetIsa>,
    pub layouts: HashMap<TypeReference, Layout>,
}

impl Declarations {
    pub fn new(
        file: &scope::File,
        isa: &Arc<dyn TargetIsa>,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            declarations: Vec::new(),
            scopes: HashMap::new(),
            isa: Arc::clone(isa),
            layouts: HashMap::new(),
        };

        declarations.append_new(file.root, &file.cache, module)?;

        Ok(declarations)
    }

    fn resolve_generics(
        &mut self,
        generics: &[Spanned<parser::GenericArgument>],
        scope: scope::Id,
    ) -> Result<Vec<GenericArgument>, SemanticError> {
        generics
            .iter()
            .map(|generic| match generic.value {
                parser::GenericArgument::Literal(length) => {
                    Ok(GenericArgument::Length(Length::Literal(length)))
                }
                parser::GenericArgument::Type(r#type) => {
                    let id = self
                        .lookup(r#type.name.value.as_ref(), scope)
                        .ok_or_else(|| SemanticError::DeclarationNotFound(r#type.name.clone()))?;
                    let value = match self.get(id) {
                        Declaration::LengthGeneric => {
                            if r#type.generics.value.is_empty() {
                                GenericArgument::Length(Length::Reference(id))
                            } else {
                                todo!("nice error")
                            }
                        }
                        Declaration::TypeGeneric | Declaration::Type(_) => {
                            GenericArgument::Type(TypeReference {
                                id,
                                generics: self.resolve_generics(&r#type.generics.value, scope)?,
                            })
                        }
                        Declaration::Function(_) => return Err(SemanticError::InvalidType),
                    };
                    Ok(value)
                }
            })
            .collect()
    }

    pub fn insert_layout(&mut self, type_reference: &TypeReference) -> Result<Layout, SemanticError> {
        // TODO: clones
        if let Some(layout) = self.layouts.get(&type_reference) {
            return Ok(layout.clone());
        }

        // TODO: handle nested layouts
        let Declaration::Type(r#type) = self.get(type_reference.id) else {
            return Err(SemanticError::InvalidType);
        };

        let layout = match r#type.clone() {
            Type::Struct { fields, .. } => {
                let mut offset = 0;
                let mut layout_fields = HashMap::new();
                for (name, r#type) in &fields {
                    layout_fields.insert(
                        name.value.0.clone(),
                        layout::Field {
                            type_id: *r#type,
                            offset: Offset32::new(
                                i32::try_from(offset).expect("struct offset exceded `i32::MAX`"),
                            ),
                        },
                    );
                    let layout = self.insert_layout(r#type)?;
                    offset += layout.size(&self.isa);
                }
                Layout::Struct(layout::Struct {
                    fields: layout_fields,
                    size: offset,
                })
            }
            Type::Primitive(primitive) => match primitive {
                Primitive::F32 => Layout::Primitive(layout::Primitive::F32),
                Primitive::F64 => Layout::Primitive(layout::Primitive::F64),
                Primitive::U8 => Layout::Primitive(layout::Primitive::U8),
                Primitive::U16 => Layout::Primitive(layout::Primitive::U16),
                Primitive::U32 => Layout::Primitive(layout::Primitive::U32),
                Primitive::U64 => Layout::Primitive(layout::Primitive::U64),
                Primitive::U128 => Layout::Primitive(layout::Primitive::U128),
                Primitive::I8 => Layout::Primitive(layout::Primitive::I8),
                Primitive::I16 => Layout::Primitive(layout::Primitive::I16),
                Primitive::I32 => Layout::Primitive(layout::Primitive::I32),
                Primitive::I64 => Layout::Primitive(layout::Primitive::I64),
                Primitive::I128 => Layout::Primitive(layout::Primitive::I128),
                Primitive::USize => Layout::Primitive(layout::Primitive::USize),
                Primitive::Array(length, item) => {
                    let size = self.insert_layout(&item)?.size(&self.isa);
                    Layout::Array(Array {
                        length: match length {
                            Length::Literal(length) => length,
                            Length::Reference(_) => todo!("resolve generics"),
                        },
                        size,
                        item,
                    })
                }
            },
        };

        self.layouts.insert(type_reference.clone(), layout);
        // TODO: `.unwrap()`
        Ok(self.layouts.get(&type_reference).unwrap().clone())
    }

    fn append_new(
        &mut self,
        scope_id: scope::Id,
        scope_cache: &scope::Cache,
        module: &mut impl Module,
    ) -> Result<(), SemanticError> {
        // TODO: `.clone()`
        let mut declarations = scope_cache[scope_id].declarations.clone();

        let scope = TopLevelScope {
            declarations: declarations
                .iter()
                .map(|(ident, ..)| (ident.clone(), self.create_empty()))
                .collect(),
            parent: scope_cache[scope_id].parent,
        };

        self.scopes.insert(scope_id, scope);

        let mut functions = Vec::new();
        let mut extern_functions = Vec::new();
        for (name, id) in self.scopes[&scope_id].declarations.clone() {
            match declarations.remove_entry(&name).expect("TODO").1 {
                parser::Declaration::Struct(r#struct) => {
                    // TODO: ↓
                    // self.append_new(r#struct.scope, scope_cache, module)?;

                    let fields = r#struct
                        .fields
                        .iter()
                        .map(|field| field.value.clone())
                        .map(|parser::top_level::Field { r#type, name }| {
                            let type_id = self
                                .lookup(
                                    &r#type.value.name.value.0, // TODO: `r#struct.scope`
                                    scope_id,
                                )
                                .ok_or_else(|| {
                                    SemanticError::DeclarationNotFound(r#type.value.name.clone())
                                })?;
                            Ok((
                                name,
                                TypeReference {
                                    id: type_id,
                                    generics: self
                                        .resolve_generics(&r#type.value.generics.value, scope_id)?,
                                },
                            ))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?;

                    self.initialise(id, Declaration::Type(Type::Struct { fields }));
                }
                parser::Declaration::Union(_) => todo!("unions"),
                parser::Declaration::Primitive(primitive) => {
                    use parser::top_level::{Length as L, PrimitiveKind as P};

                    self.initialise(
                        id,
                        Declaration::Type(Type::Primitive(match primitive.kind.value {
                            P::U8 => Primitive::U8,
                            P::U16 => Primitive::U16,
                            P::U32 => Primitive::U32,
                            P::U64 => Primitive::U64,
                            P::U128 => Primitive::U128,
                            P::I8 => Primitive::I8,
                            P::I16 => Primitive::I16,
                            P::I32 => Primitive::I32,
                            P::I64 => Primitive::I64,
                            P::I128 => Primitive::I128,
                            P::F32 => Primitive::F32,
                            P::F64 => Primitive::F64,
                            P::USize => Primitive::USize,
                            P::Array(length, item) => {
                                let length = match length.value {
                                    L::Ident(ident) => Length::Reference(
                                        self.lookup(ident.as_ref(), scope_id).ok_or_else(|| {
                                            SemanticError::DeclarationNotFound(
                                                ident.spanned(length.span),
                                            )
                                        })?,
                                    ),
                                    L::Literal(length) => Length::Literal(length),
                                };

                                let inner =
                                    self.lookup(&item.value.name.value.0, scope_id).ok_or_else(
                                        || SemanticError::DeclarationNotFound(item.value.name),
                                    )?;

                                let generics =
                                    self.resolve_generics(&item.value.generics.value, scope_id)?;

                                Primitive::Array(
                                    length,
                                    TypeReference {
                                        generics,
                                        id: inner,
                                    },
                                )
                            }
                        })),
                    );
                }
                parser::Declaration::Function(function) => functions.push((function, id)),
                parser::Declaration::ExternFunction(function) => {
                    extern_functions.push((function, id));
                }
            }
        }

        // let types = self
        //     .declarations
        //     .iter()
        //     .enumerate()
        //     .filter_map(|(index, declaration)| {
        //         declaration
        //             .as_ref()
        //             .and_then(|declaration| match declaration {
        //                 Declaration::Type(_) => Some(Id(index)),
        //                 Declaration::LengthGeneric | Declaration::TypeGeneric | Declaration::Function(_) => None,
        //             })
        //     })
        //     .collect_vec();

        // for id in types {
        //     self.insert_layout(id, scope_id)?;
        // }

        for (function, id) in functions {
            self.initialise(
                id,
                Declaration::Function(Function::Internal(function::Internal::new(
                    function, self, scope_id, module,
                )?)),
            );
        }

        for (function, id) in extern_functions {
            self.initialise(
                id,
                Declaration::Function(Function::External(function::External::new(
                    function, self, scope_id, module,
                )?)),
            );
        }

        Ok(())
    }

    pub fn create_empty(&mut self) -> Id {
        self.declarations.push(None);
        Id(self.declarations.len() - 1)
    }

    pub fn initialise(&mut self, Id(index): Id, declaration: Declaration) {
        self.declarations[index] = Some(declaration);
    }

    #[must_use]
    fn get(&self, Id(id): Id) -> &Declaration {
        self.declarations
            .get(id)
            .unwrap_or_else(|| panic!("🎉 declaration `{id}` doesn't exist 🎉"))
            .as_ref()
            .unwrap_or_else(|| panic!("🎉 declaration `{id}` was uninitialised 🎉"))
    }

    pub fn get_function(&self, id: Id) -> Result<&Function, SemanticError> {
        self.get(id).expect_function()
    }

    pub fn lookup(&self, ident: &str, scope_id: scope::Id) -> Option<Id> {
        self.scopes[&scope_id]
            .get(ident)
            .or_else(|| self.lookup(ident, self.scopes[&scope_id].parent?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelScope {
    pub declarations: HashMap<String, Id>,
    pub parent: Option<scope::Id>,
}

impl TopLevelScope {
    pub fn get(&self, ident: &str) -> Option<Id> {
        self.declarations.get(ident).copied()
    }
}
