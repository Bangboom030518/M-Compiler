use crate::layout::{self, Array, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use itertools::Itertools;
use parser::top_level::PrimitiveKind;
use parser::{scope, Ident};
use std::collections::HashMap;
use std::sync::Arc;
use tokenizer::{AsSpanned, Spanned};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Struct {
        fields: Vec<(Spanned<Ident>, Id)>,
        // ident: String,
    },
    Primitive(PrimitiveKind),
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
}

impl Declaration {
    pub const fn expect_function(&self) -> Result<&Function, SemanticError> {
        match self {
            Self::Function(function) => Ok(function),
            Self::Type(_) => Err(SemanticError::TypeUsedAsFunction),
        }
    }
}

pub struct Declarations {
    pub declarations: Vec<Option<Declaration>>,
    pub scopes: HashMap<scope::Id, TopLevelScope>,
    pub isa: Arc<dyn TargetIsa>,
    pub layouts: HashMap<Id, Layout>,
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

    fn insert_layout(&mut self, id: Id, scope: scope::Id) -> Result<Layout, SemanticError> {
        // TODO: clones
        if let Some(layout) = self.layouts.get(&id) {
            return Ok(layout.clone());
        }

        // TODO: handle nested layouts
        let Declaration::Type(r#type) = self.get(id) else {
            return Err(SemanticError::FunctionUsedAsType);
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
                    let layout = self.insert_layout(*r#type, scope)?;
                    offset += layout.size(&self.isa);
                }
                Layout::Struct(layout::Struct {
                    fields: layout_fields,
                    size: offset,
                })
            }
            Type::Primitive(primitive) => match primitive {
                PrimitiveKind::F32 => Layout::Primitive(layout::Primitive::F32),
                PrimitiveKind::F64 => Layout::Primitive(layout::Primitive::F64),
                PrimitiveKind::U8 => Layout::Primitive(layout::Primitive::U8),
                PrimitiveKind::U16 => Layout::Primitive(layout::Primitive::U16),
                PrimitiveKind::U32 => Layout::Primitive(layout::Primitive::U32),
                PrimitiveKind::U64 => Layout::Primitive(layout::Primitive::U64),
                PrimitiveKind::U128 => Layout::Primitive(layout::Primitive::U128),
                PrimitiveKind::I8 => Layout::Primitive(layout::Primitive::I8),
                PrimitiveKind::I16 => Layout::Primitive(layout::Primitive::I16),
                PrimitiveKind::I32 => Layout::Primitive(layout::Primitive::I32),
                PrimitiveKind::I64 => Layout::Primitive(layout::Primitive::I64),
                PrimitiveKind::I128 => Layout::Primitive(layout::Primitive::I128),
                PrimitiveKind::USize => Layout::Primitive(layout::Primitive::USize),
                PrimitiveKind::Array(length, item) => {
                    let item = item.value.ident().spanned(item.span);
                    let item = self
                        .lookup(&item.value.0, scope)
                        .ok_or(SemanticError::DeclarationNotFound(item))?;
                    let size = self.insert_layout(item, scope)?.size(&self.isa);
                    Layout::Array(Array { length: length.value, size, item })
                }
            },
        };

        self.layouts.insert(id, layout);
        // TODO: `.unwrap()`
        Ok(self.layouts.get(&id).unwrap().clone())
    }

    /// # Panics
    /// If a layout of `id` doesn't exist
    pub fn get_layout(&self, id: Id) -> &Layout {
        self.layouts
            .get(&id)
            .expect("Attempted to get non-existant layout")
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
                            let parser::Type::Ident(ident) = r#type.value;
                            // TODO: will need to handle generics
                            let type_id = self
                                .lookup(
                                    ident.as_ref(), // TODO: `r#struct.scope`
                                    scope_id,
                                )
                                .ok_or_else(|| {
                                    SemanticError::DeclarationNotFound(
                                        ident.clone().spanned(r#type.span),
                                    )
                                })?;
                            Ok((name, type_id))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?;

                    self.initialise(id, Declaration::Type(Type::Struct { fields }));
                }
                parser::Declaration::Union(_) => todo!("unions"),
                parser::Declaration::Primitive(primitive) => {
                    self.initialise(id, Declaration::Type(Type::Primitive(primitive.kind.value)));
                }
                parser::Declaration::Function(function) => functions.push((function, id)),
                parser::Declaration::ExternFunction(function) => {
                    extern_functions.push((function, id));
                }
            }
        }

        let types = self
            .declarations
            .iter()
            .enumerate()
            .filter_map(|(index, declaration)| {
                declaration
                    .as_ref()
                    .and_then(|declaration| match declaration {
                        Declaration::Type(_) => Some(Id(index)),
                        Declaration::Function(_) => None,
                    })
            })
            .collect_vec();

        for id in types {
            self.insert_layout(id, scope_id)?;
        }

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
