use crate::layout::{self, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use itertools::Itertools;
use parser::top_level::{DeclarationKind, PrimitiveKind};
use parser::{scope, Ident};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Struct {
        fields: Vec<(Ident, Id)>,
        ident: Ident,
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
        mut file: scope::File,
        isa: &Arc<dyn TargetIsa>,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            declarations: Vec::new(),
            scopes: HashMap::new(),
            isa: Arc::clone(isa),
            layouts: HashMap::new(),
        };

        declarations.append_new(file.root, &mut file.cache, module)?;

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
                        name.clone(),
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
            Type::Primitive(primitive) => Layout::Primitive(match primitive {
                PrimitiveKind::F32 => layout::Primitive::F32,
                PrimitiveKind::F64 => layout::Primitive::F64,
                PrimitiveKind::U8 => layout::Primitive::U8,
                PrimitiveKind::U16 => layout::Primitive::U16,
                PrimitiveKind::U32 => layout::Primitive::U32,
                PrimitiveKind::U64 => layout::Primitive::U64,
                PrimitiveKind::U128 => layout::Primitive::U128,
                PrimitiveKind::I8 => layout::Primitive::I8,
                PrimitiveKind::I16 => layout::Primitive::I16,
                PrimitiveKind::I32 => layout::Primitive::I32,
                PrimitiveKind::I64 => layout::Primitive::I64,
                PrimitiveKind::I128 => layout::Primitive::I128,
                PrimitiveKind::MutablePointer(r#type) => {
                    let ident = r#type.ident();
                    layout::Primitive::MutablePointer(
                    self.lookup(&ident, scope)
                        .ok_or_else(|| SemanticError::DeclarationNotFound(ident))?,
                )},
                // PrimitiveKind::MutableSlice(r#type) => layout::Primitive::MutableSlice(
                //     self.lookup(&r#type.ident(), scope)
                //         .ok_or(SemanticError::DeclarationNotFound)?,
                // ),
            }),
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
        scope_cache: &mut scope::Cache,
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
                DeclarationKind::Struct(r#struct) => {
                    // TODO: VV
                    // self.append_new(r#struct.scope, scope_cache, module)?;

                    let fields = r#struct
                        .fields
                        .iter()
                        .map(|parser::top_level::Field { r#type, name }| {
                            let ident = match r#type {
                                ::parser::Type::Ident(identifier) => identifier,
                            };
                            // TODO: will need to handle generics
                            let type_id = self
                                .lookup(
                                    ident,
                                    // TODO: `r#struct.scope`
                                    scope_id,
                                )
                                .ok_or_else(|| SemanticError::DeclarationNotFound(ident.clone()))?;
                            Ok((name.clone(), type_id))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?;

                    let ident = name.clone();
                    self.initialise(id, Declaration::Type(Type::Struct { ident, fields }));
                }
                DeclarationKind::Union(_) => todo!("unions"),
                DeclarationKind::Primitive(primitive) => {
                    self.initialise(id, Declaration::Type(Type::Primitive(primitive.kind)));
                }
                DeclarationKind::Function(function) => functions.push((function, id, name)),
                DeclarationKind::Const(_) => todo!("consts"),
                DeclarationKind::ExternFunction(function) => {
                    extern_functions.push((function, id, name));
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

        for (function, id, name) in functions {
            self.initialise(
                id,
                Declaration::Function(Function::Internal(function::Internal::new(
                    function, self, scope_id, name, module,
                )?)),
            );
        }

        for (function, id, name) in extern_functions {
            self.initialise(
                id,
                Declaration::Function(Function::External(function::External::new(
                    function, self, scope_id, name, module,
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

    pub fn lookup(&self, ident: &Ident, scope_id: scope::Id) -> Option<Id> {
        self.scopes[&scope_id]
            .get(ident)
            .or_else(|| self.lookup(ident, self.scopes[&scope_id].parent?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelScope {
    pub declarations: HashMap<Ident, Id>,
    pub parent: Option<scope::Id>,
}

impl TopLevelScope {
    pub fn get(&self, ident: &Ident) -> Option<Id> {
        self.declarations.get(ident).copied()
    }
}
