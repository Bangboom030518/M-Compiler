use crate::layout::{self, Array, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use parser::Ident;
use std::collections::HashMap;
use std::sync::Arc;
use tokenizer::{AsSpanned, Spanned};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct ScopeId(usize);

pub const TOP_LEVEL_SCOPE: ScopeId = ScopeId(0);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeReference {
    pub id: Id,
    pub generics: Vec<GenericArgument>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncReference {
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
        scope: ScopeId, // ident: String,
    },
    Primitive(Primitive, ScopeId),
}

#[derive(Clone, Debug, PartialEq)]
pub enum GenericFunction {
    Internal {
        function: parser::Function,
        scope_id: ScopeId,
    },
    External(function::External),
}

#[derive(Clone, Debug, PartialEq)]
pub enum ConcreteFunction {
    Internal(function::Internal),
    External(function::External),
}

impl ConcreteFunction {
    pub const fn signature(&self) -> &function::MSignature {
        match self {
            Self::Internal(function) => &function.signature,
            Self::External(function) => &function.signature,
        }
    }

    pub const fn id(&self) -> FuncId {
        match self {
            Self::Internal(function) => function.id,
            Self::External(function) => function.id,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Declaration {
    Type(Type),
    Function(GenericFunction),
    LengthGeneric(usize),
    TypeGeneric(usize),
}

impl Declaration {
    pub const fn expect_function(&self) -> Result<&GenericFunction, SemanticError> {
        match self {
            Self::Function(function) => Ok(function),
            _ => Err(SemanticError::InvalidFunction),
        }
    }

    pub const fn expect_type(&self) -> Result<&Type, SemanticError> {
        match self {
            Self::Type(ty) => Ok(ty),
            _ => Err(SemanticError::InvalidType),
        }
    }

    pub const fn expect_length_generic(&self) -> Result<usize, SemanticError> {
        match self {
            Self::LengthGeneric(generic) => Ok(*generic),
            _ => Err(SemanticError::InvalidLengthGeneric),
        }
    }

    pub const fn expect_type_generic(&self) -> Result<usize, SemanticError> {
        match self {
            Self::TypeGeneric(generic) => Ok(*generic),
            _ => Err(SemanticError::InvalidTypeGeneric),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Length {
    Literal(u128),
    Reference(Id),
}

impl Length {
    fn resolve(
        self,
        generics: &[GenericArgument],
        declarations: &mut Declarations,
    ) -> Result<u128, SemanticError> {
        let length = match self {
            Self::Literal(length) => length,
            Self::Reference(id) => {
                let index = declarations.get(id).expect_length_generic()?;
                generics
                    .get(index)
                    .ok_or(SemanticError::GenericParametersMismatch)?
                    .expect_length()?
                    .resolve(generics, declarations)?
            }
        };
        Ok(length)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericArgument {
    Type(TypeReference),
    Length(Length),
}

impl GenericArgument {
    pub const fn expect_length(&self) -> Result<&Length, SemanticError> {
        match self {
            Self::Length(length) => Ok(length),
            Self::Type(_) => Err(SemanticError::InvalidTypeGeneric),
        }
    }

    pub const fn expect_type(&self) -> Result<&TypeReference, SemanticError> {
        match self {
            Self::Type(r#type) => Ok(r#type),
            Self::Length(_) => Err(SemanticError::InvalidLengthGeneric),
        }
    }
}

pub struct Declarations {
    declarations: Vec<Option<Declaration>>,
    pub scopes: Vec<TopLevelScope>,
    pub isa: Arc<dyn TargetIsa>,
    pub layouts: HashMap<TypeReference, Layout>,
    pub concrete_functions: HashMap<FuncReference, ConcreteFunction>,
}

impl Declarations {
    pub fn new(
        mut parser_declarations: HashMap<String, parser::Declaration>,
        isa: &Arc<dyn TargetIsa>,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            declarations: Vec::new(),
            scopes: Vec::new(),
            isa: Arc::clone(isa),
            layouts: HashMap::new(),
            concrete_functions: HashMap::new(),
        };

        let scope_declarations = parser_declarations
            .iter()
            .map(|(ident, ..)| (ident.clone(), declarations.create_uninitialised()))
            .collect();

        let scope_id = declarations.create_scope(TopLevelScope {
            declarations: scope_declarations,
            parent: None,
        });

        let mut functions = Vec::new();
        let mut extern_functions = Vec::new();
        for (name, id) in declarations.scopes[scope_id.0].declarations.clone() {
            match parser_declarations
                .remove_entry(&name)
                .expect("name to exist")
                .1
            {
                parser::Declaration::Struct(r#struct) => {
                    let declaration = declarations.handle_struct(r#struct, scope_id)?;
                    declarations.initialise(id, declaration);
                }
                parser::Declaration::Union(_) => todo!("unions"),
                parser::Declaration::Primitive(primitive) => {
                    let declaration = declarations.handle_primitive(primitive, scope_id)?;
                    declarations.initialise(id, declaration);
                }
                parser::Declaration::Function(function) => functions.push((function, id)),
                parser::Declaration::ExternFunction(function) => {
                    extern_functions.push((function, id));
                }
            }
        }

        for (function, id) in functions {
            declarations.initialise(
                id,
                Declaration::Function(GenericFunction::Internal { function, scope_id }),
            );
        }

        for (function, id) in extern_functions {
            let function = Declaration::Function(GenericFunction::External(
                function::External::new(function, &mut declarations, scope_id, module)?,
            ));
            declarations.initialise(id, function);
        }

        Ok(declarations)
    }

    fn handle_struct(
        &mut self,
        ast_struct: parser::Struct,
        parent: ScopeId,
    ) -> Result<Declaration, SemanticError> {
        let inner_scope = self.resolve_generic_parameters(ast_struct.generics, parent);

        let fields = ast_struct
            .fields
            .iter()
            .map(|field| field.value.clone())
            .map(|parser::top_level::Field { r#type, name }| {
                let type_id = self
                    .lookup(&r#type.value.name.value.0, inner_scope)
                    .ok_or_else(|| SemanticError::DeclarationNotFound(r#type.value.name.clone()))?;
                Ok((
                    name,
                    TypeReference {
                        id: type_id,
                        generics: self
                            .resolve_generics(&r#type.value.generics.value.0, inner_scope)?,
                    },
                ))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;
        Ok(Declaration::Type(Type::Struct {
            fields,
            scope: inner_scope,
        }))
    }

    fn handle_primitive(
        &mut self,
        ast_primitive: parser::top_level::Primitive,
        parent: ScopeId,
    ) -> Result<Declaration, SemanticError> {
        use parser::top_level::{Length as L, PrimitiveKind as P};

        let inner_scope = self.resolve_generic_parameters(ast_primitive.generics, parent);

        let primitive = match ast_primitive.kind.value {
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
                    L::Ident(ident) => {
                        Length::Reference(self.lookup(ident.as_ref(), inner_scope).ok_or_else(
                            || SemanticError::DeclarationNotFound(ident.spanned(length.span)),
                        )?)
                    }
                    L::Literal(length) => Length::Literal(length),
                };

                let inner = self
                    .lookup(&item.value.name.value.0, inner_scope)
                    .ok_or_else(|| SemanticError::DeclarationNotFound(item.value.name))?;

                let generics = self.resolve_generics(&item.value.generics.value.0, inner_scope)?;

                Primitive::Array(
                    length,
                    TypeReference {
                        generics,
                        id: inner,
                    },
                )
            }
        };
        Ok(Declaration::Type(Type::Primitive(primitive, inner_scope)))
    }

    pub fn resolve_generics(
        &mut self,
        generics: &[Spanned<parser::GenericArgument>],
        scope: ScopeId,
    ) -> Result<Vec<GenericArgument>, SemanticError> {
        generics
            .iter()
            .map(|generic| match generic.as_ref().value {
                parser::GenericArgument::Literal(length) => {
                    Ok(GenericArgument::Length(Length::Literal(*length)))
                }
                parser::GenericArgument::Type(r#type) => {
                    let id = self
                        .lookup(r#type.name.value.as_ref(), scope)
                        .ok_or_else(|| SemanticError::DeclarationNotFound(r#type.name.clone()))?;
                    let value = match self.get(id) {
                        Declaration::LengthGeneric(_) => {
                            if r#type.generics.value.0.is_empty() {
                                GenericArgument::Length(Length::Reference(id))
                            } else {
                                return Err(SemanticError::GenericParametersMismatch);
                            }
                        }
                        Declaration::TypeGeneric(_) | Declaration::Type(_) => {
                            GenericArgument::Type(TypeReference {
                                id,
                                generics: self.resolve_generics(&r#type.generics.value.0, scope)?,
                            })
                        }
                        Declaration::Function(_) => return Err(SemanticError::InvalidType),
                    };
                    Ok(value)
                }
            })
            .collect()
    }

    pub fn insert_layout(
        &mut self,
        type_reference: &TypeReference,
    ) -> Result<Layout, SemanticError> {
        // TODO: clones
        if let Some(layout) = self.layouts.get(type_reference) {
            return Ok(layout.clone());
        }

        let Declaration::Type(r#type) = self.get(type_reference.id) else {
            return Err(SemanticError::InvalidType);
        };

        let layout = match r#type.clone() {
            Type::Struct { fields, scope } => {
                let mut offset = 0;
                let mut layout_fields = HashMap::new();
                for (name, r#type) in &fields {
                    layout_fields.insert(
                        name.value.0.clone(),
                        layout::Field {
                            type_id: r#type.clone(),
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
            Type::Primitive(primitive, scope) => match primitive {
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
                    let item = match self.get(item.id) {
                        Declaration::Type(_) => item,
                        Declaration::TypeGeneric(index) => type_reference
                            .generics
                            .get(*index)
                            .ok_or(SemanticError::GenericParametersMismatch)?
                            .expect_type()?
                            .clone(),
                        Declaration::Function(_) => return Err(SemanticError::InvalidFunction),
                        Declaration::LengthGeneric(_) => {
                            return Err(SemanticError::InvalidLengthGeneric)
                        }
                    };

                    let size = self.insert_layout(&item)?.size(&self.isa);
                    Layout::Array(Array {
                        length: length.resolve(&type_reference.generics, self)?,
                        size,
                        item,
                    })
                }
            },
        };

        self.layouts.insert(type_reference.clone(), layout.clone());
        Ok(layout)
    }

    fn create_scope(&mut self, scope: TopLevelScope) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(scope);
        ScopeId(id)
    }

    fn resolve_generic_parameters(
        &mut self,
        generics: Spanned<parser::top_level::Generics>,
        parent: ScopeId,
    ) -> ScopeId {
        let generics = generics
            .value
            .generics
            .into_iter()
            .enumerate()
            .map(|(index, generic)| match generic.value {
                parser::top_level::Generic::Length { name } => {
                    (name.value.0, self.create(Declaration::LengthGeneric(index)))
                }
                parser::top_level::Generic::Type { name } => {
                    (name.value.0, self.create(Declaration::TypeGeneric(index)))
                }
            })
            .collect();

        self.create_scope(TopLevelScope {
            declarations: generics,
            parent: Some(parent),
        })
    }

    fn create(&mut self, declaration: Declaration) -> Id {
        let id = self.declarations.len();
        self.declarations.push(Some(declaration));
        Id(id)
    }

    pub fn create_uninitialised(&mut self) -> Id {
        self.declarations.push(None);
        Id(self.declarations.len() - 1)
    }

    fn initialise(&mut self, Id(index): Id, declaration: Declaration) {
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

    pub fn insert_function(
        &mut self,
        func_reference: FuncReference,
        module: &mut impl Module,
    ) -> Result<ConcreteFunction, SemanticError> {
        // TODO: `.clone()`s
        if let Some(function) = self.concrete_functions.get(&func_reference) {
            return Ok(function.clone());
        }

        let Ok(generic_function) = self.get(func_reference.id).expect_function() else {
            return Err(SemanticError::InvalidType);
        };

        let function = match generic_function {
            GenericFunction::Internal { function, scope_id } => {
                ConcreteFunction::Internal(function::Internal::new(
                    // TODO: `function.clone()`
                    function.clone(),
                    self,
                    *scope_id,
                    module,
                    func_reference.generics.clone(),
                )?)
            }
            GenericFunction::External(external) => ConcreteFunction::External(external.clone()),
        };

        self.concrete_functions
            .insert(func_reference, function.clone());
        Ok(function)
    }

    pub fn lookup(&self, ident: &str, scope: ScopeId) -> Option<Id> {
        self.scopes[scope.0]
            .get(ident)
            .or_else(|| self.lookup(ident, self.scopes[scope.0].parent?))
    }

    pub fn lookup_type(
        &mut self,
        r#type: &parser::Type,
        scope: ScopeId,
    ) -> Result<TypeReference, SemanticError> {
        let id = self
            .lookup(r#type.name.value.as_ref(), scope)
            .ok_or_else(|| SemanticError::DeclarationNotFound(r#type.name.clone()))?;

        let generics = self.resolve_generics(&r#type.generics.value.0, scope)?;

        Ok(TypeReference { id, generics })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TopLevelScope {
    pub declarations: HashMap<String, Id>,
    pub parent: Option<ScopeId>,
}

impl TopLevelScope {
    pub fn get(&self, ident: &str) -> Option<Id> {
        self.declarations.get(ident).copied()
    }
}
