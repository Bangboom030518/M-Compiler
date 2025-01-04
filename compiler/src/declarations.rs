use crate::hir::{self, Typed};
use crate::layout::{self, Array, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use parser::Ident;
use std::collections::HashMap;
use std::iter;
use std::sync::Arc;
use tokenizer::{AsSpanned, Spanned};

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

impl Id {
    pub const fn to_type_ref(self) -> TypeReference {
        TypeReference {
            id: self,
            generics: Vec::new(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct ScopeId(usize);

pub const TOP_LEVEL_SCOPE: ScopeId = ScopeId(0);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeReference {
    pub id: Id,
    pub generics: Vec<GenericArgument>,
}

impl TypeReference {
    pub fn resolve(&self, declarations: &Declarations) -> Self {
        let Some(declaration) = declarations.get(self.id) else {
            return self.clone();
        };
        match declaration {
            Declaration::Function(_) => todo!(),
            Declaration::Length(_) => todo!(),
            Declaration::Type(_) => {
                let generics = self
                    .generics
                    .clone()
                    .into_iter()
                    .map(|generic| match generic {
                        GenericArgument::Length(_) => generic,
                        GenericArgument::Type(r#type) => {
                            GenericArgument::Type(r#type.resolve(declarations))
                        }
                    })
                    .collect();

                Self {
                    generics,
                    id: self.id,
                }
            }
            Declaration::TypeAlias(r#type) => r#type.resolve(declarations),
        }
    }
    pub fn is_initialised(&self, declarations: &Declarations) -> bool {
        declarations.is_initialised(self.id)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncReference {
    pub id: Id,
    pub generics: Vec<GenericArgument>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Type {
    kind: TypeKind,
    parent_scope: ScopeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TypeKind {
    Struct(parser::Struct),
    Primitive(parser::Primitive),
    Array(parser::Array),
}

impl TypeKind {
    fn generics(&self) -> parser::generic::Parameters {
        match self {
            Self::Primitive(_) => Default::default(),
            Self::Struct(r#struct) => r#struct.generics.value.clone(),
            Self::Array(array) => array.generics.value.clone(),
        }
    }
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
}

#[derive(Clone, Debug, PartialEq)]
enum Declaration {
    Type(Type),
    Function(GenericFunction),
    TypeAlias(TypeReference),
    Length(u128),
}

impl Declaration {
    pub const fn expect_function(&self) -> Result<&GenericFunction, SemanticError> {
        match self {
            Self::Function(function) => Ok(function),
            _ => Err(SemanticError::InvalidFunction),
        }
    }

    pub const fn expect_length(&self) -> Result<u128, SemanticError> {
        match self {
            Self::Length(generic) => Ok(*generic),
            _ => Err(SemanticError::InvalidLengthGeneric),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericArgument {
    Type(TypeReference),
    Length(Id),
}

impl GenericArgument {
    pub const fn expect_length(&self) -> Result<Id, SemanticError> {
        match self {
            Self::Length(length) => Ok(*length),
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
    store: Vec<Option<Declaration>>,
    scopes: Vec<TopLevelScope>,
    layouts: HashMap<TypeReference, Layout>,
    concrete_functions: HashMap<FuncReference, ConcreteFunction>,
    pub isa: Arc<dyn TargetIsa>,
}

impl Declarations {
    pub fn new(
        mut parser_declarations: HashMap<String, parser::Declaration>,
        isa: &Arc<dyn TargetIsa>,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            store: Vec::new(),
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
            let declaration = parser_declarations
                .remove_entry(&name)
                .expect("name to exist")
                .1;
            match declaration {
                parser::Declaration::Struct(ast_struct) => {
                    let declaration = Declaration::Type(Type {
                        kind: TypeKind::Struct(ast_struct),
                        parent_scope: scope_id,
                    });

                    declarations.initialise(id, declaration);
                }
                parser::Declaration::Union(_) => todo!("unions"),
                parser::Declaration::Primitive(primitive) => {
                    let declaration = Declaration::Type(Type {
                        parent_scope: scope_id,
                        kind: TypeKind::Primitive(primitive),
                    });

                    declarations.initialise(id, declaration);
                }
                parser::Declaration::Array(array) => {
                    let declaration = Declaration::Type(Type {
                        parent_scope: scope_id,
                        kind: TypeKind::Array(array),
                    });

                    declarations.initialise(id, declaration)
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

    pub fn is_initialised(&self, id: Id) -> bool {
        self.store.get(id.0).expect("`Id` exists").is_some()
    }

    pub fn resolve_generic_parameters(
        &mut self,
        generic_parameters: Spanned<parser::generic::Parameters>,
    ) -> HashMap<String, Id> {
        generic_parameters
            .value
            .generics
            .into_iter()
            .map(|parameter| (parameter.value.name.value.0, self.create_uninitialised()))
            .collect()
    }

    #[deprecated]
    pub fn resolve_generics(
        &mut self,
        generics: &[Spanned<parser::GenericArgument>],
        scope: ScopeId,
    ) -> Result<Vec<GenericArgument>, SemanticError> {
        generics
            .iter()
            .map(|generic| match generic.as_ref().value {
                parser::GenericArgument::Literal(length) => {
                    let length = self.create(Declaration::Length(*length));
                    Ok(GenericArgument::Length(length))
                }
                parser::GenericArgument::Type(r#type) => {
                    let id = self.lookup(&r#type.name, scope)?;
                    let declaration = self
                        .get(id)
                        .unwrap_or_else(|| todo!("handle uninitialised types"));
                    let value = match declaration {
                        Declaration::Length(_) => {
                            if r#type.generics.value.0.is_empty() {
                                GenericArgument::Length(id)
                            } else {
                                return Err(SemanticError::GenericParametersMismatch);
                            }
                        }
                        Declaration::TypeAlias(_) | Declaration::Type(_) => {
                            GenericArgument::Type(TypeReference {
                                id,
                                generics: self.resolve_generics(&r#type.generics.value.0, scope)?,
                            })
                        }
                        Declaration::Function(_) => {
                            return Err(SemanticError::InvalidType);
                        }
                    };
                    Ok(value)
                }
            })
            .collect()
    }

    pub fn check_length(&mut self, expected: u128, found: Id) -> Result<(), SemanticError> {
        if self.is_initialised(found) {
            let found = self.get_length(found)?;
            if found == expected {
                Ok(())
            } else {
                Err(SemanticError::LengthMismatch { expected, found })
            }
        } else {
            self.initialise(found, Declaration::Length(expected));
            Ok(())
        }
    }

    pub fn check_expression_type(
        &mut self,
        expression: &Typed<hir::Expression>,
        expected: &TypeReference,
    ) -> Result<(), SemanticError> {
        let expected = expected.resolve(self);
        let found = expression.type_ref.resolve(self);
        if !self.is_initialised(found.id) {
            self.initialise(found.id, Declaration::TypeAlias(expected.clone()));
        } else if !self.is_initialised(expected.id) {
            self.initialise(expected.id, Declaration::TypeAlias(found.clone()));
        }
        let expected = expected.resolve(self);
        let found = found.resolve(self);
        if expected == found {
            Ok(())
        } else {
            let expected = self.insert_layout(&expected)?;
            let found = self.insert_layout(&found)?;
            Err(SemanticError::MismatchedTypes {
                expected: expected.unwrap(),
                found: found.unwrap(),
                expression: expression.value.clone(),
            })
        }
    }

    // # Errors
    // if type is uninitialised
    pub fn insert_layout_initialised(
        &mut self,
        type_reference: &TypeReference,
    ) -> Result<Layout, SemanticError> {
        self.insert_layout(type_reference)
            .map(|layout| layout.ok_or(SemanticError::UninitialisedType))?
    }

    pub fn insert_layout(
        &mut self,
        type_reference: &TypeReference,
    ) -> Result<Option<Layout>, SemanticError> {
        if let Some(layout) = self.layouts.get(type_reference) {
            return Ok(Some(layout.clone()));
        }

        let Some(layout) = self.get(type_reference.id).cloned() else {
            return Ok(None);
        };

        let r#type = match layout {
            Declaration::Type(r#type) => r#type,
            Declaration::TypeAlias(reference) => return self.insert_layout(&reference),
            _ => {
                return Err(SemanticError::InvalidType);
            }
        };
        // TODO: r#type.parent_scope
        let scope = self
            .create_generic_scope(
                r#type.kind.generics(),
                &type_reference.generics,
                r#type.parent_scope,
            )
            .inspect_err(|_| {
                dbg!();
            })?;

        let layout = match r#type.kind {
            TypeKind::Struct(ast_struct) => {
                let mut offset = 0;
                let mut layout_fields = HashMap::new();
                for field in &ast_struct.fields {
                    let type_ref = self.lookup_type(&field.value.r#type.value, scope)?;
                    layout_fields.insert(
                        field.value.name.value.0.clone(),
                        layout::Field {
                            type_ref: type_ref.clone(),
                            offset: Offset32::new(
                                i32::try_from(offset)
                                    .map_err(|_| SemanticError::StructTooChonky)?,
                            ),
                        },
                    );
                    let layout = self
                        .insert_layout(&type_ref)?
                        .unwrap_or_else(|| todo!("uninitialised layout"));
                    offset += layout.size(self)?;
                }
                Layout::Struct(layout::Struct {
                    fields: layout_fields,
                    size: offset,
                })
            }
            TypeKind::Primitive(primitive) => Layout::Primitive(primitive.kind.value),
            TypeKind::Array(array) => {
                let element_type = self.lookup_type(&array.element_type.value, scope)?;
                let length = match array.length.value {
                    parser::Length::Ident(ident) => {
                        self.lookup(&ident.spanned(array.length.span), scope)?
                    }
                    parser::Length::Literal(length) => self.create(Declaration::Length(length)),
                };
                Layout::Array(Array {
                    length,
                    element_type,
                })
            }
        };

        self.layouts.insert(type_reference.clone(), layout.clone());
        Ok(Some(layout))
    }

    pub fn create_scope(&mut self, scope: TopLevelScope) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(scope);
        ScopeId(id)
    }

    pub fn create_generic_scope(
        &mut self,
        parameters: parser::generic::Parameters,
        arguments: &[GenericArgument],
        parameter_scope: ScopeId,
    ) -> Result<ScopeId, SemanticError> {
        let generics = if arguments.is_empty() {
            parameters
                .generics
                .into_iter()
                .map(|parameter| (parameter.value.name.value.0, self.create_uninitialised()))
                .collect()
        } else {
            if arguments.len() != parameters.generics.len() {
                return Err(SemanticError::GenericParametersMismatch);
            }

            iter::zip(parameters.generics, arguments)
                .map(|(parameter, argument)| {
                    let parameter = parameter.value;
                    match (parameter.kind, argument) {
                        (parser::generic::Kind::Type, GenericArgument::Type(type_ref)) => Ok((
                            parameter.name.value.0,
                            self.create(Declaration::TypeAlias(type_ref.clone())),
                        )),
                        (parser::generic::Kind::Length, GenericArgument::Length(length)) => {
                            Ok((parameter.name.value.0, *length))
                        }
                        _ => Err(SemanticError::GenericParametersMismatch),
                    }
                })
                .collect::<Result<_, SemanticError>>()?
        };
        Ok(self.create_scope(TopLevelScope {
            declarations: generics,
            parent: Some(parameter_scope),
        }))
    }

    fn create(&mut self, declaration: Declaration) -> Id {
        let id = self.store.len();
        self.store.push(Some(declaration));
        Id(id)
    }

    pub fn create_uninitialised(&mut self) -> Id {
        self.store.push(None);
        Id(self.store.len() - 1)
    }

    pub fn create_type_ref(&mut self) -> TypeReference {
        self.create_uninitialised().to_type_ref()
    }

    fn initialise(&mut self, Id(index): Id, declaration: Declaration) {
        self.store[index] = Some(declaration);
    }

    /// Gets the declaration at `Id`, returning `None` if the declaration is uninitialised
    ///
    /// # Panics
    /// If the declaration at `Id` does not exist
    #[must_use]
    fn get(&self, Id(id): Id) -> Option<&Declaration> {
        self.store.get(id).expect("`Id` exists").as_ref()
    }

    pub fn declare_function(
        &mut self,
        func_reference: FuncReference,
        scope: ScopeId,
        module: &mut impl Module,
    ) -> Result<FuncId, SemanticError> {
        let mut function = self.insert_function(func_reference.clone())?;
        let id = match &mut function {
            ConcreteFunction::Internal(function) => {
                if let Some(id) = function.id {
                    return Ok(id);
                }
                let cranelift_signature = function.signature.cranelift_signature(module, self)?;

                let id = module
                    .declare_function(
                        &function.signature.name.value.0,
                        cranelift_module::Linkage::Export,
                        &cranelift_signature,
                    )
                    .expect("internal module error");
                function.id = Some(id);
                id
            }
            ConcreteFunction::External(function) => function.id,
        };
        self.concrete_functions.insert(func_reference, function);
        Ok(id)
    }

    pub fn get_function(&self, func_reference: &FuncReference) -> Option<&ConcreteFunction> {
        self.concrete_functions.get(func_reference)
    }

    pub fn get_length(&self, id: Id) -> Result<u128, SemanticError> {
        self.get(id)
            .ok_or(SemanticError::UninitialisedType)?
            .expect_length()
    }

    pub fn insert_function(
        &mut self,
        func_reference: FuncReference,
    ) -> Result<ConcreteFunction, SemanticError> {
        if let Some(function) = self.concrete_functions.get(&func_reference) {
            return Ok(function.clone());
        }

        let generic_function = self
            .get(func_reference.id)
            .ok_or(SemanticError::UninitialisedType)?
            .expect_function()?;

        let function = match generic_function {
            GenericFunction::Internal {
                function,
                scope_id: parameter_scope,
            } => {
                ConcreteFunction::Internal(function::Internal::new(
                    // TODO: `function.clone()`
                    function.clone(),
                    self,
                    func_reference.generics.clone(),
                    *parameter_scope,
                )?)
            }
            GenericFunction::External(external) => ConcreteFunction::External(external.clone()),
        };

        self.concrete_functions
            .insert(func_reference, function.clone());
        Ok(function)
    }

    pub fn lookup(&self, ident: &Spanned<Ident>, scope: ScopeId) -> Result<Id, SemanticError> {
        self.scopes[scope.0]
            .get(&ident.value.0)
            .or_else(|| self.lookup(ident, self.scopes[scope.0].parent?).ok())
            .ok_or_else(|| SemanticError::DeclarationNotFound(ident.clone()))
    }

    pub fn lookup_type(
        &mut self,
        r#type: &parser::Type,
        scope: ScopeId,
    ) -> Result<TypeReference, SemanticError> {
        let id = self.lookup(&r#type.name, scope)?;
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
