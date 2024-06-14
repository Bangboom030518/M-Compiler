use crate::layout::{self, Array, Layout};
use crate::{function, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Module};
use parser::Ident;
use std::collections::HashMap;
use std::sync::Arc;
use tokenizer::Spanned;

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

impl TypeReference {
    pub fn resolve(&self, declarations: &Declarations) -> Self {
        match declarations.get(self.id) {
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

    pub fn assert_equivalent(
        &self,
        other: &TypeReference,
        declarations: &mut Declarations,
        scope: ScopeId,
        expression: &crate::hir::Expression,
    ) -> Result<(), SemanticError> {
        let left = self.resolve(declarations);
        let right = other.resolve(declarations);
        if left == right {
            Ok(())
        } else {
            let left = declarations.insert_layout(&left, scope)?;
            let right = declarations.insert_layout(&right, scope)?;
            Err(SemanticError::MismatchedTypes {
                expected: left,
                found: right,
                expression: expression.clone(),
            })
        }
    }
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
    Array(Length, Spanned<parser::Type>),
}

fn resolve_type(
    r#type: &Spanned<parser::Type>,
    declarations: &mut Declarations,
    scope: ScopeId,
) -> Result<TypeReference, SemanticError> {
    let name = r#type.value.name.clone();
    let id = declarations
        .lookup(&name.value.0, scope)
        .ok_or_else(|| SemanticError::DeclarationNotFound(name))?;
    let generics = declarations.resolve_generics(&r#type.value.generics.value.0, scope)?;

    Ok(TypeReference { id, generics })
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Type {
    kind: TypeKind,
    generic_parameters: Spanned<parser::top_level::GenericParameters>,
    parent_scope: ScopeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum TypeKind {
    Struct {
        fields: Vec<(Spanned<Ident>, Spanned<parser::Type>)>,
    },
    Primitive(Primitive),
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
    TypeAlias(TypeReference),
    Length(u128),
    // LengthGeneric(usize),
    // TypeGeneric(usize),
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

    pub const fn expect_length(&self) -> Result<u128, SemanticError> {
        match self {
            Self::Length(generic) => Ok(*generic),
            _ => Err(SemanticError::InvalidLengthGeneric),
        }
    }

    pub fn expect_type_alias(&self) -> Result<TypeReference, SemanticError> {
        match self {
            Self::TypeAlias(generic) => Ok(generic.clone()),
            _ => Err(SemanticError::InvalidTypeGeneric),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Length {
    Literal(u128),
    Generic(String),
}

impl Length {
    fn resolve(&self, declarations: &Declarations, scope: ScopeId) -> Result<u128, SemanticError> {
        match self {
            Self::Literal(length) => Ok(*length),
            Self::Generic(name) => {
                let id = declarations.lookup(name, scope).ok_or_else(|| {
                    SemanticError::DeclarationNotFound(todo!("'{name}' not found error"))
                })?;

                declarations.get(id).expect_length()
            }
        }
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
                parser::Declaration::Struct(ast_struct) => {
                    let fields = ast_struct
                        .fields
                        .iter()
                        .map(|field| field.value.clone())
                        .map(|parser::top_level::Field { r#type, name }| (name, r#type))
                        .collect();

                    let declaration = Declaration::Type(Type {
                        kind: TypeKind::Struct { fields },
                        parent_scope: scope_id,
                        generic_parameters: ast_struct.generics,
                    });

                    declarations.initialise(id, declaration);
                }
                parser::Declaration::Union(_) => todo!("unions"),
                parser::Declaration::Primitive(ast_primitive) => {
                    use parser::top_level::{Length as L, PrimitiveKind as P};

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
                        P::Array(length, element_type) => {
                            let length = match length.value {
                                L::Ident(ident) => Length::Generic(ident.0),
                                L::Literal(length) => Length::Literal(length),
                            };

                            Primitive::Array(length, element_type)
                        }
                    };

                    let primitive = Declaration::Type(Type {
                        parent_scope: scope_id,
                        generic_parameters: ast_primitive.generics,
                        kind: TypeKind::Primitive(primitive),
                    });

                    declarations.initialise(id, primitive);
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
                        Declaration::Length(_) => {
                            if r#type.generics.value.0.is_empty() {
                                GenericArgument::Length(Length::Generic(
                                    r#type.name.value.0.clone(),
                                ))
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

    pub fn insert_layout(
        &mut self,
        type_reference: &TypeReference,
        argument_scope: ScopeId,
        // function_generics: &[GenericArgument],
    ) -> Result<Layout, SemanticError> {
        // TODO: clones
        if let Some(layout) = self.layouts.get(type_reference) {
            return Ok(layout.clone());
        }

        // TODO: clone
        let r#type = match self.get(type_reference.id).clone() {
            Declaration::Type(r#type) => r#type,
            Declaration::TypeAlias(reference) => {
                return self.insert_layout(&reference, argument_scope)
            }
            _ => {
                return Err(SemanticError::InvalidType);
            }
        };
        // TODO: r#type.parent_scope
        let scope = self.create_generic_scope(
            r#type.generic_parameters.clone(),
            &type_reference.generics,
            r#type.parent_scope,
            argument_scope,
        )?;

        let layout = match r#type.kind {
            TypeKind::Struct { fields } => {
                let mut offset = 0;
                let mut layout_fields = HashMap::new();
                for (name, field) in &fields {
                    let field = resolve_type(field, self, scope)?;
                    layout_fields.insert(
                        name.value.0.clone(),
                        layout::Field {
                            type_id: field.clone(),
                            offset: Offset32::new(
                                i32::try_from(offset).expect("struct offset exceded `i32::MAX`"),
                            ),
                        },
                    );
                    let layout = self.insert_layout(&field, scope)?;
                    offset += layout.size(&self.isa);
                }
                Layout::Struct(layout::Struct {
                    fields: layout_fields,
                    size: offset,
                })
            }
            TypeKind::Primitive(primitive) => match primitive {
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
                Primitive::Array(length, element_type) => {
                    let element_type = resolve_type(&element_type, self, scope)?;
                    let element_type = match self.get(element_type.id) {
                        Declaration::Type(_) => element_type,
                        Declaration::TypeAlias(reference) => reference.clone(),
                        Declaration::Function(_) => return Err(SemanticError::InvalidFunction),
                        Declaration::Length(_) => return Err(SemanticError::InvalidLengthGeneric),
                    };

                    let size = self.insert_layout(&element_type, scope)?.size(&self.isa);

                    Layout::Array(Array {
                        length: length.resolve(self, scope)?,
                        size,
                        item: element_type,
                    })
                }
            },
        };

        self.layouts.insert(type_reference.clone(), layout.clone());
        Ok(layout)
    }

    pub fn create_scope(&mut self, scope: TopLevelScope) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(scope);
        ScopeId(id)
    }

    pub fn create_generic_scope(
        &mut self,
        parameters: Spanned<parser::top_level::GenericParameters>,
        arguments: &[GenericArgument],
        parameter_scope: ScopeId,
        argument_scope: ScopeId,
    ) -> Result<ScopeId, SemanticError> {
        if arguments.len() != parameters.value.generics.len() {
            return Err(SemanticError::GenericParametersMismatch);
        }

        let generics = parameters
            .value
            .generics
            .into_iter()
            .zip(arguments)
            .map(|(parameter, argument)| match (parameter.value, argument) {
                (
                    parser::top_level::GenericParameter::Type { name },
                    GenericArgument::Type(type_ref),
                ) => Ok((
                    name.value.0,
                    self.create(Declaration::TypeAlias(type_ref.clone())),
                )),
                (
                    parser::top_level::GenericParameter::Length { name },
                    GenericArgument::Length(length),
                ) => {
                    let length = length.resolve(self, argument_scope)?;
                    Ok((name.value.0, self.create(Declaration::Length(length))))
                }
                _ => todo!("mismatched generics"),
            })
            .collect::<Result<_, SemanticError>>()?;

        Ok(self.create_scope(TopLevelScope {
            declarations: generics,
            parent: Some(parameter_scope),
        }))
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
        context: &mut impl Module,
        argument_scope: ScopeId,
    ) -> Result<ConcreteFunction, SemanticError> {
        // TODO: `.clone()`s
        if let Some(function) = self.concrete_functions.get(&func_reference) {
            return Ok(function.clone());
        }

        let Ok(generic_function) = self.get(func_reference.id).expect_function() else {
            return Err(SemanticError::InvalidType);
        };

        let function = match generic_function {
            GenericFunction::Internal {
                function,
                scope_id: parameter_scope,
            } => {
                ConcreteFunction::Internal(function::Internal::new(
                    // TODO: `function.clone()`
                    function.clone(),
                    self,
                    context,
                    func_reference.generics.clone(),
                    *parameter_scope,
                    argument_scope,
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
