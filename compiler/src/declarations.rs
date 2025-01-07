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
    pub const fn to_type_ref(self) -> Reference {
        Reference {
            id: self,
            generics: Vec::new(),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct ScopeId(usize);

pub const TOP_LEVEL_SCOPE: ScopeId = ScopeId(0);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]

pub struct Reference {
    pub id: Id,
    pub generics: Vec<Reference>,
}

impl Reference {
    pub fn resolve(&self, declarations: &Declarations) -> Self {
        let generics = self
            .generics
            .iter()
            .map(|generic| generic.resolve(declarations))
            .collect();

        let reference = Self {
            generics,
            id: self.id,
        };

        let Some(declaration) = declarations.get(self.id) else {
            return reference;
        };

        match declaration {
            Declaration::Resolved(..) => reference,
            Declaration::Length(_) => {
                assert!(self.generics.is_empty(), "generics passed to a length");
                reference
            }
            Declaration::Alias(r#type) => {
                assert!(self.generics.is_empty(), "generics passed to an alias");
                r#type.resolve(declarations)
            }
        }
    }
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

#[derive(Clone, Debug)]
enum Declaration {
    Resolved(parser::Declaration, ScopeId),
    Alias(Reference),
    Length(u128),
}

pub struct Declarations {
    store: Vec<Option<Declaration>>,
    scopes: Vec<TopLevelScope>,
    layouts: HashMap<Reference, Layout>,
    concrete_functions: HashMap<Reference, ConcreteFunction>,
    pub isa: Arc<dyn TargetIsa>,
}

impl Declarations {
    pub fn new(
        mut parser_declarations: HashMap<String, parser::Declaration>,
        isa: &Arc<dyn TargetIsa>,
    ) -> Self {
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

        let scope = declarations.create_scope(TopLevelScope {
            declarations: scope_declarations,
            parent: None,
        });

        for (name, id) in declarations.scopes[scope.0].declarations.clone() {
            let declaration = parser_declarations
                .remove_entry(&name)
                .expect("name to exist")
                .1;
            declarations.initialise(id, Declaration::Resolved(declaration, scope));
        }

        declarations
    }

    fn is_initialised(&self, id: Id) -> bool {
        self.get(id).is_some()
    }

    pub fn build_generics(
        &mut self,
        generics: &[Spanned<parser::GenericArgument>],
        scope: ScopeId,
    ) -> Result<Vec<Reference>, SemanticError> {
        generics
            .iter()
            .map(|generic| match generic.as_ref().value {
                parser::GenericArgument::Literal(length) => Ok(Reference {
                    id: self.create(Declaration::Length(*length)),
                    generics: Vec::new(),
                }),
                parser::GenericArgument::Type(r#type) => {
                    let id = self.lookup(&r#type.name, scope)?;
                    let generics = self.build_generics(&r#type.generics.value.0, scope)?;
                    Ok(Reference { id, generics })
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
        expected: &Reference,
    ) -> Result<(), SemanticError> {
        todo!("for each of the generics within the type_ref, assert they are equivalent :)");
        let expected = expected.resolve(self);
        let found = expression.type_ref.resolve(self);

        if expected == found {
            Ok(())
        } else if !self.is_initialised(found.id) {
            self.initialise(found.id, Declaration::Alias(expected.clone()));
            Ok(())
        } else if !self.is_initialised(expected.id) {
            self.initialise(expected.id, Declaration::Alias(found.clone()));
            Ok(())
        } else {
            dbg!(&expected, &found);
            let expected = self.insert_layout(&expected)?;
            let found = self.insert_layout(&found)?;
            Err(SemanticError::MismatchedTypes {
                expected: expected.unwrap(),
                found: found.unwrap(),
                expression: expression.value.clone(),
            })
        }
    }

    /// # Errors
    /// if type is uninitialised
    pub fn insert_layout_initialised(
        &mut self,
        type_reference: &Reference,
    ) -> Result<Layout, SemanticError> {
        self.insert_layout(type_reference)
            .transpose()
            .ok_or(SemanticError::UnknownDeclaration)?
    }

    pub fn insert_layout(
        &mut self,
        type_reference: &Reference,
    ) -> Result<Option<Layout>, SemanticError> {
        if let Some(layout) = self.layouts.get(type_reference) {
            return Ok(Some(layout.clone()));
        }

        let Some(layout) = self.get(type_reference.id).cloned() else {
            return Ok(None);
        };

        let (declaration, parent_scope) = match layout {
            Declaration::Resolved(declaration, scope) => (declaration, scope),
            Declaration::Alias(reference) => return self.insert_layout(&reference),
            Declaration::Length(_) => {
                return Err(SemanticError::InvalidType);
            }
        };

        let layout = match declaration {
            parser::Declaration::Struct(ast_struct) => {
                let scope = self.create_generic_scope(
                    ast_struct.generics.value,
                    type_reference.generics.clone(),
                    parent_scope,
                )?;

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
            parser::Declaration::Primitive(primitive) => Layout::Primitive(primitive.kind.value),
            parser::Declaration::Array(array) => {
                let scope = self.create_generic_scope(
                    array.generics.value,
                    type_reference.generics.clone(),
                    parent_scope,
                )?;

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
            parser::Declaration::Union(_) => todo!("unions"),
            _ => return Err(SemanticError::InvalidType),
        };

        self.layouts.insert(type_reference.clone(), layout.clone());
        Ok(Some(layout))
    }

    fn create_scope(&mut self, scope: TopLevelScope) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(scope);
        ScopeId(id)
    }

    pub fn create_generic_scope(
        &mut self,
        parameters: parser::generic::Parameters,
        arguments: Vec<Reference>,
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
                    (
                        parameter.value.name.value.0,
                        self.create(Declaration::Alias(argument)),
                    )
                })
                .collect()
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

    fn create_uninitialised(&mut self) -> Id {
        self.store.push(None);
        Id(self.store.len() - 1)
    }

    pub fn create_type_ref(&mut self) -> Reference {
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
        func_reference: Reference,
        module: &mut impl Module,
    ) -> Result<FuncId, SemanticError> {
        let mut function = self.insert_function(&func_reference)?;
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
            ConcreteFunction::External(function) => {
                if let Some(id) = function.id {
                    return Ok(id);
                }

                let cranelift_signature = function.signature.cranelift_signature(module, self)?;
                let id = module
                    .declare_function(
                        &function.symbol.value,
                        cranelift_module::Linkage::Import,
                        &cranelift_signature,
                    )
                    .expect("internal module error");
                function.id = Some(id);
                id
            }
        };
        self.concrete_functions.insert(func_reference, function);
        Ok(id)
    }

    pub fn get_function(&self, func_reference: &Reference) -> Option<&ConcreteFunction> {
        self.concrete_functions.get(func_reference)
    }

    pub fn get_length(&self, id: Id) -> Result<u128, SemanticError> {
        match self.get(id).ok_or(SemanticError::UnknownDeclaration)? {
            Declaration::Length(length) => Ok(*length),
            Declaration::Resolved(..) => Err(SemanticError::InvalidLengthGeneric),
            Declaration::Alias(alias) => {
                let reference = alias.resolve(self);
                if reference.generics.is_empty() {
                    self.get_length(reference.id)
                } else {
                    Err(SemanticError::UnexpectedGenerics)
                }
            }
        }
    }

    pub fn insert_function(
        &mut self,
        func_reference: &Reference,
    ) -> Result<ConcreteFunction, SemanticError> {
        let func_reference = func_reference.resolve(self);

        if let Some(function) = self.concrete_functions.get(&func_reference) {
            return Ok(function.clone());
        }

        let declaration = self
            .get(func_reference.id)
            .ok_or(SemanticError::UnknownDeclaration)?;

        let Declaration::Resolved(declaration, scope) = declaration else {
            return Err(SemanticError::InvalidFunction);
        };

        let function = match declaration {
            parser::Declaration::ExternFunction(function) => {
                ConcreteFunction::External(function::External::new(function.clone(), self, *scope)?)
            }
            parser::Declaration::Function(function) => {
                ConcreteFunction::Internal(function::Internal::new(
                    function.clone(),
                    self,
                    func_reference.generics.clone(),
                    *scope,
                )?)
            }
            _ => return Err(SemanticError::InvalidFunction),
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
    ) -> Result<Reference, SemanticError> {
        let id = self.lookup(&r#type.name, scope)?;
        let generics = self.build_generics(&r#type.generics.value.0, scope)?;
        Ok(Reference { id, generics })
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
