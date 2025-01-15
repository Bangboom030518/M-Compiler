use crate::hir::{self, Typed};
use crate::layout::{self, Array, Layout};
use crate::{errors, function, Error};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::isa::TargetIsa;
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

#[derive(Clone, Debug, PartialEq)]
pub enum Function {
    Internal(function::Internal),
    External(function::External),
}

impl Function {
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
    Length(u32),
}

#[derive(Debug)]
#[deprecated = "make private"]
pub struct ResolvedReferenceMap<V> {
    pub map: HashMap<Reference, V>,
}

impl<V> ResolvedReferenceMap<V> {
    fn normalize(&mut self, declarations: &UnresolvedDeclarations) {
        let mut new_map = HashMap::new();
        for (reference, value) in self.map.drain() {
            let reference = declarations.resolve(&reference);
            new_map.insert(reference, value);
        }
        self.map = new_map;
    }

    fn insert(&mut self, reference: &Reference, value: V, declarations: &UnresolvedDeclarations) {
        self.normalize(declarations);
        let reference = declarations.resolve(reference);
        self.map.insert(reference, value);
    }

    fn get(&mut self, reference: &Reference, declarations: &UnresolvedDeclarations) -> Option<&V> {
        self.normalize(declarations);
        let reference = declarations.resolve(reference);
        self.map.get(&reference)
    }

    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
}
pub struct UnresolvedDeclarations {
    store: Vec<Option<Declaration>>,
    scopes: Vec<TopLevelScope>,
}

impl UnresolvedDeclarations {
    pub fn build_generics(
        &mut self,
        generics: &[Spanned<parser::GenericArgument>],
        scope: ScopeId,
    ) -> Result<Vec<Reference>, Error> {
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

    fn assert_equivalent(&mut self, expected: &Reference, found: &Reference) -> Result<(), Error> {
        if !self.is_initialised(found.id) {
            self.initialise(found.id, Declaration::Alias(expected.clone()));
            return Ok(());
        } else if !self.is_initialised(expected.id) {
            self.initialise(expected.id, Declaration::Alias(found.clone()));
            return Ok(());
        }

        if expected.id == found.id {
            if expected.generics.len() != found.generics.len() {
                return Err(Error {
                    span: todo!(),
                    kind: errors::Kind::MismatchedGenericArguments {
                        arguments: found.generics.len(),
                        parameters: expected.generics.len(),
                        declaration: todo!("is this error internal"),
                    },
                });
            }
            for (expected, found) in iter::zip(&expected.generics, &found.generics) {
                self.assert_equivalent(expected, found)?;
            }
            return Ok(());
        }

        Err(Error {
            kind: errors::Kind::MismatchedTypes {
                expected: expected.clone(),
                found: found.clone(),
            },
            span: todo!(),
        })
    }

    pub fn check_expression_type(
        &mut self,
        expression: &Typed<hir::Expression>,
        expected: &Reference,
    ) -> Result<(), Error> {
        let expected = self.resolve(expected);
        let found = self.resolve(&expression.type_ref);
        self.assert_equivalent(&expected, &found)
    }

    pub fn lookup(&self, ident: &Spanned<Ident>, scope: ScopeId) -> Result<Id, Error> {
        self.scopes[scope.0]
            .get(&ident.value.0)
            .or_else(|| self.lookup(ident, self.scopes[scope.0].parent?).ok())
            .ok_or_else(|| Error {
                span: ident.span.clone(),
                kind: errors::Kind::DeclarationNotFound {
                    ident: ident.value.0.clone(),
                },
            })
    }

    pub fn lookup_type(
        &mut self,
        r#type: &parser::Type,
        scope: ScopeId,
    ) -> Result<Reference, Error> {
        let id = self.lookup(&r#type.name, scope)?;
        let generics = self.build_generics(&r#type.generics.value.0, scope)?;
        Ok(Reference { id, generics })
    }

    /// Gets the declaration at `Id`, returning `None` if the declaration is uninitialised
    ///
    /// # Panics
    /// If the declaration at `Id` does not exist
    #[must_use]
    fn get(&self, Id(id): Id) -> Option<&Declaration> {
        self.store[id].as_ref()
    }

    fn is_initialised(&self, id: Id) -> bool {
        self.get(id).is_some()
    }

    pub fn get_initialised_length(&self, id: Id) -> Result<u32, Error> {
        self.get_length(id)?.ok_or_else(|| Error {
            span: todo!(),
            kind: errors::Kind::CannotInferType,
        })
    }

    fn get_length(&self, id: Id) -> Result<Option<u32>, Error> {
        let Some(declaration) = self.get(id) else {
            return Ok(None);
        };

        match declaration {
            Declaration::Length(length) => Ok(Some(*length)),
            Declaration::Resolved(..) => Err(Error {
                span: todo!(),
                kind: errors::Kind::DeclarationConstraintViolation {
                    constraint: errors::DeclarationConstraint::Length,
                    found: id.to_type_ref(),
                },
            }),
            Declaration::Alias(alias) => {
                let reference = self.resolve(alias);
                if reference.generics.is_empty() {
                    self.get_length(reference.id)
                } else {
                    Err(Error {
                        span: todo!(),
                        kind: errors::Kind::MismatchedGenericArguments {
                            arguments: reference.generics.len(),
                            parameters: 0,
                            declaration: todo!(),
                        },
                    })
                }
            }
        }
    }

    pub fn check_length(&mut self, expected: u32, found: Id) -> Result<(), Error> {
        let found = self.resolve(&found.to_type_ref()).id;
        if let Some(found) = self.get_length(found)? {
            if found == expected {
                Ok(())
            } else {
                Err(Error {
                    span: todo!(),
                    kind: errors::Kind::MismatchedLengths { expected, found },
                })
            }
        } else {
            self.initialise(found, Declaration::Length(expected));
            Ok(())
        }
    }

    pub fn resolve(&self, reference: &Reference) -> Reference {
        let generics = reference
            .generics
            .iter()
            .map(|generic| self.resolve(generic))
            .collect();

        let reference = Reference {
            generics,
            id: reference.id,
        };

        let Some(declaration) = self.get(reference.id) else {
            return reference;
        };

        match declaration {
            Declaration::Resolved(..) => reference,
            Declaration::Length(_) => {
                assert!(reference.generics.is_empty(), "generics passed to a length");
                reference
            }
            Declaration::Alias(alias) => {
                assert!(reference.generics.is_empty(), "generics passed to an alias");
                self.resolve(alias)
            }
        }
    }

    fn create_scope(&mut self, scope: TopLevelScope) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(scope);
        ScopeId(id)
    }

    pub fn create_generic_scope(
        &mut self,
        parameters: Spanned<parser::generic::Parameters>,
        arguments: Vec<Reference>,
        parameter_scope: ScopeId,
    ) -> Result<ScopeId, Error> {
        if arguments.len() != parameters.value.generics.len() {
            return Err(Error {
                span: todo!(),
                kind: errors::Kind::MismatchedGenericArguments {
                    arguments: arguments.len(),
                    parameters: parameters.value.generics.len(),
                    declaration: parameters.span,
                },
            });
        }
        let generics = iter::zip(parameters.value.generics, arguments)
            .map(|(parameter, argument)| {
                (
                    parameter.value.name.value.0,
                    self.create(Declaration::Alias(argument)),
                )
            })
            .collect();
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
        assert!(
            self.store[index].is_none(),
            "tried to overwrite existing declaration: found '{:?}'",
            self.store[index]
        );
        self.store[index] = Some(declaration);
    }
}

pub struct Declarations {
    pub unresolved: UnresolvedDeclarations,
    layouts: ResolvedReferenceMap<Layout>,
    #[deprecated = "make private"]
    pub concrete_functions: ResolvedReferenceMap<Arc<Function>>,
    pub isa: Arc<dyn TargetIsa>,
}

impl Declarations {
    pub fn new(
        mut parser_declarations: HashMap<String, parser::Declaration>,
        isa: &Arc<dyn TargetIsa>,
    ) -> Self {
        let mut declarations = Self {
            unresolved: UnresolvedDeclarations {
                store: Vec::new(),
                scopes: Vec::new(),
            },
            layouts: ResolvedReferenceMap::new(),
            concrete_functions: ResolvedReferenceMap::new(),
            isa: Arc::clone(isa),
        };

        let scope_declarations = parser_declarations
            .iter()
            .map(|(ident, ..)| {
                (
                    ident.clone(),
                    declarations.unresolved.create_uninitialised(),
                )
            })
            .collect();

        let scope = declarations.unresolved.create_scope(TopLevelScope {
            declarations: scope_declarations,
            parent: None,
        });

        for (name, id) in declarations.unresolved.scopes[scope.0].declarations.clone() {
            let declaration = parser_declarations
                .remove_entry(&name)
                .expect("name to exist")
                .1;
            declarations
                .unresolved
                .initialise(id, Declaration::Resolved(declaration, scope));
        }

        declarations
    }

    /// # Errors
    /// if type is uninitialised
    pub fn insert_layout_initialised(
        &mut self,
        type_reference: &Reference,
    ) -> Result<Layout, Error> {
        self.insert_layout(type_reference)
            .transpose()
            .ok_or_else(|| Error {
                kind: errors::Kind::CannotInferType,
                span: 0..0, // TODO: don't ignore error in translate
            })?
    }

    pub fn insert_layout(&mut self, reference: &Reference) -> Result<Option<Layout>, Error> {
        if let Some(layout) = self.layouts.get(reference, &self.unresolved) {
            return Ok(Some(layout.clone()));
        }

        let Some(layout) = self.unresolved.get(reference.id).cloned() else {
            return Ok(None);
        };

        let (declaration, parent_scope) = match layout {
            Declaration::Resolved(declaration, scope) => (declaration, scope),
            Declaration::Alias(reference) => return self.insert_layout(&reference),
            Declaration::Length(_) => {
                return Err(Error {
                    span: todo!(),
                    kind: errors::Kind::DeclarationConstraintViolation {
                        constraint: errors::DeclarationConstraint::Type,
                        found: reference.clone(),
                    },
                });
            }
        };

        let layout = match declaration {
            parser::Declaration::Struct(ast_struct) => {
                let scope = self.unresolved.create_generic_scope(
                    ast_struct.generics,
                    reference.generics.clone(),
                    parent_scope,
                )?;

                let mut offset = 0;
                let mut layout_fields = HashMap::new();
                for field in &ast_struct.fields {
                    let type_ref = self
                        .unresolved
                        .lookup_type(&field.value.r#type.value, scope)?;
                    layout_fields.insert(
                        field.value.name.value.0.clone(),
                        layout::Field {
                            type_ref: type_ref.clone(),
                            offset: Offset32::new(i32::try_from(offset).map_err(|_| Error {
                                span: todo!(),
                                kind: errors::Kind::StructTooBig,
                            })?),
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
                let scope = self.unresolved.create_generic_scope(
                    array.generics,
                    reference.generics.clone(),
                    parent_scope,
                )?;

                let element_type = self
                    .unresolved
                    .lookup_type(&array.element_type.value, scope)?;
                let length = match array.length.value {
                    parser::Length::Ident(ident) => {
                        let id = self
                            .unresolved
                            .lookup(&ident.spanned(array.length.span), scope)?;
                        id
                    }
                    parser::Length::Literal(length) => {
                        self.unresolved.create(Declaration::Length(length))
                    }
                };
                Layout::Array(Array {
                    length,
                    element_type,
                })
            }
            parser::Declaration::Union(_) => todo!("unions"),
            _ => {
                return Err(Error {
                    span: todo!(),
                    kind: errors::Kind::DeclarationConstraintViolation {
                        constraint: errors::DeclarationConstraint::Type,
                        found: reference.clone(),
                    },
                })
            }
        };

        self.layouts
            .insert(reference, layout.clone(), &self.unresolved);
        Ok(Some(layout))
    }

    #[deprecated]
    pub fn get_function(&mut self, reference: &Reference) -> Option<&Arc<Function>> {
        self.concrete_functions.get(&reference, &self.unresolved)
    }

    pub fn make_generic_arguments(&mut self, function_id: Id) -> Vec<Reference> {
        let declaration = self
            .unresolved
            .get(function_id)
            .unwrap_or_else(|| todo!("internal error?"));

        let len = match declaration {
            Declaration::Resolved(declaration, _) => match declaration {
                parser::Declaration::Function(function) => {
                    function.generic_parameters.value.generics.len()
                }
                parser::Declaration::ExternFunction(_) => 0,
                _ => todo!(),
            },
            _ => todo!(),
        };

        (0..len)
            .map(|_| self.unresolved.create_type_ref())
            .collect()
    }

    pub fn insert_function(&mut self, reference: &Reference) -> Result<(), Error> {
        let reference = self.unresolved.resolve(reference);

        if self.get_function(&reference).is_some() {
            return Ok(());
        }

        let declaration = self
            .unresolved
            .get(reference.id)
            .ok_or_else(|| todo!("internal error?: func not initialised"))?;

        let Declaration::Resolved(declaration, scope) = declaration else {
            return Err(Error {
                span: todo!(),
                kind: errors::Kind::DeclarationConstraintViolation {
                    constraint: errors::DeclarationConstraint::Function,
                    found: reference.clone(),
                },
            });
        };

        let function = match declaration {
            parser::Declaration::ExternFunction(function) => {
                Function::External(function::External::new(function.clone(), self, *scope)?)
            }
            parser::Declaration::Function(function) => {
                // assert_eq!(
                //     reference.generics.len(),
                //     function.generic_parameters.value.generics.len()
                // );

                Function::Internal(function::Internal::new(
                    function.clone(),
                    self,
                    *scope,
                    reference.generics.clone(),
                )?)
            }
            _ => {
                return Err(Error {
                    span: todo!(),
                    kind: errors::Kind::DeclarationConstraintViolation {
                        constraint: errors::DeclarationConstraint::Function,
                        found: reference.clone(),
                    },
                })
            }
        };

        self.concrete_functions
            .insert(&reference, Arc::new(function), &self.unresolved);

        Ok(())
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
