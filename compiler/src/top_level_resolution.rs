use crate::SemanticError;
use ::parser::prelude::*;
use ::parser::top_level::{DeclarationKind, Parameter, PrimitiveKind, TypeBinding};
use ::parser::Ident;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::ir::{AbiParam, Signature};
use cranelift::codegen::isa::TargetIsa;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use itertools::Itertools;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Field {
    pub r#type: Id,
    pub offset: Offset32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Layout {
    Struct {
        fields: HashMap<Ident, Field>,
        size: u32,
    },
    Primitive(PrimitiveKind),
}

impl Layout {
    pub fn size(&self) -> u32 {
        match self {
            Self::Primitive(primitive) => primitive.size(),
            Self::Struct { size, .. } => *size,
        }
    }

    pub const fn is_aggregate(&self) -> bool {
        matches!(self, Self::Struct { .. })
    }

    pub fn cranelift_type(&self, isa: &Arc<dyn TargetIsa>) -> cranelift::prelude::Type {
        match self {
            Self::Primitive(primitive_kind) => primitive_kind.cranelift_type(),
            Self::Struct { .. } => isa.pointer_type(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Type {
    Struct {
        fields: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Union {
        variants: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Primitive(PrimitiveKind),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct FunctionId(usize);

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<(Ident, Id)>,
    pub r#return: Id,
    pub signature: Signature,
    pub body: Vec<Statement>,
    pub scope: scope::Id,
    pub id: FuncId,
    pub name: Ident,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Function {
    // TODO: memcpy for structs
    pub fn new(
        function: ::parser::top_level::Function,
        declarations: &TopLevelDeclarations,
        scope: ::parser::scope::Id,
        name: &Ident,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut signature = Signature::new(declarations.isa.default_call_conv());

        let parameters = function
            .parameters
            .into_iter()
            .map(|Parameter(TypeBinding { r#type, name })| {
                let r#type = r#type
                    .as_ref()
                    .map(|r#type| match r#type {
                        ::parser::Type::Ident(ident) => ident,
                    })
                    .ok_or(SemanticError::UntypedParameter)?;

                let r#type = declarations
                    .lookup(r#type, scope)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                Ok((name, r#type))
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        signature.params = parameters
            .iter()
            .map(|r#type| {
                let r#type = declarations.get_layout(r#type.1);
                match r#type {
                    Layout::Primitive(primitive) => Ok(AbiParam::new(primitive.cranelift_type())),
                    Layout::Struct { size, .. } => Ok(AbiParam::special(
                        declarations.isa.pointer_type(),
                        codegen::ir::ArgumentPurpose::StructArgument(*size),
                    )),
                }
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        let return_type = function
            .return_type
            .as_ref()
            .map(|r#type| match r#type {
                ::parser::Type::Ident(ident) => ident,
            })
            .ok_or(SemanticError::MissingReturnType)?;

        let return_type_id = declarations
            .lookup(return_type, scope)
            .ok_or(SemanticError::DeclarationNotFound)?;

        let return_type = declarations.get_layout(return_type_id);
        let r#return = match return_type {
            Layout::Primitive(primitive) => AbiParam::new(primitive.cranelift_type()),
            Layout::Struct { .. } => {
                signature
                    .params
                    .push(AbiParam::new(declarations.isa.pointer_type()));

                AbiParam::new(declarations.isa.pointer_type())
            }
        };
        signature.returns = vec![r#return];

        let mut body = function.body;

        if let Some(Statement::Expression(expr)) = body.last_mut() {
            if !matches!(expr, Expression::Return(_)) {
                // TODO: `.clone()`
                *expr = Expression::Return(Box::new(expr.clone()));
            }
        }

        let id = module
            .declare_function(name.as_ref(), cranelift_module::Linkage::Export, &signature)
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        Ok(Self {
            signature,
            parameters,
            r#return: return_type_id,
            body,
            scope,
            id,
            name: name.clone(),
        })
    }

    pub fn compile(
        self,
        declarations: &TopLevelDeclarations,
        cranelift_context: &mut CraneliftContext<impl Module>,
    ) -> Result<FuncId, SemanticError> {
        let Self {
            parameters,
            r#return,
            signature,
            body,
            scope,
            id,
            ..
        } = self;
        let mut builder = cranelift::prelude::FunctionBuilder::new(
            &mut cranelift_context.context.func,
            &mut cranelift_context.builder_context,
        );
        builder.func.signature = signature;

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations.get_layout(self.r#return).is_aggregate() {
            let param = block_params
                .pop()
                .expect("aggregate return param not found");
            let variable = Variable::new(0);
            builder.declare_var(variable, declarations.isa.pointer_type());
            builder.def_var(variable, param);
        };

        let names = parameters
            .into_iter()
            .zip(block_params)
            .enumerate()
            .map(|(index, ((name, type_id), value))| {
                let variable = Variable::new(index + SPECIAL_VARIABLES.len());
                // TODO: get type again?
                let r#type = declarations
                    .get_layout(type_id)
                    .cranelift_type(&declarations.isa);
                builder.declare_var(variable, r#type);
                builder.def_var(variable, value);
                Ok((name, (variable, Some(type_id))))
            })
            .collect::<Result<HashMap<_, _>, SemanticError>>()?;

        let mut builder = crate::local::FunctionBuilder {
            declarations,
            r#return,
            scope,
            new_variable_index: names.len() + SPECIAL_VARIABLES.len(),
            names,
            builder,
            module: &mut cranelift_context.module,
        };

        for statement in body {
            builder.handle_statement(statement)?;
        }

        // error is here!!!!
        builder.builder.finalize();

        cranelift_context
            .module
            .define_function(id, &mut cranelift_context.context)
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        #[cfg(debug_assertions)]
        {
            use std::io::Write;
            let text = cranelift_context.context.func.display().to_string();
            let mut file = std::fs::OpenOptions::new()
                .append(true)
                .open("function-ir.clif")
                .unwrap();

            write!(file, "{text}").unwrap();
        }

        cranelift_context
            .module
            .clear_context(&mut cranelift_context.context);

        Ok(id)
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

pub struct CraneliftContext<M> {
    pub context: cranelift::codegen::Context,
    pub module: M,
    pub builder_context: cranelift::frontend::FunctionBuilderContext,
}

impl<M> CraneliftContext<M> {
    pub fn new(module: M) -> Self
    where
        M: Module,
    {
        Self {
            context: module.make_context(),
            module,
            builder_context: cranelift::frontend::FunctionBuilderContext::new(),
        }
    }
}

pub struct TopLevelDeclarations {
    pub declarations: Vec<Option<Declaration>>,
    pub scopes: HashMap<scope::Id, TopLevelScope>,
    pub isa: Arc<dyn TargetIsa>,
    pub layouts: HashMap<Id, Layout>,
}

impl TopLevelDeclarations {
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

    fn insert_layout(&mut self, id: Id) -> Result<Layout, SemanticError> {
        // TODO: clones
        if let Some(layout) = self.layouts.get(&id) {
            return Ok(layout.clone());
        }

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
                        Field {
                            r#type: *r#type,
                            offset: Offset32::new(offset as i32),
                        },
                    );
                    let layout = self.insert_layout(*r#type)?;
                    offset += layout.size();
                }
                Layout::Struct {
                    fields: layout_fields,
                    size: offset,
                }
            }
            Type::Union { .. } => todo!(),
            Type::Primitive(primitive) => Layout::Primitive(primitive),
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

        for (name, id) in self.scopes[&scope_id].declarations.clone() {
            match declarations.remove_entry(&name).expect("TODO").1 {
                DeclarationKind::Struct(r#struct) => {
                    self.append_new(r#struct.scope, scope_cache, module)?;

                    let fields = r#struct
                        .fields
                        .iter()
                        .map(|top_level::Field { r#type, name }| {
                            // TODO: will need to handle generics
                            let type_id = self
                                .lookup(
                                    match r#type {
                                        ::parser::Type::Ident(identifier) => identifier,
                                    },
                                    r#struct.scope,
                                )
                                .ok_or(SemanticError::DeclarationNotFound)?;
                            Ok((name.clone(), type_id))
                        })
                        .collect::<Result<Vec<_>, SemanticError>>()?;

                    let ident = name.clone();
                    self.initialise(id, Declaration::Type(Type::Struct { ident, fields }));
                }
                DeclarationKind::Union(_) => todo!("unions!"),
                DeclarationKind::Primitive(primitive) => {
                    self.initialise(id, Declaration::Type(Type::Primitive(primitive.kind)));
                }
                DeclarationKind::Function(function) => functions.push((function, id, name)),
                DeclarationKind::Const(_) => todo!(),
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
            self.insert_layout(id)?;
        }

        for (function, id, name) in functions {
            self.initialise(
                id,
                Declaration::Function(Function::new(function, self, scope_id, &name, module)?),
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
    pub fn get(&self, Id(id): Id) -> &Declaration {
        self.declarations.get(id).unwrap().as_ref().unwrap()
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
