use crate::SemanticError;
use ::parser::prelude::*;
use ::parser::top_level::{DeclarationKind, Parameter, PrimitiveKind, TypeBinding};
use ::parser::Ident;
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::codegen::ir::{AbiParam, Signature};
use cranelift::codegen::isa::TargetIsa;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use itertools::{FoldWhile, Itertools};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Struct {
    pub fields: Vec<(Ident, Id)>,
    pub ident: Ident,
}

impl Struct {
    pub fn size(&self, declarations: &TopLevelDeclarations) -> Result<u32, SemanticError> {
        self.fields
            .iter()
            .map(|(_, type_id)| {
                declarations
                    .get_type(*type_id)
                    .and_then(|r#type| r#type.size(declarations))
            })
            .sum::<Result<u32, _>>()
    }

    pub fn offset(
        &self,
        field: &Ident,
        declarations: &TopLevelDeclarations,
    ) -> Result<Offset32, SemanticError> {
        let offset =
            self.fields
                .iter()
                .fold_while(Ok::<_, SemanticError>(0), |offset, (name, type_id)| {
                    if name == field {
                        FoldWhile::Done(offset)
                    } else {
                        FoldWhile::Continue(offset.and_then(|offset| {
                            Ok(
                                offset
                                    + declarations.get_type(*type_id)?.size(declarations)? as i32,
                            )
                        }))
                    }
                });

        let FoldWhile::Done(offset) = offset else {
            return Err(SemanticError::NonExistentField);
        };

        Ok(Offset32::new(offset?))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Struct(Struct),
    Union {
        variants: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Primitive(PrimitiveKind),
}

impl Type {
    pub fn size(&self, declarations: &TopLevelDeclarations) -> Result<u32, SemanticError> {
        let size = match self {
            Self::Primitive(primitive) => primitive.size(),
            Self::Struct(r#struct) => r#struct.size(declarations)?,
            Self::Union { .. } => todo!(),
        };
        Ok(size)
    }

    pub fn cranelift_type(&self, isa: &Arc<dyn TargetIsa>) -> cranelift::prelude::Type {
        match self {
            Self::Primitive(primitive_kind) => primitive_kind.cranelift_type(),
            Self::Struct(_) => isa.pointer_type(),
            Self::Union { .. } => todo!(),
        }
    }
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

impl Function {
    // FIXME: signature generation must happen on a second walk of tree
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
                        ::parser::Type::Identifier(ident) => ident,
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
                let r#type = declarations.get_type(r#type.1)?;
                match r#type {
                    Type::Primitive(primitive) => Ok(AbiParam::new(primitive.cranelift_type())),
                    Type::Struct(r#struct) => Ok(AbiParam::special(
                        todo!(),
                        codegen::ir::ArgumentPurpose::StructArgument(r#struct.size(declarations)?),
                    )),
                    Type::Union { .. } => todo!(),
                }
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
            .lookup(return_type, scope)
            .ok_or(SemanticError::DeclarationNotFound)?;

        let return_type = declarations.get_type(return_type_id)?;

        let r#return = match return_type {
            Type::Primitive(primitive) => AbiParam::new(primitive.cranelift_type()),
            Type::Struct(r#struct) => AbiParam::special(
                declarations.isa.pointer_type(),
                codegen::ir::ArgumentPurpose::StructReturn,
            ),
            Type::Union { .. } => todo!(),
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
        let block_params = builder.block_params(entry_block).to_vec();

        let names = parameters
            .into_iter()
            .zip(block_params)
            .enumerate()
            .map(|(index, ((name, type_id), value))| {
                let variable = Variable::new(index);
                // TODO: get type again?
                let r#type = declarations
                    .get_type(type_id)?
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
            new_variable_index: names.len(),
            names,
            builder,
            module: &mut cranelift_context.module,
            isa: Arc::clone(&declarations.isa),
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
}

impl TopLevelDeclarations {
    pub fn new(
        mut file: scope::File,
        isa: Arc<dyn TargetIsa>,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            declarations: Vec::new(),
            scopes: HashMap::new(),
            isa: Arc::clone(&isa),
        };

        declarations.append_new(file.root, &mut file.cache, module)?;

        Ok(declarations)
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

                    let r#type = Struct {
                        fields: r#struct
                            .fields
                            .iter()
                            .map(|top_level::Field { r#type, name }| {
                                // TODO: will need to handle generics
                                let type_id = self
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
                        ident: name.clone(),
                    };
                    self.initialise(id, Declaration::Type(Type::Struct(r#type)));
                }
                DeclarationKind::Union(_) => todo!("unions!"),
                DeclarationKind::Primitive(primitive) => {
                    self.initialise(id, Declaration::Type(Type::Primitive(primitive.kind)));
                }
                DeclarationKind::Function(function) => functions.push((function, id, name)),
                DeclarationKind::Const(_) => todo!(),
            }
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

    pub fn get_type(&self, id: Id) -> Result<&Type, SemanticError> {
        self.get(id).expect_type()
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
