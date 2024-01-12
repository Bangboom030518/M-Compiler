use crate::SemanticError;
use ::parser::prelude::*;
use ::parser::top_level::{DeclarationKind, Parameter, PrimitiveKind, TypeBinding};
use cranelift::codegen::ir::{AbiParam, Signature};
use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub struct Id(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Struct {
        fields: Vec<(Ident, Id)>,
        ident: Ident,
    },
    Union {
        variants: Vec<(Ident, Id)>,
        ident: Ident,
    },
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
}

impl Type {
    pub fn cranelift_type(&self) -> cranelift::prelude::Type {
        match self {
            Self::U8 | Self::I8 => types::I8,
            Self::U16 | Self::I16 => types::I16,
            Self::F32 => types::F32,
            Self::U32 | Self::I32 => types::I32,
            Self::F64 => types::F64,
            Self::U64 | Self::I64 => types::I64,
            Self::U128 | Self::I128 => types::I128,
            _ => todo!("handle complex data structures"),
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
}

impl Function {
    // FIXME: signature generation must happen on a second walk of tree
    pub fn new(
        function: ::parser::top_level::Function,
        call_conv: CallConv,
        declarations: &TopLevelDeclarations,
        scope: ::parser::scope::Id,
        name: &Ident,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut signature = Signature::new(call_conv);

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
                Ok(AbiParam::new(
                    declarations.get_type(r#type.1)?.cranelift_type(),
                ))
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

        let return_type = declarations.get_type(return_type_id)?.cranelift_type();

        signature.returns = vec![AbiParam::new(return_type)];

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
                let r#type = declarations.get_type(type_id)?.cranelift_type();
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
}

impl TopLevelDeclarations {
    pub fn new(
        mut file: scope::File,
        call_conv: CallConv,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut declarations = Self {
            declarations: Vec::new(),
            scopes: HashMap::new(),
        };

        declarations.append_new(file.root, &mut file.cache, call_conv, module)?;

        Ok(declarations)
    }

    fn append_new(
        &mut self,
        scope_id: scope::Id,
        scope_cache: &mut scope::Cache,
        call_conv: CallConv,
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
                    self.append_new(r#struct.scope, scope_cache, call_conv, module)?;

                    let r#type = Type::Struct {
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
                    self.initialise(id, Declaration::Type(r#type));
                }
                DeclarationKind::Union(_) => todo!("unions!"),
                DeclarationKind::Primitive(primitive) => {
                    let r#type = match primitive.kind {
                        PrimitiveKind::I8 => Type::I8,
                        PrimitiveKind::I16 => Type::I16,
                        PrimitiveKind::I32 => Type::I32,
                        PrimitiveKind::I64 => Type::I64,
                        PrimitiveKind::I128 => Type::I128,
                        PrimitiveKind::U8 => Type::U8,
                        PrimitiveKind::U16 => Type::U16,
                        PrimitiveKind::U32 => Type::U32,
                        PrimitiveKind::U64 => Type::U64,
                        PrimitiveKind::U128 => Type::U128,
                        PrimitiveKind::F32 => Type::F32,
                        PrimitiveKind::F64 => Type::F64,
                    };
                    self.initialise(id, Declaration::Type(r#type));
                }
                DeclarationKind::Function(function) => functions.push((function, id, name)),
                DeclarationKind::Const(_) => todo!(),
            }
        }

        for (function, id, name) in functions {
            self.initialise(
                id,
                Declaration::Function(Function::new(
                    function, call_conv, self, scope_id, &name, module,
                )?),
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
