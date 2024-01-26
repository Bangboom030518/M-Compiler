use crate::declarations::{self, Declarations};
use crate::layout::Layout;
use crate::{CraneliftContext, SemanticError};
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use parser::top_level::TypeBinding;
use parser::{scope, Expression, Ident, Statement};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<(Ident, declarations::Id)>,
    pub return_type: declarations::Id,
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
        function: parser::top_level::Function,
        declarations: &Declarations,
        scope: ::parser::scope::Id,
        name: &Ident,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut signature = Signature::new(declarations.isa.default_call_conv());

        let parameters = function
            .parameters
            .into_iter()
            .map(
                |parser::top_level::Parameter(TypeBinding { r#type, name })| {
                    let r#type = r#type
                        .as_ref()
                        .map(|r#type| match r#type {
                            ::parser::Type::Ident(ident) => ident,
                        })
                        .ok_or(SemanticError::UntypedParameter)?;

                    let type_id = declarations
                        .lookup(r#type, scope)
                        .ok_or(SemanticError::DeclarationNotFound)?;

                    Ok((name, type_id))
                },
            )
            .collect::<Result<Vec<_>, SemanticError>>()?;

        signature.params = parameters
            .iter()
            .map(|r#type| {
                let r#type = declarations.get_layout(r#type.1);
                match r#type {
                    Layout::Primitive(primitive) => Ok(AbiParam::new(
                        primitive.cranelift_type(declarations.isa.pointer_type()),
                    )),
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
        signature.returns = vec![match return_type {
            Layout::Primitive(primitive) => {
                AbiParam::new(primitive.cranelift_type(declarations.isa.pointer_type()))
            }
            Layout::Struct { .. } => {
                signature
                    .params
                    .push(AbiParam::new(declarations.isa.pointer_type()));

                AbiParam::new(declarations.isa.pointer_type())
            }
        }];

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
            return_type: return_type_id,
            body,
            scope,
            id,
            name: name.clone(),
        })
    }

    pub fn compile(
        &self,
        declarations: &Declarations,
        cranelift_context: &mut CraneliftContext<impl Module>,
    ) -> Result<FuncId, SemanticError> {
        let Self {
            parameters,
            return_type: r#return,
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
        builder.func.signature = *signature;

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations.get_layout(self.return_type).is_aggregate() {
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
                let cranelift_type = declarations
                    .get_layout(*type_id)
                    .cranelift_type(&declarations.isa);
                builder.declare_var(variable, cranelift_type);
                builder.def_var(variable, value);
                Ok((name.clone(), variable, *type_id))
            })
            .collect::<Result<_, SemanticError>>()?;

        let mut builder = crate::hir::Builder::new(declarations, &self, names);
        let func = builder.build()?;

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
