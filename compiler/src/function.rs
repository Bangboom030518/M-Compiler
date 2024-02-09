use crate::declarations::{self, Declarations};
use crate::hir::inferer;
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{CraneliftContext, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use parser::top_level::TypeBinding;
use parser::{scope, Ident};

#[derive(Debug, Clone)]
pub struct MSignature {
    pub parameters: Vec<declarations::Id>,
    pub return_type: declarations::Id,
    pub signature: Signature,
    pub name: Ident,
}

impl MSignature {
    fn new(
        parameters: &[parser::Type],
        return_type: parser::Type,
        declarations: &Declarations,
        name: Ident,
        scope_id: scope::Id,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let mut signature = module.make_signature();

        let parameters = parameters
            .iter()
            .map(|r#type| {
                let type_id = declarations
                    .lookup(&r#type.ident(), scope_id)
                    .ok_or(SemanticError::DeclarationNotFound)?;

                Ok(type_id)
            })
            .collect::<Result<Vec<_>, SemanticError>>()?;

        signature.params = parameters
            .iter()
            .map(|type_id| {
                let layout = declarations.get_layout(*type_id);
                match layout {
                    Layout::Primitive(primitive) => {
                        AbiParam::new(primitive.cranelift_type(declarations.isa.pointer_type()))
                    }
                    Layout::Struct(layout) => AbiParam::special(
                        declarations.isa.pointer_type(),
                        codegen::ir::ArgumentPurpose::StructArgument(layout.size),
                    ),
                }
            })
            .collect();

        let return_type = declarations
            .lookup(&return_type.ident(), scope_id)
            .ok_or(SemanticError::DeclarationNotFound)?;

        Ok(Self {
            parameters,
            return_type,
            signature,
            name,
        })
    }
}

pub struct ExternFunction {
    symbol_name: String,
    signature: MSignature,
    id: FuncId,
}

pub struct MFunction {
    body: Vec<parser::Statement>,
    scope_id: scope::Id,
    signature: MSignature,
    parameter_names: Vec<Ident>,
    id: FuncId,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl MFunction {
    // TODO: memcpy for structs
    pub fn new(
        function: parser::top_level::Function,
        declarations: &Declarations,
        scope_id: parser::scope::Id,
        name: Ident,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let parameters: (Vec<_>, Vec<_>) = function
            .parameters
            .into_iter()
            .map(|parser::top_level::Parameter(TypeBinding { r#type, name })| (name, r#type))
            .unzip();

        let signature = MSignature::new(
            &parameters
                .1
                .into_iter()
                .map(|r#type| r#type.ok_or(SemanticError::UntypedParameter))
                .collect::<Result<Vec<_>, _>>()?,
            function
                .return_type
                .ok_or(SemanticError::MissingReturnType)?,
            declarations,
            name,
            scope_id,
            module,
        )?;

        let mut body = function.body;

        if let Some(parser::Statement::Expression(expr)) = body.last_mut() {
            if !matches!(expr, parser::Expression::Return(_)) {
                // TODO: `.clone()`
                *expr = parser::Expression::Return(Box::new(expr.clone()));
            }
        }

        let id = module.declare_function(
            name.as_ref(),
            cranelift_module::Linkage::Export,
            &signature.signature,
        ).expect("Internal Module Error!");

        Ok(Self {
            signature,
            parameter_names: parameters.0,
            scope_id,
            body,
            id,
        })
    }

    pub fn compile(
        &self,
        declarations: &Declarations,
        cranelift_context: &mut CraneliftContext<impl Module>,
    ) -> Result<FuncId, SemanticError> {
        let mut builder = cranelift::prelude::FunctionBuilder::new(
            &mut cranelift_context.context.func,
            &mut cranelift_context.builder_context,
        );

        builder.func.signature = self.signature.signature.clone();

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations.get_layout(self.signature.return_type).is_aggregate() {
            let param = block_params
                .pop()
                .expect("aggregate return param not found");
            let variable = Variable::new(0);
            builder.declare_var(variable, declarations.isa.pointer_type());
            builder.def_var(variable, param);
        };

        let names = self
            .signature
            .parameters
            .iter()
            .zip(self.parameter_names)
            .zip(block_params)
            .enumerate()
            .map(|(index, ((type_id, name), value))| {
                let variable = Variable::new(index + SPECIAL_VARIABLES.len());
                // TODO: get type again?
                let layout = declarations.get_layout(*type_id);
                let size = layout.size(&declarations.isa);
                let cranelift_type = declarations
                    .get_layout(*type_id)
                    .cranelift_type(&declarations.isa);

                let value = if layout.is_aggregate() {
                    value
                } else {
                    let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size,
                    });

                    builder
                        .ins()
                        .stack_store(value, stack_slot, Offset32::new(0));

                    builder.ins().stack_addr(
                        declarations.isa.pointer_type(),
                        stack_slot,
                        Offset32::new(0),
                    )
                };

                builder.declare_var(variable, cranelift_type);
                builder.def_var(variable, value);

                Ok((name.clone(), variable, *type_id))
            })
            .collect::<Result<_, SemanticError>>()?;

        let mut func = crate::hir::Builder::new(declarations, self, names).build()?;
        inferer::Inferer::function(&mut func, declarations)?;

        let mut translator = Translator::new(builder, declarations, &mut cranelift_context.module);

        for statement in func.body {
            if translator.statement(statement)? == BranchStatus::Finished {
                break;
            }
        }

        translator.finalize();

        cranelift_context
            .module
            .define_function(self.id, &mut cranelift_context.context)
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

        Ok(self.id)
    }
}
