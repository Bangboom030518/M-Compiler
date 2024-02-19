use crate::declarations::{self, Declarations};
use crate::hir::inferer;
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{CraneliftContext, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use parser::top_level::TypeBinding;
use parser::{scope, Ident};

#[derive(Debug, Clone, PartialEq)]
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
        module: &impl Module,
    ) -> Result<Self, SemanticError> {
        let mut signature = module.make_signature();
        let parameters = parameters
            .iter()
            .map(|path| {
                let ident = match path {
                    parser::Type::Ident(ident) => ident,
                };
                let type_id = declarations
                    .lookup(ident, scope_id)
                    .ok_or_else(|| SemanticError::DeclarationNotFound(ident.clone()))?;

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
                    Layout::Struct(_) => AbiParam::new(declarations.isa.pointer_type()),
                }
            })
            .collect();

        // TODO: yuck
        let return_type = {
            let ident = return_type.ident();
            declarations
                .lookup(&ident, scope_id)
                .ok_or_else(|| SemanticError::DeclarationNotFound(ident))?
        };

        signature.returns = vec![match declarations.get_layout(return_type) {
            Layout::Primitive(primitive) => {
                AbiParam::new(primitive.cranelift_type(declarations.isa.pointer_type()))
            }
            Layout::Struct(_) => {
                signature
                    .params
                    .push(AbiParam::new(declarations.isa.pointer_type()));

                AbiParam::new(declarations.isa.pointer_type())
            }
        }];

        Ok(Self {
            parameters,
            return_type,
            signature,
            name,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct External {
    pub symbol_name: String,
    pub signature: MSignature,
    pub id: FuncId,
}

impl External {
    pub fn new(
        function: parser::top_level::ExternFunction,
        declarations: &Declarations,
        scope_id: parser::scope::Id,
        name: Ident,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let signature = MSignature::new(
            &function.parameters,
            function.return_type,
            declarations,
            name,
            scope_id,
            module,
        )?;

        let id = module
            .declare_function(&function.symbol, Linkage::Import, &signature.signature)
            .expect("internal module error");

        Ok(Self {
            symbol_name: function.symbol,
            signature,
            id,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Internal {
    pub body: Vec<parser::Statement>,
    pub scope_id: scope::Id,
    pub signature: MSignature,
    pub parameter_names: Vec<Ident>,
    pub id: FuncId,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
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

        let id = module
            .declare_function(
                signature.name.as_ref(),
                cranelift_module::Linkage::Export,
                &signature.signature,
            )
            .expect("Internal Module Error!");

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

        if declarations
            .get_layout(self.signature.return_type)
            .is_aggregate()
        {
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
            .zip(&self.parameter_names)
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

                let value = if layout.should_load() {
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
                } else {
                    value
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
