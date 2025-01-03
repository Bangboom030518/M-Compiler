use crate::declarations::{Declarations, GenericArgument, ScopeId, TypeReference};
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{CraneliftContext, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use isa::CallConv;
use tokenizer::{AsSpanned, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MSignature {
    pub parameters: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub name: Spanned<parser::Ident>,
    pub call_conv: Option<CallConv>,
    pub signature: Option<Signature>,
    pub scope: ScopeId,
}

impl MSignature {
    fn new(
        parameters: &[Spanned<parser::Type>],
        return_type: &Spanned<parser::Type>,
        declarations: &mut Declarations,
        name: Spanned<parser::Ident>,
        scope: ScopeId,
        call_conv: Option<CallConv>,
    ) -> Result<Self, SemanticError> {
        let parameters = parameters
            .iter()
            .map(|parameter| declarations.lookup_type(&parameter.value, scope))
            .collect::<Result<Vec<_>, SemanticError>>()?;
        let return_type = declarations.lookup_type(&return_type.value, scope)?;

        Ok(Self {
            parameters,
            return_type,
            name,
            call_conv,
            scope,
            signature: None,
        })
    }

    pub fn cranelift_signature(
        &mut self,
        module: &impl Module,
        declarations: &mut Declarations,
    ) -> Result<Signature, SemanticError> {
        if let Some(signature) = &self.signature {
            return Ok(signature.clone());
        }
        let mut signature = module.make_signature();
        if let Some(call_conv) = self.call_conv {
            signature.call_conv = call_conv;
        }

        signature.params = self
            .parameters
            .iter()
            .map(|type_ref| -> Result<_, SemanticError> {
                let layout = declarations.insert_layout_initialised(type_ref)?;

                match layout {
                    Layout::Primitive(primitive) => Ok(AbiParam::new(
                        primitive.cranelift_type(declarations.isa.pointer_type()),
                    )),
                    Layout::Struct(_) | Layout::Array(_) => {
                        Ok(AbiParam::new(declarations.isa.pointer_type()))
                    }
                    Layout::Void => todo!("void params"),
                }
            })
            .collect::<Result<_, _>>()?;
        let return_type = declarations.insert_layout_initialised(&self.return_type)?;

        signature.returns = match return_type {
            Layout::Primitive(primitive) => {
                vec![AbiParam::new(
                    primitive.cranelift_type(declarations.isa.pointer_type()),
                )]
            }
            Layout::Struct(_) | Layout::Array(_) => {
                signature
                    .params
                    .push(AbiParam::new(declarations.isa.pointer_type()));

                vec![AbiParam::new(declarations.isa.pointer_type())]
            }
            Layout::Void => Vec::new(),
        };

        self.signature = Some(signature.clone());

        Ok(signature)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct External {
    pub signature: MSignature,
    pub id: FuncId,
}

impl External {
    pub fn new(
        function: parser::top_level::ExternFunction,
        declarations: &mut Declarations,
        scope: ScopeId,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let call_conv =
            CallConv::for_libcall(module.isa().flags(), module.isa().default_call_conv());

        let mut signature = MSignature::new(
            &function.parameters,
            &function.return_type,
            declarations,
            function.name,
            scope,
            Some(call_conv),
        )?;

        let cranelift_signature = &signature.cranelift_signature(module, declarations)?;
        let id = module
            .declare_function(&function.symbol.value, Linkage::Import, cranelift_signature)
            .expect("internal module error");

        Ok(Self { signature, id })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Internal {
    pub body: Vec<Spanned<parser::Statement>>,
    pub signature: MSignature,
    pub parameter_names: Vec<Spanned<parser::Ident>>,
    pub id: Option<FuncId>,
    pub generics: Vec<GenericArgument>,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
    pub fn new(
        function: parser::top_level::Function,
        declarations: &mut Declarations,
        generic_arguments: Vec<GenericArgument>,
        parameter_scope: ScopeId,
    ) -> Result<Self, SemanticError> {
        let scope = declarations.create_generic_scope(
            function.generic_parameters,
            &generic_arguments,
            parameter_scope,
        )?;

        let (parameter_names, parameter_types): (Vec<_>, Vec<_>) = function
            .parameters
            .into_iter()
            .map(|parameter| (parameter.value.0.name, parameter.value.0.r#type))
            .unzip();

        let parameter_types = &parameter_types
            .into_iter()
            .map(|r#type| r#type.ok_or(SemanticError::UntypedParameter))
            .collect::<Result<Vec<_>, _>>()?;
        let return_type = function
            .return_type
            .ok_or(SemanticError::MissingReturnType)?;

        let signature = MSignature::new(
            parameter_types,
            &return_type,
            declarations,
            function.name,
            scope,
            None,
        )?;

        let mut body = function.body;

        if let Some(Spanned {
            value: parser::Statement::Expression(expr),
            span,
        }) = body.last_mut()
        {
            if !matches!(expr, parser::Expression::Return(_)) {
                // TODO: `.clone()`
                *expr = parser::Expression::Return(Box::new(parser::expression::Return {
                    expression: expr.clone().spanned(span.clone()),
                }));
            }
        }

        Ok(Self {
            signature,
            parameter_names,
            body,
            id: None,
            generics: generic_arguments,
        })
    }

    /// # Panics
    /// if the function has not yet been declared
    pub(crate) fn compile(
        &self,
        declarations: &mut Declarations,
        cranelift_context: &mut CraneliftContext<impl Module>,
        function_compiler: &mut crate::FunctionCompiler,
    ) -> Result<(), SemanticError> {
        let mut builder = cranelift::prelude::FunctionBuilder::new(
            &mut cranelift_context.context.func,
            &mut cranelift_context.builder_context,
        );
        let signature = self
            .signature
            .clone()
            .signature
            .expect("signature not generated (function not declared)");

        builder.func.signature = signature;

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations
            .insert_layout_initialised(&self.signature.return_type)?
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
            .map(|(index, ((type_ref, name), value))| {
                let variable = Variable::new(index + SPECIAL_VARIABLES.len());
                // TODO: get type again?
                let layout = declarations.insert_layout_initialised(type_ref)?;
                let size = layout.size(declarations)?;

                let value = if layout.is_aggregate() {
                    value
                } else {
                    let stack_slot = builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        align_shift: 0,
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
                builder.declare_var(variable, declarations.isa.pointer_type());
                builder.def_var(variable, value);

                Ok((name.clone(), variable, type_ref.clone()))
            })
            .collect::<Result<_, SemanticError>>()?;

        let body = crate::hir::Builder::new(declarations, self, names).build_body()?;
        let id = self.id.expect("function not declared");

        let mut translator = Translator::new(
            builder,
            declarations,
            &mut cranelift_context.module,
            function_compiler,
            self.signature.scope,
        );

        for statement in body {
            if translator.statement(statement)? == BranchStatus::Finished {
                break;
            }
        }

        translator.finalize();

        cranelift_context
            .module
            .define_function(id, &mut cranelift_context.context)
            .unwrap_or_else(|error| panic!("internal cranelift error: '{error}'"));

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

        Ok(())
    }
}
