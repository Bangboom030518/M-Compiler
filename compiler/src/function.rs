use std::iter;

use crate::declarations::{Declarations, GenericArgument, ScopeId, TypeReference};
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{hir, CraneliftContext, SemanticError};
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
    pub linkage: Linkage,
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
        linkage: Linkage,
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
            linkage,
            scope,
            signature: None,
        })
    }
    fn cranelift_signature(
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
                let layout = declarations.insert_layout_initialised(type_ref, self.scope)?;

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
        let return_type = declarations.insert_layout_initialised(&self.return_type, self.scope)?;

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

    pub fn declare(
        &mut self,
        module: &mut impl Module,
        declarations: &mut Declarations,
    ) -> Result<FuncId, SemanticError> {
        let signature = &self.cranelift_signature(module, declarations)?;
        let id = module
            .declare_function(&self.name.value.0, self.linkage, signature)
            .expect("internal module error");
        Ok(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct External {
    pub symbol_name: Spanned<String>,
    pub signature: MSignature,
    pub id: Option<FuncId>,
    pub call_conv: CallConv,
    pub scope: ScopeId,
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
        let signature = MSignature::new(
            &function.parameters,
            &function.return_type,
            declarations,
            function.name,
            scope,
            Some(call_conv),
            Linkage::Import,
        )?;

        Ok(Self {
            symbol_name: function.symbol,
            signature,
            id: None,
            call_conv,
            scope,
        })
    }

    pub fn id(
        &mut self,
        module: &mut impl Module,
        declarations: &mut Declarations,
    ) -> Result<FuncId, SemanticError> {
        if let Some(id) = self.id {
            return Ok(id);
        }
        let signature = &self.signature.cranelift_signature(module, declarations)?;
        let id = module
            .declare_function(&self.symbol_name.value, Linkage::Import, signature)
            .expect("internal module error");
        self.id = Some(id);
        Ok(id)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Internal {
    pub body: Vec<Spanned<parser::Statement>>,
    pub scope: ScopeId,
    pub signature: MSignature,
    pub parameter_names: Vec<Spanned<parser::Ident>>,
    pub id: Option<FuncId>,
    pub generics: Vec<GenericArgument>,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
    // TODO: memcpy for structs
    pub fn new(
        function: parser::top_level::Function,
        declarations: &mut Declarations,
        generic_arguments: Vec<GenericArgument>,
        parameter_scope: ScopeId,
        argument_scope: ScopeId,
    ) -> Result<Self, SemanticError> {
        dbg!(&function.name);
        let scope = declarations.create_generic_scope(
            function.generic_parameters,
            &generic_arguments,
            parameter_scope,
            argument_scope,
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
            Linkage::Export,
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
            scope,
            body,
            id: None,
            generics: generic_arguments,
        })
    }

    fn id(
        &mut self,
        module: &mut impl Module,
        declarations: &mut Declarations,
    ) -> Result<FuncId, SemanticError> {
        if let Some(id) = self.id {
            return Ok(id);
        }
        let id = self.signature.declare(module, declarations)?;
        self.id = Some(id);
        Ok(id)
    }

    pub fn compile(
        &mut self,
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
            .cranelift_signature(&cranelift_context.module, declarations)
            .expect("signature not generated");

        builder.func.signature = signature;

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations
            .insert_layout_initialised(&self.signature.return_type, self.scope)?
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
                let layout = declarations.insert_layout_initialised(type_ref, self.scope)?;
                let size = layout.size(&declarations.isa);

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

        let func =
            crate::hir::Builder::new(declarations, self, names, &mut cranelift_context.module)
                .build()?;
        let id = self.id(&mut cranelift_context.module, declarations)?;

        let mut translator = Translator::new(
            builder,
            declarations,
            &mut cranelift_context.module,
            function_compiler,
            self.scope,
        );

        for statement in func.body {
            if translator.statement(statement)? == BranchStatus::Finished {
                break;
            }
        }

        translator.finalize();

        cranelift_context
            .module
            .define_function(id, &mut cranelift_context.context)
            .unwrap_or_else(|error: cranelift_module::ModuleError| {
                todo!("handle me properly: {error:?}")
            });

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
