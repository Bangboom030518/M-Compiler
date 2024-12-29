use std::iter;

use crate::declarations::{Declarations, GenericArgument, ScopeId, TypeReference};
use crate::hir::{self, inferer};
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{CraneliftContext, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use isa::CallConv;
use tokenizer::{AsSpanned, Spanned};

#[derive(Debug, Clone, PartialEq)]
pub struct CallContext<'a> {
    pub arguments: &'a Vec<hir::Typed<hir::Expression>>,
    pub call_expression: hir::Typed<hir::Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MSignature {
    pub parameters: Vec<TypeReference>,
    pub return_type: TypeReference,
    pub signature: Signature,
    pub name: Spanned<parser::Ident>,
}

impl MSignature {
    fn new(
        parameters: &[Spanned<parser::Type>],
        return_type: &Spanned<parser::Type>,
        declarations: &mut Declarations,
        name: Spanned<parser::Ident>,
        call_context: Option<CallContext>,
        scope: ScopeId,
        module: &impl Module,
        call_conv: Option<CallConv>,
    ) -> Result<Self, SemanticError> {
        let mut signature = module.make_signature();
        if let Some(call_conv) = call_conv {
            signature.call_conv = call_conv;
        }

        let parameters = parameters
            .iter()
            .map(|parameter| declarations.lookup_type(&parameter.value, scope))
            .collect::<Result<Vec<_>, SemanticError>>()?;
        let return_type = declarations.lookup_type(&return_type.value, scope)?;
        if let Some(call_context) = call_context {
            for (parameter, argument) in iter::zip(&parameters, call_context.arguments) {
                dbg!();
                if let Some(type_ref) = &argument.type_ref {
                    // TODO: is it the right scope?
                    dbg!();
                    declarations.assert_equivalent(parameter, type_ref, scope, &argument.value)?;
                }
            }
            if let Some(type_ref) = &call_context.call_expression.type_ref {
                declarations.assert_equivalent(
                    &return_type,
                    type_ref,
                    scope,
                    &call_context.call_expression.value,
                )?;
            }
        }

        signature.params = parameters
            .iter()
            .map(|type_ref| -> Result<_, SemanticError> {
                let layout = declarations
                    .insert_layout(type_ref, scope)?
                    .unwrap_or_else(|| todo!("uninitialised layout"));

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

        signature.returns = match declarations
            .insert_layout(&return_type, scope)?
            .unwrap_or_else(|| todo!("uninitialised layout"))
        {
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

        Ok(Self {
            parameters,
            return_type,
            signature,
            name,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct External {
    pub symbol_name: Spanned<String>,
    pub signature: MSignature,
    pub id: FuncId,
}

impl External {
    pub fn new(
        function: parser::top_level::ExternFunction,
        declarations: &mut Declarations,
        scope_id: ScopeId,
        module: &mut impl Module,
    ) -> Result<Self, SemanticError> {
        let signature = MSignature::new(
            &function.parameters,
            &function.return_type,
            declarations,
            function.name,
            None,
            scope_id,
            module,
            Some(CallConv::for_libcall(
                module.isa().flags(),
                module.isa().default_call_conv(),
            )),
        )?;

        let id = module
            .declare_function(
                &function.symbol.value,
                Linkage::Import,
                &signature.signature,
            )
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
    pub body: Vec<Spanned<parser::Statement>>,
    pub scope_id: ScopeId,
    pub signature: MSignature,
    pub parameter_names: Vec<Spanned<parser::Ident>>,
    pub id: FuncId,
    pub generics: Vec<GenericArgument>,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
    // TODO: memcpy for structs
    pub fn new(
        function: parser::top_level::Function,
        declarations: &mut Declarations,
        module: &mut impl Module,
        generic_arguments: Vec<GenericArgument>,
        call_context: Option<CallContext>,
        parameter_scope: ScopeId,
        argument_scope: ScopeId,
    ) -> Result<Self, SemanticError> {
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

        let signature = MSignature::new(
            parameter_types,
            &function
                .return_type
                .ok_or(SemanticError::MissingReturnType)?,
            declarations,
            function.name,
            call_context,
            scope,
            module,
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

        let id = module
            .declare_function(
                signature.name.value.as_ref(),
                cranelift_module::Linkage::Export,
                &signature.signature,
            )
            .expect("Internal Module Error!");

        Ok(Self {
            signature,
            parameter_names,
            scope_id: scope,
            body,
            id,
            generics: generic_arguments,
        })
    }

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

        builder.func.signature = self.signature.signature.clone();

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        // TODO: `to_vec()`?
        let mut block_params = builder.block_params(entry_block).to_vec();

        if declarations
            .insert_layout_initialised(&self.signature.return_type, self.scope_id)?
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
                let layout = declarations.insert_layout_initialised(type_ref, self.scope_id)?;
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

        let func = crate::hir::Builder::new(declarations, self, names).build()?;
        let func = inferer::Inferer::function(
            func,
            declarations,
            &mut cranelift_context.module,
            self.scope_id,
        )?;

        let mut translator = Translator::new(
            builder,
            declarations,
            &mut cranelift_context.module,
            function_compiler,
            self.scope_id,
        );

        for statement in func.body {
            if translator.statement(statement)? == BranchStatus::Finished {
                break;
            }
        }

        translator.finalize();

        cranelift_context
            .module
            .define_function(self.id, &mut cranelift_context.context)
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
