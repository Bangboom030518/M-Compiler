use crate::declarations::{Declarations, Reference, ScopeId};
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{errors, CraneliftContext, Error};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Module};
use isa::CallConv;
use parser::PrimitiveKind;
use std::hash::{Hash, Hasher};
use std::sync::OnceLock;
use tokenizer::{AsSpanned, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MSignature {
    pub parameters: Vec<Reference>,
    pub return_type: Reference,
    pub name: Spanned<parser::Ident>,
    pub symbol: String,
    pub scope: ScopeId,
    linkage: cranelift_module::Linkage,
    call_conv: Option<CallConv>,
    cranelift_declaration: OnceLock<(Signature, FuncId)>,
}

impl MSignature {
    fn new(
        parameters: &[Spanned<parser::Type>],
        return_type: &Spanned<parser::Type>,
        declarations: &mut Declarations,
        name: Spanned<parser::Ident>,
        symbol: String,
        scope: ScopeId,
        call_conv: Option<CallConv>,
        linkage: cranelift_module::Linkage,
    ) -> Result<Self, Error> {
        let parameters = parameters
            .iter()
            .map(|parameter| declarations.unresolved.lookup_type(&parameter.value, scope))
            .collect::<Result<Vec<_>, Error>>()?;
        let return_type = declarations
            .unresolved
            .lookup_type(&return_type.value, scope)?;

        Ok(Self {
            parameters,
            return_type,
            name,
            call_conv,
            scope,
            symbol,
            cranelift_declaration: OnceLock::new(),
            linkage,
        })
    }

    pub fn cranelift_declaration(
        &self,
        module: &mut impl Module,
        declarations: &mut Declarations,
    ) -> Result<&(Signature, FuncId), Error> {
        self.cranelift_declaration.get_or_try_init(|| {
            let mut signature = module.make_signature();
            if let Some(call_conv) = self.call_conv {
                signature.call_conv = call_conv;
            }

            signature.params = self
                .parameters
                .iter()
                .filter_map(|type_ref| {
                    let layout = match declarations.insert_layout_initialised(type_ref) {
                        Ok(layout) => layout,
                        Err(err) => return Some(Err(err)),
                    };

                    match layout {
                        Layout::Primitive(PrimitiveKind::Void) => None,
                        primitive @ Layout::Primitive(_) => Some(Ok(AbiParam::new(
                            primitive.cranelift_type(&declarations.isa),
                        ))),
                        Layout::Struct(_) | Layout::Array(_) => {
                            Some(Ok(AbiParam::new(declarations.isa.pointer_type())))
                        }
                    }
                })
                .collect::<Result<_, _>>()?;
            let return_type = declarations.insert_layout_initialised(&self.return_type)?;

            signature.returns = match return_type {
                Layout::Primitive(PrimitiveKind::Void) => Vec::new(),
                primitive @ Layout::Primitive(_) => {
                    vec![AbiParam::new(primitive.cranelift_type(&declarations.isa))]
                }
                Layout::Struct(_) | Layout::Array(_) => {
                    signature
                        .params
                        .push(AbiParam::new(declarations.isa.pointer_type()));

                    vec![AbiParam::new(declarations.isa.pointer_type())]
                }
            };
            let id = module
                .declare_function(&self.symbol, self.linkage, &signature)
                .unwrap_or_else(|err| panic!("internal module error: {err}"));

            Ok((signature, id))
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct External {
    pub signature: MSignature,
}

impl External {
    pub fn new(
        function: parser::top_level::ExternFunction,
        declarations: &mut Declarations,
        scope: ScopeId,
    ) -> Result<Self, Error> {
        let call_conv = CallConv::for_libcall(
            declarations.isa.flags(),
            declarations.isa.default_call_conv(),
        );

        let signature = MSignature::new(
            &function.parameters,
            &function.return_type,
            declarations,
            function.name,
            function.symbol.value,
            scope,
            Some(call_conv),
            cranelift_module::Linkage::Import,
        )?;

        Ok(Self { signature })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Internal {
    pub signature: MSignature,
    pub parameter_names: Vec<Spanned<parser::Ident>>,
    pub body: Vec<Spanned<parser::Statement>>,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
    pub fn new(
        function: parser::top_level::Function,
        declarations: &mut Declarations,
        scope: ScopeId,
        generic_arguments: Vec<Reference>,
    ) -> Result<Self, Error> {
        let symbol = if function.generic_parameters.value.generics.is_empty() {
            function.name.value.0.clone()
        } else {
            let mut hasher = std::hash::DefaultHasher::new();
            generic_arguments.hash(&mut hasher);
            format!("{}[{}]", function.name.value.0, hasher.finish())
        };

        let scope = declarations.unresolved.create_generic_scope(
            function.generic_parameters,
            generic_arguments,
            scope,
        )?;

        let (parameter_names, parameter_types): (Vec<_>, Vec<_>) = function
            .parameters
            .into_iter()
            .map(|parameter| (parameter.value.0.name, parameter.value.0.r#type))
            .unzip();

        let parameter_types = &parameter_types
            .into_iter()
            .map(|r#type| {
                Ok(r#type
                    .value
                    .ok_or_else(|| Error {
                        span: r#type.span.clone(),
                        kind: errors::Kind::MissingParameterType,
                    })?
                    .spanned(r#type.span))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let return_type = function
            .return_type
            .value
            .ok_or_else(|| Error {
                span: function.return_type.span.clone(),
                kind: errors::Kind::MissingReturnType,
            })?
            .spanned(function.return_type.span);

        let signature = MSignature::new(
            parameter_types,
            &return_type,
            declarations,
            function.name,
            symbol,
            scope,
            None,
            cranelift_module::Linkage::Export,
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
        })
    }

    /// # Panics
    /// if the function has not yet been declared
    pub(crate) fn compile(
        &self,
        declarations: &mut Declarations,
        cranelift_context: &mut CraneliftContext<impl Module>,
        function_compiler: &mut crate::FunctionCompiler,
    ) -> Result<(), Error> {
        let mut builder = cranelift::prelude::FunctionBuilder::new(
            &mut cranelift_context.context.func,
            &mut cranelift_context.builder_context,
        );

        let (signature, id) = self
            .signature
            .cranelift_declaration
            .get()
            .expect("signature not generated (function not declared)");

        builder.func.signature = signature.clone();

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
        }

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
            .collect::<Result<_, Error>>()?;

        let body = crate::hir::Builder::new(declarations, self, names).build_body()?;

        let mut translator = Translator::new(
            builder,
            declarations,
            &mut cranelift_context.module,
            function_compiler,
        );

        for statement in body {
            if translator.statement(statement)? == BranchStatus::Finished {
                break;
            }
        }

        translator.finalize();

        cranelift_context
            .module
            .define_function(*id, &mut cranelift_context.context)
            .unwrap_or_else(|error| {
                panic!(
                    "internal cranelift error in '{}': '{error}'",
                    self.signature.symbol.clone()
                )
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
