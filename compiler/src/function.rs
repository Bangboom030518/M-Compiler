use crate::declarations::{Declaration, Declarations, GenericArgument, ScopeId, TypeReference};
use crate::hir::inferer;
use crate::layout::Layout;
use crate::translate::{BranchStatus, Translator};
use crate::{CraneliftContext, SemanticError};
use cranelift::codegen::ir::immediates::Offset32;
use cranelift::prelude::*;
use cranelift_module::{FuncId, Linkage, Module};
use tokenizer::{AsSpanned, Spanned};

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
        scope: ScopeId,
        module: &impl Module,
        generics: &[GenericArgument]
    ) -> Result<Self, SemanticError> {
        let mut signature = module.make_signature();
        let parameters = parameters
            .iter()
            .map(|r#type| declarations.lookup_type(&r#type.value, scope))
            .collect::<Result<Vec<_>, SemanticError>>()?;

        signature.params = parameters
            .iter()
            .map(|type_ref| -> Result<_, SemanticError> {
                let layout = declarations.insert_layout(type_ref, generics)?;
                match layout {
                    Layout::Primitive(primitive) => Ok(AbiParam::new(
                        primitive.cranelift_type(declarations.isa.pointer_type()),
                    )),
                    Layout::Struct(_) | Layout::Array(_) => {
                        Ok(AbiParam::new(declarations.isa.pointer_type()))
                    }
                }
            })
            .collect::<Result<_, _>>()?;

        let return_type = declarations.lookup_type(&return_type.value, scope)?;

        signature.returns = vec![match declarations.insert_layout(&return_type, generics)? {
            Layout::Primitive(primitive) => {
                AbiParam::new(primitive.cranelift_type(declarations.isa.pointer_type()))
            }
            Layout::Struct(_) | Layout::Array(_) => {
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
            scope_id,
            module,
            &[]
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
    // pub generic_arguments: Vec<GenericArgument>,
}

pub const AGGREGATE_PARAM_VARIABLE: usize = 0;
pub const SPECIAL_VARIABLES: &[usize] = &[AGGREGATE_PARAM_VARIABLE];

impl Internal {
    // TODO: memcpy for structs
    pub fn new(
        function: parser::top_level::Function,
        declarations: &mut Declarations,
        scope: ScopeId,
        module: &mut impl Module,
        generics: Vec<GenericArgument>,
    ) -> Result<Self, SemanticError> {
        // TODO: check these!
        // let generics = std::iter::zip(function.generics.clone().value.generics, generic_arguments)
        //     .map(|(ident, argument)| (ident.value.ident().value.0, argument))
        //     .collect::<HashMap<_, _>>();

        let scope = declarations.resolve_generic_parameters(function.generics, scope);

        // let generic_arguments2 = generic_arguments
        //     .iter()
        //     .enumerate()
        //     .map(|(index, (ident, argument))| {
        //         let argument = match argument {
        //             GenericArgument::Length(_) => Declaration::LengthGeneric(index),
        //             GenericArgument::Type(_) => Declaration::TypeGeneric(index),
        //         };
        //         (ident.clone(), declarations.create(argument))
        //     })
        //     .collect();

        // let scope = declarations.create_scope(crate::declarations::TopLevelScope {
        //     declarations: generic_arguments2,
        //     parent: Some(scope),
        // });

        let parameters: (Vec<_>, Vec<_>) = function
            .parameters
            .into_iter()
            .map(|parameter| (parameter.value.0.name, parameter.value.0.r#type))
            .unzip();

        let signature = MSignature::new(
            &parameters
                .1
                .into_iter()
                .map(|r#type| r#type.ok_or(SemanticError::UntypedParameter))
                .collect::<Result<Vec<_>, _>>()?,
            &function
                .return_type
                .ok_or(SemanticError::MissingReturnType)?,
            declarations,
            function.name,
            scope,
            module,
            &generics
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
            parameter_names: parameters.0,
            scope_id: scope,
            body,
            id,
            generics
            // generic_arguments,
        })
    }

    pub fn compile(
        &self,
        declarations: &mut Declarations,
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
            .insert_layout(&self.signature.return_type, &self.generics)?
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
                let layout = declarations.insert_layout(type_ref, &self.generics)?;
                let size = layout.size(&declarations.isa);

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

                builder.declare_var(variable, declarations.isa.pointer_type());
                builder.def_var(variable, value);

                Ok((name.clone(), variable, type_ref.clone()))
            })
            .collect::<Result<_, SemanticError>>()?;

        let mut func = crate::hir::Builder::new(declarations, self, names).build()?;
        inferer::Inferer::function(&mut func, declarations, &mut cranelift_context.module, &self.generics)?;

        let mut translator = Translator::new(builder, declarations, &mut cranelift_context.module, &self.generics);

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
