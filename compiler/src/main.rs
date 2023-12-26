#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

mod local;
mod top_level_resolution;

use cranelift::codegen::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::Module;
use parser::top_level::{DeclarationKind, Parameter, TypeBinding};
use parser::Expression;
use std::collections::HashMap;
use std::sync::Arc;

// TODO: annotated results
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum SemanticError {
    #[error("Integer literal used as non-integer")]
    UnexpectedIntegerLiteral,
    #[error("Integer literal too thicc and chonky")]
    IntegerLiteralTooBig(#[from] std::num::TryFromIntError),
    #[error("Type resolution failed to infer the type")]
    UnknownType,
    #[error("Attempt to assign incorrect type to a variable")]
    InvalidAssignment,
    #[error("Declaration not found")]
    DeclarationNotFound,
    #[error("Expected a type, found a function")]
    FunctionUsedAsType,
    #[error("Expected a function, found a type")]
    TypeUsedAsFunction,
    #[error("Couldn't infer type of parameter")]
    UntypedParameter,
    #[error("Couldn't infer type of return")]
    MissingReturnType,
}

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).expect("Parse error! :(");
    let root = file.root;
    let mut declarations = top_level_resolution::TopLevelDeclarations {
        declarations: Vec::new(),
        scopes: HashMap::new(),
        file_cache: file.cache,
    };

    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
        panic!("host machine is not supported: {}", msg);
    });
    let isa: Arc<dyn TargetIsa>: Arc<dyn TargetIsa> = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    let builder = cranelift_jit::JITBuilder::with_isa(
        Arc::clone(&isa),
        cranelift_module::default_libcall_names(),
    );

    let mut module = cranelift_jit::JITModule::new(builder);
    let mut context = module.make_context();
    let mut function_builder_context = FunctionBuilderContext::new();

    top_level_resolution::TopLevelScope::append_new(
        &mut declarations,
        file.root,
        isa.default_call_conv(),
    )
    .unwrap();

    let mut functions = HashMap::new();
    for declaration in declarations.declarations.iter().flatten() {
        // TODO: not root
        let scope = root;

        let top_level_resolution::Declaration::Function(mut function) = declaration else {
            continue;
        };

        // TODO: VVV
        // if let Some(return_statement) = function.body.last_mut() {
        //     if let parser::Statement::Expression(expression) = return_statement {
        //         *return_statement =
        //             parser::Statement::Expression(Expression::Return(Box::new(expression.clone())));
        //     }
        // };

        // let parameters: Vec<(parser::prelude::Ident, top_level_resolution::Id)> = function
        //     .parameters
        //     .into_iter()
        //     .map(
        //         |parser::top_level::Parameter(parser::top_level::TypeBinding { r#type, name })| {
        //             let r#type = r#type
        //                 .and_then(|r#type| {
        //                     let parser::Type::Identifier(ident) = r#type;
        //                     declarations.lookup(&ident, scope)
        //                 })
        //                 .unwrap_or_else(|| todo!("semantic error!"));
        //             (name, r#type)
        //         },
        //     )
        //     .collect();

        let function_builder = local::FunctionBuilder::new(
            &declarations,
            root,
            parameters,
            declarations
                .lookup(
                    match function.return_type.as_ref().unwrap() {
                        parser::Type::Identifier(ident) => ident,
                    },
                    scope,
                )
                .unwrap(),
            &mut function_builder_context,
            &mut context.func,
            function,
        )
        .unwrap_or_else(|error| todo!("handle me! {error}"));

        function_builder
            .compile(&function.body)
            .unwrap_or_else(|error| todo!("handle me! {error}"));

        std::fs::write("function-ir.clif", context.func.display().to_string()).unwrap();

        let id = module
            .declare_function(
                name.as_ref(),
                cranelift_module::Linkage::Export,
                &context.func.signature,
            )
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        module
            .define_function(id, &mut context)
            .unwrap_or_else(|error| todo!("handle me properly: {error:?}"));

        module.clear_context(&mut context);

        functions.insert(name.0, id);
    }
    module.finalize_definitions().unwrap();

    let code = module.get_finalized_function(*functions.get("add").unwrap());
    let add =
        unsafe { std::mem::transmute::<*const u8, unsafe extern "C" fn(i64, i64) -> i64>(code) };
    dbg!(unsafe { add(2, 2) });
}
