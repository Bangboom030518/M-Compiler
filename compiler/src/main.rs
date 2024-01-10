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
    let isa = isa_builder
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

        let top_level_resolution::Declaration::Function(function) = declaration else {
            continue;
        };

        // TODO: `.clone()s`
        let id = local::compile_function(
            &declarations,
            scope,
            &mut context,
            &mut function_builder_context,
            &mut module,
            function.clone(),
        )
        .expect("TODO");

        functions.insert(function.name.clone(), id);
    }
    module.finalize_definitions().unwrap();

    let code = module.get_finalized_function(*functions.get("add").unwrap());
    let add =
        unsafe { std::mem::transmute::<*const u8, unsafe extern "C" fn(i64, i64) -> i64>(code) };
    dbg!(unsafe { add(2, 2) });
}
