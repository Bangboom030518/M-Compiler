#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

mod local;
mod top_level_resolution;

use cranelift::prelude::*;
use std::collections::HashMap;
use std::sync::Arc;

// TODO: annotated results
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum SemanticError {
    #[error("Number literal used as non-number")]
    UnexpectedNumberLiteral,
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
    #[error("Incorrect function arity was assumed")]
    InvalidNumberOfArguments,
    #[error("Mismatched types")]
    MismatchedTypes,
    #[error("Tried to construct something other than a struct")]
    InvalidConstructor,
    #[error("Missing a struct field that must be specified")]
    MissingStructField,
}

fn main() {
    let file = parser::parse_file(include_str!("../../input.m")).expect("Parse error! :(");

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

    let declarations =
        top_level_resolution::TopLevelDeclarations::new(file, isa, &mut module)
            .unwrap();

    let mut context = top_level_resolution::CraneliftContext::new(module);

    let mut functions = HashMap::new();
    for declaration in declarations.declarations.iter().flatten() {
        let top_level_resolution::Declaration::Function(function) = declaration else {
            continue;
        };

        let id = function
            .clone()
            .compile(&declarations, &mut context)
            .expect("TODO");

        functions.insert(function.name.to_string(), id);
    }

    context.module.finalize_definitions().unwrap();

    let code = context
        .module
        .get_finalized_function(*functions.get("fib").unwrap());

    let fib = unsafe { std::mem::transmute::<*const u8, unsafe fn(i64) -> i64>(code) };
    dbg!(unsafe { fib(11) });
}
