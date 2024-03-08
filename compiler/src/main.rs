#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

use cranelift::prelude::*;
use cranelift_module::Module;
use layout::Layout;
use tokenizer::Spanned;
use std::collections::HashMap;
use std::sync::Arc;

mod declarations;
mod function;
mod hir;
mod layout;
mod translate;

pub struct CraneliftContext<M> {
    pub context: cranelift::codegen::Context,
    pub module: M,
    pub builder_context: cranelift::frontend::FunctionBuilderContext,
}

impl<M> CraneliftContext<M> {
    pub fn new(module: M) -> Self
    where
        M: Module,
    {
        Self {
            context: module.make_context(),
            module,
            builder_context: cranelift::frontend::FunctionBuilderContext::new(),
        }
    }
}

// TODO: annotated results
#[derive(Clone, Debug, PartialEq, thiserror::Error)]
pub enum SemanticError {
    #[error("Number literal used as non-number")]
    UnexpectedNumberLiteral,
    #[error("Integer literal too thicc, phatt and chonky")]
    IntegerLiteralTooBig(#[from] std::num::TryFromIntError),
    #[error("Type resolution failed to infer the type")]
    UnknownType(hir::Expression),
    #[error("Attempt to assign incorrect type to a variable")]
    InvalidAssignment,
    #[error("Declaration not found: '{0}'")]
    DeclarationNotFound(Spanned<parser::Ident>),
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
    MismatchedTypes {
        expected: Layout,
        found: Layout,
        expression: hir::Expression,
    },
    #[error("Tried to construct something other than a struct")]
    InvalidConstructor,
    #[error("Missing a struct field that must be specified")]
    MissingStructField,
    #[error("Was stoopid and tried to access the field of a non-struct type")]
    InvalidFieldAccess(Layout),
    #[error("Tried to access a non-existent struct field")]
    NonExistentField,
    #[error("Tried to initialise non-reference type as a reference")]
    InvalidMutRef,
    #[error(
        "Actions have consequences! You used an intrinsic wrong and now you're on your own :)"
    )]
    InvalidIntrinsic,
}

fn main() {
    #[cfg(debug_assertions)]
    {
        std::fs::write("function-ir.clif", "").unwrap();
    }
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
    let declarations = declarations::Declarations::new(&file, &isa, &mut module).unwrap();

    let mut context = CraneliftContext::new(module);

    let mut functions = HashMap::new();
    for declaration in declarations.declarations.iter().flatten() {
        let declarations::Declaration::Function(declarations::Function::Internal(function)) =
            declaration
        else {
            continue;
        };

        let name = function.signature.name.value.to_string();
        let id = function.compile(&declarations, &mut context).expect("TODO");

        functions.insert(name, id);
    }

    context.module.finalize_definitions().unwrap();
    let function = *functions.get("main").unwrap();

    let code = context.module.get_finalized_function(function);

    let main = unsafe { std::mem::transmute::<*const u8, unsafe fn() -> u64>(code) };
    unsafe { main() };
}
