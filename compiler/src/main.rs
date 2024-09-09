#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

// TODO: voidz

use core::alloc;
use core::arch::global_asm;
use cranelift::prelude::*;
use cranelift_module::Module;
use declarations::{ConcreteFunction, Declarations, FuncReference};
use layout::Layout;
use std::collections::HashSet;
use std::sync::Arc;
use tokenizer::Spanned;

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
    #[error("Declaration not found: '{}'", 0.0)]
    DeclarationNotFound(Spanned<parser::Ident>),
    #[error("Expected a type")]
    InvalidType,
    #[error("Expected a function")]
    InvalidFunction,
    #[error("Couldn't infer type of parameter")]
    UntypedParameter,
    #[error("Couldn't infer type of return")]
    MissingReturnType,
    #[error("Incorrect function arity was assumed")]
    InvalidNumberOfArguments,
    #[error("Mismatched types.\nexpected '{expected:?}',\nfound    '{found:?}'.")]
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
    InvalidAddr {
        found: Layout,
        expression: hir::Expression,
    },
    #[error(
        "Actions have consequences! You used an intrinsic wrong and now you're on your own :)"
    )]
    InvalidIntrinsic,
    #[error("Used a string where a byte array wasn't expected (javascript developer 🤨)")]
    InvalidStringConst { expected: Layout },
    #[error("Invalid length generic")]
    InvalidLengthGeneric,
    #[error("Invalid type generic")]
    InvalidTypeGeneric,
    #[error("The wrong number of generics were passed to a function. Figure the rest out :)")]
    GenericParametersMismatch,
    #[error("Tried to put generics somewhere they don't belong.")]
    UnexpectedGenerics,
}

struct FunctionCompiler {
    to_compile: Vec<FuncReference>,
    compiled: HashSet<FuncReference>,
}

impl FunctionCompiler {
    fn new(id: declarations::Id) -> Self {
        Self {
            to_compile: vec![FuncReference {
                id,
                generics: Vec::new(),
            }],
            compiled: HashSet::new(),
        }
    }

    fn push(&mut self, func: FuncReference) {
        self.to_compile.push(func);
    }

    fn compile_all(
        &mut self,
        declarations: &mut Declarations,
        context: &mut CraneliftContext<impl Module>,
    ) -> Result<(), SemanticError> {
        loop {
            let Some(func_ref) = self.to_compile.pop() else {
                break;
            };

            if self.compiled.contains(&func_ref) {
                continue;
            }
            let function = declarations
                .concrete_functions
                .remove(&func_ref)
                .expect("function not found");

            let ConcreteFunction::Internal(internal) = function else {
                declarations.concrete_functions.insert(func_ref, function);
                continue;
            };
            // TODO: `.clone()`
            internal.clone().compile(declarations, context, self)?;

            declarations
                .concrete_functions
                .insert(func_ref.clone(), ConcreteFunction::Internal(internal));

            self.compiled.insert(func_ref);
        }

        Ok(())
    }
}

fn main() {
    #[cfg(debug_assertions)]
    {
        std::fs::write("function-ir.clif", "").unwrap();
    }
    let input = include_str!("../../input.m");
    let declarations = match parser::parse_file(input) {
        Ok(x) => x,
        Err(error) => {
            use parser::ParseError;
            match error {
                ParseError::UnexpectedIdentifier { expected, found } => todo!("unexpected ident"),
                ParseError::UnexpectedToken { expected, found } => panic!(
                    "expected: {expected:?}, found: {:?} in '{}'",
                    found.value,
                    input
                        .get((found.span.start - 5)..(found.span.end + 5))
                        .unwrap()
                ),
            }
        }
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

    let mut builder = cranelift_jit::JITBuilder::with_isa(
        Arc::clone(&isa),
        cranelift_module::default_libcall_names(),
    );

    extern "C" fn print_int(n: usize) -> usize {
        println!("{n}");
        0
    }

    unsafe extern "C" fn alloc_rs(size: usize) -> *mut u8 {
        use std::alloc::{alloc, handle_alloc_error, Layout};
        let layout = Layout::from_size_align(size, 4).unwrap();
        let ptr = unsafe { alloc(layout) };
        if ptr.is_null() {
            handle_alloc_error(layout);
        }
        ptr
    }

    unsafe extern "C" fn dealloc_rs(ptr: *mut u8, size: usize) -> usize {
        use std::alloc::{dealloc, Layout};
        unsafe { dealloc(ptr, Layout::from_size_align(size, 4).unwrap()) };
        0
    }

    builder.symbol("print_int", print_int as *const u8);
    builder.symbol("alloc_rs", alloc_rs as *const u8);
    builder.symbol("dealloc_rs", dealloc_rs as *const u8);

    let mut module = cranelift_jit::JITModule::new(builder);
    let mut declarations = match declarations::Declarations::new(declarations, &isa, &mut module) {
        Ok(declarations) => declarations,
        Err(error) => match error {
            SemanticError::DeclarationNotFound(ident) => {
                panic!(
                    "ident not found '{}': '{}'",
                    ident.value,
                    input
                        .get((ident.span.start - 5)..(ident.span.end + 5))
                        .unwrap()
                )
            }
            error => todo!("handle meee: {error}"),
        },
    };

    let mut context = CraneliftContext::new(module);
    let main_id = declarations
        .lookup("main", declarations::TOP_LEVEL_SCOPE)
        .expect("no main!");

    let ConcreteFunction::Internal(main) = declarations
        .insert_function(
            declarations::FuncReference {
                id: main_id,
                generics: Vec::new(),
            },
            &mut context.module,
            declarations::TOP_LEVEL_SCOPE,
        )
        .expect("🎉 uh oh!")
    else {
        panic!("main should not be extern")
    };

    // let function_compiler = FunctionCompiler::new(main_id);

    match FunctionCompiler::new(main_id).compile_all(&mut declarations, &mut context) {
        Ok(()) => {}
        Err(error) => match error {
            SemanticError::DeclarationNotFound(ident) => {
                panic!(
                    "ident not found '{}': '{}'",
                    ident.value,
                    input
                        .get((ident.span.start - 5)..(ident.span.end + 5))
                        .unwrap()
                )
            }
            error => todo!("handle meee: {error}"),
        },
    }

    // let main_function = declarations
    //     .concrete_functions
    //     .values()
    //     .find(|function| &function.signature().name.value.0 == "main")
    //     .expect("no main function!");

    // let mut functions = HashMap::new();
    // for declaration in declarations.declarations.clone().into_iter().flatten() {
    //     let declarations::Declaration::Function(declarations::GenericFunction::Internal {..}) =
    //         declaration
    //     else {
    //         continue;
    //     };

    //     let name = function.signature.name.value.to_string();
    //     let id = function
    //         .compile(&mut declarations, &mut context)
    //         .expect("TODO");

    //     functions.insert(name, id);
    // }

    context.module.finalize_definitions().unwrap();
    let function = main.id;

    let code = context.module.get_finalized_function(function);
    println!("Compilation finished! Running compiled function...");
    let main = unsafe { std::mem::transmute::<*const u8, unsafe fn() -> *const u8>(code) };
    unsafe { main() };
}
