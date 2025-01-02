#![warn(clippy::pedantic, clippy::nursery)]
#![feature(iter_collect_into)]

use cranelift::prelude::*;
use cranelift_module::Module;
use declarations::{ConcreteFunction, Declarations, FuncReference};
use layout::Layout;
use std::collections::HashSet;
use std::io::Write;
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
    #[error("Actions have consequences! You used an intrinsic wrong and now you're on your own.")]
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
    #[error("Type resolution couldn't figure out type")]
    UninitialisedType,
    #[error("A struct was created that was so violently overweight that its field offset exceeded 2^32-1 (`i32::MAX`). That's one thicc boi.")]
    StructTooChonky,
    #[error("Expected a bool, but found something else. Your guess is as good as mine as to what that is.")]
    ExpectedBool,
    #[error("Expected a type to be a struct based on usage")]
    ExpectedStruct,
}

struct FunctionCompiler {
    to_compile: Vec<FuncReference>,
    compiled: HashSet<FuncReference>,
}

impl FunctionCompiler {
    fn new(entry_function: FuncReference) -> Self {
        Self {
            to_compile: vec![entry_function],
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

            internal.compile(declarations, context, self)?;

            declarations
                .concrete_functions
                .insert(func_ref.clone(), ConcreteFunction::Internal(internal));

            self.compiled.insert(func_ref);
        }

        Ok(())
    }
}

fn main() {
    println!("Beginning compilation...");
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
                ParseError::UnexpectedIdentifier { expected, found } => {
                    todo!("unexpected ident; expected '{expected:?}', found '{found:?}")
                }
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
        panic!("host machine is not supported: {msg}");
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

    extern "C" fn print_float(f: f32) {
        println!("{f}");
    }

    unsafe extern "C" fn print_str(str_ptr: *const u8, length: usize) {
        let _ = std::io::stdout()
            .lock()
            .write(unsafe { std::slice::from_raw_parts(str_ptr, length) });
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

    unsafe extern "C" fn copy_rs(src: *const u8, dst: *mut u8, count: usize) {
        std::ptr::copy(src, dst, count);
    }

    builder.symbol("print_int", print_int as *const u8);
    builder.symbol("print_float", print_float as *const u8);
    builder.symbol("alloc_rs", alloc_rs as *const u8);
    builder.symbol("dealloc_rs", dealloc_rs as *const u8);
    builder.symbol("copy_rs", copy_rs as *const u8);
    builder.symbol("print_str", print_str as *const u8);

    let mut module = cranelift_jit::JITModule::new(builder);
    let mut declarations = declarations::Declarations::new(declarations, &isa, &mut module)
        .unwrap_or_else(|error| panic!("{error}"));

    let mut context = CraneliftContext::new(module);
    let main_ref = declarations
        .lookup("main", declarations::TOP_LEVEL_SCOPE)
        .expect("no main function");
    let main_ref = declarations::FuncReference {
        id: main_ref,
        generics: Vec::new(),
    };

    let main_func_id = declarations
        .declare_function(
            main_ref.clone(),
            declarations::TOP_LEVEL_SCOPE,
            &mut context.module,
        )
        .unwrap();

    FunctionCompiler::new(main_ref)
        .compile_all(&mut declarations, &mut context)
        .unwrap_or_else(|error| panic!("{error}"));

    context.module.finalize_definitions().unwrap();

    let code = context.module.get_finalized_function(main_func_id);
    println!("Compilation finished! Running compiled function...");
    let main = unsafe { std::mem::transmute::<*const u8, unsafe fn() -> *const u8>(code) };
    unsafe { main() };
}
