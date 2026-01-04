#![warn(clippy::pedantic, clippy::nursery, clippy::todo, clippy::dbg_macro)]
#![feature(once_cell_try)]

use cranelift::prelude::*;
use cranelift_module::Module;
use declarations::{Declarations, Function, Reference};
pub use errors::Error;
use std::collections::HashSet;
use std::io::Write;
use std::sync::Arc;
use tokenizer::AsSpanned;

mod declarations;
mod errors;
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

struct FunctionCompiler {
    to_compile: Vec<Reference>,
    compiled: HashSet<Reference>,
}

impl FunctionCompiler {
    fn new(entry_function: Reference) -> Self {
        Self {
            to_compile: vec![entry_function],
            compiled: HashSet::new(),
        }
    }

    fn push(&mut self, func: Reference) {
        self.to_compile.push(func);
    }

    fn compile_all(
        &mut self,
        declarations: &mut Declarations,
        context: &mut CraneliftContext<impl Module>,
    ) -> Result<(), Error> {
        loop {
            let Some(func_ref) = self.to_compile.pop() else {
                break;
            };

            let func_ref = declarations.unresolved.resolve(&func_ref);
            if self.compiled.contains(&func_ref) {
                continue;
            }

            let function = declarations
                .get_function(&func_ref)
                .expect("function not found");

            let function = Arc::clone(function);

            let Function::Internal(internal) = function.as_ref() else {
                continue;
            };

            internal.compile(declarations, context, self)?;

            self.compiled.insert(func_ref);
        }

        Ok(())
    }
}

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

fn main() {
    println!("Beginning compilation...");
    #[cfg(debug_assertions)]
    {
        std::fs::write("function-ir.clif", "").unwrap();
    }
    let filename = "input.m";
    let input = include_str!("../../examples/input.m");
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

    builder.symbol("print_int", print_int as *const u8);
    builder.symbol("print_float", print_float as *const u8);
    builder.symbol("alloc_rs", alloc_rs as *const u8);
    builder.symbol("dealloc_rs", dealloc_rs as *const u8);
    builder.symbol("copy_rs", copy_rs as *const u8);
    builder.symbol("print_str", print_str as *const u8);

    let module = cranelift_jit::JITModule::new(builder);
    let mut declarations = declarations::Declarations::new(declarations, &isa);

    let mut context = CraneliftContext::new(module);
    let main_ref = parser::Ident("main".to_string()).spanned(0..0);
    let main_ref = declarations
        .unresolved
        .lookup(&main_ref, declarations::TOP_LEVEL_SCOPE)
        .expect("no main function");
    let main_ref = declarations::Reference {
        id: main_ref,
        generics: Vec::new(),
    };

    declarations
        .insert_function(&main_ref)
        .expect("main didn't work");

    let function = declarations
        .get_function(&main_ref)
        .expect("function not inserted");

    let main_func_id = Arc::clone(function)
        .signature()
        .cranelift_declaration(&mut context.module, &mut declarations)
        .expect("")
        .1;

    FunctionCompiler::new(main_ref)
        .compile_all(&mut declarations, &mut context)
        .unwrap_or_else(|error| {
            error.print(input, filename);
            std::process::exit(67)
        });

    context.module.finalize_definitions().unwrap();

    let code = context.module.get_finalized_function(main_func_id);
    println!("Compilation finished! Running compiled function...");
    let main = unsafe { std::mem::transmute::<*const u8, unsafe fn() -> u8>(code) };
    unsafe { main() };
}
