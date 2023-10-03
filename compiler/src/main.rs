#![warn(clippy::pedantic, clippy::nursery)]

// use resolution::build_file;
// use parser::{ast::prelude::*, parse_functions};
fn main() {
    // let code = include_str!("../../input.m");
    // let (input, functions) = parse_functions(code).unwrap();
    // assert_eq!(input, "");

    // // Create the JIT instance, which manages all generated functions and data.
    // let mut jit: translate::JIT = translate::JIT::default();
    // let answer: i32 = unsafe { run_code(&mut jit, functions.first().unwrap().clone(), 12) }.unwrap();

    // // let tree = build_file(r"C:\Users\Ben\Desktop\Sam and Charlie\Charlie\Rust\M-Compiler\input.m").unwrap_or_else(|error| {
    // //     eprintln!("{}", error);
    // //     std::process::exit(1)
    // // });

    // println!("answer = {answer}");
    let ast = parser::parse_file(include_str!("../../input.m")).unwrap();
    dbg!(ast);
}

// /// Executes the given code using the cranelift JIT compiler.
// ///
// /// Feeds the given input into the JIT compiled function and returns the resulting output.
// ////
// /// # Safety
// ///
// /// This function is unsafe since it relies on the caller to provide it with the correct
// /// input and output types. Using incorrect types at this point may corrupt the program's state.
// unsafe fn run_code<I, O>(
//     jit: &mut translate::JIT,
//     function: Function,
//     input: I,
// ) -> Result<O, String> {
//     // Pass the string to the JIT, and it returns a raw pointer to machine code.
//     let code_ptr = jit.compile(function)?;
//     // Cast the raw pointer to a typed function pointer. This is unsafe, because
//     // this is the critical point where you have to trust that the generated code
//     // is safe to be called.
//     let code_fn = std::mem::transmute::<_, fn(I) -> O>(code_ptr);
//     // And now we can call it!
//     Ok(code_fn(input))
// }
