# M Compiler

A rust-based compiler for the [`M` programming language](https://github.com/Bangboom030518/M-Specification).

## TODO:

- Finish the pest grammar
- Generate syntax highlighting from pest tokens
- Write type-safe alternative to `pest_derive`?
- Add more tests
- Add docs
- Add module bundling
- Split parser into its own library crate
- Add constant evaluation optimizer (simple interpreter)
- Generate [LLVM IR](https://llvm.org/docs/LangRef.html).
- Compile llvm ir to machine code using [llvm-sys](https://crates.io/crates/llvm-sys) or [inkwell](https://crates.io/crates/inkwell), probably inkwell.

## Steps

> [How to write a compiler](https://softwareengineering.stackexchange.com/questions/165543/how-to-write-a-very-basic-compiler) - this gives a nice outline of steps we would need to take

- Parsing (converting the human-readable code to a machine-readable AST).
- Resolution of references to other modules
- Semantic validation (getting rid of nonsense that is technically valid to parse)
- Equivalent transformations and high-level optimization (making the developers code simpler)
- Code generation (with a lot of help from LLVM)
- Low level optimization (we literally let LLVM do this for us, because it a very generic job)
