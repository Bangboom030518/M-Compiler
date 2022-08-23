# M Compiler

A rust-based compiler for the [`M` programming language](https://github.com/Bangboom030518/M-Specification).

## File Structure

### bnf

A test parser using the `Backus-Naur Form` syntax and the [`bnf`](https://crates.io/crates/bnf) crate.

### pest

A test parser using the `pest` syntax and the [`pest`](https://crates.io/crates/pest) crate.

### src

An unfinished custom-built parser using enums.

## TODO:

- Finish the pest grammar
- Add module bundling
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
