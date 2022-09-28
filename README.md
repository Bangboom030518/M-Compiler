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
> [LLVM tutorial in the Rust language](https://github.com/jauhien/iron-kaleidoscope#chapter-2-llvm-ir-code-generation)

- Parsing (converting the human-readable code to a machine-readable AST).
- Resolution of references to other modules
- Semantic validation (getting rid of nonsense that is technically valid to parse)
- Equivalent transformations and high-level optimization (making the developers code simpler)
- Generate LLVM IR

```rust
(0..=(2.pow(bits) - 1)).map(|n| n - (2.pow(bits - 1)))
```

`1.0210`

# Fractional Digits `20` (base 10)

`0210`
1. Strip insignificant figures (remove trailing `0`s)
`021`
2. Push any leading zeros to digits as `0` bits.
`21`
3. Convert remaining digits to binary.


| Whole | Fractional |
| ----- | ---------- |
| `1`   | `2`        |

0 .. 2 ^ (exponent_bits - 1)
-(2 ^ (exponent_bits - 1)) .. (2 ^ (exponent_bits - 1) / 2

CONST min = 0;
CONST max = 2 ^ (exponent_bits - 1);

CONST min_shifted = -(max_possible / 2);
CONST max_shifted = max_possible / 2;

FUNCTION get_float_exponent(number: INT) {
RETURN number - max_shifted
}
