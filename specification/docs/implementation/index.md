## Overview

> [How to write a compiler](https://softwareengineering.stackexchange.com/questions/165543/how-to-write-a-very-basic-compiler) - this gives a nice outline of steps we would need to take

- Parsing (we could use a nice tool to write grammars here, there are [many tools](https://en.wikipedia.org/wiki/Comparison_of_parser_generators))
  - [GNU Bison](https://en.wikipedia.org/wiki/GNU_Bison)?
  - [APG - ABNF Parser Generator](https://github.com/ldthomas/apg-7.0)?
  - [Python Based ABNF Parser Generator](https://github.com/declaresub/abnf)?
- Resolution of references to other modules
- Semantic validation (getting rid of nonsense that is technically valid to parse)
- Equivalent transformations and high-level optimization (making the developers code simpler)
- Code generation (with a lot of help from LLVM)
- Low level optimization (we literally let LLVM do this for us, because it a very generic job)

## Tools we will need

- Parser - see [here](https://en.wikipedia.org/wiki/Comparison_of_parser_generators)
  - Rules using a formal grammar - defined using something like [this](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
- Code generator - see [here](https://en.wikipedia.org/wiki/LLVM) and [here](https://llvm.org/)

## Points

- We do not have to stay in one language for this, each step can be done by a different language if it is easier
