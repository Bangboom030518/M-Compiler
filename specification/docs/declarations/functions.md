> **TODO**: Traits?

> **TODO**: can you have single expressions after ifs without curly braces.
> **TODO**: 

## Syntax

Functions are declared with their parameters in brackets.

| [Pure Keyword] | Function Keyword | Name | Type Params | Params                     | [Return Type] | Arrow | Return Expression |
| -------------- | ---------------- | ------ | ----------- | -------------------------- | ------------- | ----- | ----------------- |
| `pure`         | `function`             | `add`  | (`<T>`)     | `(num1: Int8, num2: Int8)` | `: Int8`      | `->`  | `num1 + num2`     |

```m
pure function add(num1: Int8, num2: Int8): Int16 -> num1 + num2;
```

> **Note**: Unlike some languages, where the type of parameters can be inferred, type annotations are required on functional parameters.

### Parameters

Parmameters are declared with their name, followed by a colon and type to serve as their type annotation

| Name | Colon | Type     |
| ---- | ----- | -------- |
| `a`  | `: `  | `String` |

### Type Parameters

## Nil Functions

Functions that return nothing can use `Nil` as their return type. They will then implicitly return `nil` after the body has finished executing, but can return early with an empty `return` statement.

### Example

#### Code

```m
import Stdout from "std:io";

function greet(hour: UInt8): Nil -> {
  if (hour > 17) {
    Stdout::write_line("Good Evening!");
    return;
  }
  Stdout::write_line("Good Day!");
}

export function main(): Nil -> {
  greet(16);
  greet(19);
}
```

#### Expected Output

```console
Good Day!
Good Evening!
```

## The Pure Keyword

There are times when a function just needs to compute a given argument and return the result, without producing any side-effects, such as editing global variables or writing to `stdout`. In these cases, the `pure` keyword is recommended.

```
pure function function_name(arg_1: [type], arg_2: [type]) -> [return type] {
}
```

> **Note**: Pure functions cannot have a return type of `Nil`.
