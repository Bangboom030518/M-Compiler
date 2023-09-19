Variables are references to values, which tend to be stored in memory and can sometimes be mutated and reasigned.

Variables in `M` behave similarly to those in rust, though there are some differences.

## Syntax

Variables are declared with a variable keyword, followed by their name, followed by an optional type annotation and a value.

| keyword | pattern       | assignment |
| ------- | ------------- | ---------- |
| `let`   | `my_variable` | `= 12u16`  |

### Const

Variables declared with the `const` keyword are constants, meaning their value can _never_ be changed, and must be a compile-time constant.

Constants are not stored in memory, but instead have their value interpolated wherever they are used. They are useful for describing constants such as magic numbers with no cost to performance.

#### Example

##### Code

```m
import std.io.*

const MILES_TO_KILOMETERS = 1.60934

float miles_to_kilometers = (Float32 miles) Float32 -> miles * MILES_TO_KILOMETERS

function main = () !Nil ->
    St.write_line(format("2 miles ≈ {miles_to_kilometers(2)}km"))!
    Stdout::write_line(f"3 miles ≈ {miles_to_kilometers(3)}km")!
    Stdout::write_line(f"4 miles ≈ {miles_to_kilometers(4)}km")!
    nil
```

##### Output

```console
2 miles ≈ 3.21869km
3 miles ≈ 4.82803km
4 miles ≈ 6.43738km
```

### Let

`let` is like rust `let`

### Var

`var` is like rust `let mut`

### Static

A `static` variable is lazily evaluated once and stored until it's no longer needed. They should be declared with `UPPER_SNAKE_CASE`, and can only be declared in the global scope. They cannot be exported.
