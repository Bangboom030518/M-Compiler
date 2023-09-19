> **Todo**: `isize` + `usize`!!!!!

## Types

### Signed Integers

- `Int8`: 8 bit integer.
- `Int16`: 16 bit integer.
- `Int32`: 32 bit integer.
- `Int64`: 64 bit integer.
- `Int128`: 128 bit integer.

### Unsigned Integers

- `UInt8`: 8 bit unsigned integer.
- `UInt16`: 16 bit unsigned integer.
- `UInt32`: 32 bit unsigned integer.
- `UInt64`: 64 bit unsigned integer.
- `UInt128`: 128 bit unsigned integer.

### Floats

IEEE 754 floating point numbers

- `Float32`: 32 bit floating point number.
- `Float64`: 64 bit floating point number.

## Syntax

| [Base Prefix] | Digits | Underscore | Type |
| ------------- | ------ | ---------- | ---- |
| (`0x`)        | `42`   | `_`        | `u8` |

> **Note**: Underscores can be used to seperate long numbers, such as `1_000_000` every three digits moving right to left. This will be done automatically by the formatter.

### Type

In the number literal, the numeric types are aliased with shorter, more concise names.

#### Signed Integers

| Type     | Alias  |
| -------- | ------ |
| `Int8`   | `i8`   |
| `Int16`  | `i16`  |
| `Int32`  | `i32`  |
| `Int64`  | `i64`  |
| `Int128` | `i128` |

#### Unsigned Integers

| Type      | Alias  |
| --------- | ------ |
| `UInt8`   | `u8`   |
| `UInt16`  | `u16`  |
| `UInt32`  | `u32`  |
| `UInt64`  | `u64`  |
| `UInt128` | `u128` |

#### Floats

| Type      | Alias |
| --------- | ----- |
| `Float32` | `f32` |
| `Float64` | `f64` |
