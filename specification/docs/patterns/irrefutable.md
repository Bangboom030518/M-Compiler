> **TODO**: Update `struct` syntax as displayed here.

## Struct Destructuring

Destructure the properties from a struct.

### Syntax

| Struct Name | Open Brace |
| ----------- | ---------- |
| `MyStruct`  | `{`        |

### Examples

#### Code

```m
import Stdout from "std:io";

struct MyStruct {
    a: UInt8;
    b: UInt8;
}

let instance = MyStruct {
    a: 1_u8,
    b: 2_u8,
};

let MyStruct { a, b } = instance;

Stdout::write_line("a: ${a}")
Stdout::write_line("b: ${b}")
```

#### Expected Output

```console
a: 1
b: 2
```
