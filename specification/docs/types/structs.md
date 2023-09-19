> **TODO**: FINISH!

A struct is a type that defines the structure of an object, with key-value pairs.

## Syntax

Structs are initialised using the `struct` keyword.

For example, the following creates a struct called `MyStruct`, with the property `property1` as a `utf8` array and `property2` as an `int16`.

| Struct Keyword | Name       | Open Brace | Fields | Close Brace |
| -------------- | ---------- | ---------- | ------ | ----------- |
| `struct`       | `MyStruct` | `{`        | `...`  | `}`         |

### Fields

#### Syntax

| name | colon | type | [initialisation] |
| ---- | ----- | ---- | ---------------- |
| `my_property

```
struct MyStruct {
  property1: String;
  property2: UInt8;
}
```

These can also be given defaults using the `=` symbol.

For example, the following initialises `property1` to `"Hello World"` and `property2` to `42` if no other value is specified on construction.

```
struct MyStruct {
  property1: String = "Hello World";
  property2: UInt8 = 42;
}
```

> **Note**: Unlike a lot of languages, such as [Rust](https://www.rust-lang.org/), [C#](https://learn.microsoft.com/en-us/dotnet/csharp/), [C++](https://cplusplus.com/) and [Typescript](https://www.typescriptlang.org/), M disallows access modifers such as `public` or `private` as they can encourage bad design patterns that can introduce difficult bugs to a program.
