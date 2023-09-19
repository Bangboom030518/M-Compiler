> **TODO**: streams?????

Modules allow code to be shared and reused across files. M provides a simple, strict module interface to simplify working with modules.

## Exports

Exporting a declaration allows it to be accessible when the module is imported. It is achieved through the `export` keyword

| Export Keyword | Declaration                           |
| -------------- | ------------------------------------- |
| `export`       | `const MY_CONSTANT = "Hello World!"`  |
| `export`       | `function my_function(): Nil -> {..}` |

> **NOTE**: Declarations can only be exported at the top level, meaning runtime values using `let` or `var` cannot be exported.

## Imports

Import declarations allow other modules to be brought into the scope of the current file.

### Syntax

| import keyword | module path                   |
| -------------- | ----------------------------- |
| `import`       | `module`                      |
| `import`       | `module::sub_module`          |
| `import`       | `module::sub_1::sub_2::sub_3` |

### Paths

> **TODO**: Describe properly

Modules in `M` are created with a file, ending in `.m`. The file name will be used when accessing the module.

> **NOTE**: Modules should be named in `lower_snake_case`, using only alphanumeric and the `_` characters

If a module contains child modules, it should instead be a directory, with an `index.m` file to provide the entry point to that module and any child modules defined within the same directory.

```
filepath = ABSOLUTE PATH OF DEPENDANT
path = IMPORT PATH AS A LIST OF NAMESPACES 
result_path = PARENT DIERECTORY OF filepath

FOR namespace IN path {
    MATCH namespace {
        "super" => {
            IF filepath IS "index.m" {
                result_path += "/.."
            } ELSE {
                result_path += "/index.m"
            }
        }

        "package" => {
            IF FIRST ITERATION {
                result_path = ABSOLUTE PATH TO PACKAGE ROOT
            } ELSE {
                ERROR! "'package' keyword can only appear at the start of module path"
            }
        }

        DIRECTORY => {
            result_path += `/${namespace}/index.m`
        }

        ELSE => {
            result_path += `/index.m`
        }
    }
}

RETURN result_path
```

## Example

`./module.m`

```m
import std/io;

static STDOUT = io::stdout();

export function greet(hour: UInt8): Nil -> {
  let greeting = if (hour > 17) {
    "Good Evening!"
  } else {
    "Good Day!"
  };
  STDOUT.write_line(greeting);
}
```

`./index.m`

```m
import module;

export function main() -> Nil {
  module::greet(16);
  module::greet(16);
}
```
