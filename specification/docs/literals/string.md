Strings are a list of characters.

## Syntax

Strings are declared with double quotes and optional flags.

| [Flags] | Open Quotes | Content | Close Quotes |
| --- | --- | --- | --- |
| `mr` | `"` | `Hello World` | `"` |

> **Note**: flags should appear in alphabetical order. 

### Example

#### Code

```m
import Console from "std";

const my_string = "Hello World";
Console::write_line(my_string);
```

#### Expected Output

```console
Hello World
```

## String Templates

*All* strings in M can be templates. The compiler determines whether or not this is neccessary. This means values can be formatted and interpolated at specific points within a string literal.

#### Syntax

Values are interpolated using the `$` sign followed by curly braces.

| Dollar Sign | Open Curly Brace |  Expression | [debug] | Close Curly Brace |
| --- | --- | --- | --- | --- |
| `$` | `{` | `my_sussy_variable` | `:?` | `}` |

> **NOTE**: For info on how to impliment formatters and debug formatters, read [The Format Namespace](/std/Format.md) in the standard library.

## Flags

There are a number of flags that modify the behavior of strings.

### 'Raw'

The `r` flag creates a 'raw' string, where any escape sequences are escaped. This means that single backslashes (`\`) will be interpretted literally without the need to escape them.

#### Example

##### Code
```m
import Console from "std";

Console::write_line("1: Hello \n World");
Console::write_line(r"2: Hello \n World");
```

##### Expected Output
```console
1: Hello
World
2: Hello \n World
```
### 'Multiline'

The `m` flag creates a 'multiline' string, where the string can span multiple lines.

#### Syntax

The open quotes should be immediately preceeded by a new line and all following lines should be indented, up until the end quotes, which should be on their own line and not indented. The indents and the leading and trailing line breaks will be ignored.

#### Example

##### Code

```m
import Console from "std";

Console::write_line("1: Hello World");
Console::write_line(m"
    2: Hello
    World
")
```

##### Expected Output

```console
1: Hello World
2: Hello
World
```

## Type

`String`