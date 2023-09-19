> TODO: type keyword go brrrr.
> TODO: what about chars?
> **TODO**: Are these on the `String` struct?

The library containing all the basic utility functions used on `String`.

## To Lower Case

```
export pure fn to_lower_case((text: String) -> String);
```

Returns `text`, with all characters in lower case.

## To Upper Case

```
export const to_upper_case: pure ((text: String) -> String);
```

Returns `text`, with all characters in upper case.

### Example

#### Code

```m
import String from "std";
import Stdout from "std:io";

export fn main() {
    Stdout::write_line(String::to_upper_case("hello world"));
}
```

#### Expected Output

```console
HELLO WORLD
```
