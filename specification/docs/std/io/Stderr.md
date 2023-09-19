The library for writing to `stderr`.

## Write Line

```m
export void write_line(text: String);
```

Appends the text `text` to `stderr`, on a new line.

### Example

#### Code:

```m
import Stderr from "std:io";
Stderr::write_line("Hello");
Stderr::write_line("World");
```

#### Output:

```console
Hello
World
```

## Write

```m
export void write(text: String);
```

Appends the text `text` to `stderr`.

### Example

#### Code

```m
import Stderr from "std:io";
Stderr::write("Hello");
Stderr::write("World");
```

#### Output

```console
HelloWorld
```

## Clear

```m
export void clear();
```

Clears `stderr`.

### Example
