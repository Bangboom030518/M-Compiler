The library for writing to `stdout`.

## Write Line

```m
export void write_line(text: String);
```

Appends the text `text` to `stdout`, on a new line.

### Example

#### Code:

```m
import Stdout from "std:io";
Stdout::write_line("Hello");
Stdout::write_line("World");
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

Appends the text `text` to `stdout`.

### Example

#### Code:

```m
import Stdout from "std:io";
Stdout::write("Hello ");
Stdout::write("World");
```

#### Output:

```console
Hello World
```

## Clear

```m
export void clear();
```

Clears `stdout`.
