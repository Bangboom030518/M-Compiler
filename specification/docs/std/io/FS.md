The library used for interacting with the file system.

> **TODO**: Write the following:
> - Write File
> - Append File
> - Remove File
> - Read Directory
> - Get File Info
>   - Do we use a path struct?
> - Error

## Error

## File
```
struct File {...}

interface File {}

```

## Read To Bytes

Attempts to read file at path `path` (relative to the project root), and returns a result storing either the file content as a list of bytes or an error.

```
export fn read_to_bytes(path: String) -> Result<List<UInt8>, Error>;
```

## Read To String

```
export fn read_to_string(path: String, encoding: Encoding::Method) -> Result<String, Error>;
```

