# M Compiler

A rust-based compiler for the `M` programming language.

## Syntax Examples

```m
fn<T, U> Map<T, U> map(Iterator<T> iterator, Fn<(T), U> mapping)
    Map
        iterator = iterator,
        mapping = mapping,
    end
end
```
