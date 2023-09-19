> **TODO**: figure out how to make IO events pure for this, we should not be inconsistent

The library used for temporary logs during development. Unlike in the `Stdout` module, these can be used within pure functions but their use anywhere within the project will raise a warning during development and prevent building for production.

## Log

Logs `item` to the console.

```m
export fn log<T: Debug>(item: T) -> Nil; 
```
