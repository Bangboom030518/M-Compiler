When working with M, these guidelines should be **strictly** followed:

- Types should have the highest specificity possible. It should be impossible to pass an incorrect value to a function.
- There should be a function in the standard library for all generic tasks. The developer should *never* implement code that replaces these.
- There should only ever be one preferred way of doing things. The developer should need to make as few decisions as possible.
    - This way should be the fastest, securest and most memory-safe way possible.

> **Note**: Failure of an M developer to comply with these guidelines will result in their immediate termination.
