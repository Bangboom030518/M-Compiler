## Labelled Blocks

```rust
function _() {
    inline(|| lambda:{
        return "Hello!" // Returns from the `_` function
        lambda:return "Hello!" // Returns from the inline callback

        lambda:return? value;
    })
}
```