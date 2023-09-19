> **TODO**: work out tests properly.
> **TODO**: functional decorators instead?
The library used to write unit tests.

## Equal

```
export const equal: <T>(item_1: T, item_2: T) -> void;
```

Asserts that `item_1` and `item_2` are equal.

## create_test

```
type Test struct {
    test_fn: (() -> void) -> Test<(() -> void)>
}

export fn create_test(test_fn: (() -> void)) -> Test
```

Wraps a test to be exported.

These can be be run with:

```bash
mlang test
```

For example:

```
export const my_unit_test = Test::create_test(|| {
    // 2 + 2 makes 5
    Test::equal(2 + 2, 5)
})
```
