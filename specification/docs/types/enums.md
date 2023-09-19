> TODO: improve definition.

Enums, like in any other language, are a list of named constants used to provide a set of unique options for a specific type. The M complier then stores these constants as regular integers in memory, starting from 0.

## Syntax

### Declaration

Enums are declared with the `type` and `enum` keywords. The body is placed within curly braces (`{}`), and contains items written in `PascalCase` seperated by commas.

| `enum` keyword | Identifier | Body                  |
| -------------- | ---------- | --------------------- |
| `enum`         | `MyEnum`   | `{ ITEM_1, ITEM_2, }` |

```
type MyEnum enum {
    ITEM_1,
    ITEM_2,
}
```

### Accessing

Items within enums are then accessed using the double colon syntax (`::`).

```
// ...

const function = (param: MyEnum) -> {
    // do something
};

function(MyEnum::ITEM_1);

```
