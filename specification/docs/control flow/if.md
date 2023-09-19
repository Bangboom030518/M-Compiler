If expressions are used to execute and evaluate code based on a single condition.

> **Note**: For more complex conditional expressions, use [match](/control_flow/match).

## Syntax

| `if` keyword | condition    | `do` keyword | expression             | [`else` branch] |
| ------------ | ------------ | ------------ | ---------------------- | --------------- |
| `if`         | `2 + 2 != 5` | `do`         | `"Needs re-education"` |                 |
| `if`         | `2 + 2 != 5` | `do`         | `"Needs re-education"` | `else "Passed"` |

> **Note**: Without an `else` branch present, `expression` is required to evaluate to `nil`.
