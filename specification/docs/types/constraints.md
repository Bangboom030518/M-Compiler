> TODO finish

`type InList<T>(list: T[]) constraint (value: uint8) -> {
  const a: T = 10000000000
  return value > a
};`

`type InList constraint <T>(list: T[], value: T) -> boolean {
  Array::has(list, value)
}`

| `type` keyword | identifier | \[type parameters\] | parameters | `constraint` keyword | tester function (implied `pure` and with return type `boolean`, no access to runtime values) |
|---|---|---|---|---|---|
| `type` | `InList` | `<T>` | `(list: T[])` | `constraint` | `(value: T);` |

This would be used as?

type A union

`const x: uint8 & InList([1u8, 2u8, 3u8]) = 1`
