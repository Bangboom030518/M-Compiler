- Do we have `%{...}%` or `%{...}` to interpolate values into string templates.
- External packages!!!!
- Module namespace conflicts (`as` required in conflicts?)
- Do we have a return label system, like [Kotlin](https://kotlinlang.org/docs/returns.html#return-to-labels)
  - Yes
- Do we use `let` for properties?
- NO SEMICOLONS
- `.` instead of `::`
- ENHANCED ENUMS?????????
- intrinsics: '@' + identifier + '(' + Expression,* + ')'
# If, while and match

Do we have required parenthesis and/or curly braces, like this?

```m
match money {
    Money.Lots => println("yay!"),
    Money.AsLittleAsProles => println("I will not exist for long!")
}

function apple(pear: Pear) -> Int do 1 + 1

class Entry<K, V> {
  let key: K
  let value: V

  function key()
}

struct HashMap<K, V> {
  buckets: Vec<Bucket<K, V>>
}

type Option<T> = union {
  Some: T,
  None,
}

// TODO: trait bounds BAD
type Result<T, E: Error> = union {
  Ok: T,
  Error: E,
}

interface Iterator {
  type Item
  function next() Item?

  function map<I, F: Function<Item, I>>(self, binding: F) Map<F, Self> do Map.new(binding, self)
  Map<F, Self> map<I, Function<Item, I>>(self, binding: F) => Map.new(binding, self)
}

function next() -> None!Item {

}

function ask_user_age() -> error {
  IO.WriteError,
  IO.ReadError,
  Bytes.ParseError<String>,
  String.ParseError<Int>
}!Int {
  // write_line(line: String) -> IO.Error!Nil
  IO.Stdout.write_line("What is your age?")!
  // read_line() -> IO.Error!Bytes
  return IO.Stdin.read_line()!.parse_string()!.parse()!
}

Option.Some(t)
Result<Int, Int>

nil

interface Optional {
  fn map()
}

struct {
  fn HashMap(comptime K: type, comptime V: type) type {
    return struct {
      const Self = @This();

      const Entry = struct {
        key: K,
        value: V,
      };

      const Bucket = LinkedList(Entry);

      buckets: []Bucket;

      fn init() Self { ... }
    }
  }
}

class Int {
  fn add()
}
Int.add(a, b)

class Person {
  fn speak(self);
}

interface {
  method
}

Person.new() |> speak()
Person.new().speak()

if condition do expression else expression;



while 2 + 2 == 4 do {
    println("Re-education in progress...")
    return
}

```

What about `if let`?

```m
let x = if let Some(x) = nullable_expression {

} else {

};

let x = if Some(x) = (nullable_expression) {

} else {

};
```
