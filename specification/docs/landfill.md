```
import Int from "std::integer";
import Length from "std::array";

...

  // var my_int: Int(1, 12) = 1;

  var my_int: uint8 = 1;

  const my_fn = pure <T>(list: T[], index: Int(0, Length(list))) -> {
    return list[index];
  }

  type In

  Min(10)
  
  type NumberUpTo constraint (max = UIntGeneric) -> Range<UIntGeneric>(0, max)

  const my_fn = pure <T>(
    list: T[],
    index: Range<UIntGeneric>(0, Length(list))
  ): T -> {
    return list[index];
  }

...

var func = (list: T[], index) => list[index];
```

```
import Vector from "std";

const x: uint8[] = [1, 12, 43, 2];
const y: Vector<uint8> = Vector::new([1, 12, 43, 2]);
var z: Vector<uint8> = Vector<uint8>! { 1, 12, 43, 2 };

Vector::append(z, 5);

RegExp! {
  <behind>12</behind><number />.<latin1 />+?
}

```

```
struct Dog {
  name: String
}

impl CanSpeak for Dog {
  const SOUND: &'static str = "woof";
  fn get_name(self) -> String {
    self.name
  }
}

trait CanSpeak {
  const SOUND: &'static str;
  fn get_name(self) -> String;

  fn speak(self) {
    println!("{} says {}", self.get_name(), Self::SOUND)
  }
}

trait CanShout {
  fn shout(self);
}

impl <T: CanSpeak>CanShout for T {
  fn shout(self) {
     println!("{} shout {}", self.get_name(), Self::SOUND)
  }
}

Dog {
  name: "Lundy".to_string()
}.speak()

trait CanSpeak<Self> {
  const SOUND: &'static str;
  fn get_name(self) -> String;

  fn speak(self) {
    println!("{} says {}", self.get_name(), Self::SOUND)
  }
}

trait CanShout<Self: CanSpeak> {
  fn speak(self) {
    println!("{} shouts {}", self.get_name(), Self::SOUND)
  }
}

type Dog struct {
  name: string;

  trait CanSpeak {
    const SOUND = "woof";

    fn get_name(self) {
      self.name
    }
  };

  trait CanShout;
}
```

```m
R[] map<T, R>(map_fn: ((val: T, [index: usize]) -> R), iter: T[]);

character
integer
boolean

map([1, 2], (a) -> a ** 2);
```

```m
trait Map {
   R[] map<T, R>(self, map_fn: ((val: T, [index: usize]) -> R))) {

   };
}

impl Map for <T: Sized> {

}

struct T {
    prop1: int16,
    prop2: String,

    Self new(&self, self.int16, self.String) -> {

    }

    trait Map {
        fn map()
    }
}
```

```
int16 x = 12;
```

```
struct A {
  let prop: String;
  
  impl MyTrait {
    let prop2: String;
  }
  
  impl MyTrait2 {
    let prop2: String;
  }
}

Trait1::method(self)
self.method()

let a = A {
  prop: "_",
  MyTrait {
    prop2: "_",
  }
};

<a as T>.prop2;

a@T.prop2
```
