> Shall we shorten these, e.g `UnsignedIntGeneric` -> `UnsignedInt` and `SignedIntGeneric` -> `SignedInt`?

## Types

### Unsigned

```
type Unsigned union = uint8 | uint16 | uint32 | uint64;
```

Any unsigned integer.

### Signed

```
type Signed union int8 | int16 | int32 | int64;
```

Any signed integer.

### Generic

```
type Generic union Unsigned | Signed;
```

Any integer.

### Experimental Syntax

```
type Min<T extends Signed = Signed>(value: T, min: T) constraint (value: T, min: T) -> boolean;
```

Accepts any value of type `T` that is greater than `min`.

```
type Max<T extends Generic>(max: T) constraint (value: T) -> boolean;

const my_int: unint8 & Max<uint8>(10) = 10
```

```
type Range<T extends SignedIntGeneric = SignedIntGeneric>(min: T, max: T) constraint (value: UIntGeneric | IntGeneric) -> boolean;
```
