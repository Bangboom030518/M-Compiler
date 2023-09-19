The library for all mathematical functions.

## Trigonometry

```
export Trigonometry from "trigonometry";
```

All the trigonometric functions, such as `sin`, `cos` and `tan`. [See All](/std/Math/Trigonometry).

## Statistics

```
export Statistics from "statistics";
```

The functions used for statistical analysis, such as `mean`, `median` and `range`. [See All](/std/Math/Trigonometry).

## Types

### Angle

> TODO: Number type!!!!

> TODO: Constraint

```
export type Angle union Number & Number::Min(0) & Number::Max(2 * Math::PI)
```

An angle in radians.
