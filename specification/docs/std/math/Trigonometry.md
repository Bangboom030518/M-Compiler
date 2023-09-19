> TODO: Number type!

> TODO: Constraint

The library for all the basic trigonometric functions.

## Sin

```
export const sin: pure (angle: Angle) -> double & Number::Min(-1) & Number::Max(1);
```

Returns the sine of angle `angle`, where `angle` is in radians.

## Cos

```
export const cos: pure (angle: Angle) -> double & Number::Min(-1) & Number::Max(1);
```

Returns the cosine of angle `angle`, where `angle` is in radians.

## Tan

```
export const tan: pure (angle: Angle) -> double;
```

Returns the tangent of angle `angle`, where `angle` is in radians.

## Acos

```
export const acos: pure (cos: double & Number::range(-1, 1)) -> double;
```

Returns the angle of the cosine value `cos`
