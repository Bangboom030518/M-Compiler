> TODO: Number type!

## Mean

```
export const mean: pure <T extends Number::Generic, R extends Number::Generic>(list: T[]) -> R;
```

Returns the mean of the list `list`.

## Median

```
export pure fn median<T extends Number::Generic, R extends Number::Generic>(list: List<T>) -> R;
```

Returns the median of the list `list`.

## Range

```
export pure fn range<T extends Number::Generic>(list: List<T>) -> T;
```

Returns the range of the list `list`.

## Interquartile Range

```
export const interquartile_range: pure <T extends Number::Generic>(list: T[]) -> T;
```

Returns the interquartile range of the list `list`.
