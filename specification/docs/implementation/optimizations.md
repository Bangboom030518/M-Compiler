1. Modulo Operator - Powers of 2:

    Using the algorithm *x % 2<sup>n</sup> == x & (2<sup>n</sup> - 1)*, we'll transform module operations on powers of 2 like this:
    - `x % 2` -> `x & 1`
    - `x % 4` -> `x & 3`
    - `x % 8` -> `x & 7`
