---
title: MATH
---

# MATH

[TOC]

### `is_close`

#### Description

Returns a boolean scalar/array where two scalars/arrays are element-wise equal within a tolerance.

The tolerance values are positive, typically very small numbers. The relative difference `(rtol*abs(b))` and the absolute difference `atol` are added together to compare against the absolute difference between `a` and `b`.

```fortran
!> For `real` type
abs(a - b) <= rtol*abs(b) + atol
!> For `complex` type
abs(a%re - b%re) <= rtol*abs(b%re) + atol
abs(a%im - b%im) <= rtol*abs(b%im) + atol
```

#### Syntax

`bool = [[forlab_math(module):is_close(interface)]] (a, b [, rtol, atol])`

#### Status

Experimental.

#### Class

Elemental function.

#### Arguments

`a`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`b`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`rtol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `1.0e-5` by default.

`atol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `1.0e-8` by default.

Note: All `real/complex` arguments must have same `kind`.
If the value of `rtol/atol` is negative (not recommended), it will be corrected to `abs(rtol/atol)` by the internal process of `is_close`.

#### Result value

Returns a `logical` scalar/array.

#### Example

```fortran
program demo_math_is_close
    use forlab_math, only: is_close
    use stdlib_error, only: check
    real :: x(2) = [1, 2]
    print *, is_close(x,[real :: 1, 2.1])     !! [T, F]
    print *, all(is_close(x,[real :: 1, 2.1]))!! F
    print *, is_close(2.0, 2.1, atol=0.1)     !! T
    call check(all(is_close(x, [2.0, 2.0])), msg="all(is_close(x, [2.0, 2.0])) failed.", warn=.true.)
        !! all(is_close(x, [2.0, 2.0])) failed.
end program demo_math_is_close
```