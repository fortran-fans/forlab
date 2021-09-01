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

### `arange`

#### Status

Experimental

#### Class

Pure function.

#### Description

Creates a rank-1 `array` of the `integer/real` type with fixed-spaced values of given spacing, within a given interval.

#### Syntax

`result = [[forlab_math(module):arange(interface)]](start [, end, step])`

#### Arguments

All arguments should be the same type and kind.

`start`: Shall be an `integer/real` scalar.
This is an `intent(in)` argument.  
The default `start` value is `1`.

`end`: Shall be an `integer/real` scalar.
This is an `intent(in)` and `optional` argument.  
The default `end` value is the inputted `start` value.

`step`: Shall be an `integer/real` scalar and large than `0`. 
This is an `intent(in)` and `optional` argument.   
The default `step` value is `1`.

##### Warning
If `step = 0`, the `step` argument will be corrected to `1/1.0` by the internal process of the `arange` function.   
If `step < 0`, the `step` argument will be corrected to `abs(step)` by the internal process of the `arange` function. 

#### Return value

Returns a rank-1 `array` of fixed-spaced values.

For `integer` type arguments, the length of the result vector is `(end - start)/step + 1`.  
For `real` type arguments, the length of the result vector is `floor((end - start)/step) + 1`.

#### Example

```fortran
program demo_math_arange
    use forlab_math, only: arange

    print *, arange(3)                 !! [1,2,3]
    print *, arange(-1)                !! [1,0,-1]
    print *, arange(0,2)               !! [0,1,2]
    print *, arange(1,-1)              !! [1,0,-1]
    print *, arange(0, 2, 2)           !! [0,2]

    print *, arange(3.0)               !! [1.0,2.0,3.0]
    print *, arange(0.0,5.0)           !! [0.0,1.0,2.0,3.0,4.0,5.0]
    print *, arange(0.0,6.0,2.5)       !! [0.0,2.5,5.0]

    print *, (1.0,1.0)*arange(3)       !! [(1.0,1.0),(2.0,2.0),[3.0,3.0]]

    print *, arange(0.0,2.0,-2.0)      !! [0.0,2.0].     Not recommended: `step` argument is negative!
    print *, arange(0.0,2.0,0.0)       !! [0.0,1.0,2.0]. Not recommended: `step` argument is zero!

end program demo_math_arange
```