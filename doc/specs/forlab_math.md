---
title: MATH
---

# MATH

[TOC]

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

### `signum`

#### Status

Experimental

#### Class

Elemental function.

#### Description

Returns the sign of variables.

#### Syntax

`sign = [[forlab_math(module):signum(interface)]](x)`

#### Argument

`x`: Shall be an `integer/real/complex` elemental variable.
This is an `intent(in)` argument.

#### Return value

Returns the sign of variables.

For `complex` types, return the regularization result: `sign = x/abs(x)`.

#### Example

```fortran
!> fpm run --example math_signum
program demo_math_signum
    use forlab_math, only: signum

    print *, signum(1 - 2)
    print *, signum([0.0, 2.1])
    print *, signum((1.0, -2.0))

    !>    -1
    !>   0.00000000       1.00000000    
    !>          (0.447213590,-0.894427180)

end program demo_math_signum
```


### `is_close`

#### Description

Returns a boolean scalar/array where two scalars/arrays are element-wise equal within a tolerance, behaves like `isclose` in Python stdlib.

```fortran
!> For `real` type
is_close(a, b, rel_tol, abs_tol) = abs(a - b) <= max(rel_tol*(abs(a), abs(b)), abs_tol)
!> For `complex` type
is_close(a, b, rel_tol, abs_tol) = is_close(a%re, b%re, rel_tol, abs_tol) .and. &
                                   is_close(a%im, b%im, rel_tol, abs_tol)
```

#### Syntax

`bool = [[stdlib_math(module):is_close(interface)]] (a, b [, rel_tol, abs_tol])`

#### Status

Experimental.

#### Class

Elemental function.

#### Arguments

`a`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`b`: Shall be a `real/complex` scalar/array.
This argument is `intent(in)`.

`rel_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `1.0e-9` by default.

`abs_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `0.0` by default.

Note: All `real/complex` arguments must have same `kind`.  
If the value of `rel_tol/abs_tol` is negative (not recommended), 
it will be corrected to `abs(rel_tol/abs_tol)` by the internal process of `is_close`.

#### Result value

Returns a `logical` scalar/array.

#### Example

```fortran
program demo_math_is_close
    use forlab_math,  only: is_close
    use stdlib_error, only: check
    real :: x(2) = [1, 2]
    print *, is_close(x,[real :: 1, 2.1])        !! [T, F]
    print *, is_close(2.0, 2.1, abs_tol=0.1)     !! T
    call check(all(is_close(x, [2.0, 2.0])), msg="all(is_close(x, [2.0, 2.0])) failed.", warn=.true.)
        !! all(is_close(x, [2.0, 2.0])) failed.
end program demo_math_is_close
```

### `all_close`

#### Description

Returns a boolean scalar where two arrays are element-wise equal within a tolerance, behaves like `all(is_close(a, b [, rel_tol, abs_tol]))`.

#### Syntax

`bool = [[stdlib_math(module):all_close(interface)]] (a, b [, rel_tol, abs_tol])`

#### Status

Experimental.

#### Class

Impure function.

#### Arguments

`a`: Shall be a `real/complex` array.
This argument is `intent(in)`.

`b`: Shall be a `real/complex` array.
This argument is `intent(in)`.

`rel_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `1.0e-9` by default.

`abs_tol`: Shall be a `real` scalar.
This argument is `intent(in)` and `optional`, which is `0.0` by default.

Note: All `real/complex` arguments must have same `kind`.  
If the value of `rel_tol/abs_tol` is negative (not recommended), 
it will be corrected to `abs(rel_tol/abs_tol)` by the internal process of `all_close`.

#### Result value

Returns a `logical` scalar.

#### Example

```fortran
program demo_math_all_close
    use forlab_math,  only: all_close
    use stdlib_error, only: check
    real    :: x(2) = [1, 2], random(4, 4)
    complex :: z(4, 4)
    
    call check(all_close(x, [2.0, 2.0], rel_tol=1.0e-6, abs_tol=1.0e-3), &
               msg="all_close(x, [2.0, 2.0]) failed.", warn=.true.)
               !! all_close(x, [2.0, 2.0]) failed.
    call random_number(random(4, 4))
    z = 1.0
    print *, all_close(z+1.0e-11*random, z)     !! T
    
end program demo_math_all_close
```