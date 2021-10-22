# `cross`

## Description

Cross product.

## Syntax

```fortran
z = cross(x, y)
z = x .c. y
```

## Status

Experimental.

## Class

Pure function.

## Arguments

`x`: Shall be a `real/integer` and `dimension(3)` array.
This argument is `intent(in)`.

`y`: Shall be a `real/integer` and `dimension(3)` array.
This argument is `intent(in)`.

Note: All `real/integer` arguments must have same `kind`.  

## Result value

Returns a `real/integer` and `dimension(3)` array.

## Example

```fortran
program demo_math_cross
    use, intrinsic :: iso_fortran_env, only: int8
    use forlab_math,  only: cross, operator(.c.)
    real, dimesion(3) :: x, y
    
    x = 1_int8 ; y = 2_int8
    print *, x.c.y          !! [0.0, 0.0, 0.0]
    print *, cross(x,y)     !! [0.0, 0.0, 0.0]
    
end program demo_math_cross
```

## Source

```fortran
{{#include ../../../src/forlab_math_cross.f90}}
```