# `angle`

## Description

Solve for the argument of a complex number, or the angle between two three-dimensional vectors.

## Syntax

```fortran
angle = angle(X, Y)
arg = angle(z)
```

## Status

Experimental.

## Class

Pure function.

## Arguments

`X`: Shall be a `real` and `dimension(3)` array.
This argument is `intent(in)`.

`Y`: Shall be a `real` and `dimension(3)` array.
This argument is `intent(in)`.

`z`: Shall be a `complex` scalar.
This argument is `intent(in)`.

Note: All `real/integer` arguments must have same `kind`.  

## Result value

Returns a `real` scalar.

## Example

```fortran
program demo_math_angle
    use, intrinsic :: iso_fortran_env, only: int8
    use forlab_math,  only: angle
    real, dimesion(3) :: x, y
    complex :: z = cmplx(3.0, 4.0)
    
    x = 1_int8 ; y = 2_int8
    print *, angle(x, y)    !! 0.0
    print *, angle(z)       !! 0.927295208
     
end program demo_math_angle
```

## Source(incomplete)

```fortran
{{#include ../../../src/forlab_math_angle.f90}}
```