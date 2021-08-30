---
title: LINALG
---

# LINALG

[TOC]

## `diff` - diff computes differences of arrays.

### Status

Experimental

### Class

Pure function.

### Description

y = diff(x) returns differences between adjacent elements of vector x.  
y = diff(x, n) returns the nth difference by applying the diff(x) operator recursively n times.  
B = diff(A) returns differences between adjacent elements of array A along the first dimension.  
B = diff(A, n) returns the nth difference by applying the diff(A) operator recursively n times.  
B = diff(A, dim) returns differences between adjacent elements of array A along the dimension given by dim.  
B = diff(A, n, dim) returns the nth difference along the dimension given by dim by applying the diff(A, dim) operator recursively n times.


### Syntax

For vector:  
`result = [[forlab_linalg(module):diff(interface)]](x [, n])`

For matrix:  
`result = [[forlab_linalg(module):diff(interface)]](A [, n, dim])`

### Arguments

`x`: Shall be a `real` type of verctor.
`A`: Shall be a `real` type of matrix.

`n` (optional): Shall be a `integer` type.
`dim` (optional): Shall be a `integer` type.


### Return value

Return differences between adjacent elements of vector `x` or matrix `A`.

### Example

```fortran
program test_linalg_diff
    use forlab_linalg, only: diff
    use forlab_linalg, only: linspace
    use forlab_io, only: disp

    real :: x(10)
    
    call linspace(x, 0.0, 9.0)
    call disp(x, "linspace(x) : ")
    call disp(diff(x), "test_linalg_diff : ")
    
    
end program test_linalg_diff
```

## `zeros/ones`

### Description

`zeros` creates a rank-1 or rank-2 `array` of the given shape, filled completely with `0` `integer` type values.  
`ones` creates a rank-1 or rank-2 `array` of the given shape, filled completely with `1` `integer` type values.

#### Warning

It is not recommended to use the `zeros/ones` function, it is recommended to use `allocate(array(dim1, dim2, ..), source=0.0/1.0)`.

### Status

Experimental

### Class

Pure function.

### Syntax

For rank-1 array:  
`result = [[forlab_linalg(module):zeros(interface)]](dim)`  
`result = [[forlab_linalg(module):ones(interface)]](dim)`

For rank-2 array:  
`result = [[forlab_linalg(module):zeros(interface)]](dim1, dim2)`  
`result = [[forlab_linalg(module):ones(interface)]](dim1, dim2)`


### Arguments

`dim/dim1`: Shall be an `integer` type.
This is an `intent(in)` argument.

`dim2`: Shall be an `integer` type.
This is an `intent(in)` argument.

### Return value

Returns a rank-1 or rank-2 `array` of the given shape, filled completely with either `0` or `1` `integer` type values.

#### Warning

Since the result of `ones` is of `integer` type, one should be careful about using it in arithmetic expressions. For example:
```fortran
real :: A(:,:)
!> Be careful
A = ones(2,2)/2     !! A = 1/2 = 0.0
!> Recommend
A = ones(2,2)/2.0   !! A = 1/2.0 = 0.5
```

### Example

```fortran
program demo_linalg_zerosones
    use forlab_linalg, only: zeros, ones
    use forlab_io, only: disp

    real, allocatable :: zero(:, :), one(:, :)
    real, allocatable :: array(:, :)

    zero = zeros(1, 2)
    one  = ones (2, 1)

    call disp(zero, "zeros: ")
    call disp(one , "ones : ")

    call disp(ones(2, 2)/2, "!attention: `ones(2, 2)/2` is like `1/2 == 0`")

    array = zeros(2, 2)
    call disp(array, "array with zeros: ")

    array = ones (2, 2)
    call disp(array, "array with ones :")

end program demo_linalg_zerosones
```