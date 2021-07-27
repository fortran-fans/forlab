---
title: LINALG
---

# LINALG

[TOC]

## `diff` - diff computes differences of arrays.

### Status

Experimental

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
    implicit none

    real :: x(10)
    
    call linspace(x, 0.0, 9.0)
    call disp(x, "Linspace(x) : ")
    call disp(diff(x), "Test_linalg_diff : ")
    
    
end program test_linalg_diff
```