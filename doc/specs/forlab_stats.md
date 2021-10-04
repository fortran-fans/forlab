---
title: STATS
---

# STATS

#### Notes

In daily use, vectors and matrices are more practical than higher-order arrays, 
and Fortran does not support array generics, we can use `reshape` to reshape vectors into higher-order arrays.

```fortran
real :: x(2,3,4)
x = reshape(randn(mean=0.0, std=1.0, ndim=2*3*4), [2,3,4])
```

[TOC]

### `randu`

#### Description

Generate an uniformly distributed data scalar or vector.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`random = [[forlab_stats(module):randu(interface)]](start, end [, ndim])`

#### Arguments

`random/start/end` should keep the same type and kind.

`start`: Shall be an `integer/real` scalar.
This argument is `intenet(in)`.

`end`: Shall be an `integer/real` scalar.
This argument is `inetent(in)`.

`ndim`: Shall be an `integer` scalar.
This argument is `intent(in)` and `optional`.

#### Result value

Returns an `integer/real` scalar or rank-1 array.

#### Example

```fortran
program demo_stats_randu

    use forlab_stats, only: randu

    print *, "running `demo_stats_randu`.."

    print *, randu(start=1, end=2)
    print *, randu(start=1.0, end=2.0, ndim=3)
    print *, reshape(randu(1.0, 2.0, 2*2), [2,2])

    !> Possible output:

    !! 2
    !! 1.65676987       1.11625218       1.03502560
    !! 1.74973476       1.82997108       1.77998054       1.14384007

end program demo_stats_randu
```

### `randn`

#### Description

Generate a normal distributed data scalar or vector.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`random = [[forlab_stats(module):randn(interface)]](mean, std [, ndim])`

#### Arguments

`random/mean/std` should keep the same type and kind.

`mean`: Shall be an `integer/real` scalar.
This argument is `intenet(in)`.

`std`: Shall be an `integer/real` scalar.
This argument is `inetent(in)`.

`ndim`: Shall be an `integer` scalar.
This argument is `intent(in)` and `optional`.

#### Result value

Returns an `integer/real` scalar or rank-1 array.

#### Example

```fortran
program demo_stats_randn

    use forlab_stats, only: randn

    print *, "running `demo_stats_randn`.."

    print *, randn(mean=0.0, std=2.0)
    print *, randn(mean=0.0, std=2.0, ndim=3)
    print *, reshape(randn(0.0, 2.0, 2*2), [2,2])

    !> Chi-square distribution of 3 degrees of freedom
    print *, sum(randn(mean=0.0, std=1.0, ndim=3)**2)
    print *, sum(reshape(randn(mean=0.0, std=1.0, ndim=5*3), [5, 3])**2, dim=2)

    !> Possible output:

    !! -0.387298465    
    !! -1.37615824     -0.529266298       5.43095016
    !! -1.35311902       1.81701779      0.772518456     -0.269844353

    !! 9.45483303
    !! 0.962645471      0.698421597      0.687875450       4.75956964       1.71025097

end program demo_stats_randn
```