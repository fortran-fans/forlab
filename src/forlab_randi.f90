!! This file contains the multi-precision `randi` function implementation.

submodule(forlab) forlab_randi
    !! Version: experimental
    !!
    !!## randi
    !! randi generates uniformly distributed random integers.
    !!
    !!### Syntax
    !!     x = randi(imax)
    !!     x = randi([imin, imax])
    !!     x = randi(imax, dim1)
    !!     x = randi([imin, imax], dim1)
    !!     A = randi(imax, dim1, dim2)
    !!     A = randi([imin, imax], dim1, dim2)
    !!     X = randi(imax, dim1, dim2, dim3)
    !!     X = randi([imin, imax], dim1, dim2, dim3)
    !!
    !!### Description
    !! `x = randi(imax)` returns a random scalar integer between `1` and `imax`.
    !!
    !! `x = randi([imin, imax])` returns a random scalar integer between `imin`
    !! and `imax`.
    !!
    !! `x = randi(imax, dim1)` returns a dim1 vector of random scalar integers
    !! between `1` and `imax`.
    !!
    !! `x = randi([imin, imax], dim1)` returns a dim1 vector of random scalar
    !! integers between `imin` and `imax`.
    !!
    !! `A = randi(imax, dim1, dim2)` returns a dim1-by-dim2 matrix of random
    !! scalar integers between `1` and `imax`.
    !!
    !! `A = randi([imin, imax], dim1, dim2)` returns a dim1-by-dim2 matrix of
    !! random scalar integers between `imin` and `imax`.
    !!
    !! `X = randi(imax, dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of random scalar integers between `1` and `imax`.
    !!
    !! `X = randi([imin, imax], dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of random scalar integers between `imin` and `imax`.
    use forlab_kinds
    implicit none

contains
    module procedure randi_0_0
        randi_0_0 = floor(randu()*real(imax)) + 1
    return
    end procedure
    module procedure randi_0_1
        randi_0_1 = minval(imax) + nint(randu()*real(maxval(imax) - minval(imax)))
    return
    end procedure
    module procedure randi_1_0
        randi_1_0 = floor(randu(dim1)*real(imax)) + 1
    return
    end procedure
    module procedure randi_1_1
        randi_1_1 = minval(imax) + nint(randu(dim1)*real(maxval(imax) - minval(imax)))
    return
    end procedure
    module procedure randi_2_0
        randi_2_0 = floor(randu(dim1, dim2)*real(imax)) + 1
    return
    end procedure
    module procedure randi_2_1
        randi_2_1 = minval(imax) + nint(randu(dim1, dim2)*real(maxval(imax) - minval(imax)))
    return
    end procedure
    module procedure randi_3_0
        randi_3_0 = floor(randu(dim1, dim2, dim3)*real(imax)) + 1
    return
    end procedure
    module procedure randi_3_1
        randi_3_1 = minval(imax) + nint(randu(dim1, dim2, dim3)*real(maxval(imax) - minval(imax)))
    return
    end procedure
end submodule
