submodule(forlab) forlab_linspace
    !! Version: experimental
    !!
    !! linspace creates a linearly spaced vector.   
    !!([Interface](../interface/linspace.html))
    !!### Syntax
    !!    x = linspace(x1, x2, n)
    !!
    !!### Description
    !! `x = linspace(x1, x2, n)` returns a vector of n evenly spaced points
    !! between `x1` and `x2`.
    !!
    !!### Examples
    !!    x = linspace(0, 10, 11)
    !!        0.  1.  2.  3.  4.  5.  6.  7.  8.  9.  10.
    use forlab_kinds
    implicit none

contains

    module procedure linspace_sp
        integer :: i, n
        real(sp) :: by
        n = size(X)
        by = (to - from)/real(n - 1, sp)
        X = from + by*real([(i - 1, i=1, n)], sp)
        return
    end procedure

    module procedure logspace_sp
        call linspace(X, log10(from), log10(to))
        X = 10._sp**X
        return
    end procedure
    module procedure linspace_dp
        integer :: i, n
        real(dp) :: by
        n = size(X)
        by = (to - from)/real(n - 1, dp)
        X = from + by*real([(i - 1, i=1, n)], dp)
        return
    end procedure

    module procedure logspace_dp
        call linspace(X, log10(from), log10(to))
        X = 10._dp**X
        return
    end procedure
    module procedure linspace_qp
        integer :: i, n
        real(qp) :: by
        n = size(X)
        by = (to - from)/real(n - 1, qp)
        X = from + by*real([(i - 1, i=1, n)], qp)
        return
    end procedure

    module procedure logspace_qp
        call linspace(X, log10(from), log10(to))
        X = 10._qp**X
        return
    end procedure

end submodule
