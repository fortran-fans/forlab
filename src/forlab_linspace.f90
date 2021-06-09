submodule(forlab) forlab_linspace
    !! Version: experimental
    !!
    !! linspace creates a linearly spaced vector.   
    !!
    !!## Syntax
    !!    x = linspace(x1, x2, n)
    !!
    !!## Description
    !! `x = linspace(x1, x2, n)` returns a vector of n evenly spaced points
    !! between x1 and x2.
    !!
    !!## Examples
    !!    x = linspace(0, 10, 11)
    !!        0.  1.  2.  3.  4.  5.  6.  7.  8.  9.  10.
    use forlab_kinds
    implicit none

contains

    module procedure linspace_rr_sp
        integer :: i
        real(sp) :: step
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        allocate (linspace_rr_sp(n))
        step = (last - first)/(n - 1)
        linspace_rr_sp = first + step*real([(i - 1, i=1, n)], sp)
        return
    end procedure

    module procedure linspace_ii_sp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ii_sp = slinspace(real(first, kind=sp), real(last, kind=sp), n)
    end procedure 

    module procedure linspace_ri_sp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ri_sp = slinspace(first, real(last, kind=sp), n)
        return
    end procedure

    module procedure linspace_ir_sp
        if (n>1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ir_sp = slinspace(real(first, kind=sp), last, n)
        return
    end procedure

    module procedure linspace_rr_dp
        integer :: i
        real(dp) :: step
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        allocate (linspace_rr_dp(n))
        step = (last - first)/(n - 1)
        linspace_rr_dp = first + step*real([(i - 1, i=1, n)], dp)
        return
    end procedure

    module procedure linspace_ii_dp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ii_dp = dlinspace(real(first, kind=dp), real(last, kind=dp), n)
    end procedure 

    module procedure linspace_ri_dp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ri_dp = dlinspace(first, real(last, kind=dp), n)
        return
    end procedure

    module procedure linspace_ir_dp
        if (n>1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ir_dp = dlinspace(real(first, kind=dp), last, n)
        return
    end procedure

    module procedure linspace_rr_qp
        integer :: i
        real(qp) :: step
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        allocate (linspace_rr_qp(n))
        step = (last - first)/(n - 1)
        linspace_rr_qp = first + step*real([(i - 1, i=1, n)], qp)
        return
    end procedure

    module procedure linspace_ii_qp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ii_qp = qlinspace(real(first, kind=qp), real(last, kind=qp), n)
    end procedure 

    module procedure linspace_ri_qp
        if (n<=1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ri_qp = qlinspace(first, real(last, kind=qp), n)
        return
    end procedure

    module procedure linspace_ir_qp
        if (n>1) print *, ('Error: in linspace, n should be > 1.')
        linspace_ir_qp = qlinspace(real(first, kind=qp), last, n)
        return
    end procedure


end submodule
