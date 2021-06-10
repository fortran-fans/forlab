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
        linspace_ii_sp = slinspace(real(first, kind=sp), real(last, kind=sp), n)
    end procedure 

    module procedure linspace_ri_sp
        linspace_ri_sp = slinspace(first, real(last, kind=sp), n)
        return
    end procedure

    module procedure linspace_ir_sp
        linspace_ir_sp = slinspace(real(first, kind=sp), last, n)
        return
    end procedure

    module procedure logspace_rr_sp
        logspace_rr_sp = slinspace(log10(first), log10(last), n)
        logspace_rr_sp = 10._sp**logspace_rr_sp
        return
    end procedure

    module procedure logspace_ii_sp
        logspace_ii_sp = slinspace(log10(real(first, kind=sp)), log10(real(last, kind=sp)), n)
        logspace_ii_sp = 10._sp**logspace_ii_sp
    end procedure 

    module procedure logspace_ri_sp
        logspace_ri_sp = slinspace(log10(first), log10(real(last, kind=sp)), n)
        logspace_ri_sp = 10._sp**logspace_ri_sp
        return
    end procedure

    module procedure logspace_ir_sp
        logspace_ir_sp = slinspace(log10(real(first, kind=sp)), log10(last), n)
        logspace_ir_sp = 10._sp**logspace_ir_sp
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
        linspace_ii_dp = dlinspace(real(first, kind=dp), real(last, kind=dp), n)
    end procedure 

    module procedure linspace_ri_dp
        linspace_ri_dp = dlinspace(first, real(last, kind=dp), n)
        return
    end procedure

    module procedure linspace_ir_dp
        linspace_ir_dp = dlinspace(real(first, kind=dp), last, n)
        return
    end procedure

    module procedure logspace_rr_dp
        logspace_rr_dp = dlinspace(log10(first), log10(last), n)
        logspace_rr_dp = 10._dp**logspace_rr_dp
        return
    end procedure

    module procedure logspace_ii_dp
        logspace_ii_dp = dlinspace(log10(real(first, kind=dp)), log10(real(last, kind=dp)), n)
        logspace_ii_dp = 10._dp**logspace_ii_dp
    end procedure 

    module procedure logspace_ri_dp
        logspace_ri_dp = dlinspace(log10(first), log10(real(last, kind=dp)), n)
        logspace_ri_dp = 10._dp**logspace_ri_dp
        return
    end procedure

    module procedure logspace_ir_dp
        logspace_ir_dp = dlinspace(log10(real(first, kind=dp)), log10(last), n)
        logspace_ir_dp = 10._dp**logspace_ir_dp
        return
    end procedure

    module procedure linspace_rr_qp
        integer :: i
        real(qp) :: step
        if (n<=1) print *, ('Error: in `linspace/logspace`, n should be > 1.')
        allocate (linspace_rr_qp(n))
        step = (last - first)/(n - 1)
        linspace_rr_qp = first + step*real([(i - 1, i=1, n)], qp)
        return
    end procedure

    module procedure linspace_ii_qp
        linspace_ii_qp = qlinspace(real(first, kind=qp), real(last, kind=qp), n)
    end procedure 

    module procedure linspace_ri_qp
        linspace_ri_qp = qlinspace(first, real(last, kind=qp), n)
        return
    end procedure

    module procedure linspace_ir_qp
        linspace_ir_qp = qlinspace(real(first, kind=qp), last, n)
        return
    end procedure

    module procedure logspace_rr_qp
        logspace_rr_qp = qlinspace(log10(first), log10(last), n)
        logspace_rr_qp = 10._qp**logspace_rr_qp
        return
    end procedure

    module procedure logspace_ii_qp
        logspace_ii_qp = qlinspace(log10(real(first, kind=qp)), log10(real(last, kind=qp)), n)
        logspace_ii_qp = 10._qp**logspace_ii_qp
    end procedure 

    module procedure logspace_ri_qp
        logspace_ri_qp = qlinspace(log10(first), log10(real(last, kind=qp)), n)
        logspace_ri_qp = 10._qp**logspace_ri_qp
        return
    end procedure

    module procedure logspace_ir_qp
        logspace_ir_qp = qlinspace(log10(real(first, kind=qp)), log10(last), n)
        logspace_ir_qp = 10._qp**logspace_ir_qp
        return
    end procedure


end submodule
