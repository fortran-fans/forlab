submodule(forlab) forlab_ones
    !! Version: experimental
    !!
    !! ones creates array all of ones.
    !!
    !!## Syntax
    !!   x = ones(dim1)
    !!   A = ones(dim1, dim2)
    !!   X = ones(dim1, dim2, dim3)
    !!
    !!## Description
    !! `x = ones(dim1)` returns a dim1 vector of ones.
    !!
    !! `A = ones(dim1, dim2)` returns a dim1-by-dim2 matrix of ones.
    !!
    !! `X = ones(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of ones.
    !!
    !!## Examples
    !!    x = ones(3)
    !!    x =
    !!        1.  1.  1.
    !!
    !!    A = ones(3, 3)
    !!    A =
    !!        1.  1.  1.
    !!        1.  1.  1.
    !!        1.  1.  1.
    use forlab_kinds
    implicit none

contains

    module procedure ones_rsp   
        X = 1.0_sp
        return
    end procedure

    module procedure ones_rdp   
        X = 1.0_dp
        return
    end procedure

    module procedure ones_rqp   
        X = 1.0_qp
        return
    end procedure

    module procedure ones_csp   
        X = cmplx(1.0_sp, 1.0_sp, sp)
        return
    end procedure

    module procedure ones_cdp   
        X = cmplx(1.0_dp, 1.0_dp, dp)
        return
    end procedure

    module procedure ones_cqp   
        X = cmplx(1.0_qp, 1.0_qp, qp)
        return
    end procedure

    module procedure ones_iint8   
        X = 1
        return
    end procedure

    module procedure ones_iint16   
        X = 1
        return
    end procedure

    module procedure ones_iint32   
        X = 1
        return
    end procedure

    module procedure ones_iint64   
        X = 1
        return
    end procedure

end submodule
