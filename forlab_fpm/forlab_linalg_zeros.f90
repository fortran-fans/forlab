
submodule(forlab_linalg) forlab_linalg_zeros

    implicit none

contains

    module procedure zeros_rsp   
        X = 0.0_sp
        return
    end procedure
    module procedure zeros_rdp   
        X = 0.0_dp
        return
    end procedure
    module procedure zeros_rqp   
        X = 0.0_qp
        return
    end procedure
    module procedure zeros_csp   
        X = cmplx(0.0_sp, 0.0_sp, sp)
        return
    end procedure
    module procedure zeros_cdp   
        X = cmplx(0.0_dp, 0.0_dp, dp)
        return
    end procedure
    module procedure zeros_cqp   
        X = cmplx(0.0_qp, 0.0_qp, qp)
        return
    end procedure
    module procedure zeros_iint8   
        X = 0
        return
    end procedure
    module procedure zeros_iint16   
        X = 0
        return
    end procedure
    module procedure zeros_iint32   
        X = 0
        return
    end procedure
    module procedure zeros_iint64   
        X = 0
        return
    end procedure

end submodule forlab_linalg_zeros
