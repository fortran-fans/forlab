
submodule(forlab_linalg) forlab_linalg_ones

    implicit none

contains

    module procedure ones_rsp   
        X = 1.0_sp
        return
    end procedure ones_rsp 
    module procedure ones_rdp   
        X = 1.0_dp
        return
    end procedure ones_rdp 
    module procedure ones_rqp   
        X = 1.0_qp
        return
    end procedure ones_rqp 
    module procedure ones_csp   
        X = cmplx(1.0_sp, 1.0_sp, sp)
        return
    end procedure ones_csp 
    module procedure ones_cdp   
        X = cmplx(1.0_dp, 1.0_dp, dp)
        return
    end procedure ones_cdp 
    module procedure ones_cqp   
        X = cmplx(1.0_qp, 1.0_qp, qp)
        return
    end procedure ones_cqp 
    module procedure ones_iint8   
        X = 1
        return
    end procedure ones_iint8 
    module procedure ones_iint16   
        X = 1
        return
    end procedure ones_iint16 
    module procedure ones_iint32   
        X = 1
        return
    end procedure ones_iint32 
    module procedure ones_iint64   
        X = 1
        return
    end procedure ones_iint64 

end submodule forlab_linalg_ones
