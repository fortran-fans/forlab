submodule(forlab) forlab_zeros
    !! Version: experimental
    !!## zeros: `szeros(zeros)/dzeros/qzeros`
    !! `zeros` creates array all of zeros.
    !!
    !!### Syntax
    !!    x = zeros(dim1)                
    !!    A = zeros(dim1, dim2)          
    !!    X = zeros(dim1, dim2, dim3)    
    !! 
    !!### Description
    !! 
    !! Unlike other dynamic languages, Fortran has a variety of precisions, 
    !! using integer input and outputting polymorphic return 
    !! values is difficult to achieve, so we set three `zeros` functions: 
    !! `szeros(zeros)/dzeros/qzeros`.
    !!
    !! `x = (s)zeros(dim1)` returns a dim1 vector of zeros with single precision.
    !! `x = dzeros(dim1)` returns a dim1 vector of zeros with double precision.
    !! `x = qzeros(dim1)` returns a dim1 vector of zeros with quadruple precision.
    !!
    !! `A = zeros(dim1, dim2)` returns a dim1-by-dim2 matrix of zeros.
    !!
    !! `X = zeros(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of zeros.
    !!
    !!### Examples
    !!    x = zeros(3)  
    !!    x =  
    !!        0.  0.  0.  
    !!
    !!    A = zeros(3, 3)  
    !!    A =  
    !!        0.  0.  0.  
    !!        0.  0.  0.  
    !!        0.  0.  0.
    use forlab_kinds
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

end submodule
