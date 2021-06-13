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
    module procedure zeros_1_sp
        integer :: ierr
        allocate (zeros_1_sp (dim1), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_1_sp = 0.0_sp
        end if
        return
    end procedure

    module procedure zeros_2_sp
        integer :: ierr
        allocate (zeros_2_sp (dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_2_sp = 0.0_sp
        end if
        return
    end procedure

    module procedure zeros_3_sp
        integer :: ierr
        allocate (zeros_3_sp (dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_3_sp = 0.0_sp
        end if
        return
    end procedure

    module procedure zeros_1_dp
        integer :: ierr
        allocate (zeros_1_dp (dim1), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_1_dp = 0.0_dp
        end if
        return
    end procedure

    module procedure zeros_2_dp
        integer :: ierr
        allocate (zeros_2_dp (dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_2_dp = 0.0_dp
        end if
        return
    end procedure

    module procedure zeros_3_dp
        integer :: ierr
        allocate (zeros_3_dp (dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_3_dp = 0.0_dp
        end if
        return
    end procedure

    module procedure zeros_1_qp
        integer :: ierr
        allocate (zeros_1_qp (dim1), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_1_qp = 0.0_qp
        end if
        return
    end procedure

    module procedure zeros_2_qp
        integer :: ierr
        allocate (zeros_2_qp (dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_2_qp = 0.0_qp
        end if
        return
    end procedure

    module procedure zeros_3_qp
        integer :: ierr
        allocate (zeros_3_qp (dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            call disp("Error: in (s/d/q)zeros, could not allocate array.")
            stop
        else
            zeros_3_qp = 0.0_qp
        end if
        return
    end procedure

end submodule
