

submodule(forlab) forlab_zeros
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    module procedure zeros_1_sp
        integer :: ierr
        allocate (zeros_1_sp (dim1), stat=ierr)
        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
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
            print *, "Error: in zeros, could not allocate array."
            stop
        else
            zeros_3_qp = 0.0_qp
        end if
        return
    end procedure

end submodule
