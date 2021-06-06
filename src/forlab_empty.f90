
submodule(forlab) forlab_empty
    use forlab_kinds
contains
    module procedure empty_1_default
        integer :: ierr
        allocate (empty_1_default(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

    module procedure empty_2_default
        integer :: ierr
        allocate (empty_2_default(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

    module procedure empty_3_default
        integer :: ierr
        allocate (empty_3_default(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

    module procedure empty_1_sp
        integer :: ierr
        allocate (empty_1_sp(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_1_dp
        integer :: ierr
        allocate (empty_1_dp(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_1_qp
        integer :: ierr
        allocate (empty_1_qp(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_2_sp
        integer :: ierr
        allocate (empty_2_sp(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_2_dp
        integer :: ierr
        allocate (empty_2_dp(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_2_qp
        integer :: ierr
        allocate (empty_2_qp(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_3_sp
        integer :: ierr
        allocate (empty_3_sp(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_3_dp
        integer :: ierr
        allocate (empty_3_dp(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
    module procedure empty_3_qp
        integer :: ierr
        allocate (empty_3_qp(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure
end submodule

