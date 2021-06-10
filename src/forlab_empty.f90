
submodule(forlab) forlab_empty
    !! Version: experimental
    !!
    !! Create uninitialized matrices quickly, faster than `ones`/`zeros` function,
    !! and use `empty` function with caution.
    !!
    !!## Example
    !!    real, allocatable :: x(:, :)
    !!    x = empty(2, 3)
    use forlab_kinds
    implicit none
    
contains
    module procedure empty_1_sp
        integer :: ierr
        allocate (empty_1_sp(dim1), stat=ierr)
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

    module procedure empty_3_sp
        integer :: ierr
        allocate (empty_3_sp(dim1, dim2, dim3), stat=ierr)
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

    module procedure empty_2_dp
        integer :: ierr
        allocate (empty_2_dp(dim1, dim2), stat=ierr)
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

    module procedure empty_1_qp
        integer :: ierr
        allocate (empty_1_qp(dim1), stat=ierr)
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

    module procedure empty_3_qp
        integer :: ierr
        allocate (empty_3_qp(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

end submodule

