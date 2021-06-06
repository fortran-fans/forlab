
submodule(forlab) forlab_empty
    use forlab_kinds
contains
    module procedure empty1
        integer :: ierr
        allocate (empty1(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

    module procedure empty2
        integer :: ierr
        allocate (empty2(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

    module procedure empty3
        integer :: ierr
        allocate (empty3(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print*,"ERROR: in empty, could not allocate array."
            stop
        end if
    end procedure

end submodule

