
submodule(forlab) forlab_zeros
    use forlab_kinds

contains
    module function zeros1(dim1)
        real(dp), dimension(:), allocatable :: zeros1
        integer, intent(in) :: dim1
        integer :: ierr

        allocate (zeros1(dim1), stat=ierr)
        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        else
            zeros1 = 0.0d0
        end if
        return
    end function
    module function zeros2(dim1, dim2)
        real(dp), dimension(:, :), allocatable :: zeros2
        integer, intent(in) :: dim1, dim2
        integer :: ierr

        allocate (zeros2(dim1, dim2), stat=ierr)
        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        else
            zeros2 = 0.0d0
        end if
        return
    end function
    module function zeros3(dim1, dim2, dim3)
        real(dp), dimension(:, :, :), allocatable :: zeros3
        integer, intent(in) :: dim1, dim2, dim3
        integer :: ierr

        allocate (zeros3(dim1, dim2, dim3), stat=ierr)
        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        else
            zeros3 = 0.0d0
        end if
        return
    end function
end submodule
