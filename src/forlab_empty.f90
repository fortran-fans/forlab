

submodule(forlab) forlab_empty
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
    
contains
    !! Default versions
    module function empty_1_default (dim1)
        integer, intent(in) :: dim1
        real(dp), allocatable :: empty_1_default (:)
        integer :: ierr

        allocate (empty_1_default (dim1), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_2_default (dim1, dim2)
        integer, intent(in) :: dim1, dim2
        real(dp), allocatable :: empty_2_default (:,:)
        integer :: ierr

        allocate (empty_2_default (dim1, dim2), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_3_default (dim1, dim2, dim3)
        integer, intent(in) :: dim1, dim2, dim3
        real(dp), allocatable :: empty_3_default (:,:,:)
        integer :: ierr

        allocate (empty_3_default (dim1, dim2, dim3), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    !! Multi-precision versions
    !! Unlike dynamic scripting languages, static languages generally
    !! have multiple precision variables, so we need to explicitly provide precision hints.
    module function empty_1_sp (dim1, flag)
        integer, intent(in) :: dim1
        real(sp), allocatable :: empty_1_sp (:)
        real(sp), intent(in) :: flag
        integer :: ierr

        allocate (empty_1_sp (dim1), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_1_dp (dim1, flag)
        integer, intent(in) :: dim1
        real(dp), allocatable :: empty_1_dp (:)
        real(dp), intent(in) :: flag
        integer :: ierr

        allocate (empty_1_dp (dim1), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_1_qp (dim1, flag)
        integer, intent(in) :: dim1
        real(qp), allocatable :: empty_1_qp (:)
        real(qp), intent(in) :: flag
        integer :: ierr

        allocate (empty_1_qp (dim1), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_2_sp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(sp), allocatable :: empty_2_sp (:,:)
        real(sp), intent(in) :: flag
        integer :: ierr

        allocate (empty_2_sp (dim1, dim2), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_2_dp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(dp), allocatable :: empty_2_dp (:,:)
        real(dp), intent(in) :: flag
        integer :: ierr

        allocate (empty_2_dp (dim1, dim2), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_2_qp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        real(qp), allocatable :: empty_2_qp (:,:)
        real(qp), intent(in) :: flag
        integer :: ierr

        allocate (empty_2_qp (dim1, dim2), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_3_sp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(sp), allocatable :: empty_3_sp (:,:,:)
        real(sp), intent(in) :: flag
        integer :: ierr

        allocate (empty_3_sp (dim1, dim2, dim3), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_3_dp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(dp), allocatable :: empty_3_dp (:,:,:)
        real(dp), intent(in) :: flag
        integer :: ierr

        allocate (empty_3_dp (dim1, dim2, dim3), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
    module function empty_3_qp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        real(qp), allocatable :: empty_3_qp (:,:,:)
        real(qp), intent(in) :: flag
        integer :: ierr

        allocate (empty_3_qp (dim1, dim2, dim3), stat=ierr)

        if (ierr .ne. 0) then
            print *, "Error: in zeros, could not allocate array."
            stop
        end if
        return
    end function
end submodule
