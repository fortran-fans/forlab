

submodule(forlab) forlab_randn
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    !! Default versions
    module function randn_0_default ()
        real(dp) :: u, v, s
        real(dp) :: randn_0_default 
        
        do
            u = 2.*randu() - 1.
            v = 2.*randu() - 1.
            s = u*u + v*v
            if ((s > 0.) .and. (s < 1.)) exit
        end do
        randn_0_default = u*sqrt(-2.0d0*log(s)/s)

        return
    end function
    
    module function randn_1_default (dim1)
        integer, intent(in) :: dim1
        integer :: i
        real(dp), allocatable :: randn_1_default (:)
        
        allocate (randn_1_default (dim1))
        do i = 1, dim1
            randn_1_default (i) = randn_0_default()
        end do

        return
    end function
    
    module function randn_2_default (dim1, dim2)
        integer, intent(in) :: dim1, dim2
        integer :: i, j
        real(dp), allocatable :: randn_2_default (:,:)
        
        allocate (randn_2_default (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_default (i, j) = randn_0_default()
            end do
        end do

        return
    end function
    
    module function randn_3_default (dim1, dim2, dim3)
        integer, intent(in) :: dim1, dim2, dim3
        integer :: i, j, k
        real(dp), allocatable :: randn_3_default (:,:,:)
        
        allocate (randn_3_default (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_default (i, j, k) = randn_0_default()
                end do
            end do
        end do

        return
    end function
    
    !! Multi-precision versions
    module function randn_0_sp (flag)
        real(sp) :: u, v, s
        real(sp) :: randn_0_sp 
        real(sp), intent(in) :: flag

        do
            u = 2._sp*randu(flag) - 1._sp
            v = 2._sp*randu(flag) - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do
        randn_0_sp = u*sqrt(-2.0_sp*log(s)/s)

        return
    end function

    module function randn_0_dp (flag)
        real(dp) :: u, v, s
        real(dp) :: randn_0_dp 
        real(dp), intent(in) :: flag

        do
            u = 2._dp*randu(flag) - 1._dp
            v = 2._dp*randu(flag) - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do
        randn_0_dp = u*sqrt(-2.0_dp*log(s)/s)

        return
    end function

    module function randn_0_qp (flag)
        real(qp) :: u, v, s
        real(qp) :: randn_0_qp 
        real(qp), intent(in) :: flag

        do
            u = 2._qp*randu(flag) - 1._qp
            v = 2._qp*randu(flag) - 1._qp
            s = u*u + v*v
            if ((s > 0._qp) .and. (s < 1._qp)) exit
        end do
        randn_0_qp = u*sqrt(-2.0_qp*log(s)/s)

        return
    end function

    module function randn_1_sp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        integer :: i
        real(sp), allocatable :: randn_1_sp (:)
        real(sp), intent(in) :: flag

        allocate (randn_1_sp (dim1))
        do i = 1, dim1
            randn_1_sp (i) = randn_0_sp (flag)
        end do

        return
    end function

    module function randn_1_dp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        integer :: i
        real(dp), allocatable :: randn_1_dp (:)
        real(dp), intent(in) :: flag

        allocate (randn_1_dp (dim1))
        do i = 1, dim1
            randn_1_dp (i) = randn_0_dp (flag)
        end do

        return
    end function

    module function randn_1_qp (dim1, flag)
        !! Unlike dynamic scripting languages, static languages generally
        !! have multiple precision variables, so we need to explicitly provide precision hints.
        integer, intent(in) :: dim1
        integer :: i
        real(qp), allocatable :: randn_1_qp (:)
        real(qp), intent(in) :: flag

        allocate (randn_1_qp (dim1))
        do i = 1, dim1
            randn_1_qp (i) = randn_0_qp (flag)
        end do

        return
    end function

    module function randn_2_sp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        integer :: i, j
        real(sp), allocatable :: randn_2_sp (:,:)
        real(sp), intent(in) :: flag

        allocate (randn_2_sp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_sp (i, j) = randn_0_sp (flag)
            end do
        end do

        return
    end function

    module function randn_2_dp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        integer :: i, j
        real(dp), allocatable :: randn_2_dp (:,:)
        real(dp), intent(in) :: flag

        allocate (randn_2_dp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_dp (i, j) = randn_0_dp (flag)
            end do
        end do

        return
    end function

    module function randn_2_qp (dim1, dim2, flag)
        integer, intent(in) :: dim1, dim2
        integer :: i, j
        real(qp), allocatable :: randn_2_qp (:,:)
        real(qp), intent(in) :: flag

        allocate (randn_2_qp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_qp (i, j) = randn_0_qp (flag)
            end do
        end do

        return
    end function

    module function randn_3_sp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        integer :: i, j, k
        real(sp), allocatable :: randn_3_sp (:,:,:)
        real(sp), intent(in) :: flag

        allocate (randn_3_sp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_sp (i, j, k) = randn_0_sp (flag)
                end do
            end do
        end do

        return
    end function

    module function randn_3_dp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        integer :: i, j, k
        real(dp), allocatable :: randn_3_dp (:,:,:)
        real(dp), intent(in) :: flag

        allocate (randn_3_dp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_dp (i, j, k) = randn_0_dp (flag)
                end do
            end do
        end do

        return
    end function

    module function randn_3_qp (dim1, dim2, dim3, flag)
        integer, intent(in) :: dim1, dim2, dim3
        integer :: i, j, k
        real(qp), allocatable :: randn_3_qp (:,:,:)
        real(qp), intent(in) :: flag

        allocate (randn_3_qp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_qp (i, j, k) = randn_0_qp (flag)
                end do
            end do
        end do

        return
    end function

end submodule
