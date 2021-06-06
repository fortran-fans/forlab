

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
    module procedure randn_0_default
        real(dp) :: u, v, s
        do
            u = 2.d0*randu() - 1.d0
            v = 2.d0*randu() - 1.d0
            s = u*u + v*v
            if ((s > 0.d0) .and. (s < 1.d0)) exit
        end do
        randn_0_default = u*sqrt(-2.0d0*log(s)/s)

        return
    end procedure
    
    module procedure randn_1_default
        integer :: i
        allocate (randn_1_default (dim1))
        do i = 1, dim1
            randn_1_default (i) = randn_0_default()
        end do

        return
    end procedure
    
    module procedure randn_2_default
        integer :: i, j
        allocate (randn_2_default (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_default (i, j) = randn_0_default()
            end do
        end do

        return
    end procedure
    
    module procedure randn_3_default
        integer :: i, j, k
        allocate (randn_3_default (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_default (i, j, k) = randn_0_default()
                end do
            end do
        end do

        return
    end procedure
    
    !! Multi-precision versions
    module procedure randn_0_sp
        real(sp) :: u, v, s

        do
            u = 2._sp*randu(flag) - 1._sp
            v = 2._sp*randu(flag) - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do
        randn_0_sp = u*sqrt(-2.0_sp*log(s)/s)

        return
    end procedure

    module procedure randn_0_dp
        real(dp) :: u, v, s

        do
            u = 2._dp*randu(flag) - 1._dp
            v = 2._dp*randu(flag) - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do
        randn_0_dp = u*sqrt(-2.0_dp*log(s)/s)

        return
    end procedure

    module procedure randn_0_qp
        real(qp) :: u, v, s

        do
            u = 2._qp*randu(flag) - 1._qp
            v = 2._qp*randu(flag) - 1._qp
            s = u*u + v*v
            if ((s > 0._qp) .and. (s < 1._qp)) exit
        end do
        randn_0_qp = u*sqrt(-2.0_qp*log(s)/s)

        return
    end procedure

    module procedure randn_1_sp
        integer :: i

        allocate (randn_1_sp (dim1))
        do i = 1, dim1
            randn_1_sp (i) = randn_0_sp (flag)
        end do

        return
    end procedure

    module procedure randn_1_dp
        integer :: i

        allocate (randn_1_dp (dim1))
        do i = 1, dim1
            randn_1_dp (i) = randn_0_dp (flag)
        end do

        return
    end procedure

    module procedure randn_1_qp
        integer :: i

        allocate (randn_1_qp (dim1))
        do i = 1, dim1
            randn_1_qp (i) = randn_0_qp (flag)
        end do

        return
    end procedure

    module procedure randn_2_sp
        integer :: i, j

        allocate (randn_2_sp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_sp (i, j) = randn_0_sp (flag)
            end do
        end do

        return
    end procedure

    module procedure randn_2_dp
        integer :: i, j

        allocate (randn_2_dp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_dp (i, j) = randn_0_dp (flag)
            end do
        end do

        return
    end procedure

    module procedure randn_2_qp
        integer :: i, j

        allocate (randn_2_qp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_qp (i, j) = randn_0_qp (flag)
            end do
        end do

        return
    end procedure

    module procedure randn_3_sp
        integer :: i, j, k

        allocate (randn_3_sp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_sp (i, j, k) = randn_0_sp (flag)
                end do
            end do
        end do

        return
    end procedure

    module procedure randn_3_dp
        integer :: i, j, k

        allocate (randn_3_dp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_dp (i, j, k) = randn_0_dp (flag)
                end do
            end do
        end do

        return
    end procedure

    module procedure randn_3_qp
        integer :: i, j, k

        allocate (randn_3_qp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_qp (i, j, k) = randn_0_qp (flag)
                end do
            end do
        end do

        return
    end procedure

end submodule
