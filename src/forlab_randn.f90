

submodule(forlab) forlab_randn
    !! Version: experimental
    !!
    !! randn generates normally distributed random numbers using polar
    !! Box-Muller algorithm.
    !!
    !!## Syntax
    !!    x = randn()
    !!    x = randn(dim1)
    !!
    !!## Description
    !! `x = randn()` returns a single normally distributed random number with
    !! mean 0 and standard deviation 1.
    !!
    !! `x = randn(dim1)` returns a dim1 vector of normally distributed random
    !! numbers.
    !!
    !! `A = randn(dim1, dim2)` returns a dim1-by-dim2 matrix of normally
    !! distributed random numbers.
    !!
    !! `X = randn(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of normally distributed random numbers.
    !!
    !!## Examples
    !!    x = randn(3)
    !!        -1.22003853  -0.211721316   0.522971511
    use forlab_kinds
    implicit none
contains
    !! Default versions
    module procedure randn_0_sp
        real(sp) :: u, v, s
        real(sp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_sp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_sp
        end if

        do
            u = 2._sp*randu() - 1._sp
            v = 2._sp*randu() - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do
        randn_0_sp = opt_mean + u*sqrt(-2.0_sp*log(s)/s)*opt_std

        return
    end procedure

    module procedure randn_1_sp
        integer :: i
        real(sp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_sp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_sp
        end if

        allocate (randn_1_sp (dim1))
        do i = 1, dim1
            randn_1_sp (i) = randn_0_sp(mean=opt_mean, std=opt_std)
        end do

        return
    end procedure

    module procedure randn_2_sp
        integer :: i, j
        real(sp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_sp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_sp
        end if

        allocate (randn_2_sp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_sp (i, j) = randn_0_sp(mean=opt_mean, std=opt_std)
            end do
        end do

        return
    end procedure

    module procedure randn_3_sp
        integer :: i, j, k
        real(sp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_sp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_sp
        end if

        allocate (randn_3_sp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_sp (i, j, k) = randn_0_sp(mean=opt_mean, std=opt_std)
                end do
            end do
        end do

        return
    end procedure

    module procedure randn_0_dp
        real(dp) :: u, v, s
        real(dp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_dp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_dp
        end if

        do
            u = 2._dp*randu() - 1._dp
            v = 2._dp*randu() - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do
        randn_0_dp = opt_mean + u*sqrt(-2.0_dp*log(s)/s)*opt_std

        return
    end procedure

    module procedure randn_1_dp
        integer :: i
        real(dp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_dp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_dp
        end if

        allocate (randn_1_dp (dim1))
        do i = 1, dim1
            randn_1_dp (i) = randn_0_dp(mean=opt_mean, std=opt_std)
        end do

        return
    end procedure

    module procedure randn_2_dp
        integer :: i, j
        real(dp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_dp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_dp
        end if

        allocate (randn_2_dp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_dp (i, j) = randn_0_dp(mean=opt_mean, std=opt_std)
            end do
        end do

        return
    end procedure

    module procedure randn_3_dp
        integer :: i, j, k
        real(dp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_dp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_dp
        end if

        allocate (randn_3_dp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_dp (i, j, k) = randn_0_dp(mean=opt_mean, std=opt_std)
                end do
            end do
        end do

        return
    end procedure

    module procedure randn_0_qp
        real(qp) :: u, v, s
        real(qp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_qp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_qp
        end if

        do
            u = 2._qp*randu() - 1._qp
            v = 2._qp*randu() - 1._qp
            s = u*u + v*v
            if ((s > 0._qp) .and. (s < 1._qp)) exit
        end do
        randn_0_qp = opt_mean + u*sqrt(-2.0_qp*log(s)/s)*opt_std

        return
    end procedure

    module procedure randn_1_qp
        integer :: i
        real(qp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_qp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_qp
        end if

        allocate (randn_1_qp (dim1))
        do i = 1, dim1
            randn_1_qp (i) = randn_0_qp(mean=opt_mean, std=opt_std)
        end do

        return
    end procedure

    module procedure randn_2_qp
        integer :: i, j
        real(qp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_qp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_qp
        end if

        allocate (randn_2_qp (dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                randn_2_qp (i, j) = randn_0_qp(mean=opt_mean, std=opt_std)
            end do
        end do

        return
    end procedure

    module procedure randn_3_qp
        integer :: i, j, k
        real(qp) :: opt_mean, opt_std 
        if(present(mean)) then
            opt_mean = mean
        else
            opt_mean = 0.0_qp
        endif
        if (present(std)) then
            opt_std = std
        else
            opt_std = 1.0_qp
        end if

        allocate (randn_3_qp (dim1, dim2, dim3))
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim3
                    randn_3_qp (i, j, k) = randn_0_qp(mean=opt_mean, std=opt_std)
                end do
            end do
        end do

        return
    end procedure

end submodule
