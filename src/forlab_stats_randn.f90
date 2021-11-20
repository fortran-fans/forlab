submodule(forlab_stats) forlab_stats_randn

    implicit none

contains

    module function randn_0_sp(mean, std) result(random)
        real(sp), intent(in) :: mean, std
        real(sp) :: random

        real(sp) :: u, v, s

        do
            call random_number(u)
            call random_number(v)
            u = 2._sp*u - 1._sp
            v = 2._sp*v - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do

        random = mean + u*sqrt(-2.0_sp*log(s)/s)*std

    end function randn_0_sp

    module function randn_1_sp(mean, std, ndim) result(random)
        real(sp), intent(in) :: mean, std
        integer, intent(in) :: ndim
        real(sp) :: random(ndim)

        integer :: i

        do i = 1, ndim
            random(i) = randn_0_sp(mean, std)
        end do

    end function randn_1_sp
    module function randn_0_dp(mean, std) result(random)
        real(dp), intent(in) :: mean, std
        real(dp) :: random

        real(dp) :: u, v, s

        do
            call random_number(u)
            call random_number(v)
            u = 2._dp*u - 1._dp
            v = 2._dp*v - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do

        random = mean + u*sqrt(-2.0_dp*log(s)/s)*std

    end function randn_0_dp

    module function randn_1_dp(mean, std, ndim) result(random)
        real(dp), intent(in) :: mean, std
        integer, intent(in) :: ndim
        real(dp) :: random(ndim)

        integer :: i

        do i = 1, ndim
            random(i) = randn_0_dp(mean, std)
        end do

    end function randn_1_dp

end submodule forlab_stats_randn
