
module forlab_stats

    use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
    implicit none
    private

    public :: mean, var, std
    public :: rng, randu, randn

    interface mean
        !! mean computes the mean value of an array.
        module function mean_1_sp(x) result(mean)
            real(sp), dimension(:), intent(in) :: x
            real(sp) :: mean
        end function mean_1_sp
        module function mean_2_sp(A, dim) result(mean)
            real(sp), dimension(:), allocatable :: mean
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: dim
        end function mean_2_sp
        module function mean_1_dp(x) result(mean)
            real(dp), dimension(:), intent(in) :: x
            real(dp) :: mean
        end function mean_1_dp
        module function mean_2_dp(A, dim) result(mean)
            real(dp), dimension(:), allocatable :: mean
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: dim
        end function mean_2_dp
        module function mean_1_qp(x) result(mean)
            real(qp), dimension(:), intent(in) :: x
            real(qp) :: mean
        end function mean_1_qp
        module function mean_2_qp(A, dim) result(mean)
            real(qp), dimension(:), allocatable :: mean
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: dim
        end function mean_2_qp
    end interface mean

    !> Version: Experimental
    !>
    !> Generate a normal distributed data scalar or vector.
    !> ([Specification](../page/specs/forlab_stats.html#randn))
    interface randn
        module function randn_0_sp(mean, std) result(random)
            real(sp), intent(in) :: mean, std
            real(sp) :: random
        end function randn_0_sp
        module function randn_1_sp(mean, std, ndim) result(random)
            real(sp), intent(in) :: mean, std
            integer, intent(in) :: ndim
            real(sp) :: random(ndim)
        end function randn_1_sp
        module function randn_0_dp(mean, std) result(random)
            real(dp), intent(in) :: mean, std
            real(dp) :: random
        end function randn_0_dp
        module function randn_1_dp(mean, std, ndim) result(random)
            real(dp), intent(in) :: mean, std
            integer, intent(in) :: ndim
            real(dp) :: random(ndim)
        end function randn_1_dp
        module function randn_0_qp(mean, std) result(random)
            real(qp), intent(in) :: mean, std
            real(qp) :: random
        end function randn_0_qp
        module function randn_1_qp(mean, std, ndim) result(random)
            real(qp), intent(in) :: mean, std
            integer, intent(in) :: ndim
            real(qp) :: random(ndim)
        end function randn_1_qp
    end interface randn

    !> Version: Experimental
    !>
    !> Generate an uniformly distributed data scalar or vector.
    !> ([Specification](../page/specs/forlab_stats.html#randomrandu))
    interface randu
        module function randu_0_rsp(start, end) result(random)
            real(sp), intent(in) :: start, end
            real(sp) :: random
        end function randu_0_rsp
        module function randu_1_rsp(start, end, ndim) result(random)
            real(sp), intent(in) :: start, end
            integer, intent(in) :: ndim
            real(sp) :: random(ndim)
        end function randu_1_rsp
        module function randu_0_rdp(start, end) result(random)
            real(dp), intent(in) :: start, end
            real(dp) :: random
        end function randu_0_rdp
        module function randu_1_rdp(start, end, ndim) result(random)
            real(dp), intent(in) :: start, end
            integer, intent(in) :: ndim
            real(dp) :: random(ndim)
        end function randu_1_rdp
        module function randu_0_rqp(start, end) result(random)
            real(qp), intent(in) :: start, end
            real(qp) :: random
        end function randu_0_rqp
        module function randu_1_rqp(start, end, ndim) result(random)
            real(qp), intent(in) :: start, end
            integer, intent(in) :: ndim
            real(qp) :: random(ndim)
        end function randu_1_rqp
        module function randu_0_iint8(start, end) result(random)
            integer(int8), intent(in) :: start, end
            integer(int8) :: random
        end function randu_0_iint8
        module function randu_1_iint8(start, end, ndim) result(random)
            integer(int8), intent(in) :: start, end
            integer, intent(in) :: ndim
            integer(int8) :: random(ndim)
        end function randu_1_iint8
        module function randu_0_iint16(start, end) result(random)
            integer(int16), intent(in) :: start, end
            integer(int16) :: random
        end function randu_0_iint16
        module function randu_1_iint16(start, end, ndim) result(random)
            integer(int16), intent(in) :: start, end
            integer, intent(in) :: ndim
            integer(int16) :: random(ndim)
        end function randu_1_iint16
        module function randu_0_iint32(start, end) result(random)
            integer(int32), intent(in) :: start, end
            integer(int32) :: random
        end function randu_0_iint32
        module function randu_1_iint32(start, end, ndim) result(random)
            integer(int32), intent(in) :: start, end
            integer, intent(in) :: ndim
            integer(int32) :: random(ndim)
        end function randu_1_iint32
        module function randu_0_iint64(start, end) result(random)
            integer(int64), intent(in) :: start, end
            integer(int64) :: random
        end function randu_0_iint64
        module function randu_1_iint64(start, end, ndim) result(random)
            integer(int64), intent(in) :: start, end
            integer, intent(in) :: ndim
            integer(int64) :: random(ndim)
        end function randu_1_iint64
    end interface randu

    interface
        module subroutine rng(seed)
            integer, intent(in), optional :: seed
        end subroutine rng
    end interface

    interface var
        !! `var` computes vector and matrix variances.
        !!([Specification](../module/forlab_var.html))
        real(sp) module function var_1_sp(x, w)
            real(sp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function var_1_sp
        module function var_2_sp(A, w, dim)
            real(sp), dimension(:), allocatable :: var_2_sp
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function var_2_sp
        real(dp) module function var_1_dp(x, w)
            real(dp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function var_1_dp
        module function var_2_dp(A, w, dim)
            real(dp), dimension(:), allocatable :: var_2_dp
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function var_2_dp
        real(qp) module function var_1_qp(x, w)
            real(qp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function var_1_qp
        module function var_2_qp(A, w, dim)
            real(qp), dimension(:), allocatable :: var_2_qp
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function var_2_qp
    end interface var
    interface std
        !! `std` computes vector and matrix standard deviations.
        !!([Specification](../module/forlab_var.html))
        real(sp) module function std_1_sp(x, w)
            real(sp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function std_1_sp
        module function std_2_sp(A, w, dim)
            real(sp), dimension(:), allocatable :: std_2_sp
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function std_2_sp
        real(dp) module function std_1_dp(x, w)
            real(dp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function std_1_dp
        module function std_2_dp(A, w, dim)
            real(dp), dimension(:), allocatable :: std_2_dp
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function std_2_dp
        real(qp) module function std_1_qp(x, w)
            real(qp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function std_1_qp
        module function std_2_qp(A, w, dim)
            real(qp), dimension(:), allocatable :: std_2_qp
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function std_2_qp
    end interface std

end module forlab_stats