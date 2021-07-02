
module forlab_stats
    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64
    use stdlib_optval, only: optval
    implicit none
    private

    public :: mean, var, std
    public :: rng, randu, randn, norm, chi2rand

    interface chi2rand
        impure elemental module subroutine chi2rand_sp(X, v)
            implicit none
            real(sp), intent(out) :: X
            integer, intent(in) :: v
        end subroutine
        impure elemental module subroutine chi2rand_dp(X, v)
            implicit none
            real(dp), intent(out) :: X
            integer, intent(in) :: v
        end subroutine
        impure elemental module subroutine chi2rand_qp(X, v)
            implicit none
            real(qp), intent(out) :: X
            integer, intent(in) :: v
        end subroutine
    end interface chi2rand

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

    interface norm
        !! norm computes vector and matrix norms.
        module function norm1_sp(x,p)result(norm1)
            real(sp), dimension(:), intent(in) :: x
            real(sp), intent(in), optional :: p
            real(sp):: norm1
        end function norm1_sp
        module function norm2_sp(A,p)result(norm2)
            real(sp), dimension(:,:), intent(in) :: A
            real(sp), intent(in), optional :: p
            real(sp):: norm2
        end function norm2_sp
        module function norm1_dp(x,p)result(norm1)
            real(dp), dimension(:), intent(in) :: x
            real(dp), intent(in), optional :: p
            real(dp):: norm1
        end function norm1_dp
        module function norm2_dp(A,p)result(norm2)
            real(dp), dimension(:,:), intent(in) :: A
            real(dp), intent(in), optional :: p
            real(dp):: norm2
        end function norm2_dp
        module function norm1_qp(x,p)result(norm1)
            real(qp), dimension(:), intent(in) :: x
            real(qp), intent(in), optional :: p
            real(qp):: norm1
        end function norm1_qp
        module function norm2_qp(A,p)result(norm2)
            real(qp), dimension(:,:), intent(in) :: A
            real(qp), intent(in), optional :: p
            real(qp):: norm2
        end function norm2_qp
    end interface norm

    interface randn
        impure elemental module subroutine randn_sp(X, mean, std)
            real(sp), intent(out) :: X
            real(sp), optional, intent(in) :: mean, std
        end subroutine
        impure elemental module subroutine randn_dp(X, mean, std)
            real(dp), intent(out) :: X
            real(dp), optional, intent(in) :: mean, std
        end subroutine
        impure elemental module subroutine randn_qp(X, mean, std)
            real(qp), intent(out) :: X
            real(qp), optional, intent(in) :: mean, std
        end subroutine
    end interface

    interface randu
        impure elemental module subroutine randu_rsp(X, from, to)
            real(sp), intent(out) :: X
            real(sp), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_rdp(X, from, to)
            real(dp), intent(out) :: X
            real(dp), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_rqp(X, from, to)
            real(qp), intent(out) :: X
            real(qp), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_iint8(X, from, to)
            integer(int8), intent(out) :: X
            integer(int8), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_iint16(X, from, to)
            integer(int16), intent(out) :: X
            integer(int16), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_iint32(X, from, to)
            integer(int32), intent(out) :: X
            integer(int32), optional, intent(in) :: from, to 
        end subroutine
        impure elemental module subroutine randu_iint64(X, from, to)
            integer(int64), intent(out) :: X
            integer(int64), optional, intent(in) :: from, to 
        end subroutine
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