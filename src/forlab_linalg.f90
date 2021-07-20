module forlab_linalg
    
    use stdlib_error, only: error_stop
    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64
    use forlab_sorting, only: argsort
    use stdlib_optval, only: optval
    implicit none
    private

    public :: is_square, is_symmetric
    public :: zeros, ones, linspace, logspace, seq
    public :: eye
    public :: horzcat, vertcat
    public :: diag, det, lu, matpow, qr, svd, trace, tril, triu, chol, norm, &
        diff
    public :: operator(.i.), operator(.x.), inv, outer, solve, svdsolve

    interface chol
        !! chol computes Cholesky's decomposition of a symmetric positive
        !! definite matrix.
        module function chol_sp (A) result(L)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable :: L
        end function
        module function chol_dp (A) result(L)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable :: L
        end function
        module function chol_qp (A) result(L)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable :: L
        end function
        !#:for k1,t1 in CMPLX_KINDS_TYPES
        !    module function chol_cqp (A) result(L)
        !        real(qp), dimension(:, :), intent(in) :: A
        !        real(qp), dimension(:, :), allocatable :: L
        !    end function
        !#:endfor
    end interface chol

    interface det
        !! det computes the matrix determinant.
        module function det_sp(A, outL, outU) result(det)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable, intent(inout), optional :: outL, outU
            real(sp) :: det
        end function det_sp
        module function det_dp(A, outL, outU) result(det)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable, intent(inout), optional :: outL, outU
            real(dp) :: det
        end function det_dp
        module function det_qp(A, outL, outU) result(det)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable, intent(inout), optional :: outL, outU
            real(qp) :: det
        end function det_qp
    end interface det

    interface diag
        !! diag creates diagonal matrix or get the diagonal of a matrix.
        module function diag1_sp(A) result(diag)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:), allocatable :: diag
        end function diag1_sp
        module function diag2_sp(x) result(diag)
            real(sp), dimension(:), intent(in) :: x
            real(sp), dimension(:, :), allocatable :: diag
        end function diag2_sp
        module function diag1_dp(A) result(diag)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:), allocatable :: diag
        end function diag1_dp
        module function diag2_dp(x) result(diag)
            real(dp), dimension(:), intent(in) :: x
            real(dp), dimension(:, :), allocatable :: diag
        end function diag2_dp
        module function diag1_qp(A) result(diag)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:), allocatable :: diag
        end function diag1_qp
        module function diag2_qp(x) result(diag)
            real(qp), dimension(:), intent(in) :: x
            real(qp), dimension(:, :), allocatable :: diag
        end function diag2_qp
    end interface diag

    !> Version: Experimental
    !>
    !> `diff` computes differences of arrays.
    interface diff
        pure module function diff_1_sp(x, n) result(result)
            real(sp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            real(sp), dimension(:), allocatable :: result
        end function diff_1_sp
        pure module function diff_2_sp(A, n, dim) result(result)
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            real(sp), dimension(:, :), allocatable :: result
        end function diff_2_sp
        pure module function diff_1_dp(x, n) result(result)
            real(dp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            real(dp), dimension(:), allocatable :: result
        end function diff_1_dp
        pure module function diff_2_dp(A, n, dim) result(result)
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            real(dp), dimension(:, :), allocatable :: result
        end function diff_2_dp
        pure module function diff_1_qp(x, n) result(result)
            real(qp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            real(qp), dimension(:), allocatable :: result
        end function diff_1_qp
        pure module function diff_2_qp(A, n, dim) result(result)
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            real(qp), dimension(:, :), allocatable :: result
        end function diff_2_qp
        pure module function diff_1_int8(x, n) result(result)
            integer(int8), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            integer(int8), dimension(:), allocatable :: result
        end function diff_1_int8
        pure module function diff_2_int8(A, n, dim) result(result)
            integer(int8), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            integer(int8), dimension(:, :), allocatable :: result
        end function diff_2_int8
        pure module function diff_1_int16(x, n) result(result)
            integer(int16), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            integer(int16), dimension(:), allocatable :: result
        end function diff_1_int16
        pure module function diff_2_int16(A, n, dim) result(result)
            integer(int16), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            integer(int16), dimension(:, :), allocatable :: result
        end function diff_2_int16
        pure module function diff_1_int32(x, n) result(result)
            integer(int32), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            integer(int32), dimension(:), allocatable :: result
        end function diff_1_int32
        pure module function diff_2_int32(A, n, dim) result(result)
            integer(int32), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            integer(int32), dimension(:, :), allocatable :: result
        end function diff_2_int32
        pure module function diff_1_int64(x, n) result(result)
            integer(int64), dimension(:), intent(in) :: x
            integer, intent(in), optional :: n
            integer(int64), dimension(:), allocatable :: result
        end function diff_1_int64
        pure module function diff_2_int64(A, n, dim) result(result)
            integer(int64), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: n, dim
            integer(int64), dimension(:, :), allocatable :: result
        end function diff_2_int64
    end interface diff

    interface eig
        !! eig computes eigenvalues and eigenvectors of symmetric matrix using Jacobi algorithm.
        module subroutine eig_sp(A,V,d,itermax)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable, intent(out) :: V
            real(sp), dimension(:), allocatable, intent(out) :: d
            integer, intent(in), optional :: itermax
        end subroutine eig_sp
        module subroutine eig_dp(A,V,d,itermax)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable, intent(out) :: V
            real(dp), dimension(:), allocatable, intent(out) :: d
            integer, intent(in), optional :: itermax
        end subroutine eig_dp
        module subroutine eig_qp(A,V,d,itermax)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable, intent(out) :: V
            real(qp), dimension(:), allocatable, intent(out) :: d
            integer, intent(in), optional :: itermax
        end subroutine eig_qp
    end interface eig

    interface eye
        module subroutine eye_sp(X)
            real(sp), intent(out) :: X(:, :)
        end subroutine eye_sp
        module subroutine eye_dp(X)
            real(dp), intent(out) :: X(:, :)
        end subroutine eye_dp
        module subroutine eye_qp(X)
            real(qp), intent(out) :: X(:, :)
        end subroutine eye_qp
    end interface eye

    interface horzcat
        module function horzcat_r_1_sp(x1, x2)
            real(sp), dimension(:, :), allocatable :: horzcat_r_1_sp
            real(sp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_r_2_sp(A1, A2)
            real(sp), dimension(:, :), allocatable :: horzcat_r_2_sp
            real(sp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_r_21_sp(A1, x2)
            real(sp), dimension(:, :), allocatable :: horzcat_r_21_sp
            real(sp), dimension(:, :), intent(in) :: A1
            real(sp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_r_12_sp(x1, A2)
            real(sp), dimension(:, :), allocatable :: horzcat_r_12_sp
            real(sp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_r_1_dp(x1, x2)
            real(dp), dimension(:, :), allocatable :: horzcat_r_1_dp
            real(dp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_r_2_dp(A1, A2)
            real(dp), dimension(:, :), allocatable :: horzcat_r_2_dp
            real(dp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_r_21_dp(A1, x2)
            real(dp), dimension(:, :), allocatable :: horzcat_r_21_dp
            real(dp), dimension(:, :), intent(in) :: A1
            real(dp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_r_12_dp(x1, A2)
            real(dp), dimension(:, :), allocatable :: horzcat_r_12_dp
            real(dp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_r_1_qp(x1, x2)
            real(qp), dimension(:, :), allocatable :: horzcat_r_1_qp
            real(qp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_r_2_qp(A1, A2)
            real(qp), dimension(:, :), allocatable :: horzcat_r_2_qp
            real(qp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_r_21_qp(A1, x2)
            real(qp), dimension(:, :), allocatable :: horzcat_r_21_qp
            real(qp), dimension(:, :), intent(in) :: A1
            real(qp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_r_12_qp(x1, A2)
            real(qp), dimension(:, :), allocatable :: horzcat_r_12_qp
            real(qp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_c_1_sp(x1, x2)
            complex(sp), dimension(:, :), allocatable :: horzcat_c_1_sp
            complex(sp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_c_2_sp(A1, A2)
            complex(sp), dimension(:, :), allocatable :: horzcat_c_2_sp
            complex(sp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_c_21_sp(A1, x2)
            complex(sp), dimension(:, :), allocatable :: horzcat_c_21_sp
            complex(sp), dimension(:, :), intent(in) :: A1
            complex(sp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_c_12_sp(x1, A2)
            complex(sp), dimension(:, :), allocatable :: horzcat_c_12_sp
            complex(sp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_c_1_dp(x1, x2)
            complex(dp), dimension(:, :), allocatable :: horzcat_c_1_dp
            complex(dp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_c_2_dp(A1, A2)
            complex(dp), dimension(:, :), allocatable :: horzcat_c_2_dp
            complex(dp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_c_21_dp(A1, x2)
            complex(dp), dimension(:, :), allocatable :: horzcat_c_21_dp
            complex(dp), dimension(:, :), intent(in) :: A1
            complex(dp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_c_12_dp(x1, A2)
            complex(dp), dimension(:, :), allocatable :: horzcat_c_12_dp
            complex(dp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_c_1_qp(x1, x2)
            complex(qp), dimension(:, :), allocatable :: horzcat_c_1_qp
            complex(qp), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_c_2_qp(A1, A2)
            complex(qp), dimension(:, :), allocatable :: horzcat_c_2_qp
            complex(qp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_c_21_qp(A1, x2)
            complex(qp), dimension(:, :), allocatable :: horzcat_c_21_qp
            complex(qp), dimension(:, :), intent(in) :: A1
            complex(qp), dimension(:), intent(in) :: x2
        end function
        module function horzcat_c_12_qp(x1, A2)
            complex(qp), dimension(:, :), allocatable :: horzcat_c_12_qp
            complex(qp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_i_1_int8(x1, x2)
            integer(int8), dimension(:, :), allocatable :: horzcat_i_1_int8
            integer(int8), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_i_2_int8(A1, A2)
            integer(int8), dimension(:, :), allocatable :: horzcat_i_2_int8
            integer(int8), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_i_21_int8(A1, x2)
            integer(int8), dimension(:, :), allocatable :: horzcat_i_21_int8
            integer(int8), dimension(:, :), intent(in) :: A1
            integer(int8), dimension(:), intent(in) :: x2
        end function
        module function horzcat_i_12_int8(x1, A2)
            integer(int8), dimension(:, :), allocatable :: horzcat_i_12_int8
            integer(int8), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_i_1_int16(x1, x2)
            integer(int16), dimension(:, :), allocatable :: horzcat_i_1_int16
            integer(int16), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_i_2_int16(A1, A2)
            integer(int16), dimension(:, :), allocatable :: horzcat_i_2_int16
            integer(int16), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_i_21_int16(A1, x2)
            integer(int16), dimension(:, :), allocatable :: horzcat_i_21_int16
            integer(int16), dimension(:, :), intent(in) :: A1
            integer(int16), dimension(:), intent(in) :: x2
        end function
        module function horzcat_i_12_int16(x1, A2)
            integer(int16), dimension(:, :), allocatable :: horzcat_i_12_int16
            integer(int16), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_i_1_int32(x1, x2)
            integer(int32), dimension(:, :), allocatable :: horzcat_i_1_int32
            integer(int32), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_i_2_int32(A1, A2)
            integer(int32), dimension(:, :), allocatable :: horzcat_i_2_int32
            integer(int32), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_i_21_int32(A1, x2)
            integer(int32), dimension(:, :), allocatable :: horzcat_i_21_int32
            integer(int32), dimension(:, :), intent(in) :: A1
            integer(int32), dimension(:), intent(in) :: x2
        end function
        module function horzcat_i_12_int32(x1, A2)
            integer(int32), dimension(:, :), allocatable :: horzcat_i_12_int32
            integer(int32), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function horzcat_i_1_int64(x1, x2)
            integer(int64), dimension(:, :), allocatable :: horzcat_i_1_int64
            integer(int64), dimension(:), intent(in) :: x1, x2
        end function
        module function horzcat_i_2_int64(A1, A2)
            integer(int64), dimension(:, :), allocatable :: horzcat_i_2_int64
            integer(int64), dimension(:, :), intent(in) :: A1, A2
        end function
        module function horzcat_i_21_int64(A1, x2)
            integer(int64), dimension(:, :), allocatable :: horzcat_i_21_int64
            integer(int64), dimension(:, :), intent(in) :: A1
            integer(int64), dimension(:), intent(in) :: x2
        end function
        module function horzcat_i_12_int64(x1, A2)
            integer(int64), dimension(:, :), allocatable :: horzcat_i_12_int64
            integer(int64), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
    end interface
    interface vertcat
        module function vertcat_r_1_sp(x1, x2)
            real(sp), dimension(:, :), allocatable :: vertcat_r_1_sp
            real(sp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_r_2_sp(A1, A2)
            real(sp), dimension(:, :), allocatable :: vertcat_r_2_sp
            real(sp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_r_21_sp(A1, x2)
            real(sp), dimension(:, :), allocatable :: vertcat_r_21_sp
            real(sp), dimension(:, :), intent(in) :: A1
            real(sp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_r_12_sp(x1, A2)
            real(sp), dimension(:, :), allocatable :: vertcat_r_12_sp
            real(sp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_r_1_dp(x1, x2)
            real(dp), dimension(:, :), allocatable :: vertcat_r_1_dp
            real(dp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_r_2_dp(A1, A2)
            real(dp), dimension(:, :), allocatable :: vertcat_r_2_dp
            real(dp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_r_21_dp(A1, x2)
            real(dp), dimension(:, :), allocatable :: vertcat_r_21_dp
            real(dp), dimension(:, :), intent(in) :: A1
            real(dp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_r_12_dp(x1, A2)
            real(dp), dimension(:, :), allocatable :: vertcat_r_12_dp
            real(dp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_r_1_qp(x1, x2)
            real(qp), dimension(:, :), allocatable :: vertcat_r_1_qp
            real(qp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_r_2_qp(A1, A2)
            real(qp), dimension(:, :), allocatable :: vertcat_r_2_qp
            real(qp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_r_21_qp(A1, x2)
            real(qp), dimension(:, :), allocatable :: vertcat_r_21_qp
            real(qp), dimension(:, :), intent(in) :: A1
            real(qp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_r_12_qp(x1, A2)
            real(qp), dimension(:, :), allocatable :: vertcat_r_12_qp
            real(qp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_c_1_sp(x1, x2)
            complex(sp), dimension(:, :), allocatable :: vertcat_c_1_sp
            complex(sp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_c_2_sp(A1, A2)
            complex(sp), dimension(:, :), allocatable :: vertcat_c_2_sp
            complex(sp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_c_21_sp(A1, x2)
            complex(sp), dimension(:, :), allocatable :: vertcat_c_21_sp
            complex(sp), dimension(:, :), intent(in) :: A1
            complex(sp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_c_12_sp(x1, A2)
            complex(sp), dimension(:, :), allocatable :: vertcat_c_12_sp
            complex(sp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_c_1_dp(x1, x2)
            complex(dp), dimension(:, :), allocatable :: vertcat_c_1_dp
            complex(dp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_c_2_dp(A1, A2)
            complex(dp), dimension(:, :), allocatable :: vertcat_c_2_dp
            complex(dp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_c_21_dp(A1, x2)
            complex(dp), dimension(:, :), allocatable :: vertcat_c_21_dp
            complex(dp), dimension(:, :), intent(in) :: A1
            complex(dp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_c_12_dp(x1, A2)
            complex(dp), dimension(:, :), allocatable :: vertcat_c_12_dp
            complex(dp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_c_1_qp(x1, x2)
            complex(qp), dimension(:, :), allocatable :: vertcat_c_1_qp
            complex(qp), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_c_2_qp(A1, A2)
            complex(qp), dimension(:, :), allocatable :: vertcat_c_2_qp
            complex(qp), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_c_21_qp(A1, x2)
            complex(qp), dimension(:, :), allocatable :: vertcat_c_21_qp
            complex(qp), dimension(:, :), intent(in) :: A1
            complex(qp), dimension(:), intent(in) :: x2
        end function
        module function vertcat_c_12_qp(x1, A2)
            complex(qp), dimension(:, :), allocatable :: vertcat_c_12_qp
            complex(qp), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_i_1_int8(x1, x2)
            integer(int8), dimension(:, :), allocatable :: vertcat_i_1_int8
            integer(int8), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_i_2_int8(A1, A2)
            integer(int8), dimension(:, :), allocatable :: vertcat_i_2_int8
            integer(int8), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_i_21_int8(A1, x2)
            integer(int8), dimension(:, :), allocatable :: vertcat_i_21_int8
            integer(int8), dimension(:, :), intent(in) :: A1
            integer(int8), dimension(:), intent(in) :: x2
        end function
        module function vertcat_i_12_int8(x1, A2)
            integer(int8), dimension(:, :), allocatable :: vertcat_i_12_int8
            integer(int8), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_i_1_int16(x1, x2)
            integer(int16), dimension(:, :), allocatable :: vertcat_i_1_int16
            integer(int16), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_i_2_int16(A1, A2)
            integer(int16), dimension(:, :), allocatable :: vertcat_i_2_int16
            integer(int16), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_i_21_int16(A1, x2)
            integer(int16), dimension(:, :), allocatable :: vertcat_i_21_int16
            integer(int16), dimension(:, :), intent(in) :: A1
            integer(int16), dimension(:), intent(in) :: x2
        end function
        module function vertcat_i_12_int16(x1, A2)
            integer(int16), dimension(:, :), allocatable :: vertcat_i_12_int16
            integer(int16), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_i_1_int32(x1, x2)
            integer(int32), dimension(:, :), allocatable :: vertcat_i_1_int32
            integer(int32), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_i_2_int32(A1, A2)
            integer(int32), dimension(:, :), allocatable :: vertcat_i_2_int32
            integer(int32), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_i_21_int32(A1, x2)
            integer(int32), dimension(:, :), allocatable :: vertcat_i_21_int32
            integer(int32), dimension(:, :), intent(in) :: A1
            integer(int32), dimension(:), intent(in) :: x2
        end function
        module function vertcat_i_12_int32(x1, A2)
            integer(int32), dimension(:, :), allocatable :: vertcat_i_12_int32
            integer(int32), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        module function vertcat_i_1_int64(x1, x2)
            integer(int64), dimension(:, :), allocatable :: vertcat_i_1_int64
            integer(int64), dimension(:), intent(in) :: x1, x2
        end function
        module function vertcat_i_2_int64(A1, A2)
            integer(int64), dimension(:, :), allocatable :: vertcat_i_2_int64
            integer(int64), dimension(:, :), intent(in) :: A1, A2
        end function
        module function vertcat_i_21_int64(A1, x2)
            integer(int64), dimension(:, :), allocatable :: vertcat_i_21_int64
            integer(int64), dimension(:, :), intent(in) :: A1
            integer(int64), dimension(:), intent(in) :: x2
        end function
        module function vertcat_i_12_int64(x1, A2)
            integer(int64), dimension(:, :), allocatable :: vertcat_i_12_int64
            integer(int64), dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
    end interface

    interface inv
        module function inv_rsp(A) result(inv)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable :: inv
        end function inv_rsp
        module function inv_rdp(A) result(inv)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable :: inv
        end function inv_rdp
        module function inv_rqp(A) result(inv)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable :: inv
        end function inv_rqp
        module function inv_csp(A) result(inv)
            complex(sp), dimension(:, :), intent(in) :: A
            complex(sp), dimension(:, :), allocatable :: inv
        end function inv_csp
        module function inv_cdp(A) result(inv)
            complex(dp), dimension(:, :), intent(in) :: A
            complex(dp), dimension(:, :), allocatable :: inv
        end function inv_cdp
        module function inv_cqp(A) result(inv)
            complex(qp), dimension(:, :), intent(in) :: A
            complex(qp), dimension(:, :), allocatable :: inv
        end function inv_cqp
    end interface inv

    interface is_square
        procedure :: is_square_rsp
        procedure :: is_square_rdp
        procedure :: is_square_rqp
        procedure :: is_square_csp
        procedure :: is_square_cdp
        procedure :: is_square_cqp
    end interface is_square

    interface is_symmetric
        procedure :: is_symmetric_rsp
        procedure :: is_symmetric_rdp
        procedure :: is_symmetric_rqp
        procedure :: is_symmetric_csp
        procedure :: is_symmetric_cdp
        procedure :: is_symmetric_cqp
    end interface is_symmetric

    interface linspace
        pure module subroutine linspace_sp(X, from, to)
            real(sp), intent(out) :: X(:)
            real(sp), intent(in) :: from, to
        end subroutine linspace_sp
        pure module subroutine linspace_dp(X, from, to)
            real(dp), intent(out) :: X(:)
            real(dp), intent(in) :: from, to
        end subroutine linspace_dp
        pure module subroutine linspace_qp(X, from, to)
            real(qp), intent(out) :: X(:)
            real(qp), intent(in) :: from, to
        end subroutine linspace_qp
    end interface linspace
    interface logspace
        pure module subroutine logspace_sp(X, from, to)
            real(sp), intent(out) :: X(:)
            real(sp), intent(in) :: from, to
        end subroutine logspace_sp
        pure module subroutine logspace_dp(X, from, to)
            real(dp), intent(out) :: X(:)
            real(dp), intent(in) :: from, to
        end subroutine logspace_dp
        pure module subroutine logspace_qp(X, from, to)
            real(qp), intent(out) :: X(:)
            real(qp), intent(in) :: from, to
        end subroutine logspace_qp
    end interface logspace

    interface lu
        !! lu computes the LU matrix factorization.
        module subroutine lu_sp (A, L, U)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable, intent(out) :: L, U
        end subroutine lu_sp
        module subroutine lu_dp (A, L, U)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable, intent(out) :: L, U
        end subroutine lu_dp
        module subroutine lu_qp (A, L, U)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable, intent(out) :: L, U
        end subroutine lu_qp
    end interface lu

    interface matpow
        !! Calculat matrix power
        module function matpow_sp(a,num)result(c)
            real(sp), dimension(:,:), intent(in) :: a
            real(sp),allocatable :: c(:,:)
            integer::num
        end function matpow_sp
        module function matpow_dp(a,num)result(c)
            real(dp), dimension(:,:), intent(in) :: a
            real(dp),allocatable :: c(:,:)
            integer::num
        end function matpow_dp
        module function matpow_qp(a,num)result(c)
            real(qp), dimension(:,:), intent(in) :: a
            real(qp),allocatable :: c(:,:)
            integer::num
        end function matpow_qp
    end interface matpow

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
    
    interface operator(.i.)
        !! Calculate the inverse of a real matrix.
        procedure inv_rsp
        procedure inv_rdp
        procedure inv_rqp
        procedure inv_csp
        procedure inv_cdp
        procedure inv_cqp
    end interface operator(.i.)

    interface operator(.x.)
        procedure :: rmut_sp
        procedure :: rmut_dp
        procedure :: rmut_qp
        procedure :: cmut_sp
        procedure :: cmut_dp
        procedure :: cmut_qp
        procedure :: rcmut_sp
        procedure :: rcmut_dp
        procedure :: rcmut_qp
        procedure :: crmut_sp
        procedure :: crmut_dp
        procedure :: crmut_qp
    end interface operator(.x.)

    interface ones
        elemental module subroutine ones_rsp(X)
            real(sp), intent(out) :: X
        end subroutine ones_rsp
        elemental module subroutine ones_rdp(X)
            real(dp), intent(out) :: X
        end subroutine ones_rdp
        elemental module subroutine ones_rqp(X)
            real(qp), intent(out) :: X
        end subroutine ones_rqp
        elemental module subroutine ones_csp(X)
            complex(sp), intent(out) :: X
        end subroutine ones_csp
        elemental module subroutine ones_cdp(X)
            complex(dp), intent(out) :: X
        end subroutine ones_cdp
        elemental module subroutine ones_cqp(X)
            complex(qp), intent(out) :: X
        end subroutine ones_cqp
        elemental module subroutine ones_iint8(X)
            integer(int8), intent(out) :: X
        end subroutine ones_iint8
        elemental module subroutine ones_iint16(X)
            integer(int16), intent(out) :: X
        end subroutine ones_iint16
        elemental module subroutine ones_iint32(X)
            integer(int32), intent(out) :: X
        end subroutine ones_iint32
        elemental module subroutine ones_iint64(X)
            integer(int64), intent(out) :: X
        end subroutine ones_iint64
    end interface ones

    interface outer
        module function outer_int8(x, y)
            integer(int8), dimension(:,:), allocatable :: outer_int8
            integer(int8), dimension(:), intent(in) :: x, y
        end function
        module function outer_int16(x, y)
            integer(int16), dimension(:,:), allocatable :: outer_int16
            integer(int16), dimension(:), intent(in) :: x, y
        end function
        module function outer_int32(x, y)
            integer(int32), dimension(:,:), allocatable :: outer_int32
            integer(int32), dimension(:), intent(in) :: x, y
        end function
        module function outer_int64(x, y)
            integer(int64), dimension(:,:), allocatable :: outer_int64
            integer(int64), dimension(:), intent(in) :: x, y
        end function
        module function outer_sp(x, y)
            real(sp), dimension(:,:), allocatable :: outer_sp
            real(sp), dimension(:), intent(in) :: x, y
        end function
        module function outer_dp(x, y)
            real(dp), dimension(:,:), allocatable :: outer_dp
            real(dp), dimension(:), intent(in) :: x, y
        end function
        module function outer_qp(x, y)
            real(qp), dimension(:,:), allocatable :: outer_qp
            real(qp), dimension(:), intent(in) :: x, y
        end function
    end interface

    interface qr
        module subroutine qr_sp(a,q,r,l)
            real(sp),intent(in)::a(:,:)
            real(sp),allocatable,intent(out)::q(:,:),r(:,:)
            integer,optional::l
        end subroutine qr_sp
        module subroutine qr_dp(a,q,r,l)
            real(dp),intent(in)::a(:,:)
            real(dp),allocatable,intent(out)::q(:,:),r(:,:)
            integer,optional::l
        end subroutine qr_dp
        module subroutine qr_qp(a,q,r,l)
            real(qp),intent(in)::a(:,:)
            real(qp),allocatable,intent(out)::q(:,:),r(:,:)
            integer,optional::l
        end subroutine qr_qp
    end interface qr

    interface seq
        !! seq returns evenly spaced vector.
        module subroutine seq_sp (X, from, to, by)
            real(sp), dimension(:), allocatable, intent(out) :: X
            real(sp), intent(in) :: from, to
            real(sp), optional, intent(in) :: by
        end subroutine seq_sp
        module subroutine seq_dp (X, from, to, by)
            real(dp), dimension(:), allocatable, intent(out) :: X
            real(dp), intent(in) :: from, to
            real(dp), optional, intent(in) :: by
        end subroutine seq_dp
        module subroutine seq_qp (X, from, to, by)
            real(qp), dimension(:), allocatable, intent(out) :: X
            real(qp), intent(in) :: from, to
            real(qp), optional, intent(in) :: by
        end subroutine seq_qp
        module subroutine seq_int8 (X, from, to, by)
            integer(int8), dimension(:), allocatable, intent(out) :: X
            integer(int8), intent(in) :: from, to
            integer(int8), optional, intent(in) :: by
        end subroutine seq_int8
        module subroutine seq_int16 (X, from, to, by)
            integer(int16), dimension(:), allocatable, intent(out) :: X
            integer(int16), intent(in) :: from, to
            integer(int16), optional, intent(in) :: by
        end subroutine seq_int16
        module subroutine seq_int32 (X, from, to, by)
            integer(int32), dimension(:), allocatable, intent(out) :: X
            integer(int32), intent(in) :: from, to
            integer(int32), optional, intent(in) :: by
        end subroutine seq_int32
        module subroutine seq_int64 (X, from, to, by)
            integer(int64), dimension(:), allocatable, intent(out) :: X
            integer(int64), intent(in) :: from, to
            integer(int64), optional, intent(in) :: by
        end subroutine seq_int64
    end interface seq

    interface solve
        module function solve_sp(A, b) result(x)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:), intent(in) :: b
            real(sp), dimension(:), allocatable :: x
        end function solve_sp
        module function solve_dp(A, b) result(x)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:), intent(in) :: b
            real(dp), dimension(:), allocatable :: x
        end function solve_dp
        module function solve_qp(A, b) result(x)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:), intent(in) :: b
            real(qp), dimension(:), allocatable :: x
        end function solve_qp
    end interface solve

    interface svd
        module subroutine svd_sp(a, w, u, v , d, ierr)
            real(sp), dimension(:, :), intent(in) :: a
            real(sp), dimension(:), allocatable, intent(out) :: w
            real(sp), dimension(:, :), allocatable, intent(out), optional :: u, v
            integer, intent(out), optional :: ierr
            logical,intent(in),optional ::d
        end subroutine svd_sp
        module subroutine svd_dp(a, w, u, v , d, ierr)
            real(dp), dimension(:, :), intent(in) :: a
            real(dp), dimension(:), allocatable, intent(out) :: w
            real(dp), dimension(:, :), allocatable, intent(out), optional :: u, v
            integer, intent(out), optional :: ierr
            logical,intent(in),optional ::d
        end subroutine svd_dp
        module subroutine svd_qp(a, w, u, v , d, ierr)
            real(qp), dimension(:, :), intent(in) :: a
            real(qp), dimension(:), allocatable, intent(out) :: w
            real(qp), dimension(:, :), allocatable, intent(out), optional :: u, v
            integer, intent(out), optional :: ierr
            logical,intent(in),optional ::d
        end subroutine svd_qp
    end interface svd

    interface svdsolve
        module function svdsolve_sp(A, b, cutoff) result(x)
            real(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:), intent(in) :: b
            real(sp), dimension(:), allocatable :: x
            integer, intent(in), optional :: cutoff
        end function svdsolve_sp
        module function svdsolve_dp(A, b, cutoff) result(x)
            real(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:), intent(in) :: b
            real(dp), dimension(:), allocatable :: x
            integer, intent(in), optional :: cutoff
        end function svdsolve_dp
        module function svdsolve_qp(A, b, cutoff) result(x)
            real(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:), intent(in) :: b
            real(qp), dimension(:), allocatable :: x
            integer, intent(in), optional :: cutoff
        end function svdsolve_qp
    end interface svdsolve

    interface trace
        procedure :: trace_sp
        procedure :: trace_dp
        procedure :: trace_qp
    end interface trace

    interface tril
        module function tril_int8(A,k)
            integer(int8), dimension(:, :), allocatable :: tril_int8
            integer(int8), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_int8
        module function tril_int16(A,k)
            integer(int16), dimension(:, :), allocatable :: tril_int16
            integer(int16), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_int16
        module function tril_int32(A,k)
            integer(int32), dimension(:, :), allocatable :: tril_int32
            integer(int32), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_int32
        module function tril_int64(A,k)
            integer(int64), dimension(:, :), allocatable :: tril_int64
            integer(int64), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_int64
        module function tril_sp(A,k)
            real(sp), dimension(:, :), allocatable :: tril_sp
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_sp
        module function tril_dp(A,k)
            real(dp), dimension(:, :), allocatable :: tril_dp
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_dp
        module function tril_qp(A,k)
            real(qp), dimension(:, :), allocatable :: tril_qp
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_qp
        module function tril_csp(A,k)
            complex(sp), dimension(:, :), allocatable :: tril_csp
            complex(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_csp
        module function tril_cdp(A,k)
            complex(dp), dimension(:, :), allocatable :: tril_cdp
            complex(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_cdp
        module function tril_cqp(A,k)
            complex(qp), dimension(:, :), allocatable :: tril_cqp
            complex(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_cqp
    end interface tril

    interface triu
        module function triu_int8(A,k)
            integer(int8), dimension(:, :), allocatable :: triu_int8
            integer(int8), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_int8
        module function triu_int16(A,k)
            integer(int16), dimension(:, :), allocatable :: triu_int16
            integer(int16), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_int16
        module function triu_int32(A,k)
            integer(int32), dimension(:, :), allocatable :: triu_int32
            integer(int32), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_int32
        module function triu_int64(A,k)
            integer(int64), dimension(:, :), allocatable :: triu_int64
            integer(int64), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_int64
        module function triu_sp(A,k)
            real(sp), dimension(:, :), allocatable :: triu_sp
            real(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_sp
        module function triu_dp(A,k)
            real(dp), dimension(:, :), allocatable :: triu_dp
            real(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_dp
        module function triu_qp(A,k)
            real(qp), dimension(:, :), allocatable :: triu_qp
            real(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_qp
        module function triu_csp(A,k)
            complex(sp), dimension(:, :), allocatable :: triu_csp
            complex(sp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_csp
        module function triu_cdp(A,k)
            complex(dp), dimension(:, :), allocatable :: triu_cdp
            complex(dp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_cdp
        module function triu_cqp(A,k)
            complex(qp), dimension(:, :), allocatable :: triu_cqp
            complex(qp), dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_cqp
    end interface triu

    interface zeros
        elemental module subroutine zeros_rsp(X)
            real(sp), intent(out) :: X
        end subroutine zeros_rsp
        elemental module subroutine zeros_rdp(X)
            real(dp), intent(out) :: X
        end subroutine zeros_rdp
        elemental module subroutine zeros_rqp(X)
            real(qp), intent(out) :: X
        end subroutine zeros_rqp
        elemental module subroutine zeros_csp(X)
            complex(sp), intent(out) :: X
        end subroutine zeros_csp
        elemental module subroutine zeros_cdp(X)
            complex(dp), intent(out) :: X
        end subroutine zeros_cdp
        elemental module subroutine zeros_cqp(X)
            complex(qp), intent(out) :: X
        end subroutine zeros_cqp
        elemental module subroutine zeros_iint8(X)
            integer(int8), intent(out) :: X
        end subroutine zeros_iint8
        elemental module subroutine zeros_iint16(X)
            integer(int16), intent(out) :: X
        end subroutine zeros_iint16
        elemental module subroutine zeros_iint32(X)
            integer(int32), intent(out) :: X
        end subroutine zeros_iint32
        elemental module subroutine zeros_iint64(X)
            integer(int64), intent(out) :: X
        end subroutine zeros_iint64
    end interface zeros

contains

    function rmut_sp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(sp), intent(in) :: m1(:, :), m2(:, :)
        real(sp) :: res(size(m1, 1), size(m2, 2))   


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rmut_sp
    function rmut_dp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(dp), intent(in) :: m1(:, :), m2(:, :)
        real(dp) :: res(size(m1, 1), size(m2, 2))   


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rmut_dp
    function rmut_qp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(qp), intent(in) :: m1(:, :), m2(:, :)
        real(qp) :: res(size(m1, 1), size(m2, 2))   


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rmut_qp
    function cmut_sp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(sp), intent(in) :: m1(:, :), m2(:, :)
        complex(sp) :: res(size(m1, 1), size(m2, 2))  


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function cmut_sp
    function cmut_dp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(dp), intent(in) :: m1(:, :), m2(:, :)
        complex(dp) :: res(size(m1, 1), size(m2, 2))  


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function cmut_dp
    function cmut_qp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(qp), intent(in) :: m1(:, :), m2(:, :)
        complex(qp) :: res(size(m1, 1), size(m2, 2))  


        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function cmut_qp
    function rcmut_sp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(sp), intent(in) :: m1(:, :)
        complex(sp), intent(in) :: m2(:, :)
        complex(sp) :: res(size(m1, 1), size(m2, 2))
        

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rcmut_sp
    function rcmut_dp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(dp), intent(in) :: m1(:, :)
        complex(dp), intent(in) :: m2(:, :)
        complex(dp) :: res(size(m1, 1), size(m2, 2))
        

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rcmut_dp
    function rcmut_qp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        real(qp), intent(in) :: m1(:, :)
        complex(qp), intent(in) :: m2(:, :)
        complex(qp) :: res(size(m1, 1), size(m2, 2))
        

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function rcmut_qp
    function crmut_sp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(sp), intent(in) :: m1(:, :)
        real(sp), intent(in) :: m2(:, :)
        complex(sp) :: res(size(m1,1), size(m2, 2))

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function crmut_sp
    function crmut_dp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(dp), intent(in) :: m1(:, :)
        real(dp), intent(in) :: m2(:, :)
        complex(dp) :: res(size(m1,1), size(m2, 2))

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function crmut_dp
    function crmut_qp(m1, m2) result(res)
        !! integer(int64) matrix multiplication
        complex(qp), intent(in) :: m1(:, :)
        real(qp), intent(in) :: m2(:, :)
        complex(qp) :: res(size(m1,1), size(m2, 2))

        if (size(m1, 2) == size(m2, 1)) then
            res = matmul(m1, m2)
        else
            call error_stop('Error: size(matrix_1, 2) /= size(matrix_2, 1)') 
        end if
    end function crmut_qp

    function is_square_rsp(A) result(is_square)
        !! real(sp) matrix is square or not.
        real(sp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_rsp
    function is_square_rdp(A) result(is_square)
        !! real(dp) matrix is square or not.
        real(dp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_rdp
    function is_square_rqp(A) result(is_square)
        !! real(qp) matrix is square or not.
        real(qp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_rqp
    function is_square_csp(A) result(is_square)
        !! complex(sp) matrix is square or not.
        complex(sp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_csp
    function is_square_cdp(A) result(is_square)
        !! complex(dp) matrix is square or not.
        complex(dp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_cdp
    function is_square_cqp(A) result(is_square)
        !! complex(qp) matrix is square or not.
        complex(qp), intent(in) :: A(:,:)
        logical :: is_square
        is_square = .false.
        if (size(A, 1) == size(A, 2)) is_square = .true.
        return
    end function is_square_cqp

    function is_symmetric_rsp(A) result(is_symmetric)
        !! real(sp) matrix is symmetric or not.
        real(sp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_rsp
    function is_symmetric_rdp(A) result(is_symmetric)
        !! real(dp) matrix is symmetric or not.
        real(dp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_rdp
    function is_symmetric_rqp(A) result(is_symmetric)
        !! real(qp) matrix is symmetric or not.
        real(qp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_rqp
    function is_symmetric_csp(A) result(is_symmetric)
        !! complex(sp) matrix is symmetric or not.
        complex(sp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_csp
    function is_symmetric_cdp(A) result(is_symmetric)
        !! complex(dp) matrix is symmetric or not.
        complex(dp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_cdp
    function is_symmetric_cqp(A) result(is_symmetric)
        !! complex(qp) matrix is symmetric or not.
        complex(qp), intent(in) :: A(:,:)
        logical :: is_symmetric
        integer :: i, j, n
        is_symmetric = .true.
        if (.not. is_square(A)) then
            is_symmetric = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        is_symmetric = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end function is_symmetric_cqp

    function trace_sp(A) result(trace)
        real(sp), dimension(:, :), intent(in) :: A
        real(sp) :: trace

        trace = sum(diag(A))

    end function trace_sp
    function trace_dp(A) result(trace)
        real(dp), dimension(:, :), intent(in) :: A
        real(dp) :: trace

        trace = sum(diag(A))

    end function trace_dp
    function trace_qp(A) result(trace)
        real(qp), dimension(:, :), intent(in) :: A
        real(qp) :: trace

        trace = sum(diag(A))

    end function trace_qp


end module forlab_linalg