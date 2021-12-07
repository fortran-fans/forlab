module forlab_math

    use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
    use stdlib_optval, only: optval
    implicit none
    private

    public :: angle
    public :: cosd, sind, tand
    public :: acosd, asind, atand
    public :: arange, signum

    public :: is_close, all_close
    public :: cross, operator(.c.)

    interface acosd
        !! degree circular functions
        pure elemental module function acosd_sp(x)
            real(sp), intent(in)::x
            real(sp)::acosd_sp
        end function acosd_sp
        pure elemental module function acosd_dp(x)
            real(dp), intent(in)::x
            real(dp)::acosd_dp
        end function acosd_dp
    end interface acosd
    interface asind
        !! degree circular functions
        pure elemental module function asind_sp(x)
            real(sp), intent(in)::x
            real(sp)::asind_sp
        end function asind_sp
        pure elemental module function asind_dp(x)
            real(dp), intent(in)::x
            real(dp)::asind_dp
        end function asind_dp
    end interface asind
    interface atand
        !! degree circular functions
        pure elemental module function atand_sp(x)
            real(sp), intent(in)::x
            real(sp)::atand_sp
        end function atand_sp
        pure elemental module function atand_dp(x)
            real(dp), intent(in)::x
            real(dp)::atand_dp
        end function atand_dp
    end interface atand

    interface cosd
        pure elemental module function cosd_sp(x)
            real(sp), intent(in)::x
            real(sp)::cosd_sp
        end function cosd_sp
        pure elemental module function cosd_dp(x)
            real(dp), intent(in)::x
            real(dp)::cosd_dp
        end function cosd_dp
    end interface cosd
    interface sind
        pure elemental module function sind_sp(x)
            real(sp), intent(in)::x
            real(sp)::sind_sp
        end function sind_sp
        pure elemental module function sind_dp(x)
            real(dp), intent(in)::x
            real(dp)::sind_dp
        end function sind_dp
    end interface sind
    interface tand
        pure elemental module function tand_sp(x)
            real(sp), intent(in)::x
            real(sp)::tand_sp
        end function tand_sp
        pure elemental module function tand_dp(x)
            real(dp), intent(in)::x
            real(dp)::tand_dp
        end function tand_dp
    end interface tand

    interface angle
        !! Version: experimental
        !!
        !! angle compute the phase angle.
        !!([Interface](../interface/angle.html))
        module procedure :: angle_sp
        pure module function angle_2_sp(x, y) result(angle)
            real(sp), dimension(3), intent(in) :: x, y
            real(sp) :: angle
        end function angle_2_sp
        module procedure :: angle_dp
        pure module function angle_2_dp(x, y) result(angle)
            real(dp), dimension(3), intent(in) :: x, y
            real(dp) :: angle
        end function angle_2_dp
    end interface angle

    !> Version: experimental
    !>
    !> Returns a boolean scalar/array where two scalar/arrays are element-wise equal within a tolerance.
    !> ([Specification](../page/specs/forlab_math.html#is_close))
    interface is_close
        elemental module logical function is_close_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_rsp
        elemental module logical function is_close_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_rdp
        elemental module logical function is_close_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_csp
        elemental module logical function is_close_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_cdp
    end interface is_close

    !> Version: experimental
    !>
    !> Returns a boolean scalar where two arrays are element-wise equal within a tolerance.
    !> ([Specification](../page/specs/forlab_math.html#all_close))
    interface all_close
        logical pure module function all_close_1_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:), b(:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_rsp
        logical pure module function all_close_2_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:, :), b(:, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_rsp
        logical pure module function all_close_3_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:, :, :), b(:, :, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_rsp
        logical pure module function all_close_4_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:, :, :, :), b(:, :, :, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_rsp
        logical pure module function all_close_1_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:), b(:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_rdp
        logical pure module function all_close_2_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:, :), b(:, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_rdp
        logical pure module function all_close_3_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:, :, :), b(:, :, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_rdp
        logical pure module function all_close_4_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:, :, :, :), b(:, :, :, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_rdp
        logical pure module function all_close_1_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:), b(:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_csp
        logical pure module function all_close_2_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:, :), b(:, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_csp
        logical pure module function all_close_3_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:, :, :), b(:, :, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_csp
        logical pure module function all_close_4_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:, :, :, :), b(:, :, :, :)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_csp
        logical pure module function all_close_1_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:), b(:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_cdp
        logical pure module function all_close_2_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:, :), b(:, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_cdp
        logical pure module function all_close_3_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:, :, :), b(:, :, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_cdp
        logical pure module function all_close_4_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:, :, :, :), b(:, :, :, :)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_cdp
    end interface all_close

    !> Version: experimental
    !>
    !> `arange` creates a rank-1 `array` of the `integer/real` type
    !>  with fixed-spaced values of given spacing, within a given interval.
    !> ([Specification](../page/specs/forlab_math.html#arange))
    interface arange
        pure module function arange_r_sp(start, end, step) result(result)
            real(sp), intent(in) :: start
            real(sp), intent(in), optional :: end, step
            real(sp), allocatable :: result(:)
        end function arange_r_sp
        pure module function arange_r_dp(start, end, step) result(result)
            real(dp), intent(in) :: start
            real(dp), intent(in), optional :: end, step
            real(dp), allocatable :: result(:)
        end function arange_r_dp
        pure module function arange_i_int8(start, end, step) result(result)
            integer(int8), intent(in) :: start
            integer(int8), intent(in), optional :: end, step
            integer(int8), allocatable :: result(:)
        end function arange_i_int8
        pure module function arange_i_int16(start, end, step) result(result)
            integer(int16), intent(in) :: start
            integer(int16), intent(in), optional :: end, step
            integer(int16), allocatable :: result(:)
        end function arange_i_int16
        pure module function arange_i_int32(start, end, step) result(result)
            integer(int32), intent(in) :: start
            integer(int32), intent(in), optional :: end, step
            integer(int32), allocatable :: result(:)
        end function arange_i_int32
        pure module function arange_i_int64(start, end, step) result(result)
            integer(int64), intent(in) :: start
            integer(int64), intent(in), optional :: end, step
            integer(int64), allocatable :: result(:)
        end function arange_i_int64
    end interface arange

    !> Version: experimental
    !>
    !> `signum` returns the sign of variables.
    !> ([Specification](../page/specs/forlab_math.html#signum))
    interface signum
        real(sp) elemental module function signum_rsp(x) result(sign)
            real(sp), intent(in) :: x
        end function signum_rsp
        real(dp) elemental module function signum_rdp(x) result(sign)
            real(dp), intent(in) :: x
        end function signum_rdp
        integer(int8) elemental module function signum_iint8(x) result(sign)
            integer(int8), intent(in) :: x
        end function signum_iint8
        integer(int16) elemental module function signum_iint16(x) result(sign)
            integer(int16), intent(in) :: x
        end function signum_iint16
        integer(int32) elemental module function signum_iint32(x) result(sign)
            integer(int32), intent(in) :: x
        end function signum_iint32
        integer(int64) elemental module function signum_iint64(x) result(sign)
            integer(int64), intent(in) :: x
        end function signum_iint64
        complex(sp) elemental module function signum_csp(x) result(sign)
            complex(sp), intent(in) :: x
        end function signum_csp
        complex(dp) elemental module function signum_cdp(x) result(sign)
            complex(dp), intent(in) :: x
        end function signum_cdp
    end interface signum

    interface cross
        pure module function cross_rsp(x, y) result(cross)
            real(sp), intent(in) :: x(3), y(3)
            real(sp) :: cross(3)
        end function cross_rsp
        pure module function cross_rdp(x, y) result(cross)
            real(dp), intent(in) :: x(3), y(3)
            real(dp) :: cross(3)
        end function cross_rdp
        pure module function cross_iint8(x, y) result(cross)
            integer(int8), intent(in) :: x(3), y(3)
            integer(int8) :: cross(3)
        end function cross_iint8
        pure module function cross_iint16(x, y) result(cross)
            integer(int16), intent(in) :: x(3), y(3)
            integer(int16) :: cross(3)
        end function cross_iint16
        pure module function cross_iint32(x, y) result(cross)
            integer(int32), intent(in) :: x(3), y(3)
            integer(int32) :: cross(3)
        end function cross_iint32
        pure module function cross_iint64(x, y) result(cross)
            integer(int64), intent(in) :: x(3), y(3)
            integer(int64) :: cross(3)
        end function cross_iint64
    end interface cross

    interface operator(.c.)
        procedure :: cross_rsp
        procedure :: cross_rdp
        procedure :: cross_iint8
        procedure :: cross_iint16
        procedure :: cross_iint32
        procedure :: cross_iint64
    end interface operator(.c.)

contains

    elemental function angle_sp(value) result(angle)
        real(sp) :: angle
        complex(sp), intent(in) :: value

        angle = aimag(log(value))

    end function angle_sp
    elemental function angle_dp(value) result(angle)
        real(dp) :: angle
        complex(dp), intent(in) :: value

        angle = aimag(log(value))

    end function angle_dp

end module forlab_math
