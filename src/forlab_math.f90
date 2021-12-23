module forlab_math

    use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
    use stdlib_optval, only: optval
    use stdlib_math, only: arange, is_close, all_close
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
