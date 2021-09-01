module forlab_math
    use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64
    use stdlib_optval, only: optval
    implicit none
    private

    public :: angle
    public :: cosd, sind,tand
    public :: acosd, asind, atand
    public :: is_close, arange

    interface acosd
        !! degree circular functions
        pure elemental module function acosd_sp(x)
        real(sp),intent(in)::x
        real(sp)::acosd_sp
        end function acosd_sp
        pure elemental module function acosd_dp(x)
        real(dp),intent(in)::x
        real(dp)::acosd_dp
        end function acosd_dp
        pure elemental module function acosd_qp(x)
        real(qp),intent(in)::x
        real(qp)::acosd_qp
        end function acosd_qp
    end interface acosd
    interface asind
        !! degree circular functions
        pure elemental module function asind_sp(x)
        real(sp),intent(in)::x
        real(sp)::asind_sp
        end function asind_sp
        pure elemental module function asind_dp(x)
        real(dp),intent(in)::x
        real(dp)::asind_dp
        end function asind_dp
        pure elemental module function asind_qp(x)
        real(qp),intent(in)::x
        real(qp)::asind_qp
        end function asind_qp
    end interface asind
    interface atand
        !! degree circular functions
        pure elemental module function atand_sp(x)
        real(sp),intent(in)::x
        real(sp)::atand_sp
        end function atand_sp
        pure elemental module function atand_dp(x)
        real(dp),intent(in)::x
        real(dp)::atand_dp
        end function atand_dp
        pure elemental module function atand_qp(x)
        real(qp),intent(in)::x
        real(qp)::atand_qp
        end function atand_qp
    end interface atand

    interface cosd
        pure elemental module function cosd_sp(x)
        real(sp),intent(in)::x
        real(sp)::cosd_sp
        end function cosd_sp
        pure elemental module function cosd_dp(x)
        real(dp),intent(in)::x
        real(dp)::cosd_dp
        end function cosd_dp
        pure elemental module function cosd_qp(x)
        real(qp),intent(in)::x
        real(qp)::cosd_qp
        end function cosd_qp
    end interface cosd
    interface sind
        pure elemental module function sind_sp(x)
        real(sp),intent(in)::x
        real(sp)::sind_sp
        end function sind_sp
        pure elemental module function sind_dp(x)
        real(dp),intent(in)::x
        real(dp)::sind_dp
        end function sind_dp
        pure elemental module function sind_qp(x)
        real(qp),intent(in)::x
        real(qp)::sind_qp
        end function sind_qp
    end interface sind
    interface tand
        pure elemental module function tand_sp(x)
        real(sp),intent(in)::x
        real(sp)::tand_sp
        end function tand_sp
        pure elemental module function tand_dp(x)
        real(dp),intent(in)::x
        real(dp)::tand_dp
        end function tand_dp
        pure elemental module function tand_qp(x)
        real(qp),intent(in)::x
        real(qp)::tand_qp
        end function tand_qp
    end interface tand
    
    interface angle
        !! Version: experimental
        !!
        !! angle compute the phase angle.
        !!([Interface](../interface/angle.html))
        procedure :: angle_sp
        procedure :: angle_dp
        procedure :: angle_qp
    end interface angle

    !> Version: experimental
    !>
    !> Determines whether the values of `a` and `b` are close.
    !> ([Specification](../page/specs/forlab_logic.html#is_close))
    interface is_close
        elemental module function is_close_rsp(a, b, rtol, atol) result(result)
            real(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_rsp
        elemental module function is_close_rdp(a, b, rtol, atol) result(result)
            real(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_rdp
        elemental module function is_close_rqp(a, b, rtol, atol) result(result)
            real(qp), intent(in) :: a, b
            real(qp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_rqp
        elemental module function is_close_csp(a, b, rtol, atol) result(result)
            complex(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_csp
        elemental module function is_close_cdp(a, b, rtol, atol) result(result)
            complex(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_cdp
        elemental module function is_close_cqp(a, b, rtol, atol) result(result)
            complex(qp), intent(in) :: a, b
            real(qp), intent(in), optional :: rtol, atol
            logical :: result
        end function is_close_cqp
    end interface is_close

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
        pure module function arange_r_qp(start, end, step) result(result)
            real(qp), intent(in) :: start
            real(qp), intent(in), optional :: end, step
            real(qp), allocatable :: result(:)
        end function arange_r_qp
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

contains

    elemental function angle_sp(value) result(angle) 
        !! angle compute the phase angle.
        real(sp) :: angle
        complex(sp),intent(in) :: value

        angle = aimag(log(value))

    end function angle_sp
    elemental function angle_dp(value) result(angle) 
        !! angle compute the phase angle.
        real(dp) :: angle
        complex(dp),intent(in) :: value

        angle = aimag(log(value))

    end function angle_dp
    elemental function angle_qp(value) result(angle) 
        !! angle compute the phase angle.
        real(qp) :: angle
        complex(qp),intent(in) :: value

        angle = aimag(log(value))

    end function angle_qp

end module forlab_math