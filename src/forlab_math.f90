module forlab_math
    use stdlib_kinds, only: sp, dp, qp
    implicit none
    private

    public :: angle
    public :: cosd, sind,tand
    public :: acosd, asind, atand

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