submodule(forlab_math) forlab_math_signum

contains

    real(sp) elemental module function signum_rsp(x) result(sign)

        real(sp), intent(in) :: x

        if (x < 0.0_sp) then; sign = -1.0_sp
        elseif (x > 0.0_sp) then; sign = 1.0_sp
        else; sign = 0.0_sp
        end if

    end function signum_rsp
    real(dp) elemental module function signum_rdp(x) result(sign)

        real(dp), intent(in) :: x

        if (x < 0.0_dp) then; sign = -1.0_dp
        elseif (x > 0.0_dp) then; sign = 1.0_dp
        else; sign = 0.0_dp
        end if

    end function signum_rdp

    integer(int8) elemental module function signum_iint8(x) result(sign)

        integer(int8), intent(in) :: x

        if (x < 0_int8) then; sign = -1_int8
        elseif (x > 0_int8) then; sign = 1_int8
        else; sign = 0_int8
        end if

    end function signum_iint8
    integer(int16) elemental module function signum_iint16(x) result(sign)

        integer(int16), intent(in) :: x

        if (x < 0_int16) then; sign = -1_int16
        elseif (x > 0_int16) then; sign = 1_int16
        else; sign = 0_int16
        end if

    end function signum_iint16
    integer(int32) elemental module function signum_iint32(x) result(sign)

        integer(int32), intent(in) :: x

        if (x < 0_int32) then; sign = -1_int32
        elseif (x > 0_int32) then; sign = 1_int32
        else; sign = 0_int32
        end if

    end function signum_iint32
    integer(int64) elemental module function signum_iint64(x) result(sign)

        integer(int64), intent(in) :: x

        if (x < 0_int64) then; sign = -1_int64
        elseif (x > 0_int64) then; sign = 1_int64
        else; sign = 0_int64
        end if

    end function signum_iint64

    complex(sp) elemental module function signum_csp(x) result(sign)

        complex(sp), intent(in) :: x

        if (x == (0.0_sp, 0.0_sp)) then; sign = x
        else; sign = x/abs(x)
        end if

    end function signum_csp
    complex(dp) elemental module function signum_cdp(x) result(sign)

        complex(dp), intent(in) :: x

        if (x == (0.0_dp, 0.0_dp)) then; sign = x
        else; sign = x/abs(x)
        end if

    end function signum_cdp

end submodule forlab_math_signum
