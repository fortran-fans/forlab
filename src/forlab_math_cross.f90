submodule(forlab_math) forlab_math_cross

    implicit none

contains

    pure module function cross_rsp(x, y) result(cross)
        real(sp), intent(in) :: x(3), y(3)
        real(sp) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_rsp
    pure module function cross_rdp(x, y) result(cross)
        real(dp), intent(in) :: x(3), y(3)
        real(dp) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_rdp
    pure module function cross_iint8(x, y) result(cross)
        integer(int8), intent(in) :: x(3), y(3)
        integer(int8) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_iint8
    pure module function cross_iint16(x, y) result(cross)
        integer(int16), intent(in) :: x(3), y(3)
        integer(int16) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_iint16
    pure module function cross_iint32(x, y) result(cross)
        integer(int32), intent(in) :: x(3), y(3)
        integer(int32) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_iint32
    pure module function cross_iint64(x, y) result(cross)
        integer(int64), intent(in) :: x(3), y(3)
        integer(int64) :: cross(3)

        cross(1) = x(2)*y(3) - x(3)*y(2)
        cross(2) = x(3)*y(1) - x(1)*y(3)
        cross(3) = x(1)*y(2) - x(2)*y(1)

    end function cross_iint64

end submodule forlab_math_cross
