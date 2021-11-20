
submodule(forlab_math) forlab_math_angle
    implicit none
contains

    pure module function angle_2_sp(x, y) result(angle)
        real(sp), dimension(3), intent(in) :: x, y
        real(sp) :: angle

        angle = acos(dot_product(x, y)/(norm2(x)*norm2(y)))

    end function angle_2_sp
    pure module function angle_2_dp(x, y) result(angle)
        real(dp), dimension(3), intent(in) :: x, y
        real(dp) :: angle

        angle = acos(dot_product(x, y)/(norm2(x)*norm2(y)))

    end function angle_2_dp

end submodule forlab_math_angle
