program test_math_angle
    use forlab_math, only: angle
    use stdlib_kinds, only: sp
    implicit none
    complex(sp) :: c, cX(2)

    c = (1.0, 2.0)
    cX = (2.0, 3.0)

    print *, angle(c)
    print *, angle(cX)

end program test_math_angle