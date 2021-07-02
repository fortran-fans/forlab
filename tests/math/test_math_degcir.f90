program test_math_degcir
    use forlab_math, only: acosd, tand
    use forlab_io, only: disp
    ! test degrees circular function
    call disp('Test degrees circular function')
    call disp("acosd(1.d0)=")
    call disp(acosd(1.d0))
    call disp("tand([45.0,60.0,0.0])")
    call disp(tand([45.0, 60.0, 0.0]))
end program test_math_degcir
