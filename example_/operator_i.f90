program main
    use forlab, only: operator(.i.), disp
    real :: x(2, 2)
    complex :: c(2, 2)

    x = 0.0
    x(1, 1) = 0.5
    x(2, 2) = 1.0

    call disp(x, 'x matrix:')
    call disp((.i.x), 'inv of "x":')
    call disp(matmul(.i.x, x), 'check:')

    c = 0.0
    c(1, 1) = cmplx(1.d0, 1.2d0)
    c(2, 2) = cmplx(1.1d0, 1.d0)

    call disp(c, 'c matrix:')
    call disp(.i. (c), 'inv of "c":')
    call disp(matmul(.i. (c), c), 'check:')

end program
