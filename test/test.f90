program main
    use forlab, only: operator(.i.), RPRE, disp
    real :: x(2, 2)

    x = 0.0
    x(1, 1) = 0.5
    x(2, 2) = 1.0

    call disp(dble(x), 'x matrix:')
    call disp(.i.dble(x), 'inv of "x":')
    call disp(matmul(.i.dble(x), x), 'check:')

end program
