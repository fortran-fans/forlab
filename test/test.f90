!! fpm test test
program main
    use forlab
    implicit none
    real :: x(2, 2)

    x = 0.0
    x(1, 1) = 1.0
    x(1, 2) = 2.0
    x(2, 1) = 3.0
    x(2, 2) = 4.0

    call disp(x, 'x matrix:')

end program
