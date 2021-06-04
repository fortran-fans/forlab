!! fpm test test
program main
    use forlab
    implicit none
<<<<<<< HEAD
    real*8, allocatable :: x(:, :)

    x = empty(2, 3)
    call disp(x, 'empty x:')

    x = zeros(2, 4)
    call disp(x, 'zeros x:')
=======
    real :: x(2, 2)

    x = 0.0
    x(1, 1) = 1.0
    x(1, 2) = 2.0
    x(2, 1) = 3.0
    x(2, 2) = 4.0

    call disp(x, 'x matrix:')
>>>>>>> 53028a26e71ec0e8c5456bc25e4c8a18db7bd69c

end program
