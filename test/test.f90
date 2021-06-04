!! fpm test test
program main
    use forlab
    implicit none
    real*8, allocatable :: x(:, :)

    x = empty(2, 3)
    call disp(x, 'empty x:')

    x = zeros(2, 4)
    call disp(x, 'zeros x:')

end program
