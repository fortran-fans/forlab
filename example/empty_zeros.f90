program main
    use forlab, only: empty, zeros, disp, qzeros
    implicit none

    real, allocatable :: x(:, :)
    real(16), allocatable :: y(:, :)

    call disp('-----------------------')
    x = empty(2, 3)
    call disp(x, 'empty x:')

    y = qzeros(2, 3)
    call disp(y, 'zeros z:')

end program
