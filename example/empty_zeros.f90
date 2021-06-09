program main
    use forlab, only: empty, zeros, disp
    implicit none

    real, allocatable :: x(:, :)
    real(8), allocatable :: y(:, :)
    real(16), allocatable :: z(:, :)

    call disp('-----------------------')
    call disp('TEST for SP👍:')

    x = empty(2, 3)
    call disp(x, 'empty x:')

    x = zeros(2, 3)
    call disp(x, 'zeros x:')

    call disp('-----------------------')
    call disp('TEST for DP👍:')

    y = empty(2, 3)
    call disp(y, 'empty y:')

    y = zeros(2, 3)
    call disp(y, 'zeros y:')

    call disp('-----------------------')
    call disp('TEST for QP👍:')

    z = empty(2, 3)
    call disp(z, 'empty z:')

    z = zeros(2, 3)
    call disp(z, 'zeros z:')

    call disp('-----------------------')

end program
