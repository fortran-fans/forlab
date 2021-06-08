program main
    use forlab, only: empty, zeros, disp
    implicit none

    real, allocatable :: x(:, :)
    real(8), allocatable :: y(:, :)
    real(16), allocatable :: z(:, :)

    real :: flag_sp
    real(8) :: flag_dp
    real(16) :: flag_qp

    call disp('-----------------------')
    call disp('TEST for SP👍:')

    x = empty(2, 3)
    call disp(x, 'empty x without flag:')

    x = zeros(2, 3)
    call disp(x, 'zeros x without flag:')

    call disp('-----------------------')
    call disp('TEST for DP👍:')

    y = empty(2, 3)
    call disp(y, 'empty y without flag:')

    y = zeros(2, 3)
    call disp(y, 'zeros y without flag:')

    call disp('-----------------------')
    call disp('TEST for QP👍:')

    z = empty(2, 3)
    call disp(z, 'empty z without flag:')

    z = zeros(2, 3)
    call disp(z, 'zeros z without flag:')

    call disp('-----------------------')

end program
