program main
    use forlab, only: empty, disp, operator(.x.)
    implicit none
    real, dimension(:, :), allocatable :: x_sp, y_sp
    real(8), dimension(:, :), allocatable :: x_dp, y_dp
    real(16), dimension(:, :), allocatable :: x_qp, y_qp

    x_sp = empty(2, 2); x_dp = empty(2, 2); x_qp = empty(2, 2)
    y_sp = empty(2, 2); y_dp = empty(2, 2); y_qp = empty(2, 2)

    x_sp = 1.; x_dp = 1.; x_qp = 1.
    y_sp = 2.; y_dp = 2.; y_qp = 2.

    call disp('----------------------------')
    call disp(x_sp .x. y_sp, 'x .x. y SP✨:')
    call disp(x_dp .x. y_dp, 'x .x. y DP✨:')
    call disp(x_qp .x. y_qp, 'x .x. y QP✨:')
    
    
end program