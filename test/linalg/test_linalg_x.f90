program test_linalg_x
    use forlab_linalg, only: operator(.x.)
    use forlab_io, only: disp
    real, dimension(:, :), allocatable :: x_sp, y_sp

    allocate(x_sp(2,2), y_sp(2,2))

    x_sp = 1.
    y_sp = 2.

    call disp(x_sp.x.y_sp, 'x .x. y SP✨:')
end program test_linalg_x