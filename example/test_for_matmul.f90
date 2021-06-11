program main
    block
        use forlab, only: empty, disp, operator(.x.), tic, toc
        real, dimension(:, :), allocatable :: x_sp, y_sp, z_sp

        call tic()
        x_sp = empty(10000, 10000)
        y_sp = empty(10000, 10000)
        z_sp = x_sp .x. y_sp
        call toc()

    end block
end program