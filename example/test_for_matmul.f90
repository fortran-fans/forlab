program main
    block
        use forlab, only: operator(.x.), tic, toc
        real, dimension(:, :), allocatable :: x_sp, y_sp, z_sp
        integer :: N = 10000

        allocate(x_sp(N, N), y_sp(N, N), z_sp(N, N))
        call tic()
        z_sp = x_sp .x. y_sp
        call toc()

    end block
end program