program test_time_tioc
    use forlab_time, only: tic, toc
    use forlab_io, only: disp
    use forlab_stats, only: rng, randn
    real :: time_sp
    real(8) :: time_dp
    real(8), allocatable :: x(:)

    call disp('----------------------------')
    call tic()
    allocate(x(1000))
    x = randn(mean=0.0, std=1.0, ndim=1000)
    call disp(size(x), 'x size:')

    call toc()
    call toc(time_sp)
    call disp(time_sp, 'tic/toc sp-version is passed:')

    call toc()
    call toc(time_dp)
    call disp(time_dp, 'tic/toc dp-version is passed:')

end program test_time_tioc