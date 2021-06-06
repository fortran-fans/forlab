program main
    use forlab, only: tic, toc, disp, rng, randn
    real :: time_sp
    real(8) :: time_dp
    real(16) :: time_qp
    real, allocatable :: x(:)

    call disp('----------------------------')
    call tic()
    x = randn(10000)
    call disp(size(x))

    call toc()
    call toc(time_sp)
    call disp(time_sp, 'tic/toc sp-version is passed:')

    call toc()
    call toc(time_dp)
    call disp(time_dp, 'tic/toc dp-version is passed:')

    call toc()
    call toc(time_qp)
    call disp(time_qp, 'tic/toc qp-version is passed:')
end program
