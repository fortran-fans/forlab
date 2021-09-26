program test_io_bin
    use forlab_io, only: disp, savebin, loadbin, savetxt, loadtxt
    use forlab_stats, only: rng, randn
    real(8), allocatable :: x(:)
    real(8), allocatable :: y(:)
    call rng()
    allocate(X(5))
    X = randn(mean=0.0_8, std=1.0_8, ndim=5)
    call disp(x,'call randn(X):')
    call savebin('DP.bin', x)
    call savetxt('DP.txt', x)
    call loadtxt('DP.txt', y)
    call disp(y,'read from DP.txt')
    call loadbin('DP.bin', y)
    call disp(y,'read from DP.bin')
end program test_io_bin