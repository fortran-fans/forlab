program test_stats_var
    use forlab_io, only: disp
    use forlab_stats, only: var,randn,rng,mean,std
    real, allocatable :: x(:)
    call rng()
    allocate(X(5))
    x = randn(mean=0.0, std=1.0, ndim=5)
    call disp(x,'randn(n)')
    call disp(mean(x),'mean(randn(n)):')
    call disp(var(x),'var(randn(n)):')
    call disp(std(x), 'std(randn(n)):')
    if(allocated(X)) deallocate(X)
    allocate(X(4))  !!\FIXME:
    x = randn(mean=10.0, std=1.0, ndim=4)
    call disp(X,'call randn(X,10.,1.0)')
end program test_stats_var