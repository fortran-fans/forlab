!! fpm test test
program main
    use forlab
    implicit none
    real*8 :: flag

    call rng()
    call disp(randu(1, 5),'randu series:')
    call disp(randu(1, 5, flag),'randu series:')

    call disp(randn(1, 5),'randn series:')
    call disp(randn(1, 5, flag),'randn series:')

end program
