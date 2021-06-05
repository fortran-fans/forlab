!! fpm test test
program main
    use forlab, only: rng, savetxt, randn, disp 
    implicit none
    real :: flag

    call rng()
    call savetxt('randn_series.txt', randn(100, flag))
    call disp('randn_series.txt has been generated.')
    block 
        use forlab,only:operator(.i.)
        ! test inv and solve equation Ax=b, A^(-1) * b
        real :: a(2,2)
        real :: b(2)
        call random_number(a)
        call random_number(b)
        call disp(matmul(.i.a,b))
    end block

    block 
        use forlab ,only:acosd,asind,atand
        ! test degrees circular function
        call disp(acosd(1.d0))
        call disp(asind(0.5d0))
        call disp(acosd([1.0,0.5]))
        call disp(asind([1.0,0.5]))
        call disp(atand([50.0,0.0,-50.0]))
    end block
end program
