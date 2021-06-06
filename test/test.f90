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
        real :: a(2,2)=reshape([1,2,3,1],[2,2])
        real :: b(2)=[1,1]
        call disp("Test Inv and solve equation Ax=b")
        call disp('A=')
        call disp(a)
        call disp("b=")
        call disp(b)
        call disp("x=")
        call disp(matmul(.i.a,b))
    end block

    block 
        use forlab ,only:acosd,tand
        ! test degrees circular function
        call disp('Test degrees circular function')
        call disp("acosd(1.d0)=")
        call disp(acosd(1.d0))
        call disp("tand([45.0,60.0,0.0])")
        call disp(tand([45.0,60.0,0.0]))
    end block
end program
