program test_stats_rand
    use forlab_stats, only: rng, randu, randn
    use forlab_io, only: disp
    implicit none
    integer(kind=4) :: iX(5)
    real(kind=8) :: rX(5)

    call rng()
    call randu(iX)
    call disp(iX, 'RANDU(iX(5)) : ')
    call randu(iX, from=-10_4, to=10_4)
    call disp(iX, 'RANDU(iX, from=-10_4, to=10_4) : ')

    call randu(rX)
    call disp(rX, 'RANDU(rX(5)) : ')
    call randu(rX, from=-10.d0, to=10.d0)
    call disp(rX, 'RANDU(rX, from=-10.d0, to=10.d0) : ')
    
    call randn(rX)
    call disp(rX, 'RANDN(rX(5)) : ')
    call randn(rX, mean=0.d0, std=10.d0)
    call disp(rX, 'RANDN(rX, mean=0.d0, std=10.d0) : ')

end program test_stats_rand