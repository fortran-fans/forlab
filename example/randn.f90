program main
    use forlab, only: rng, disp, randu, randn, drandn
    implicit none

    call rng()
    call disp(randu(1, 5), 'randu series:')

    call disp(drandn(1, 5), 'randn series:')

end program
