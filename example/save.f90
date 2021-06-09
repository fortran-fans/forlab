program main
    use forlab, only: rng, savetxt, randn, disp
    implicit none

    call rng()
    call savetxt('randn_series.txt', randn(100))
    call disp('randn_series.txt has been generated.')

end program