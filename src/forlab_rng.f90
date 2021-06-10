
submodule(forlab) forlab_rng
    !! Version: experimental
    !!
    ! rng
    !-----------------------------------------------------------------------
    ! rng controls random number generation.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! call rng()
    ! call rng(seed)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! call rng() uses the current date and time as seed for random number
    ! generation.
    !
    ! call rng(seed) sets the input seed for random number generation.
    !
    ! Notes
    !-----------------------------------------------------------------------
    ! It is advised to call rng at the beginning of a program so that each
    ! run of the program produces different sequences of random numbers.
    implicit none

contains
    module procedure rng
        integer :: seed_size, values(8)
        integer, dimension(:), allocatable :: seed_put

        call random_seed(size=seed_size)
        allocate (seed_put(seed_size))
        if (present(seed)) then
            seed_put = seed
        else
            call date_and_time(values=values)
            seed_put = values(8)*values(7)*values(6)
        end if
        call random_seed(put=seed_put)
        return
    end procedure rng
end submodule
