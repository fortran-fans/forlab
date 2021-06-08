
submodule(forlab) forlab_rng
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
