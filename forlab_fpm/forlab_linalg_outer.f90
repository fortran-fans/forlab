
submodule(forlab_linalg) forlab_linalg_outer

    implicit none

contains

    module procedure outer_int8
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int8, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_int8
    module procedure outer_int16
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int16, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_int16
    module procedure outer_int32
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int32, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_int32
    module procedure outer_int64
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int64, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_int64

    module procedure outer_sp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_sp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_sp
    module procedure outer_dp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_dp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_dp
    module procedure outer_qp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_qp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure outer_qp

end submodule forlab_linalg_outer
