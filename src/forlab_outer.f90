submodule(forlab) forlab_outer
    !! outer
    !!-----------------------------------------------------------------------
    !! outer computes the outer product of two vectors.
    !!
    !! Syntax
    !!-----------------------------------------------------------------------
    !! A = outer(x, y)
    !!
    !! Description
    !!-----------------------------------------------------------------------
    !! A = outer(x, y) returns the outer product of vectors x and y.
    use forlab_kinds

contains

    module procedure outer_int8
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int8, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

    module procedure outer_int16
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int16, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

    module procedure outer_int32
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int32, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

    module procedure outer_int64
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_int64, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure


    module procedure outer_sp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_sp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

    module procedure outer_dp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_dp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

    module procedure outer_qp
        integer :: m, n

        m = size(x)
        n = size(y)
        allocate(outer_qp, &
                 source=spread(x, 2, n) * spread(y, 1, m))

    end procedure

end submodule
