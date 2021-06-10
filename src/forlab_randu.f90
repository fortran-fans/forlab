

submodule(forlab) forlab_randu
    !! Version: experimental
    !!
    !! randu generates uniformly distributed random numbers.
    !!
    !!## Syntax
    !!    x = randu()
    !!    x = randu(dim1)
    !!    A = randu(dim1, dim2)
    !!    X = randu(dim1, dim2, dim3)
    !!
    !!## Description
    !! `x = randu()` returns a single uniformly distributed random number in
    !! the interval [0,1].
    !!
    !! `x = randu(dim1)` returns a dim1 vector of uniformly distributed random
    !! numbers.
    !!
    !! `A = randu(dim1, dim2)` returns a dim1-by-dim2 matrix of uniformly
    !! distributed random numbers.
    !!
    !! `X = randu(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of uniformly distributed random numbers.
    !!
    !!## Examples
    !!    x = randu()
    !!        0.383413825
    !!
    !!    x = randu(5)*2 - 1
    !!        0.640258908  -0.873707294   0.787327528
    use forlab_kinds
    implicit none
contains
    module procedure randu_0_sp


        call random_number(randu_0_sp)
        return
    end procedure

    module procedure randu_1_sp
        allocate (randu_1_sp (dim1))

        call random_number(randu_1_sp)
        return
    end procedure

    module procedure randu_2_sp
        allocate (randu_2_sp (dim1, dim2))

        call random_number(randu_2_sp)
        return
    end procedure

    module procedure randu_3_sp
        allocate (randu_3_sp (dim1, dim2, dim3))

        call random_number(randu_3_sp)
        return
    end procedure

    module procedure randu_0_dp


        call random_number(randu_0_dp)
        return
    end procedure

    module procedure randu_1_dp
        allocate (randu_1_dp (dim1))

        call random_number(randu_1_dp)
        return
    end procedure

    module procedure randu_2_dp
        allocate (randu_2_dp (dim1, dim2))

        call random_number(randu_2_dp)
        return
    end procedure

    module procedure randu_3_dp
        allocate (randu_3_dp (dim1, dim2, dim3))

        call random_number(randu_3_dp)
        return
    end procedure

    module procedure randu_0_qp


        call random_number(randu_0_qp)
        return
    end procedure

    module procedure randu_1_qp
        allocate (randu_1_qp (dim1))

        call random_number(randu_1_qp)
        return
    end procedure

    module procedure randu_2_qp
        allocate (randu_2_qp (dim1, dim2))

        call random_number(randu_2_qp)
        return
    end procedure

    module procedure randu_3_qp
        allocate (randu_3_qp (dim1, dim2, dim3))

        call random_number(randu_3_qp)
        return
    end procedure

end submodule
