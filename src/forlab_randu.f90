

submodule(forlab) forlab_randu
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
