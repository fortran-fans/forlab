submodule(forlab) forlab_ones
    !! Version: experimental
    !!
    !! ones creates array all of ones.
    !!
    !!## Syntax
    !!   x = ones(dim1)
    !!   A = ones(dim1, dim2)
    !!   X = ones(dim1, dim2, dim3)
    !!
    !!## Description
    !! `x = ones(dim1)` returns a dim1 vector of ones.
    !!
    !! `A = ones(dim1, dim2)` returns a dim1-by-dim2 matrix of ones.
    !!
    !! `X = ones(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of ones.
    !!
    !!## Examples
    !!    x = ones(3)
    !!    x =
    !!        1.  1.  1.
    !!
    !!    A = ones(3, 3)
    !!    A =
    !!        1.  1.  1.
    !!        1.  1.  1.
    !!        1.  1.  1.
    use forlab_kinds
    implicit none

contains
    module procedure ones_1_sp
        integer :: ierr

        allocate (ones_1_sp(dim1), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_1_sp = 1.0_sp
        end if
        return
    end procedure

    module procedure ones_2_sp
        integer :: ierr

        allocate (ones_2_sp(dim1, dim2), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_2_sp = 1.0_sp
        end if
        return
    end procedure

    module procedure ones_3_sp
        integer :: ierr

        allocate (ones_3_sp(dim1, dim2, dim3), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_3_sp = 1.0_sp
        end if
        return
    end procedure

    module procedure ones_1_dp
        integer :: ierr

        allocate (ones_1_dp(dim1), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_1_dp = 1.0_dp
        end if
        return
    end procedure

    module procedure ones_2_dp
        integer :: ierr

        allocate (ones_2_dp(dim1, dim2), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_2_dp = 1.0_dp
        end if
        return
    end procedure

    module procedure ones_3_dp
        integer :: ierr

        allocate (ones_3_dp(dim1, dim2, dim3), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_3_dp = 1.0_dp
        end if
        return
    end procedure

    module procedure ones_1_qp
        integer :: ierr

        allocate (ones_1_qp(dim1), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_1_qp = 1.0_qp
        end if
        return
    end procedure

    module procedure ones_2_qp
        integer :: ierr

        allocate (ones_2_qp(dim1, dim2), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_2_qp = 1.0_qp
        end if
        return
    end procedure

    module procedure ones_3_qp
        integer :: ierr

        allocate (ones_3_qp(dim1, dim2, dim3), stat=ierr)

        if (ierr /= 0) then
            print *, "Error: in ones, could not allocate array."
            stop
        else
            ones_3_qp = 1.0_qp
        end if
        return
    end procedure

end submodule
