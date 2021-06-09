submodule(forlab) forlab_eye
    use forlab_kinds
    implicit none
contains
    module procedure eye_1_sp
        integer :: i
        eye_1_sp = szeros(dim1, dim1)
        do i = 1, dim1
            eye_1_sp (i, i) = 1.0_sp
        end do
        return
    end procedure

    module procedure eye_2_sp
        integer :: i
        eye_2_sp = szeros(dim1, dim2)
        do i = 1, min(dim1, dim2)
            eye_2_sp (i, i) = 1.0_sp
        end do
        return
    end procedure

    module procedure eye_1_dp
        integer :: i
        eye_1_dp = dzeros(dim1, dim1)
        do i = 1, dim1
            eye_1_dp (i, i) = 1.0_dp
        end do
        return
    end procedure

    module procedure eye_2_dp
        integer :: i
        eye_2_dp = dzeros(dim1, dim2)
        do i = 1, min(dim1, dim2)
            eye_2_dp (i, i) = 1.0_dp
        end do
        return
    end procedure

    module procedure eye_1_qp
        integer :: i
        eye_1_qp = qzeros(dim1, dim1)
        do i = 1, dim1
            eye_1_qp (i, i) = 1.0_qp
        end do
        return
    end procedure

    module procedure eye_2_qp
        integer :: i
        eye_2_qp = qzeros(dim1, dim2)
        do i = 1, min(dim1, dim2)
            eye_2_qp (i, i) = 1.0_qp
        end do
        return
    end procedure

end submodule
