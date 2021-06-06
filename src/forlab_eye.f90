submodule(forlab) forlab_eye
    use forlab_kinds

contains
    !! Default Versions
    module procedure eye_1_default
        integer :: i
        eye_1_default = zeros(dim1, dim1)
        do i = 1, dim1
            eye_1_default (i, i) = 1.0d0
        end do
        return
    end procedure

    module procedure eye_2_default
        integer :: i
        eye_2_default = zeros(dim1, dim2)
        do i = 1, min(dim1, dim2)
            eye_2_default (i, i) = 1.0d0
        end do
        return
    end procedure

    !! Multi-precision versions
    module procedure eye_1_sp
        integer :: i
        eye_1_sp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_sp (i, i) = 1.0_sp
        end do
        return
    end procedure

    module procedure eye_2_sp
        integer :: i
        eye_2_sp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_sp (i, i) = 1.0_sp
        end do
        return
    end procedure
    
    module procedure eye_1_dp
        integer :: i
        eye_1_dp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_dp (i, i) = 1.0_dp
        end do
        return
    end procedure

    module procedure eye_2_dp
        integer :: i
        eye_2_dp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_dp (i, i) = 1.0_dp
        end do
        return
    end procedure
    
    module procedure eye_1_qp
        integer :: i
        eye_1_qp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_qp (i, i) = 1.0_qp
        end do
        return
    end procedure

    module procedure eye_2_qp
        integer :: i
        eye_2_qp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_qp (i, i) = 1.0_qp
        end do
        return
    end procedure
    
end submodule
