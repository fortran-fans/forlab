submodule(forlab) forlab_eye
    use forlab_kinds

contains
    !! Default Versions
    module function eye_1_default (dim1)
        real(dp), dimension(:, :), allocatable :: eye_1_default
        integer, intent(in) :: dim1
        integer :: i

        eye_1_default = zeros(dim1, dim1)
        do i = 1, dim1
            eye_1_default (i, i) = 1.0d0
        end do
        return
    end function

    module function eye_2_default (dim1, dim2)
        real(dp), dimension(:, :), allocatable :: eye_2_default
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        integer :: i

        eye_2_default = zeros(dim1, dim2)
        do i = 1, min(dim1, dim2)
            eye_2_default (i, i) = 1.0d0
        end do

        return
    end function
    !! Multi-precision versions
    module function eye_1_sp (dim1, flag)
        real(sp), dimension(:, :), allocatable :: eye_1_sp 
        integer, intent(in) :: dim1
        real(sp), intent(in) :: flag
        integer :: i

        eye_1_sp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_sp (i, i) = 1.0_sp
        end do
        return
    end function

    module function eye_2_sp (dim1, dim2, flag)
        real(sp), dimension(:, :), allocatable :: eye_2_sp
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        real(sp), intent(in) :: flag
        integer :: i

        eye_2_sp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_sp (i, i) = 1.0_sp
        end do

        return
    end function
    
    module function eye_1_dp (dim1, flag)
        real(dp), dimension(:, :), allocatable :: eye_1_dp 
        integer, intent(in) :: dim1
        real(dp), intent(in) :: flag
        integer :: i

        eye_1_dp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_dp (i, i) = 1.0_dp
        end do
        return
    end function

    module function eye_2_dp (dim1, dim2, flag)
        real(dp), dimension(:, :), allocatable :: eye_2_dp
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        real(dp), intent(in) :: flag
        integer :: i

        eye_2_dp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_dp (i, i) = 1.0_dp
        end do

        return
    end function
    
    module function eye_1_qp (dim1, flag)
        real(qp), dimension(:, :), allocatable :: eye_1_qp 
        integer, intent(in) :: dim1
        real(qp), intent(in) :: flag
        integer :: i

        eye_1_qp = zeros(dim1, dim1, flag)
        do i = 1, dim1
            eye_1_qp (i, i) = 1.0_qp
        end do
        return
    end function

    module function eye_2_qp (dim1, dim2, flag)
        real(qp), dimension(:, :), allocatable :: eye_2_qp
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        real(qp), intent(in) :: flag
        integer :: i

        eye_2_qp = zeros(dim1, dim2, flag)
        do i = 1, min(dim1, dim2)
            eye_2_qp (i, i) = 1.0_qp
        end do

        return
    end function
    
end submodule
