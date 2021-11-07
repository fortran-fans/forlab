
submodule(forlab_linalg) forlab_linalg_norm

    implicit none

contains

    real(sp) module function norm1_sp(x, p)
        real(sp), dimension(:), intent(in) :: x
        real(sp), intent(in), optional :: p
        real(sp)::temp
        if (.not. present(p)) then
            temp = 2.0_sp
        else
            temp = p
        end if

        if (temp == 2.0_sp) then
            norm1_sp = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_sp) then
            norm1_sp = sum(abs(x))
        else
            norm1_sp = (sum(abs(x)**p))**(1.0_sp/p)
        end if
    end function norm1_sp

    real(sp) module function norm2_sp(A, p)
        real(sp), dimension(:, :), intent(in) :: A
        real(sp), intent(in), optional :: p
        real(sp)::temp
        real(sp), dimension(:), allocatable :: w
        if (.not. present(p)) then
            temp = 2.0_sp
        else
            temp = p
        end if
        if (temp == 2.0_sp) then
            call svd(A, w)
            norm2_sp = maxval(w)
        elseif (temp == 1.0_sp) then
            norm2_sp = maxval(sum(abs(A), dim=2))
        end if
    end function norm2_sp
    real(dp) module function norm1_dp(x, p)
        real(dp), dimension(:), intent(in) :: x
        real(dp), intent(in), optional :: p
        real(dp)::temp
        if (.not. present(p)) then
            temp = 2.0_dp
        else
            temp = p
        end if

        if (temp == 2.0_dp) then
            norm1_dp = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_dp) then
            norm1_dp = sum(abs(x))
        else
            norm1_dp = (sum(abs(x)**p))**(1.0_dp/p)
        end if
    end function norm1_dp

    real(dp) module function norm2_dp(A, p)
        real(dp), dimension(:, :), intent(in) :: A
        real(dp), intent(in), optional :: p
        real(dp)::temp
        real(dp), dimension(:), allocatable :: w
        if (.not. present(p)) then
            temp = 2.0_dp
        else
            temp = p
        end if
        if (temp == 2.0_dp) then
            call svd(A, w)
            norm2_dp = maxval(w)
        elseif (temp == 1.0_dp) then
            norm2_dp = maxval(sum(abs(A), dim=2))
        end if
    end function norm2_dp
    real(qp) module function norm1_qp(x, p)
        real(qp), dimension(:), intent(in) :: x
        real(qp), intent(in), optional :: p
        real(qp)::temp
        if (.not. present(p)) then
            temp = 2.0_qp
        else
            temp = p
        end if

        if (temp == 2.0_qp) then
            norm1_qp = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_qp) then
            norm1_qp = sum(abs(x))
        else
            norm1_qp = (sum(abs(x)**p))**(1.0_qp/p)
        end if
    end function norm1_qp

    real(qp) module function norm2_qp(A, p)
        real(qp), dimension(:, :), intent(in) :: A
        real(qp), intent(in), optional :: p
        real(qp)::temp
        real(qp), dimension(:), allocatable :: w
        if (.not. present(p)) then
            temp = 2.0_qp
        else
            temp = p
        end if
        if (temp == 2.0_qp) then
            call svd(A, w)
            norm2_qp = maxval(w)
        elseif (temp == 1.0_qp) then
            norm2_qp = maxval(sum(abs(A), dim=2))
        end if
    end function norm2_qp

end submodule forlab_linalg_norm

