


submodule(forlab) forlab_norm
    use forlab_kinds

contains
    module procedure norm1_sp
        real(sp)::temp
        if (.not. present(p))then
            temp =2.0_sp
        else
            temp = p
        end if

        if(temp == 2.0_sp) then
            norm1 = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_sp) then
            norm1 = sum(abs(x))
        else
            norm1 = (sum(abs(x)**p))**(1.0_sp/p)
        end if
    end procedure norm1_sp

    module procedure norm2_sp
        real(sp)::temp
        real(sp), dimension(:), allocatable :: w
        if (.not. present(p))then
            temp =2.0_sp
        else
            temp = p
        end if
        if(temp==2.0_sp)then
            call svd(A, w)
            norm2 = maxval(w)
        elseif (temp == 1.0_sp) then
            norm2 = maxval(sum(abs(A), dim=2))
        end if
    end procedure norm2_sp
    module procedure norm1_dp
        real(dp)::temp
        if (.not. present(p))then
            temp =2.0_dp
        else
            temp = p
        end if

        if(temp == 2.0_dp) then
            norm1 = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_dp) then
            norm1 = sum(abs(x))
        else
            norm1 = (sum(abs(x)**p))**(1.0_dp/p)
        end if
    end procedure norm1_dp

    module procedure norm2_dp
        real(dp)::temp
        real(dp), dimension(:), allocatable :: w
        if (.not. present(p))then
            temp =2.0_dp
        else
            temp = p
        end if
        if(temp==2.0_dp)then
            call svd(A, w)
            norm2 = maxval(w)
        elseif (temp == 1.0_dp) then
            norm2 = maxval(sum(abs(A), dim=2))
        end if
    end procedure norm2_dp
    module procedure norm1_qp
        real(qp)::temp
        if (.not. present(p))then
            temp =2.0_qp
        else
            temp = p
        end if

        if(temp == 2.0_qp) then
            norm1 = sqrt(sum(abs(x)**2))
        elseif (temp == 1.0_qp) then
            norm1 = sum(abs(x))
        else
            norm1 = (sum(abs(x)**p))**(1.0_qp/p)
        end if
    end procedure norm1_qp

    module procedure norm2_qp
        real(qp)::temp
        real(qp), dimension(:), allocatable :: w
        if (.not. present(p))then
            temp =2.0_qp
        else
            temp = p
        end if
        if(temp==2.0_qp)then
            call svd(A, w)
            norm2 = maxval(w)
        elseif (temp == 1.0_qp) then
            norm2 = maxval(sum(abs(A), dim=2))
        end if
    end procedure norm2_qp
end submodule

