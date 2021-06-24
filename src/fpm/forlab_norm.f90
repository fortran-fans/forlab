


submodule(forlab) forlab_norm
    !! norm computes vector and matrix norms.
    !!([Interface](../interface/norm.html))
    !!## Syntax
    !!
    !!      y = norm(x)
    !!      y = norm(x, p)
    !!      x = norm(A)
    !!      x = norm(A, p)
    !!
    !!## Description
    !!
    !! `y = norm(x)` returns the 2-norm or Euclidian norm of vector x.
    !!
    !! `y = norm(x, p)` returns the p-norm of vector x, where p is any positive
    !! real value.
    !!
    !! `x = norm(A)` returns the 2-norm of matrix A (largest singular value).
    !!
    !! `x = norm(A, p)` returns the p-norm of matrix A, where p is {1, 2}.
    !!
    !!## Examples
    !!
    !!      x = [ 1., 2., 3. ]
    !!      y = norm(x)
    !!          3.74165750
    !!      y = norm(x, 3.)
    !!          3.30192733
    use forlab_kinds
    implicit none
    
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

