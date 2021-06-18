submodule(forlab) forlab_diag
    !! Version: experimental
    !!
    !! diag creates diagonal matrix or get the diagonal of a matrix.
    !!([Interface](../interface/diag.html))
    !!### Syntax
    !!    x = diag(A)
    !!    A = diag(x)
    !!
    !!### Description
    !! `x = diag(A)` returns the main diagonal of matrix `A`.
    !!
    !! `A = diag(x)` returns a square diagonal matrix with the elements of `x` on
    !! the main diagonal.
    !!
    !!### Examples
    !!    A = eye(3)
    !!    x = diag(A)
    !!        1.  1.  1.
    !!
    !!    x = [ 1., 2., 3. ]
    !!    A = diag(x)
    !!        1.  0.  0.
    !!        0.  2.  0.
    !!        0.  0.  3.
    use forlab_kinds
    implicit none
    
contains
    module procedure diag1_sp
        integer :: i, n
        n = min(size(A, 1), size(A, 2))
        allocate (diag1_sp (n))
        do i = 1, n
            diag1_sp (i) = A(i, i)
        end do
        return
    end procedure

    module procedure diag2_sp
        integer :: i, n
        n = size(x)
        allocate(diag2_sp(n,n))
        call zeros(diag2_sp)
        do i = 1, n
            diag2_sp (i, i) = x(i)
        end do
        return
    end procedure
    module procedure diag1_dp
        integer :: i, n
        n = min(size(A, 1), size(A, 2))
        allocate (diag1_dp (n))
        do i = 1, n
            diag1_dp (i) = A(i, i)
        end do
        return
    end procedure

    module procedure diag2_dp
        integer :: i, n
        n = size(x)
        allocate(diag2_dp(n,n))
        call zeros(diag2_dp)
        do i = 1, n
            diag2_dp (i, i) = x(i)
        end do
        return
    end procedure
    module procedure diag1_qp
        integer :: i, n
        n = min(size(A, 1), size(A, 2))
        allocate (diag1_qp (n))
        do i = 1, n
            diag1_qp (i) = A(i, i)
        end do
        return
    end procedure

    module procedure diag2_qp
        integer :: i, n
        n = size(x)
        allocate(diag2_qp(n,n))
        call zeros(diag2_qp)
        do i = 1, n
            diag2_qp (i, i) = x(i)
        end do
        return
    end procedure
end submodule
