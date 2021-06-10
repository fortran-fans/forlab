submodule(forlab) forlab_lu
    !! lu computes the LU matrix factorization.
    !!
    !! Syntax
    !!-----------------------------------------------------------------------
    !! call lu(A, L, U)
    !!
    !! Description
    !!---------------------------------------------------------------------
    !! call lu(A, L, U) returns the LU matrix factorization of the input
    !! square m-by-m matrix A. The output matrices are:
    !!   -   L is a m-by-m lower triangular matrix with ones on the diagonal,
    !!   -   U is a m-by-m upper triangular matrix.
    !!
    !! Examples
    !!---------------------------------------------------------------------
    !! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
    !!             order = [ 2, 1 ])
    !! call lu(A, L, U)
    !! call disp(L)
    !!     1.  0.  0.
    !!     4.  1.  0.
    !!     7.  2.  1.
    !! call disp(U)
    !!     1.  2.  3.
    !!     0. -3. -6.
    !!     0.  0.  0.
    !! call disp(matmul(L, U))
    !!     1.  2.  3.
    !!     4.  5.  6.
    !!     7.  8.  9.
    use forlab_kinds
    implicit none
    
contains
    module procedure lu_sp
        integer :: i, j, k, m

        if (issquare(A)) then
            m = size(A, 1)
            if (.not. allocated(L)) L = seye(m)
            if (.not. allocated(U)) U = szeros(m, m)

            do i = 1, m
                do j = 1, m
                    U(i, j) = A(i, j)
                    do k = 1, i - 1
                        U(i, j) = U(i, j) - L(i, k)*U(k, j)
                    end do
                end do
                do j = i + 1, m
                    L(j, i) = A(j, i)
                    do k = 1, i - 1
                        L(j, i) = L(j, i) - L(j, k)*U(k, i)
                    end do
                    L(j, i) = L(j, i)/U(i, i)
                end do
            end do
        else
            stop "Error: in A = LU, A should be square."
        end if
        return
    end procedure

    module procedure lu_dp
        integer :: i, j, k, m

        if (issquare(A)) then
            m = size(A, 1)
            if (.not. allocated(L)) L = deye(m)
            if (.not. allocated(U)) U = dzeros(m, m)

            do i = 1, m
                do j = 1, m
                    U(i, j) = A(i, j)
                    do k = 1, i - 1
                        U(i, j) = U(i, j) - L(i, k)*U(k, j)
                    end do
                end do
                do j = i + 1, m
                    L(j, i) = A(j, i)
                    do k = 1, i - 1
                        L(j, i) = L(j, i) - L(j, k)*U(k, i)
                    end do
                    L(j, i) = L(j, i)/U(i, i)
                end do
            end do
        else
            stop "Error: in A = LU, A should be square."
        end if
        return
    end procedure

    module procedure lu_qp
        integer :: i, j, k, m

        if (issquare(A)) then
            m = size(A, 1)
            if (.not. allocated(L)) L = qeye(m)
            if (.not. allocated(U)) U = qzeros(m, m)

            do i = 1, m
                do j = 1, m
                    U(i, j) = A(i, j)
                    do k = 1, i - 1
                        U(i, j) = U(i, j) - L(i, k)*U(k, j)
                    end do
                end do
                do j = i + 1, m
                    L(j, i) = A(j, i)
                    do k = 1, i - 1
                        L(j, i) = L(j, i) - L(j, k)*U(k, i)
                    end do
                    L(j, i) = L(j, i)/U(i, i)
                end do
            end do
        else
            stop "Error: in A = LU, A should be square."
        end if
        return
    end procedure

end submodule
