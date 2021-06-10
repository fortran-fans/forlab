submodule(forlab) forlab_det
    !! Version: experimental
    !!
    !! det computes the matrix determinant.
    !!
    !!## Syntax
    !!    x = det(A)
    !!    x = det(A, L, U)
    !!
    !!## Description
    !! `x = det(A)` returns the determinant of the square matrix A, as the
    !! product of the diagonal elements of the upper triangular matrix from
    !! the LU factorization of A.
    !!
    !! `x = det(A, L, U)` returns the determinant of the square matrix A and
    !! outputs the LU factorization matrices of A used for the calculation.
    !!
    !!## Examples
    !!    A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 0. ], [ 3, 3 ], &
    !!                order = [ 2, 1 ])
    !!    x = det(A)
    !!        27.
    use forlab_kinds
    implicit none
    
contains
    module procedure det_sp
        real(sp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (issquare(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det_sp = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det_sp = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det_sp = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            stop "Error: in det(A), A should be square."
        end if
        return
    end procedure
    module procedure det_dp
        real(dp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (issquare(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det_dp = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det_dp = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det_dp = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            stop "Error: in det(A), A should be square."
        end if
        return
    end procedure
    module procedure det_qp
        real(qp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (issquare(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det_qp = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det_qp = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det_qp = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            stop "Error: in det(A), A should be square."
        end if
        return
    end procedure
end submodule
