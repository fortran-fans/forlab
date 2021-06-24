


submodule(forlab) forlab_solve
    !! solve solves a linear matrix equation.
    !!
    !!## Syntax
    !!
    !!      x = solve(A, b)
    !!
    !!## Description
    !!
    !! `x = solve(A, b)` returns the "exact" solution of the well-determined
    !! linear matrix equation Ax = b.
    !!
    !!## Examples
    !!
    !!      A = reshape([ 11., 7., 3., 2., 12., 6., 7., 9., 4. ], [ 3, 3 ], &
    !!                  order = [ 2, 1 ])
    !!      b = [ 34., 44., 37. ]
    !!      x = solve(A, b)
    !!          1.  2.  3.
    use forlab_kinds

contains
    module procedure solve_sp
        integer :: i, j, m, n
        real(sp), dimension(:), allocatable :: w, y
        real(sp), dimension(:, :), allocatable :: L, U, V
        m = size(A, 1)
        n = size(A, 2)
        if (issquare(A)) then
            allocate(x(m),y(m))
            call zeros(x)
            call zeros(y)
            y(1) = b(1)
            ! LU decomposition to solve LUx = b
            !===================================
            call lu(A, L, U)
            ! Forward substitution: Ly = b
            !==============================
            do i = 2, m
                y(i) = b(i)
                do j = 1, i - 1
                    y(i) = y(i) - y(j)*L(i, j)
                end do
            end do
            ! Back substitution: Ux = y
            !===========================
            x(m) = y(m)/U(m, m)
            do i = m - 1, 1, -1
                x(i) = y(i)
                do j = m, i + 1, -1
                    x(i) = x(i) - x(j)*U(i, j)
                end do
                x(i) = x(i)/U(i, i)
            end do
        else
            x = svdsolve(A, b)
        end if
    end procedure solve_sp
    module procedure solve_dp
        integer :: i, j, m, n
        real(dp), dimension(:), allocatable :: w, y
        real(dp), dimension(:, :), allocatable :: L, U, V
        m = size(A, 1)
        n = size(A, 2)
        if (issquare(A)) then
            allocate(x(m),y(m))
            call zeros(x)
            call zeros(y)
            y(1) = b(1)
            ! LU decomposition to solve LUx = b
            !===================================
            call lu(A, L, U)
            ! Forward substitution: Ly = b
            !==============================
            do i = 2, m
                y(i) = b(i)
                do j = 1, i - 1
                    y(i) = y(i) - y(j)*L(i, j)
                end do
            end do
            ! Back substitution: Ux = y
            !===========================
            x(m) = y(m)/U(m, m)
            do i = m - 1, 1, -1
                x(i) = y(i)
                do j = m, i + 1, -1
                    x(i) = x(i) - x(j)*U(i, j)
                end do
                x(i) = x(i)/U(i, i)
            end do
        else
            x = svdsolve(A, b)
        end if
    end procedure solve_dp
    module procedure solve_qp
        integer :: i, j, m, n
        real(qp), dimension(:), allocatable :: w, y
        real(qp), dimension(:, :), allocatable :: L, U, V
        m = size(A, 1)
        n = size(A, 2)
        if (issquare(A)) then
            allocate(x(m),y(m))
            call zeros(x)
            call zeros(y)
            y(1) = b(1)
            ! LU decomposition to solve LUx = b
            !===================================
            call lu(A, L, U)
            ! Forward substitution: Ly = b
            !==============================
            do i = 2, m
                y(i) = b(i)
                do j = 1, i - 1
                    y(i) = y(i) - y(j)*L(i, j)
                end do
            end do
            ! Back substitution: Ux = y
            !===========================
            x(m) = y(m)/U(m, m)
            do i = m - 1, 1, -1
                x(i) = y(i)
                do j = m, i + 1, -1
                    x(i) = x(i) - x(j)*U(i, j)
                end do
                x(i) = x(i)/U(i, i)
            end do
        else
            x = svdsolve(A, b)
        end if
    end procedure solve_qp
end submodule

