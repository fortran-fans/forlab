



submodule(forlab) forlab_svdsolve
    !! svdsolve solves a linear matrix equation from the singular value
    !! decomposition of A.
    !!
    !!## Syntax
    !!
    !!      x = svdsolve(A, b)
    !!      x = svdsolve(A, b, k)
    !!
    !!## Description
    !!
    !! `x = svdsolve(A, b)` returns the full-rank solution of the linear matrix
    !! equation Ax = b.
    !!
    !! `x = svdsolve(A, b, k)` returns the reduced-rank solution of the linear
    !! matrix equation Ax = b, where A is a m-by-n matrix. Therefore, the
    !! rank of the solution is n-k.
    !!
    !!## Notes
    !!
    !! Sometimes, too small singular values produce very large solution. A
    !! proper way to determine the cut-off singular value is using a L-curve
    !! criterion. The cut-off singular value corresponds to the singular
    !! value from which the residuals norm is not improved while the solution
    !! norm increases substantially.
    use forlab_kinds

contains
    module procedure svdsolve_sp
        integer :: i, k, n
        real(sp), dimension(:), allocatable :: w, xnorm, resnorm
        real(sp), dimension(:, :), allocatable :: U, V
        n = size(A, 2)
        k = n
        if (present(cutoff)) k = k - cutoff
        xnorm = zeros(n)
        resnorm = zeros(n)
        call svd(A, w, U, V)
        do i = 1, n
            x = matmul(matmul(matmul(V(:, :i), diag(1/w(:i))), transpose(U(:, :i))), b)
            xnorm(i) = norm(x)
            resnorm(i) = norm(matmul(A, x) - b)
        end do
        x = matmul(matmul(matmul(V(:, :k), diag(1/w(:k))), transpose(U(:, :k))), b)
    end procedure svdsolve_sp
    module procedure svdsolve_dp
        integer :: i, k, n
        real(dp), dimension(:), allocatable :: w, xnorm, resnorm
        real(dp), dimension(:, :), allocatable :: U, V
        n = size(A, 2)
        k = n
        if (present(cutoff)) k = k - cutoff
        xnorm = zeros(n)
        resnorm = zeros(n)
        call svd(A, w, U, V)
        do i = 1, n
            x = matmul(matmul(matmul(V(:, :i), diag(1/w(:i))), transpose(U(:, :i))), b)
            xnorm(i) = norm(x)
            resnorm(i) = norm(matmul(A, x) - b)
        end do
        x = matmul(matmul(matmul(V(:, :k), diag(1/w(:k))), transpose(U(:, :k))), b)
    end procedure svdsolve_dp
    module procedure svdsolve_qp
        integer :: i, k, n
        real(qp), dimension(:), allocatable :: w, xnorm, resnorm
        real(qp), dimension(:, :), allocatable :: U, V
        n = size(A, 2)
        k = n
        if (present(cutoff)) k = k - cutoff
        xnorm = zeros(n)
        resnorm = zeros(n)
        call svd(A, w, U, V)
        do i = 1, n
            x = matmul(matmul(matmul(V(:, :i), diag(1/w(:i))), transpose(U(:, :i))), b)
            xnorm(i) = norm(x)
            resnorm(i) = norm(matmul(A, x) - b)
        end do
        x = matmul(matmul(matmul(V(:, :k), diag(1/w(:k))), transpose(U(:, :k))), b)
    end procedure svdsolve_qp
end submodule

