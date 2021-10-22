
submodule(forlab_linalg) forlab_linalg_svdsolve

    implicit none

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

end submodule forlab_linalg_svdsolve

