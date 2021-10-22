submodule(forlab_linalg) forlab_linalg_diag

    implicit none

contains
    module procedure diag1_sp
    integer :: i, n
    n = min(size(A, 1), size(A, 2))
    allocate (diag(n))
    do i = 1, n
        diag(i) = A(i, i)
    end do
    return
    end procedure

    module procedure diag2_sp
    integer :: i, n
    n = size(x)
    diag = zeros(n, n)
    do i = 1, n
        diag(i, i) = x(i)
    end do
    return
    end procedure diag2_sp
    module procedure diag1_dp
    integer :: i, n
    n = min(size(A, 1), size(A, 2))
    allocate (diag(n))
    do i = 1, n
        diag(i) = A(i, i)
    end do
    return
    end procedure

    module procedure diag2_dp
    integer :: i, n
    n = size(x)
    diag = zeros(n, n)
    do i = 1, n
        diag(i, i) = x(i)
    end do
    return
    end procedure diag2_dp
    module procedure diag1_qp
    integer :: i, n
    n = min(size(A, 1), size(A, 2))
    allocate (diag(n))
    do i = 1, n
        diag(i) = A(i, i)
    end do
    return
    end procedure

    module procedure diag2_qp
    integer :: i, n
    n = size(x)
    diag = zeros(n, n)
    do i = 1, n
        diag(i, i) = x(i)
    end do
    return
    end procedure diag2_qp

end submodule forlab_linalg_diag
