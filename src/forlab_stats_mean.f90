
submodule(forlab_stats) forlab_stats_mean

    implicit none

contains

    module procedure mean_1_sp
    mean = sum(x)/size(x)
    return
    end procedure mean_1_sp

    module procedure mean_2_sp
    integer :: i, m, n
    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim == 1)) then
        allocate (mean(n))
        do i = 1, n
            mean(i) = mean_1_sp(A(:, i))
        end do
    elseif (dim == 2) then
        allocate (mean(m))
        do i = 1, m
            mean(i) = mean_1_sp(A(i, :))
        end do
    end if
    return
    end procedure mean_2_sp
    module procedure mean_1_dp
    mean = sum(x)/size(x)
    return
    end procedure mean_1_dp

    module procedure mean_2_dp
    integer :: i, m, n
    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim == 1)) then
        allocate (mean(n))
        do i = 1, n
            mean(i) = mean_1_dp(A(:, i))
        end do
    elseif (dim == 2) then
        allocate (mean(m))
        do i = 1, m
            mean(i) = mean_1_dp(A(i, :))
        end do
    end if
    return
    end procedure mean_2_dp
    module procedure mean_1_qp
    mean = sum(x)/size(x)
    return
    end procedure mean_1_qp

    module procedure mean_2_qp
    integer :: i, m, n
    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim == 1)) then
        allocate (mean(n))
        do i = 1, n
            mean(i) = mean_1_qp(A(:, i))
        end do
    elseif (dim == 2) then
        allocate (mean(m))
        do i = 1, m
            mean(i) = mean_1_qp(A(i, :))
        end do
    end if
    return
    end procedure mean_2_qp

end submodule forlab_stats_mean
