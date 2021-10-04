
submodule(forlab_stats) forlab_stats_var

    implicit none

contains

    module procedure var_1_sp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        select case (opt_w)
        case (0)
            var_1_sp = sum((x - mean(x))**2)/(size(x) - 1)
        case (1)
            var_1_sp = sum((x - mean(x))**2)/size(x)
        end select
        return
    end procedure var_1_sp

    module procedure var_2_sp
        integer :: opt_w, i, m, n

        opt_w = 0
        if (present(w)) opt_w = w
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (var_2_sp(n))
            do i = 1, n
                var_2_sp(i) = var_1_sp(A(:, i), opt_w)
            end do
        elseif (dim == 2) then
            allocate (var_2_sp(m))
            do i = 1, m
                var_2_sp(i) = var_1_sp(A(i, :), opt_w)
            end do
        end if
        return
    end procedure var_2_sp
    module procedure var_1_dp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        select case (opt_w)
        case (0)
            var_1_dp = sum((x - mean(x))**2)/(size(x) - 1)
        case (1)
            var_1_dp = sum((x - mean(x))**2)/size(x)
        end select
        return
    end procedure var_1_dp

    module procedure var_2_dp
        integer :: opt_w, i, m, n

        opt_w = 0
        if (present(w)) opt_w = w
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (var_2_dp(n))
            do i = 1, n
                var_2_dp(i) = var_1_dp(A(:, i), opt_w)
            end do
        elseif (dim == 2) then
            allocate (var_2_dp(m))
            do i = 1, m
                var_2_dp(i) = var_1_dp(A(i, :), opt_w)
            end do
        end if
        return
    end procedure var_2_dp
    module procedure var_1_qp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        select case (opt_w)
        case (0)
            var_1_qp = sum((x - mean(x))**2)/(size(x) - 1)
        case (1)
            var_1_qp = sum((x - mean(x))**2)/size(x)
        end select
        return
    end procedure var_1_qp

    module procedure var_2_qp
        integer :: opt_w, i, m, n

        opt_w = 0
        if (present(w)) opt_w = w
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (var_2_qp(n))
            do i = 1, n
                var_2_qp(i) = var_1_qp(A(:, i), opt_w)
            end do
        elseif (dim == 2) then
            allocate (var_2_qp(m))
            do i = 1, m
                var_2_qp(i) = var_1_qp(A(i, :), opt_w)
            end do
        end if
        return
    end procedure var_2_qp
    module procedure std_1_sp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        std_1_sp = sqrt(var_1_sp(x, opt_w))
        return
    end procedure std_1_sp

    module procedure std_2_sp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        if (.not. present(dim)) then
            std_2_sp = sqrt(var_2_sp(A, opt_w))
        else
            std_2_sp = sqrt(var_2_sp(A, opt_w, dim))
        end if
        return
    end procedure std_2_sp
    module procedure std_1_dp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        std_1_dp = sqrt(var_1_dp(x, opt_w))
        return
    end procedure std_1_dp

    module procedure std_2_dp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        if (.not. present(dim)) then
            std_2_dp = sqrt(var_2_dp(A, opt_w))
        else
            std_2_dp = sqrt(var_2_dp(A, opt_w, dim))
        end if
        return
    end procedure std_2_dp
    module procedure std_1_qp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        std_1_qp = sqrt(var_1_qp(x, opt_w))
        return
    end procedure std_1_qp

    module procedure std_2_qp
        integer :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w
        if (.not. present(dim)) then
            std_2_qp = sqrt(var_2_qp(A, opt_w))
        else
            std_2_qp = sqrt(var_2_qp(A, opt_w, dim))
        end if
        return
    end procedure std_2_qp

end submodule forlab_stats_var
