submodule(forlab) forlab_diag
    use forlab_kinds

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
            diag2_sp = zeros(n, n)
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
            diag2_dp = zeros(n, n)
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
            diag2_qp = zeros(n, n)
            do i = 1, n
                diag2_qp (i, i) = x(i)
            end do
            return
        end procedure
end submodule
