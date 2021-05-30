submodule(forlab) forlab_diag
    use forlab_kinds

contains
        module function diag1_sp (A)
            real(sp), dimension(:), allocatable :: diag1_sp
            real(sp), dimension(:, :), intent(in) :: A
            integer :: i, n

            n = min(size(A, 1), size(A, 2))
            allocate (diag1_sp (n))
            do i = 1, n
                diag1_sp (i) = A(i, i)
            end do
            return
        end function
        module function diag2_sp (x)
            real(sp), dimension(:, :), allocatable :: diag2_sp
            real(sp), dimension(:), intent(in) :: x
            integer :: i, n

            n = size(x)
            diag2_sp = zeros(n, n)
            do i = 1, n
                diag2_sp (i, i) = x(i)
            end do
            return
        end function
        module function diag1_dp (A)
            real(dp), dimension(:), allocatable :: diag1_dp
            real(dp), dimension(:, :), intent(in) :: A
            integer :: i, n

            n = min(size(A, 1), size(A, 2))
            allocate (diag1_dp (n))
            do i = 1, n
                diag1_dp (i) = A(i, i)
            end do
            return
        end function
        module function diag2_dp (x)
            real(dp), dimension(:, :), allocatable :: diag2_dp
            real(dp), dimension(:), intent(in) :: x
            integer :: i, n

            n = size(x)
            diag2_dp = zeros(n, n)
            do i = 1, n
                diag2_dp (i, i) = x(i)
            end do
            return
        end function
        module function diag1_qp (A)
            real(qp), dimension(:), allocatable :: diag1_qp
            real(qp), dimension(:, :), intent(in) :: A
            integer :: i, n

            n = min(size(A, 1), size(A, 2))
            allocate (diag1_qp (n))
            do i = 1, n
                diag1_qp (i) = A(i, i)
            end do
            return
        end function
        module function diag2_qp (x)
            real(qp), dimension(:, :), allocatable :: diag2_qp
            real(qp), dimension(:), intent(in) :: x
            integer :: i, n

            n = size(x)
            diag2_qp = zeros(n, n)
            do i = 1, n
                diag2_qp (i, i) = x(i)
            end do
            return
        end function
end submodule
