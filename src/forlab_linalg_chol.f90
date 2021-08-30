

submodule(forlab_linalg) forlab_linalg_chol

    implicit none
    
contains

    module procedure chol_sp
        
        integer :: i, j, k, n
        real(sp) :: sum1, sum2
        real(sp), dimension(:), allocatable :: d
        real(sp), dimension(:,:), allocatable :: V
        real(sp), parameter::zero=0.0_sp,one=1.0_sp
        call eig(A, V, d)
        deallocate(V)
        if (all(d >= zero)) then
            n = size(A, 1)
            L = zeros(n, n)
            L(1, 1) = sqrt(A(1, 1))
            do i = 2, n
                L(i, 1) = A(i, 1)/L(1, 1)
            end do
            do i = 2, n
                do k = 1, i
                    sum1 = zero
                    sum2 = zero
                    do j = 1, k - 1
                        if (i == k) then
                            sum1 = sum1 + (L(k, j)*L(k, j))
                            L(k, k) = sqrt(A(k, k) - sum1)
                        elseif (i .gt. k) then
                            sum2 = sum2 + (L(i, j)*L(k, j))
                            L(i, k) = (one/L(k, k))*(A(i, k) - sum2)
                        else
                            L(i, k) = zero
                        end if
                    end do
                end do
            end do
        else
            error stop "Error: in chol(A), A should be positive definite."
        end if
        deallocate(d)
    end procedure chol_sp
    module procedure chol_dp
        
        integer :: i, j, k, n
        real(dp) :: sum1, sum2
        real(dp), dimension(:), allocatable :: d
        real(dp), dimension(:,:), allocatable :: V
        real(dp), parameter::zero=0.0_dp,one=1.0_dp
        call eig(A, V, d)
        deallocate(V)
        if (all(d >= zero)) then
            n = size(A, 1)
            L = zeros(n, n)
            L(1, 1) = sqrt(A(1, 1))
            do i = 2, n
                L(i, 1) = A(i, 1)/L(1, 1)
            end do
            do i = 2, n
                do k = 1, i
                    sum1 = zero
                    sum2 = zero
                    do j = 1, k - 1
                        if (i == k) then
                            sum1 = sum1 + (L(k, j)*L(k, j))
                            L(k, k) = sqrt(A(k, k) - sum1)
                        elseif (i .gt. k) then
                            sum2 = sum2 + (L(i, j)*L(k, j))
                            L(i, k) = (one/L(k, k))*(A(i, k) - sum2)
                        else
                            L(i, k) = zero
                        end if
                    end do
                end do
            end do
        else
            error stop "Error: in chol(A), A should be positive definite."
        end if
        deallocate(d)
    end procedure chol_dp
    module procedure chol_qp
        
        integer :: i, j, k, n
        real(qp) :: sum1, sum2
        real(qp), dimension(:), allocatable :: d
        real(qp), dimension(:,:), allocatable :: V
        real(qp), parameter::zero=0.0_qp,one=1.0_qp
        call eig(A, V, d)
        deallocate(V)
        if (all(d >= zero)) then
            n = size(A, 1)
            L = zeros(n, n)
            L(1, 1) = sqrt(A(1, 1))
            do i = 2, n
                L(i, 1) = A(i, 1)/L(1, 1)
            end do
            do i = 2, n
                do k = 1, i
                    sum1 = zero
                    sum2 = zero
                    do j = 1, k - 1
                        if (i == k) then
                            sum1 = sum1 + (L(k, j)*L(k, j))
                            L(k, k) = sqrt(A(k, k) - sum1)
                        elseif (i .gt. k) then
                            sum2 = sum2 + (L(i, j)*L(k, j))
                            L(i, k) = (one/L(k, k))*(A(i, k) - sum2)
                        else
                            L(i, k) = zero
                        end if
                    end do
                end do
            end do
        else
            error stop "Error: in chol(A), A should be positive definite."
        end if
        deallocate(d)
    end procedure chol_qp
    

end submodule forlab_linalg_chol

