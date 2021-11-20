
submodule(forlab_linalg) forlab_linalg_lu

    implicit none

contains

    module procedure lu_sp
    integer :: i, j, k, m

    if (is_square(A)) then
        m = size(A, 1)
        if (.not. allocated(L)) then
            allocate (L(m, m))
            call eye(L)
        end if
        if (.not. allocated(U)) then
            U = zeros(m, m)
        end if

        do i = 1, m
            do j = 1, m
                U(i, j) = A(i, j)
                do k = 1, i - 1
                    U(i, j) = U(i, j) - L(i, k)*U(k, j)
                end do
            end do
            do j = i + 1, m
                L(j, i) = A(j, i)
                do k = 1, i - 1
                    L(j, i) = L(j, i) - L(j, k)*U(k, i)
                end do
                L(j, i) = L(j, i)/U(i, i)
            end do
        end do
    else
        call error_stop("Error: in A = LU, A should be square.")
    end if
    return
    end procedure
    module procedure lu_dp
    integer :: i, j, k, m

    if (is_square(A)) then
        m = size(A, 1)
        if (.not. allocated(L)) then
            allocate (L(m, m))
            call eye(L)
        end if
        if (.not. allocated(U)) then
            U = zeros(m, m)
        end if

        do i = 1, m
            do j = 1, m
                U(i, j) = A(i, j)
                do k = 1, i - 1
                    U(i, j) = U(i, j) - L(i, k)*U(k, j)
                end do
            end do
            do j = i + 1, m
                L(j, i) = A(j, i)
                do k = 1, i - 1
                    L(j, i) = L(j, i) - L(j, k)*U(k, i)
                end do
                L(j, i) = L(j, i)/U(i, i)
            end do
        end do
    else
        call error_stop("Error: in A = LU, A should be square.")
    end if
    return
    end procedure

end submodule forlab_linalg_lu
