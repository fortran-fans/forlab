
submodule(forlab_linalg) forlab_linalg_det

    implicit none
    
contains

    module procedure det_sp
        real(sp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (is_square(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            call error_stop("Error: in det(A), A should be square.")
        end if
        return

    end procedure det_sp
    module procedure det_dp
        real(dp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (is_square(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            call error_stop("Error: in det(A), A should be square.")
        end if
        return

    end procedure det_dp
    module procedure det_qp
        real(qp), dimension(:, :), allocatable :: L, U
        integer :: m

        if (is_square(A)) then
            m = size(A, 1)
            if (m .eq. 2) then
                det = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
            elseif (m .eq. 3) then
                det = A(1, 1)*A(2, 2)*A(3, 3) &
                                + A(2, 1)*A(3, 2)*A(1, 3) &
                                + A(3, 1)*A(1, 2)*A(2, 3) &
                                - A(1, 1)*A(3, 2)*A(2, 3) &
                                - A(3, 1)*A(2, 2)*A(1, 3) &
                                - A(2, 1)*A(1, 2)*A(3, 3)
            else
                call lu(A, L, U)
                det = product(diag(U))
                if (present(outL)) outL = L
                if (present(outU)) outU = U
            end if
        else
            call error_stop("Error: in det(A), A should be square.")
        end if
        return

    end procedure det_qp

end submodule forlab_linalg_det
