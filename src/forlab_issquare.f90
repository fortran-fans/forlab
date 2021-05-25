
submodule(forlab) forlab_issquare
    use forlab_kinds

contains
        logical module function issquare0_sp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(sp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare0_dp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(dp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare0_qp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(qp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function

        logical module function issquare1_sp (A)
            !! Determine if it is a square complex matrix
            complex(sp), dimension(:, :), intent(in) :: A

            issquare1 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare1 = .true.
            return
        end function
        logical module function issquare1_dp (A)
            !! Determine if it is a square complex matrix
            complex(dp), dimension(:, :), intent(in) :: A

            issquare1 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare1 = .true.
            return
        end function
        logical module function issquare1_qp (A)
            !! Determine if it is a square complex matrix
            complex(qp), dimension(:, :), intent(in) :: A

            issquare1 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare1 = .true.
            return
        end function
end submodule

