submodule(forlab) forlab_issquare
    use forlab_kinds

contains
        logical module function issquare_rsp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(sp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare_rdp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(dp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare_rqp (A)
            !！ issquare0 determines whether a real matrix is square.
            real(qp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare_csp (A)
            !！ issquare0 determines whether a real matrix is square.
            complex(sp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare_cdp (A)
            !！ issquare0 determines whether a real matrix is square.
            complex(dp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
        logical module function issquare_cqp (A)
            !！ issquare0 determines whether a real matrix is square.
            complex(qp), dimension(:, :), intent(in) :: A

            issquare0 = .false.
            if (size(A, 1) .eq. size(A, 2)) issquare0 = .true.
            return
        end function
end submodule
