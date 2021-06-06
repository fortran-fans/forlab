submodule(forlab) forlab_issquare
    use forlab_kinds

contains
    logical module function issquare_rsp (A)
        !！ issquare0 determines whether a real matrix is square.
        real(sp), dimension(:, :), intent(in) :: A

        issquare_rsp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rsp = .true.
        return
    end function
    logical module function issquare_rdp (A)
        !！ issquare0 determines whether a real matrix is square.
        real(dp), dimension(:, :), intent(in) :: A

        issquare_rdp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rdp = .true.
        return
    end function
    logical module function issquare_rqp (A)
        !！ issquare0 determines whether a real matrix is square.
        real(qp), dimension(:, :), intent(in) :: A

        issquare_rqp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rqp = .true.
        return
    end function
    logical module function issquare_csp (A)
        !！ issquare0 determines whether a real matrix is square.
        complex(sp), dimension(:, :), intent(in) :: A

        issquare_csp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_csp = .true.
        return
    end function
    logical module function issquare_cdp (A)
        !！ issquare0 determines whether a real matrix is square.
        complex(dp), dimension(:, :), intent(in) :: A

        issquare_cdp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_cdp = .true.
        return
    end function
    logical module function issquare_cqp (A)
        !！ issquare0 determines whether a real matrix is square.
        complex(qp), dimension(:, :), intent(in) :: A

        issquare_cqp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_cqp = .true.
        return
    end function
end submodule
