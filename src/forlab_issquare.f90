submodule(forlab) forlab_issquare
    use forlab_kinds

contains
    module procedure issquare_rsp

        issquare_rsp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rsp = .true.
        return
    end procedure
    module procedure issquare_rdp

        issquare_rdp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rdp = .true.
        return
    end procedure
    module procedure issquare_rqp

        issquare_rqp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_rqp = .true.
        return
    end procedure
    module procedure issquare_csp

        issquare_csp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_csp = .true.
        return
    end procedure
    module procedure issquare_cdp

        issquare_cdp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_cdp = .true.
        return
    end procedure
    module procedure issquare_cqp

        issquare_cqp = .false.
        if (size(A, 1) .eq. size(A, 2)) issquare_cqp = .true.
        return
    end procedure
end submodule
