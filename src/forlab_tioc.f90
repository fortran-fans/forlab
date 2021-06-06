submodule(forlab) forlab_tioc
    !! We believe that the accuracy requirements of the `tic` and `toc` functions are not high,
    !! and we only provide the double-precision version at the preliminary stage.
    use forlab_kinds
    implicit none
    real(sp), save :: tic_time_sp
    real(dp), save :: tic_time_dp
    real(qp), save :: tic_time_qp
contains
    module procedure tic
        integer :: values(8)
        call date_and_time(values=values)
        tic_time_qp = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0_qp*60.0_qp*60.0_qp
        tic_time_dp = real(tic_time_qp, 8)
        tic_time_sp = real(tic_time_dp, 4)
        return
    end procedure

    module procedure toc_default
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time_dp

        print *, "Elapsed time: " &
            //num2str(elapsed_time, "(F12.3)") &
            //" seconds"
        return
    end procedure

    module procedure toc_sp
        integer :: values(8)
        real(sp) :: toc_time_sp, elapsed_time

        call date_and_time(values=values)
        toc_time_sp = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0_sp*60.0_sp*60.0_sp
        elapsed_time = toc_time_sp - tic_time_sp
            
        t = elapsed_time
        return
    end procedure

    module procedure toc_dp
        integer :: values(8)
        real(dp) :: toc_time_dp, elapsed_time

        call date_and_time(values=values)
        toc_time_dp = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0_dp*60.0_dp*60.0_dp
        elapsed_time = toc_time_dp - tic_time_dp
            
        t = elapsed_time
        return
    end procedure

    module procedure toc_qp
        integer :: values(8)
        real(qp) :: toc_time_qp, elapsed_time

        call date_and_time(values=values)
        toc_time_qp = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0_qp*60.0_qp*60.0_qp
        elapsed_time = toc_time_qp - tic_time_qp
            
        t = elapsed_time
        return
    end procedure

end submodule
