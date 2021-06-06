submodule(forlab) forlab_tioc
    !! We believe that the accuracy requirements of the `tic` and `toc` functions are not high,
    !! and we only provide the double-precision version at the preliminary stage.
    use forlab_kinds
    implicit none
    real(dp), save :: tic_time
contains
    module procedure tic
        integer :: values(8)
        call date_and_time(values=values)
        tic_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        return
    end procedure

    module procedure toc_default
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        print *, "Elapsed time: " &
            //num2str(elapsed_time, "(F12.3)") &
            //" seconds"
        return
    end procedure

    module procedure toc_sp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        t = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure

    module procedure toc_dp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        t = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure

    module procedure toc_qp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                           values(6), values(7), values(8)*1000) &
                   *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        t = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure

end submodule
