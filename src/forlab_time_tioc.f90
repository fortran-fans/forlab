
submodule(forlab_time) forlab_time_tioc
   
    use stdlib_strings, only: to_string
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
    end procedure tic

    module procedure toc_default
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                            values(6), values(7), values(8)*1000) &
                            *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        call disp("Elapsed time: " &
            //to_string(elapsed_time, "(F12.3)") &
            //" seconds")
        return
    end procedure toc_default

    module procedure toc_sp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                            values(6), values(7), values(8)*1000) &
                            *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        time = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure toc_sp
    module procedure toc_dp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                            values(6), values(7), values(8)*1000) &
                            *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        time = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure toc_dp
    module procedure toc_qp
        integer :: values(8)
        real(dp) :: toc_time, elapsed_time

        call date_and_time(values=values)
        toc_time = datenum(values(1), values(2), values(3), values(5), &
                            values(6), values(7), values(8)*1000) &
                            *24.0d0*60.0d0*60.0d0
        elapsed_time = toc_time - tic_time

        time = elapsed_time        !!\ATTENTION@zuo.zhihua@qq.com: Accuracy is converted here.
        return
    end procedure toc_qp

end submodule forlab_time_tioc
