
module forlab_time

    use forlab_io, only: disp
    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64
    implicit none
    private

    public :: datenum, time_string
    public :: is_leap
    public :: tic, toc

    interface datenum
        real(dp) module function datenum0(year, month, day, hour, minute, &
                                    second, microsecond)
            integer, intent(in) :: year, month, day
            integer, intent(in), optional :: hour, minute, second, microsecond
        end function datenum0
    end interface datenum

    interface is_leap
        procedure :: is_leap_int8
        procedure :: is_leap_int16
        procedure :: is_leap_int32
        procedure :: is_leap_int64
    end interface is_leap

    interface
        module subroutine tic()
        end subroutine tic
    end interface

    interface toc
        module subroutine toc_default()
        end subroutine toc_default
        module subroutine toc_sp(time)
            real(sp), intent(out) :: time
        end subroutine toc_sp
        module subroutine toc_dp(time)
            real(dp), intent(out) :: time
        end subroutine toc_dp
        module subroutine toc_qp(time)
            real(qp), intent(out) :: time
        end subroutine toc_qp
    end interface toc

contains
    
    logical function is_leap_int8(year) result(is_leap)
        integer(int8), intent(in) :: year
        if ((mod(year, 400) == 0) .or. &
            ((mod(year, 4) == 0) .and. (mod(year, 100) /= 0))) then
            is_leap = .true.
        else
            is_leap = .false.
        end if
        return
    end function is_leap_int8
    logical function is_leap_int16(year) result(is_leap)
        integer(int16), intent(in) :: year
        if ((mod(year, 400) == 0) .or. &
            ((mod(year, 4) == 0) .and. (mod(year, 100) /= 0))) then
            is_leap = .true.
        else
            is_leap = .false.
        end if
        return
    end function is_leap_int16
    logical function is_leap_int32(year) result(is_leap)
        integer(int32), intent(in) :: year
        if ((mod(year, 400) == 0) .or. &
            ((mod(year, 4) == 0) .and. (mod(year, 100) /= 0))) then
            is_leap = .true.
        else
            is_leap = .false.
        end if
        return
    end function is_leap_int32
    logical function is_leap_int64(year) result(is_leap)
        integer(int64), intent(in) :: year
        if ((mod(year, 400) == 0) .or. &
            ((mod(year, 4) == 0) .and. (mod(year, 100) /= 0))) then
            is_leap = .true.
        else
            is_leap = .false.
        end if
        return
    end function is_leap_int64

    character(19) function time_string()
        implicit none
        character(10) :: data, time
        call date_and_time(data, time)
        time_string = data(1:4)//'-'//data(5:6)//'-'//data(7:8)//' '//time(1:2) &
        //':'//time(3:4)//':'//time(5:6)
    end function time_string

end module forlab_time