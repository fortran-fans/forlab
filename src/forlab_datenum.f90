submodule(forlab) forlab_datenum
    use forlab_kinds
    implicit none

contains
    module procedure datenum0
        integer :: i, days_per_month(12)

        if ((month .lt. 1) .and. (month .gt. 12)) then
            print *, "Error: month should be between 1 and 12 ("//num2str(month)//")."
        end if
        if ((day .lt. 1) .and. (day .gt. 31)) then
            print *, "Error: day should be between 1 and 31 ("//num2str(day)//")."
        end if
        if ((present(hour)) .and. (hour .lt. 0) .and. (hour .gt. 23)) then
            print *, "Error: hour should be between 0 and 23 ("//num2str(hour)//")."
        end if
        if ((present(minute)) .and. (minute .lt. 0) .and. (minute .gt. 59)) then
            print *, "Error: minute should be between 0 and 59 ("//num2str(minute)//")."
        end if
        if ((present(second)) .and. (second .lt. 0) .and. (second .gt. 59)) then
            print *, "Error: second should be between 0 and 59 ("//num2str(second)//")."
        end if
        if ((present(microsecond)) .and. (microsecond .lt. 0) .and. (microsecond .ge. 1.0d+6)) then
            print *, "Error: microsecond should be between 0 and 999,999 ("//num2str(microsecond)//")."
        end if
        days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        datenum0 = 0
        do i = 0, year - 1
        if (isleap(i)) then
            datenum0 = datenum0 + 366
        else
            datenum0 = datenum0 + 365
        end if
        end do
        datenum0 = datenum0 + sum(days_per_month(:month - 1))
        if (isleap(year) .and. (month .gt. 2)) datenum0 = datenum0 + 1
        datenum0 = datenum0 + day
        if (present(hour)) datenum0 = datenum0 + real(hour, kind=8)/24.0d0
        if (present(minute)) datenum0 = datenum0 + real(minute, kind=8)/(24.0d0*60.0d0)
        if (present(second)) datenum0 = datenum0 + real(second, kind=8)/(24.0d0*60.0d0*60.0d0)
        if (present(microsecond)) datenum0 = datenum0 + real(microsecond, kind=8)/(24.0d0*60.0d0*60.0d0*1.0d+6)
        return
    end procedure
end submodule
