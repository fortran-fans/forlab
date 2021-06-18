submodule(forlab) forlab_num2str
    !! Version: experimental
    !!
    !! num2str converts numbers to strings.
    !!
    !!## Syntax
    !!    str = num2str(x)
    !!    str = num2str(x, fmt)
    !!
    !!## Description
    !! `str = num2str(x)` converts `x` into a string.
    !!
    !! `str = num2str(x, fmt)` converts `x` into a string with the format fmt.
    !!
    !!## Examples
    !!    print *, "Percentage: " // num2str(50.431, "(F6.2)") // "%"
    !!        Percentage: 50.43%
    use forlab_kinds
    implicit none

contains
    module procedure num2str_sp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_sp = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_dp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_dp = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_qp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_qp = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_int8
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_int8 = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_int16
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_int16 = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_int32
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_int32 = trim(adjustl(xstr))
        return
    end procedure

    module procedure num2str_int64
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_int64 = trim(adjustl(xstr))
        return
    end procedure

end submodule
