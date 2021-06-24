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
    module procedure num2str_rsp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_rsp = trim(adjustl(xstr))
        return
    end procedure num2str_rsp

    module procedure num2str_rdp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_rdp = trim(adjustl(xstr))
        return
    end procedure num2str_rdp

    module procedure num2str_rqp
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_rqp = trim(adjustl(xstr))
        return
    end procedure num2str_rqp

    module procedure num2str_iint8
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_iint8 = trim(adjustl(xstr))
        return
    end procedure num2str_iint8

    module procedure num2str_iint16
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_iint16 = trim(adjustl(xstr))
        return
    end procedure num2str_iint16

    module procedure num2str_iint32
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_iint32 = trim(adjustl(xstr))
        return
    end procedure num2str_iint32

    module procedure num2str_iint64
        character(len=CLEN) :: xstr

        if (present(fmt)) then
            write (xstr, fmt) x
        else
            write (xstr, *) x
        end if
        num2str_iint64 = trim(adjustl(xstr))
        return
    end procedure num2str_iint64

    module procedure num2str_csp
        character(len=CLEN) :: xstr

        if(present(fmt)) then
            write (xstr, *) '('//&
                        num2str_rsp(real(x), fmt)//','// &
                        num2str_rsp(imag(x), fmt)//')'
        else
            write (xstr, *) '('//&
                        num2str_rsp(real(x))//','// &
                        num2str_rsp(imag(x))//')'
        end if
        num2str_csp = trim(adjustl(xstr))
    end procedure num2str_csp

    module procedure num2str_cdp
        character(len=CLEN) :: xstr

        if(present(fmt)) then
            write (xstr, *) '('//&
                        num2str_rdp(real(x), fmt)//','// &
                        num2str_rdp(imag(x), fmt)//')'
        else
            write (xstr, *) '('//&
                        num2str_rdp(real(x))//','// &
                        num2str_rdp(imag(x))//')'
        end if
        num2str_cdp = trim(adjustl(xstr))
    end procedure num2str_cdp

    module procedure num2str_cqp
        character(len=CLEN) :: xstr

        if(present(fmt)) then
            write (xstr, *) '('//&
                        num2str_rqp(real(x), fmt)//','// &
                        num2str_rqp(imag(x), fmt)//')'
        else
            write (xstr, *) '('//&
                        num2str_rqp(real(x))//','// &
                        num2str_rqp(imag(x))//')'
        end if
        num2str_cqp = trim(adjustl(xstr))
    end procedure num2str_cqp

end submodule forlab_num2str
