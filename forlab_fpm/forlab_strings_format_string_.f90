
submodule (forlab_strings) forlab_strings_format_string

    implicit none
    integer, parameter :: buffer_len = 512

contains

    module procedure format_string_rsp
        !! Format real(sp) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_rsp
    module procedure format_string_rdp
        !! Format real(dp) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_rdp
    module procedure format_string_rqp
        !! Format real(qp) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_rqp
    module procedure format_string_iint8
        !! Format integer(int8) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_iint8
    module procedure format_string_iint16
        !! Format integer(int16) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_iint16
    module procedure format_string_iint32
        !! Format integer(int32) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_iint32
    module procedure format_string_iint64
        !! Format integer(int64) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_iint64
    module procedure format_string_llk
        !! Format logical(lk) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_llk
    module procedure format_string_lc_bool
        !! Format logical(c_bool) variable as character sequence
        character(len=buffer_len) :: buffer
        integer :: stat

        write(buffer, optval(format, "(g0)"), iostat=stat) value
        if (stat == 0) then
            string = trim(buffer)
        else
            string = '[*]'
                !!\TODO: *?
        end if

    end procedure format_string_lc_bool

    module procedure format_string_csp
        !! Format complex(sp) variable as character sequence

        string = '('//format_string_rsp(value%re, format)//','// &
            format_string_rsp(value%im, format)//')'

    end procedure format_string_csp
    module procedure format_string_cdp
        !! Format complex(dp) variable as character sequence

        string = '('//format_string_rdp(value%re, format)//','// &
            format_string_rdp(value%im, format)//')'

    end procedure format_string_cdp
    module procedure format_string_cqp
        !! Format complex(qp) variable as character sequence

        string = '('//format_string_rqp(value%re, format)//','// &
            format_string_rqp(value%im, format)//')'

    end procedure format_string_cqp

end submodule forlab_strings_format_string