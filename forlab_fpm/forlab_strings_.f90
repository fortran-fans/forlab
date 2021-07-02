
module forlab_strings

    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64, lk, c_bool
    use stdlib_optval, only: optval
    implicit none
    private

    public :: format_string
    public :: progress_bar, progress_perc

    interface format_string
        !! version: experimental
        !!
        !! Format other types as character sequence.
        !! ([Specification](../page/specs/stdlib_strings.html#description))
        pure module function format_string_rsp(value, format) result(string)
            character(len=:), allocatable :: string
            real(sp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_rsp
        pure module function format_string_rdp(value, format) result(string)
            character(len=:), allocatable :: string
            real(dp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_rdp
        pure module function format_string_rqp(value, format) result(string)
            character(len=:), allocatable :: string
            real(qp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_rqp
        pure module function format_string_iint8(value, format) result(string)
            character(len=:), allocatable :: string
            integer(int8), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_iint8
        pure module function format_string_iint16(value, format) result(string)
            character(len=:), allocatable :: string
            integer(int16), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_iint16
        pure module function format_string_iint32(value, format) result(string)
            character(len=:), allocatable :: string
            integer(int32), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_iint32
        pure module function format_string_iint64(value, format) result(string)
            character(len=:), allocatable :: string
            integer(int64), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_iint64
        pure module function format_string_llk(value, format) result(string)
            character(len=:), allocatable :: string
            logical(lk), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_llk
        pure module function format_string_lc_bool(value, format) result(string)
            character(len=:), allocatable :: string
            logical(c_bool), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_lc_bool
        pure module function format_string_csp(value, format) result(string)
            character(len=:), allocatable :: string
            complex(sp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_csp
        pure module function format_string_cdp(value, format) result(string)
            character(len=:), allocatable :: string
            complex(dp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_cdp
        pure module function format_string_cqp(value, format) result(string)
            character(len=:), allocatable :: string
            complex(qp), intent(in) :: value
            character(len=*), intent(in), optional :: format
        end function format_string_cqp
    end interface format_string

    interface
        module subroutine progress_bar(iter, itermax, step)
            integer, intent(in) :: iter, itermax
            integer, intent(in), optional :: step
        end subroutine progress_bar
        module subroutine progress_perc(iter, itermax, prefix)
            integer, intent(in) :: iter, itermax
            character(len=*), intent(in), optional :: prefix
        end subroutine progress_perc
    end interface

end module forlab_strings