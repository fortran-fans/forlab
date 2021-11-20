module forlab_io

    use stdlib_error, only: error_stop
    use stdlib_io, only: open, parse_mode
    use stdlib_strings, only: to_string
    use stdlib_kinds, only: sp, dp, qp, int8, int16, int32, int64, lk, c_bool
    use stdlib_optval, only: optval
    use stdlib_strings, only: to_string
    use stdlib_string_type, only: string_type
    implicit none
    private

    public :: file
    public :: countlines, file_exist
    public :: savebin, loadbin, savetxt, loadtxt
    public :: color
    public :: disp

    public :: progress_bar, progress_perc

    !> Version: experimental
    !>
    !> forlab file derived type.
    !> ([Specification](../page/specs/forlab_io.html#file-file-constructor-and-file-derived-type))
    type file
        character(:), allocatable :: filename
        character(3) :: mode
        integer :: unit
        integer :: lines
    contains
        procedure :: exist => file_exist1
        procedure :: open => open_file
        procedure :: countlines => countlines1
        procedure :: close
    end type file

    !> version: experimental
    !>
    !> Display a scalar, vector or matrix.
    !> ([Specification](../page/specs/stdlib_io.html#disp-display-your-data-to-the-screen-or-another-output-unit))
    interface disp
        module subroutine disp_0_rsp(x, header, unit, brief)
            real(sp), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_rsp
        module subroutine disp_1_rsp(x, header, unit, brief)
            real(sp), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_rsp
        module subroutine disp_2_rsp(x, header, unit, brief)
            real(sp), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_rsp
        module subroutine disp_0_rdp(x, header, unit, brief)
            real(dp), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_rdp
        module subroutine disp_1_rdp(x, header, unit, brief)
            real(dp), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_rdp
        module subroutine disp_2_rdp(x, header, unit, brief)
            real(dp), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_rdp
        module subroutine disp_0_iint8(x, header, unit, brief)
            integer(int8), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint8
        module subroutine disp_1_iint8(x, header, unit, brief)
            integer(int8), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint8
        module subroutine disp_2_iint8(x, header, unit, brief)
            integer(int8), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint8
        module subroutine disp_0_iint16(x, header, unit, brief)
            integer(int16), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint16
        module subroutine disp_1_iint16(x, header, unit, brief)
            integer(int16), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint16
        module subroutine disp_2_iint16(x, header, unit, brief)
            integer(int16), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint16
        module subroutine disp_0_iint32(x, header, unit, brief)
            integer(int32), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint32
        module subroutine disp_1_iint32(x, header, unit, brief)
            integer(int32), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint32
        module subroutine disp_2_iint32(x, header, unit, brief)
            integer(int32), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint32
        module subroutine disp_0_iint64(x, header, unit, brief)
            integer(int64), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_iint64
        module subroutine disp_1_iint64(x, header, unit, brief)
            integer(int64), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_iint64
        module subroutine disp_2_iint64(x, header, unit, brief)
            integer(int64), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_iint64
        module subroutine disp_0_csp(x, header, unit, brief)
            complex(sp), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_csp
        module subroutine disp_1_csp(x, header, unit, brief)
            complex(sp), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_csp
        module subroutine disp_2_csp(x, header, unit, brief)
            complex(sp), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_csp
        module subroutine disp_0_cdp(x, header, unit, brief)
            complex(dp), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_cdp
        module subroutine disp_1_cdp(x, header, unit, brief)
            complex(dp), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_cdp
        module subroutine disp_2_cdp(x, header, unit, brief)
            complex(dp), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_cdp
        module subroutine disp_0_llk(x, header, unit, brief)
            logical(lk), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_llk
        module subroutine disp_1_llk(x, header, unit, brief)
            logical(lk), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_llk
        module subroutine disp_2_llk(x, header, unit, brief)
            logical(lk), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_llk
        module subroutine disp_0_lc_bool(x, header, unit, brief)
            logical(c_bool), intent(in) :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_0_lc_bool
        module subroutine disp_1_lc_bool(x, header, unit, brief)
            logical(c_bool), intent(in) :: x(:)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_1_lc_bool
        module subroutine disp_2_lc_bool(x, header, unit, brief)
            logical(c_bool), intent(in) :: x(:, :)
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_2_lc_bool
        module subroutine disp_character(x, header, unit, brief)
            character(len=*), intent(in), optional :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_character
        module subroutine disp_string_type(x, header, unit, brief)
            type(string_type), intent(in)           :: x
            character(len=*), intent(in), optional :: header
            integer, intent(in), optional :: unit
            logical, intent(in), optional :: brief
        end subroutine disp_string_type
    end interface disp

    interface file
        procedure init_file
    end interface file

    interface countlines
        procedure countlines2
    end interface countlines

    interface file_exist
        procedure file_exist2
    end interface file_exist

    interface loadbin
        module subroutine loadbin_1_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_1_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
    end interface

    interface loadtxt
        module subroutine loadtxt_1_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_1_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
    end interface

    interface savebin
        module subroutine savebin_1_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_1_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :, :), intent(in) :: X
        end subroutine
    end interface savebin

    interface savetxt
        module subroutine savetxt_1_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_1_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_iint8(filename, X)
            character(len=*), intent(in) :: filename
            integer(int8), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_iint16(filename, X)
            character(len=*), intent(in) :: filename
            integer(int16), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_iint32(filename, X)
            character(len=*), intent(in) :: filename
            integer(int32), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_iint64(filename, X)
            character(len=*), intent(in) :: filename
            integer(int64), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_csp(filename, X)
            character(len=*), intent(in) :: filename
            complex(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_cdp(filename, X)
            character(len=*), intent(in) :: filename
            complex(dp), dimension(:, :), intent(in) :: X
        end subroutine
    end interface savetxt

    !> Version: experimental
    !>
    !> `color` sets color for console text.
    !> ([Specification](../page/specs/forlab_io.html#color))
    interface
        module subroutine color(string)
            character(len=*), intent(in), optional :: string
        end subroutine color
    end interface

    !> Version: expermental
    !>
    !> Print a progress_bar.
    !> ([Specification](../page/specs/forlab_io.html#progress_bar))
    interface progress_bar
        module subroutine progress_bar_int8(iter, itermax, step, symbol)
            integer(int8), intent(in) :: iter, itermax
            integer(int8), intent(in), optional :: step
            character(*), intent(in), optional :: symbol
        end subroutine progress_bar_int8
        module subroutine progress_bar_int16(iter, itermax, step, symbol)
            integer(int16), intent(in) :: iter, itermax
            integer(int16), intent(in), optional :: step
            character(*), intent(in), optional :: symbol
        end subroutine progress_bar_int16
        module subroutine progress_bar_int32(iter, itermax, step, symbol)
            integer(int32), intent(in) :: iter, itermax
            integer(int32), intent(in), optional :: step
            character(*), intent(in), optional :: symbol
        end subroutine progress_bar_int32
        module subroutine progress_bar_int64(iter, itermax, step, symbol)
            integer(int64), intent(in) :: iter, itermax
            integer(int64), intent(in), optional :: step
            character(*), intent(in), optional :: symbol
        end subroutine progress_bar_int64
    end interface progress_bar

    !> Version: expermental
    !>
    !> Print a progress percentage message.
    !> ([Specification](../page/specs/forlab_io.html#progress_perc))
    interface progress_perc
        module subroutine progress_perc_int8(iter, itermax, prefix)
            integer(int8), intent(in) :: iter, itermax
            character(*), intent(in), optional :: prefix
        end subroutine progress_perc_int8
        module subroutine progress_perc_int16(iter, itermax, prefix)
            integer(int16), intent(in) :: iter, itermax
            character(*), intent(in), optional :: prefix
        end subroutine progress_perc_int16
        module subroutine progress_perc_int32(iter, itermax, prefix)
            integer(int32), intent(in) :: iter, itermax
            character(*), intent(in), optional :: prefix
        end subroutine progress_perc_int32
        module subroutine progress_perc_int64(iter, itermax, prefix)
            integer(int64), intent(in) :: iter, itermax
            character(*), intent(in), optional :: prefix
        end subroutine progress_perc_int64
    end interface progress_perc

contains

    !> Version: experimental
    !>
    !> Open the file. ([Specification](../page/specs/forlab_io.html#fileopen))
    subroutine open_file(self)
        class(file), intent(inout) :: self
        integer :: stat

        self%unit = open (self%filename, self%mode, iostat=stat)
        if (stat /= 0) call error_stop('Error: File "'//self%filename// &
                                       '" open failed, iostat = '//to_string(stat))

    end subroutine open_file

    !> Version: experimental
    !>
    !> `close` closes a `file` object, deallocate `file%filename`.
    !> ([Specification](../page/specs/forlab_io.html#fileclose))
    subroutine close (self)
        class(file), intent(inout) :: self

        deallocate (self%filename)
        close (self%unit)

    end subroutine close

    !> Version: experimental
    !>
    !> `countlines` counts the number of lines in a txt file.
    !> ([Specification](../page/specs/forlab_io.html#filecountlines))
    subroutine countlines1(self)
        class(file), intent(inout) :: self
        integer :: ierr
        logical :: ok

        self%lines = 0
        ok = .false.
        inquire (unit=self%unit, opened=ok)
        if (ok) then
            rewind (self%unit)
            do
                read (self%unit, *, iostat=ierr)
                if (ierr < 0) exit
                self%lines = self%lines + 1
            end do
            rewind (self%unit)
        else
            call self%open()
            do
                read (self%unit, *, iostat=ierr)
                if (ierr < 0) exit
                self%lines = self%lines + 1
            end do
            call self%close()
        end if
        if (self%lines == 0) then
            call disp('Warn: linecounts is 0 in "'//self%filename//'".')
        end if

    end subroutine countlines1

    !> The `countlines2` function returns the number of lines of the file
    integer function countlines2(filename)
        character(len=*), intent(in) :: filename
        type(file) :: infile

        infile = file(trim(filename))
        if (.not. infile%exist()) call error_stop('Error: File "'//infile%filename &
                                                  //'" open failed.')
        call infile%open()
        call infile%countlines()
        call infile%close()
        countlines2 = infile%lines

    end function countlines2

    !> Version: experimental
    !>
    !> `file_exist` determines whether a `file` object already exists.
    !> ([Specification](../page/specs/forlab_io.html#fileexist))
    function file_exist1(self) result(result)
        class(File), intent(inout) :: self
        logical :: result

        inquire (file=self%filename, exist=result)

    end function file_exist1

    logical function file_exist2(filename)
        !! The `countlines2` function returns the number of lines of the file
        character(len=*), intent(in) :: filename
        type(File) :: infile

        infile = File(filename)
        file_exist2 = infile%exist()
        return
    end function file_exist2

    type(file) function init_file(filename, mode)
        !!## File (constructor)
        !! File constructs a File object.
        !!
        !!### Syntax
        !! ofile = file(filename, mode)
        !!
        !!### Description
        !! `ofile = file(filename, mode)` returns a File object associated to the
        !! file filename with the identifier unit.
        !!
        !!### Examples
        !!    type(file) :: ofile
        !!
        !!    ofile = file("myfile.txt", "r")
        !!    call ofile%open()
        !!    !! ... some operations on this file ...
        !!    call ofile%close()
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: mode
        character(len=3) :: mode_

        mode_ = parse_mode(optval(mode, ''))
        init_file%filename = filename
        init_file%mode = mode_

        return
    end function init_file

end module forlab_io
