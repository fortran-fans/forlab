module forlab_io

    use stdlib_error,  only: error_stop
    use stdlib_io, only: open, parse_mode
    use stdlib_ascii, only: to_string
    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64, lk, c_bool
    use stdlib_optval, only: optval
    use forlab_strings, only: format_string
    implicit none
    private

    public :: file
    public :: countlines, file_exist
    public :: savebin, loadbin, savetxt, loadtxt
    public :: set_color
    public :: disp

    type file
        integer :: unit
        character(:), allocatable :: filename
        character(3) :: mode
        integer :: lines
    contains
        procedure :: exist => file_exist1
        procedure :: open => open_file
        procedure :: countlines => countlines1
        procedure :: close
    end type file

    interface disp
        !! version: experimental
        !!
        !! Quickly display strings, scalars and low-dimensional arrays to the default output_unit
        !! ([Specification](../page/specs/stdlib_io.html#description))
        module subroutine disp_0_rsp(value, string)
            real(sp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_rsp
        module subroutine disp_1_rsp(value, string)
            real(sp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_rsp
        module subroutine disp_2_rsp(value, string)
            real(sp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_rsp
        module subroutine disp_3_rsp(value, dim, string)
            real(sp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_rsp
        module subroutine disp_0_rdp(value, string)
            real(dp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_rdp
        module subroutine disp_1_rdp(value, string)
            real(dp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_rdp
        module subroutine disp_2_rdp(value, string)
            real(dp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_rdp
        module subroutine disp_3_rdp(value, dim, string)
            real(dp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_rdp
        module subroutine disp_0_rqp(value, string)
            real(qp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_rqp
        module subroutine disp_1_rqp(value, string)
            real(qp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_rqp
        module subroutine disp_2_rqp(value, string)
            real(qp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_rqp
        module subroutine disp_3_rqp(value, dim, string)
            real(qp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_rqp
        module subroutine disp_0_iint8(value, string)
            integer(int8), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_iint8
        module subroutine disp_1_iint8(value, string)
            integer(int8), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_iint8
        module subroutine disp_2_iint8(value, string)
            integer(int8), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_iint8
        module subroutine disp_3_iint8(value, dim, string)
            integer(int8), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_iint8
        module subroutine disp_0_iint16(value, string)
            integer(int16), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_iint16
        module subroutine disp_1_iint16(value, string)
            integer(int16), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_iint16
        module subroutine disp_2_iint16(value, string)
            integer(int16), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_iint16
        module subroutine disp_3_iint16(value, dim, string)
            integer(int16), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_iint16
        module subroutine disp_0_iint32(value, string)
            integer(int32), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_iint32
        module subroutine disp_1_iint32(value, string)
            integer(int32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_iint32
        module subroutine disp_2_iint32(value, string)
            integer(int32), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_iint32
        module subroutine disp_3_iint32(value, dim, string)
            integer(int32), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_iint32
        module subroutine disp_0_iint64(value, string)
            integer(int64), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_iint64
        module subroutine disp_1_iint64(value, string)
            integer(int64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_iint64
        module subroutine disp_2_iint64(value, string)
            integer(int64), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_iint64
        module subroutine disp_3_iint64(value, dim, string)
            integer(int64), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_iint64
        module subroutine disp_0_csp(value, string)
            complex(sp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_csp
        module subroutine disp_1_csp(value, string)
            complex(sp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_csp
        module subroutine disp_2_csp(value, string)
            complex(sp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_csp
        module subroutine disp_3_csp(value, dim, string)
            complex(sp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_csp
        module subroutine disp_0_cdp(value, string)
            complex(dp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_cdp
        module subroutine disp_1_cdp(value, string)
            complex(dp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_cdp
        module subroutine disp_2_cdp(value, string)
            complex(dp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_cdp
        module subroutine disp_3_cdp(value, dim, string)
            complex(dp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_cdp
        module subroutine disp_0_cqp(value, string)
            complex(qp), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_cqp
        module subroutine disp_1_cqp(value, string)
            complex(qp), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_cqp
        module subroutine disp_2_cqp(value, string)
            complex(qp), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_cqp
        module subroutine disp_3_cqp(value, dim, string)
            complex(qp), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_cqp
        module subroutine disp_0_llk(value, string)
            logical(lk), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_llk
        module subroutine disp_1_llk(value, string)
            logical(lk), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_llk
        module subroutine disp_2_llk(value, string)
            logical(lk), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_llk
        module subroutine disp_3_llk(value, dim, string)
            logical(lk), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_llk
        module subroutine disp_0_lc_bool(value, string)
            logical(c_bool), intent(in) :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_0_lc_bool
        module subroutine disp_1_lc_bool(value, string)
            logical(c_bool), intent(in) :: value(:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_1_lc_bool
        module subroutine disp_2_lc_bool(value, string)
            logical(c_bool), intent(in) :: value(:,:)
            character(len=*), intent(in), optional :: string
        end subroutine disp_2_lc_bool
        module subroutine disp_3_lc_bool(value, dim, string)
            logical(c_bool), intent(in) :: value(:,:,:)
            integer, intent(in) :: dim
            character(len=*), intent(in), optional :: string
        end subroutine disp_3_lc_bool
        module subroutine disp_str(value, string)
            character(len=*), intent(in), optional :: value
            character(len=*), intent(in), optional :: string
        end subroutine disp_str
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
        module subroutine loadbin_1_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:), allocatable, intent(out) :: X
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
        module subroutine loadbin_1_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_2_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :), allocatable, intent(out) :: X
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
        module subroutine loadbin_2_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadbin_3_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :, :), allocatable, intent(out) :: X
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
        module subroutine loadbin_3_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :, :), allocatable, intent(out) :: X
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
        module subroutine loadtxt_1_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:), allocatable, intent(out) :: X
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
        module subroutine loadtxt_1_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), allocatable, intent(out) :: X
        end subroutine
        module subroutine loadtxt_2_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :), allocatable, intent(out) :: X
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
        module subroutine loadtxt_2_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :), allocatable, intent(out) :: X
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
        module subroutine savebin_1_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:), intent(in) :: X
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
        module subroutine savebin_1_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savebin_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_2_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :), intent(in) :: X
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
        module subroutine savebin_2_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :, :), intent(in) :: X
        end subroutine
        module subroutine savebin_3_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :, :), intent(in) :: X
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
        module subroutine savebin_3_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :, :), intent(in) :: X
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
        module subroutine savetxt_1_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:), intent(in) :: X
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
        module subroutine savetxt_1_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_rsp(filename, X)
            character(len=*), intent(in) :: filename
            real(sp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_rdp(filename, X)
            character(len=*), intent(in) :: filename
            real(dp), dimension(:, :), intent(in) :: X
        end subroutine
        module subroutine savetxt_2_rqp(filename, X)
            character(len=*), intent(in) :: filename
            real(qp), dimension(:, :), intent(in) :: X
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
        module subroutine savetxt_2_cqp(filename, X)
            character(len=*), intent(in) :: filename
            complex(qp), dimension(:, :), intent(in) :: X
        end subroutine
    end interface savetxt

    interface
        !> `set_color` set color for console text.
        subroutine set_color(value) bind(c, name="color")
            use, intrinsic :: iso_c_binding, only: c_long
            integer(c_long), value, intent(in) :: value
        end subroutine set_color
    end interface

contains

    subroutine open_file(self)
        !! Version: experimental
        !!
        !! Open a file
        !! ([Specification](../page/specs/forlab_io.html#description))
        !!
        !!##### Behavior
        !!
        !!
        !! To open a file to read:
        !!
        !!```fortran
        !! u = open("somefile.txt")        ! The default `mode` is "rt"
        !! u = open("somefile.txt", "r")
        !!```
        !!
        !! To open a file to write:
        !!    u = open("somefile.txt", "w")
        !!
        !! To append to the end of the file if it exists:
        !!
        !!    u = open("somefile.txt", "a")

        class(file) :: self
        integer :: stat

        self%unit = open(self%filename, self%mode, iostat=stat)
        if(stat /= 0) call error_stop('Error: File "'//self%filename//&
            '" open failed, iostat = '//to_string(stat))

    end subroutine open_file

    !> `close` closes a `file` object, 
    !>  deallocate `file%filename`.
    subroutine close (self)

        class(file), intent(out) :: self

        deallocate(self%filename)
        close(self%unit)

    end subroutine close

    !> `countlines` counts the number of lines in a txt file.
    subroutine countlines1(self)
        class(file), intent(inout) :: self
        integer :: ierr
        logical :: ok
        
        self%lines = 0
        ok = .false.
        inquire (unit=self%unit, opened=ok)
        if (ok) then
            rewind(self%unit)
            do
                read (self%unit, *, iostat=ierr)
                if (ierr < 0) exit
                self%lines = self%lines + 1
            end do
            rewind(self%unit)
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
            call disp('Warn: linecounts is 0 in ', "'"//self%filename//"'")
        end if

    end subroutine countlines1

    !> The `countlines2` function returns the number of lines of the file
    integer function countlines2(filename)
        character(len=*), intent(in) :: filename
        type(file) :: infile

        infile = file(trim(filename))
        if(.not.infile%exist()) call error_stop('Error: File "'//infile%filename &
                                        //'" open failed.')
        call infile%open()
        call infile%countlines()
        call infile%close()
        countlines2 = infile%lines

    end function countlines2

    !> `file_exist` determines whether a `file` object already exists.
    logical function file_exist1(self)

        class(File), intent(inout) :: self

        inquire (file=self%filename, exist=file_exist1)

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