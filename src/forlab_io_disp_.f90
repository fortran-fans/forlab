
submodule (forlab_io) forlab_io_disp

    implicit none
    character(len=*), parameter :: fmt_r = '(*(g12.4, 1x))'
    character(len=*), parameter :: fmt_c = '(*(g25.0, 1x))'
    
contains
    
    module procedure disp_0_rsp
        !! Disp real(sp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_rsp

    module procedure disp_1_rsp
        !! Disp real(sp) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_rsp

    module procedure disp_2_rsp
        !! Disp real(sp) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_rsp

    module procedure disp_3_rsp
        !! Disp real(sp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_rsp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_rsp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_rsp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_rsp
    module procedure disp_0_rdp
        !! Disp real(dp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_rdp

    module procedure disp_1_rdp
        !! Disp real(dp) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_rdp

    module procedure disp_2_rdp
        !! Disp real(dp) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_rdp

    module procedure disp_3_rdp
        !! Disp real(dp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_rdp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_rdp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_rdp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_rdp
    module procedure disp_0_rqp
        !! Disp real(qp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_rqp

    module procedure disp_1_rqp
        !! Disp real(qp) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_rqp

    module procedure disp_2_rqp
        !! Disp real(qp) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_rqp

    module procedure disp_3_rqp
        !! Disp real(qp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_rqp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_rqp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_rqp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_rqp
    module procedure disp_0_iint8
        !! Disp integer(int8) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_iint8

    module procedure disp_1_iint8
        !! Disp integer(int8) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_iint8

    module procedure disp_2_iint8
        !! Disp integer(int8) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_iint8

    module procedure disp_3_iint8
        !! Disp integer(int8) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_iint8(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_iint8(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_iint8(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_iint8
    module procedure disp_0_iint16
        !! Disp integer(int16) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_iint16

    module procedure disp_1_iint16
        !! Disp integer(int16) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_iint16

    module procedure disp_2_iint16
        !! Disp integer(int16) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_iint16

    module procedure disp_3_iint16
        !! Disp integer(int16) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_iint16(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_iint16(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_iint16(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_iint16
    module procedure disp_0_iint32
        !! Disp integer(int32) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_iint32

    module procedure disp_1_iint32
        !! Disp integer(int32) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_iint32

    module procedure disp_2_iint32
        !! Disp integer(int32) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_iint32

    module procedure disp_3_iint32
        !! Disp integer(int32) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_iint32(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_iint32(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_iint32(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_iint32
    module procedure disp_0_iint64
        !! Disp integer(int64) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_iint64

    module procedure disp_1_iint64
        !! Disp integer(int64) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_iint64

    module procedure disp_2_iint64
        !! Disp integer(int64) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_iint64

    module procedure disp_3_iint64
        !! Disp integer(int64) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_iint64(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_iint64(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_iint64(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_iint64
    module procedure disp_0_llk
        !! Disp logical(lk) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_llk

    module procedure disp_1_llk
        !! Disp logical(lk) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_llk

    module procedure disp_2_llk
        !! Disp logical(lk) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_llk

    module procedure disp_3_llk
        !! Disp logical(lk) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_llk(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_llk(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_llk(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_llk
    module procedure disp_0_lc_bool
        !! Disp logical(c_bool) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value
    end procedure disp_0_lc_bool

    module procedure disp_1_lc_bool
        !! Disp logical(c_bool) vector variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_r, value(:)
    end procedure disp_1_lc_bool

    module procedure disp_2_lc_bool
        !! Disp logical(c_bool) 2D array variable to default output_unit
        integer :: i, m
        m = size(value, 1)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, value(i,:)
        end do
    end procedure disp_2_lc_bool

    module procedure disp_3_lc_bool
        !! Disp logical(c_bool) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_lc_bool(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_lc_bool(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_lc_bool(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_lc_bool

    module procedure disp_0_csp
        !! Disp complex(sp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_c, format_string(value, '(g0.4)')
    end procedure disp_0_csp

    module procedure disp_1_csp
        !! Disp complex(sp) vector variable to default output_unit
        integer :: i, m
        m = size(value)
        if(present(string)) print *, trim(string)
        print fmt_c, (format_string(value(i), '(g0.4)'), i=1, m)
    end procedure disp_1_csp

    module procedure disp_2_csp
        !! Disp complex(sp) 2D array variable to default output_unit
        integer :: i, j, m, n
        m = size(value, 1)
        n = size(value, 2)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_c, (format_string(value(i, j), '(g0.4)'), j=1, n)
        end do
    end procedure disp_2_csp

    module procedure disp_3_csp
        !! Disp complex(sp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_csp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_csp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_csp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_csp
    module procedure disp_0_cdp
        !! Disp complex(dp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_c, format_string(value, '(g0.4)')
    end procedure disp_0_cdp

    module procedure disp_1_cdp
        !! Disp complex(dp) vector variable to default output_unit
        integer :: i, m
        m = size(value)
        if(present(string)) print *, trim(string)
        print fmt_c, (format_string(value(i), '(g0.4)'), i=1, m)
    end procedure disp_1_cdp

    module procedure disp_2_cdp
        !! Disp complex(dp) 2D array variable to default output_unit
        integer :: i, j, m, n
        m = size(value, 1)
        n = size(value, 2)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_c, (format_string(value(i, j), '(g0.4)'), j=1, n)
        end do
    end procedure disp_2_cdp

    module procedure disp_3_cdp
        !! Disp complex(dp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_cdp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_cdp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_cdp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_cdp
    module procedure disp_0_cqp
        !! Disp complex(qp) variable to default output_unit
        if(present(string)) print *, trim(string)
        print fmt_c, format_string(value, '(g0.4)')
    end procedure disp_0_cqp

    module procedure disp_1_cqp
        !! Disp complex(qp) vector variable to default output_unit
        integer :: i, m
        m = size(value)
        if(present(string)) print *, trim(string)
        print fmt_c, (format_string(value(i), '(g0.4)'), i=1, m)
    end procedure disp_1_cqp

    module procedure disp_2_cqp
        !! Disp complex(qp) 2D array variable to default output_unit
        integer :: i, j, m, n
        m = size(value, 1)
        n = size(value, 2)
        if(present(string)) print *, trim(string)
        do i = 1, m
            print fmt_c, (format_string(value(i, j), '(g0.4)'), j=1, n)
        end do
    end procedure disp_2_cqp

    module procedure disp_3_cqp
        !! Disp complex(qp) 3D array variable to default output_unit
        integer :: i, dim1, dim2, dim3
        dim1 = size(value, 1)
        dim2 = size(value, 2)
        dim3 = size(value, 3)
        if(present(string)) print *, trim(string)
        if(dim == 1) then
            do i = 1, dim1
                print *, 'Slice ('//to_string(i)//',:,:):'
                call disp_2_cqp(value(i, :, :))
            end do
        elseif(dim == 2) then
            do i = 1, dim2
                print *, 'Slice (:,'//to_string(i)//',:):'
                call disp_2_cqp(value(:, i, :))
            end do
        elseif (dim == 3) then
            do i = 1, dim3
                print *, 'Slice (:,:,'//to_string(i)//'):'
                call disp_2_cqp(value(:, :, i))
            end do
        else
            call error_stop('Error(disp): wrong dimension')
        end if
    end procedure disp_3_cqp

    module procedure disp_str
        !! Disp character variable to default output_unit
        if(present(string)) print *, trim(string)
        if(present(value)) then
            print *, trim(value)
        else
            print *, ''
        end if
    end procedure disp_str

end submodule forlab_io_disp