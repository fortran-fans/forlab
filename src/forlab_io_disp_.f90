
submodule(forlab_io) forlab_io_disp

    use, intrinsic :: iso_fortran_env, only: output_unit
    use stdlib_string_type, only: char
    implicit none

    character(len=*), parameter :: rfmt = '(*(g12.4, 1x))'
    character(len=*), parameter :: cfmt = '(*(g25.0, 1x))'
    character(len=*), parameter :: fmt_ = 'g0.4'
    integer, parameter :: brief_row = 5
    integer, parameter :: brief_col = 5
    integer, parameter :: default_row = 50
    integer, parameter :: default_col = 10

contains

    module procedure disp_0_rsp
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_rsp

    module procedure disp_1_rsp
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_rsp

    module procedure disp_2_rsp
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_rsp
    module procedure disp_0_rdp
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_rdp

    module procedure disp_1_rdp
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_rdp

    module procedure disp_2_rdp
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_rdp
    module procedure disp_0_iint8
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_iint8

    module procedure disp_1_iint8
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_iint8

    module procedure disp_2_iint8
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_iint8
    module procedure disp_0_iint16
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_iint16

    module procedure disp_1_iint16
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_iint16

    module procedure disp_2_iint16
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_iint16
    module procedure disp_0_iint32
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_iint32

    module procedure disp_1_iint32
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_iint32

    module procedure disp_2_iint32
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_iint32
    module procedure disp_0_iint64
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_iint64

    module procedure disp_1_iint64
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_iint64

    module procedure disp_2_iint64
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_iint64
    module procedure disp_0_llk
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_llk

    module procedure disp_1_llk
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_llk

    module procedure disp_2_llk
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_llk
    module procedure disp_0_lc_bool
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, rfmt) x

    end procedure disp_0_lc_bool

    module procedure disp_1_lc_bool
    integer :: unit_
    logical :: brief_
    integer :: n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, rfmt) x(1:col - 2), '...', x(n)
    else
        write (unit_, rfmt) x(:)
    end if

    end procedure disp_1_lc_bool

    module procedure disp_2_lc_bool
    integer :: unit_
    logical :: brief_
    integer :: i, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
            write (unit_, rfmt) colon(1:col)
            write (unit_, rfmt) x(m, 1:col - 2), '...', x(m, n)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, rfmt) x(i, :)
            end do
            write (unit_, rfmt) colon(1:n)
            write (unit_, rfmt) x(m, :)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, rfmt) x(i, 1:col - 2), '...', x(i, n)
            end do
        end if
    else
        do i = 1, m
            write (unit_, rfmt) x(i, :)
        end do
    end if

    end procedure disp_2_lc_bool

    module procedure disp_0_csp
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, cfmt) to_string(x, fmt_)

    end procedure disp_0_csp

    module procedure disp_1_csp
    integer :: unit_
    logical :: brief_
    integer :: i, n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, cfmt) (to_string(x(i), fmt_), i=1, col - 2), '...', to_string(x(n), fmt_)
    else
        write (unit_, cfmt) (to_string(x(i), fmt_), i=1, n)
    end if

    end procedure disp_1_csp

    module procedure disp_2_csp
    integer :: unit_
    logical :: brief_
    integer :: i, j, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, col - 2), '...', to_string(x(i, n), fmt_)
            end do
            write (unit_, cfmt) colon(1:col)
            write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, col - 2), '...', to_string(x(m, n), fmt_)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, n)
            end do
            write (unit_, cfmt) colon(1:n)
            write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, n)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, col - 2), '...', to_string(x(m, n), fmt_)
            end do
        end if
    else
        do i = 1, m
            write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, n)
        end do
    end if

    end procedure disp_2_csp
    module procedure disp_0_cdp
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, cfmt) to_string(x, fmt_)

    end procedure disp_0_cdp

    module procedure disp_1_cdp
    integer :: unit_
    logical :: brief_
    integer :: i, n, col

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    n = size(x, 1)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[vector size: '//to_string(n)//']'

    if (brief_ .and. n > col) then
        write (unit_, cfmt) (to_string(x(i), fmt_), i=1, col - 2), '...', to_string(x(n), fmt_)
    else
        write (unit_, cfmt) (to_string(x(i), fmt_), i=1, n)
    end if

    end procedure disp_1_cdp

    module procedure disp_2_cdp
    integer :: unit_
    logical :: brief_
    integer :: i, j, m, n
    integer :: row, col
    character(len=1) :: colon(default_col)

    unit_ = optval(unit, output_unit)
    brief_ = optval(brief, .true.)
    row = merge(brief_row, default_row, present(brief) .and. brief_)
    col = merge(brief_col, default_col, present(brief) .and. brief_)
    m = size(x, 1)
    n = size(x, 2)

    if (present(header)) write (unit_, *) header
    write (unit_, *) '[matrix size: '//to_string(m)//'×'//to_string(n)//']'

    if (brief_ .and. (m > row .or. n > col)) then
        colon = ':'
        if (m > row .and. n > col) then
            do i = 1, row - 2
                write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, col - 2), '...', to_string(x(i, n), fmt_)
            end do
            write (unit_, cfmt) colon(1:col)
            write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, col - 2), '...', to_string(x(m, n), fmt_)
        elseif (m > row .and. n <= col) then
            do i = 1, row - 2
                write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, n)
            end do
            write (unit_, cfmt) colon(1:n)
            write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, n)
        elseif (m <= row .and. n > col) then
            do i = 1, m
                write (unit_, cfmt) (to_string(x(m, j), fmt_), j=1, col - 2), '...', to_string(x(m, n), fmt_)
            end do
        end if
    else
        do i = 1, m
            write (unit_, cfmt) (to_string(x(i, j), fmt_), j=1, n)
        end do
    end if

    end procedure disp_2_cdp

    module procedure disp_character
    character(len=:), allocatable :: x_
    integer :: unit_

    x_ = optval(x, '')
    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, *) x_

    end procedure disp_character

    module procedure disp_string_type
    integer :: unit_

    unit_ = optval(unit, output_unit)

    if (present(header)) write (unit_, *) header
    write (unit_, *) char(x)

    end procedure disp_string_type

end submodule forlab_io_disp
