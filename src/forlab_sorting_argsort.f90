
submodule(forlab_sorting) forlab_sorting_argsort

    implicit none

contains

    module procedure argsort_int8
    integer::i, n
    integer(int8), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_int8 = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_int8(xsort, argsort_int8, n, order1)
    end procedure argsort_int8
    recursive subroutine quickargsort_int8(x, idx, n, order)
        integer(int8), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        integer(int8) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_int8(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_int8(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_int8
    module procedure argsort_int16
    integer::i, n
    integer(int16), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_int16 = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_int16(xsort, argsort_int16, n, order1)
    end procedure argsort_int16
    recursive subroutine quickargsort_int16(x, idx, n, order)
        integer(int16), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        integer(int16) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_int16(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_int16(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_int16
    module procedure argsort_int32
    integer::i, n
    integer(int32), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_int32 = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_int32(xsort, argsort_int32, n, order1)
    end procedure argsort_int32
    recursive subroutine quickargsort_int32(x, idx, n, order)
        integer(int32), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        integer(int32) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_int32(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_int32(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_int32
    module procedure argsort_int64
    integer::i, n
    integer(int64), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_int64 = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_int64(xsort, argsort_int64, n, order1)
    end procedure argsort_int64
    recursive subroutine quickargsort_int64(x, idx, n, order)
        integer(int64), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        integer(int64) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_int64(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_int64(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_int64
    module procedure argsort_sp
    integer::i, n
    real(sp), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_sp = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_sp(xsort, argsort_sp, n, order1)
    end procedure argsort_sp
    recursive subroutine quickargsort_sp(x, idx, n, order)
        real(sp), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        real(sp) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_sp(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_sp(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_sp
    module procedure argsort_dp
    integer::i, n
    real(dp), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_dp = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_dp(xsort, argsort_dp, n, order1)
    end procedure argsort_dp
    recursive subroutine quickargsort_dp(x, idx, n, order)
        real(dp), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        real(dp) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_dp(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_dp(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_dp
    module procedure argsort_qp
    integer::i, n
    real(qp), allocatable::xsort(:)
    integer::order1
    n = size(x)
    xsort = x
    argsort_qp = [(i, i=1, n)]
    if (.not. present(order)) then
        order1 = 1
    else
        order1 = order
    end if
    call quickargsort_qp(xsort, argsort_qp, n, order1)
    end procedure argsort_qp
    recursive subroutine quickargsort_qp(x, idx, n, order)
        real(qp), dimension(n), intent(inout) :: x
        integer, dimension(n), intent(inout) :: idx
        integer, intent(in) :: n, order
        integer:: left, right, marker
        real(qp) :: pivot, tmp1
        integer::tmp2
        if (n > 1) then
            left = 0
            right = n + 1
            pivot = x(randu(1, n))
            select case (order)
            case (1)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) < pivot)
                        left = left + 1
                    end do
                    do while (x(right) > pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case (2)
                do while (left < right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) > pivot)
                        left = left + 1
                    end do
                    do while (x(right) < pivot)
                        right = right - 1
                    end do
                    if (left < right) then
                        tmp1 = x(left)
                        x(left) = x(right)
                        x(right) = tmp1
                        tmp2 = idx(left)
                        idx(left) = idx(right)
                        idx(right) = tmp2
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left == right) then
                marker = left + 1
            else
                marker = left
            end if
            call quickargsort_qp(x(:marker - 1), idx(:marker - 1), marker - 1, order)
            call quickargsort_qp(x(marker:), idx(marker:), n - marker + 1, order)
        end if
    end subroutine quickargsort_qp

end submodule forlab_sorting_argsort
