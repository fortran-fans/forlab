

submodule(forlab_sorting) forlab_sorting_sort

    implicit none

contains

    module procedure sort_int8
        integer :: n,order1
        n = size(x)
        sort_int8 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_int8(sort_int8, n, order1)
    end procedure sort_int8
    recursive subroutine quicksort_int8(x, n, order)
        integer(int8), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        integer(int8) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_int8(x(:marker - 1), marker - 1, order)
            call quicksort_int8(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_int8
    module procedure sort_int16
        integer :: n,order1
        n = size(x)
        sort_int16 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_int16(sort_int16, n, order1)
    end procedure sort_int16
    recursive subroutine quicksort_int16(x, n, order)
        integer(int16), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        integer(int16) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_int16(x(:marker - 1), marker - 1, order)
            call quicksort_int16(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_int16
    module procedure sort_int32
        integer :: n,order1
        n = size(x)
        sort_int32 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_int32(sort_int32, n, order1)
    end procedure sort_int32
    recursive subroutine quicksort_int32(x, n, order)
        integer(int32), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        integer(int32) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_int32(x(:marker - 1), marker - 1, order)
            call quicksort_int32(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_int32
    module procedure sort_int64
        integer :: n,order1
        n = size(x)
        sort_int64 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_int64(sort_int64, n, order1)
    end procedure sort_int64
    recursive subroutine quicksort_int64(x, n, order)
        integer(int64), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        integer(int64) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_int64(x(:marker - 1), marker - 1, order)
            call quicksort_int64(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_int64
    module procedure sort_sp
        integer :: n,order1
        n = size(x)
        sort_sp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_sp(sort_sp, n, order1)
    end procedure sort_sp
    recursive subroutine quicksort_sp(x, n, order)
        real(sp), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        real(sp) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_sp(x(:marker - 1), marker - 1, order)
            call quicksort_sp(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_sp
    module procedure sort_dp
        integer :: n,order1
        n = size(x)
        sort_dp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_dp(sort_dp, n, order1)
    end procedure sort_dp
    recursive subroutine quicksort_dp(x, n, order)
        real(dp), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        real(dp) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_dp(x(:marker - 1), marker - 1, order)
            call quicksort_dp(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_dp
    module procedure sort_qp
        integer :: n,order1
        n = size(x)
        sort_qp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort_qp(sort_qp, n, order1)
    end procedure sort_qp
    recursive subroutine quicksort_qp(x, n, order)
        real(qp), dimension(n), intent(inout) :: x
        integer, intent(in) :: n, order
        integer :: left, right, marker
        real(qp) :: pivot, tmp

        if (n .gt. 1) then
            left = 0
            right = n + 1
            block
                integer :: tmp
                call randu(tmp,1,n)
                pivot = x(tmp)
            endblock
            select case (order)
            case (1)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .lt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .gt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case (2)
                do while (left .lt. right)
                    left = left + 1
                    right = right - 1
                    do while (x(left) .gt. pivot)
                        left = left + 1
                    end do
                    do while (x(right) .lt. pivot)
                        right = right - 1
                    end do
                    if (left .lt. right) then
                        tmp = x(left)
                        x(left) = x(right)
                        x(right) = tmp
                    end if
                end do
            case default
                error stop "Error:Sort order MUST be 1 or 2"
            end select
            if (left .eq. right) then
                marker = left + 1
            else
                marker = left
            end if
            call quicksort_qp(x(:marker - 1), marker - 1, order)
            call quicksort_qp(x(marker:), n - marker + 1, order)
        end if
    end subroutine quicksort_qp

end submodule forlab_sorting_sort

