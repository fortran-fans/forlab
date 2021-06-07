
submodule(forlab) forlab_argsort
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    !! Default versions
    ! argsort
    !-----------------------------------------------------------------------
    ! argsort generates the indices that would sort an array.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = argsort(x)
    ! y = argsort(x, 1)
    ! y = argsort(x, 2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = argsort(x) returns the indices that would sort an array in
    ! ascending order.
    !
    ! y = argsort(x, 1) (see y = argsort(x)).
    !
    ! y = argsort(x, 2) returns the indices that would sort an array in
    ! descending order.
    !
    ! Notes
    !-----------------------------------------------------------------------
    ! x(argsort(x), order) returns the same result as sort(x, order).

    module procedure argsort_int8
        integer::i,n
        integer(int8),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_int8 = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_int8, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            integer(int8), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            integer(int8) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_int8

    module procedure argsort_int16
        integer::i,n
        integer(int16),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_int16 = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_int16, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            integer(int16), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            integer(int16) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_int16

    module procedure argsort_int32
        integer::i,n
        integer(int32),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_int32 = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_int32, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            integer(int32), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            integer(int32) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_int32

    module procedure argsort_int64
        integer::i,n
        integer(int64),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_int64 = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_int64, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            integer(int64), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            integer(int64) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_int64

    module procedure argsort_sp
        integer::i,n
        real(sp),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_sp = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_sp, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            real(sp), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            real(sp) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_sp

    module procedure argsort_dp
        integer::i,n
        real(dp),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_dp = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_dp, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            real(dp), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            real(dp) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_dp

    module procedure argsort_qp
        integer::i,n
        real(qp),allocatable::xsort(:)
        integer::order1
        n = size(x)
        xsort = x
        argsort_qp = [(i, i=1, n)]
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
        call quickargsort(xsort, argsort_qp, n, order1)
    contains
        recursive subroutine quickargsort(x, idx, n, order)
            real(qp), dimension(n), intent(inout) :: x
            integer, dimension(n), intent(inout) :: idx
            integer, intent(in) :: n, order
            integer:: left, right, marker
            real(qp) :: pivot,tmp1
            integer::tmp2
            if (n .gt. 1) then
                left = 0
                right = n + 1
                pivot = x(randi(n))
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
                            tmp1 = x(left)
                            x(left) = x(right)
                            x(right) = tmp1
                            tmp2 = idx(left)
                            idx(left) = idx(right)
                            idx(right) = tmp2
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
                if (left .eq. right) then
                    marker = left + 1
                else
                    marker = left
                end if
                call quickargsort(x(:marker - 1), idx(:marker - 1), marker - 1, order)
                call quickargsort(x(marker:), idx(marker:), n - marker + 1, order)
            end if
        end subroutine quickargsort
    end procedure argsort_qp

end submodule
