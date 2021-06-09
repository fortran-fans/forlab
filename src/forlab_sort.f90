

submodule(forlab) forlab_sort
    !! Version: experimental
    !!
    !! Discussion:
    !! ----
    !! https://fortran-lang.discourse.group/t/fortran-function-return-value-polymorphism/1350/5
    use forlab_kinds
    implicit none
contains
    !! Default versions
    ! sort
    !-----------------------------------------------------------------------
    ! sort sorts arrays elements.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = sort(x)
    ! y = sort(x, 1)
    ! y = sort(x, 2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = sort(x) returns the sorted elements of the vector x in the
    ! ascending order.
    !
    ! y = sort(x, 1) (see y = sort(x)).
    !
    ! y = sort(x, 2) returns the sorted elements of the vector x in the
    ! descending order.
    module procedure sort_int8
        integer :: n,order1
        n = size(x)
        sort_int8 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_int8, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            integer(int8), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            integer(int8) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_int8

    module procedure sort_int16
        integer :: n,order1
        n = size(x)
        sort_int16 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_int16, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            integer(int16), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            integer(int16) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_int16

    module procedure sort_int32
        integer :: n,order1
        n = size(x)
        sort_int32 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_int32, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            integer(int32), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            integer(int32) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_int32

    module procedure sort_int64
        integer :: n,order1
        n = size(x)
        sort_int64 = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_int64, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            integer(int64), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            integer(int64) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_int64

    module procedure sort_sp
        integer :: n,order1
        n = size(x)
        sort_sp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_sp, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            real(sp), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            real(sp) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_sp

    module procedure sort_dp
        integer :: n,order1
        n = size(x)
        sort_dp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_dp, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            real(dp), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            real(dp) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_dp

    module procedure sort_qp
        integer :: n,order1
        n = size(x)
        sort_qp = x
        if (.not. present(order))then
            order1=1
        else
            order1=order
        end if
            call quicksort(sort_qp, n, order1)
    contains
        !-------------------------------------------------------------------
        ! quicksort
        !-------------------------------------------------------------------
        recursive subroutine quicksort(x, n, order)
            real(qp), dimension(n), intent(inout) :: x
            integer, intent(in) :: n, order
            integer :: left, right, marker
            real(qp) :: pivot, tmp

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
                call quicksort(x(:marker - 1), marker - 1, order)
                call quicksort(x(marker:), n - marker + 1, order)
            end if
        end subroutine quicksort
    end procedure sort_qp

end submodule

