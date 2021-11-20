module forlab_sorting
    use stdlib_kinds, only: sp, dp, qp, &
                            int8, int16, int32, int64
    use forlab_stats, only: randu
    implicit none
    private

    public :: argsort, sort

    interface argsort
        !! argsort generates the indices that would sort an array.
        module function argsort_int8(x, order)
            integer, allocatable::argsort_int8(:)
            integer(int8), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_int8
        module function argsort_int16(x, order)
            integer, allocatable::argsort_int16(:)
            integer(int16), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_int16
        module function argsort_int32(x, order)
            integer, allocatable::argsort_int32(:)
            integer(int32), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_int32
        module function argsort_int64(x, order)
            integer, allocatable::argsort_int64(:)
            integer(int64), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_int64
        module function argsort_sp(x, order)
            integer, allocatable::argsort_sp(:)
            real(sp), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_sp
        module function argsort_dp(x, order)
            integer, allocatable::argsort_dp(:)
            real(dp), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function argsort_dp
    end interface argsort

    interface sort
        module function sort_int8(x, order)
            integer(int8), allocatable::sort_int8(:)
            integer(int8), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_int8
        module function sort_int16(x, order)
            integer(int16), allocatable::sort_int16(:)
            integer(int16), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_int16
        module function sort_int32(x, order)
            integer(int32), allocatable::sort_int32(:)
            integer(int32), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_int32
        module function sort_int64(x, order)
            integer(int64), allocatable::sort_int64(:)
            integer(int64), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_int64
        module function sort_sp(x, order)
            real(sp), allocatable::sort_sp(:)
            real(sp), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_sp
        module function sort_dp(x, order)
            real(dp), allocatable::sort_dp(:)
            real(dp), intent(in)::x(:)
            integer, optional, intent(in)::order
        end function sort_dp
    end interface sort

end module forlab_sorting
