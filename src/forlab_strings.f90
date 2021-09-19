
module forlab_strings

    use stdlib_kinds, only: sp, dp, qp, &
        int8, int16, int32, int64, lk, c_bool
    use stdlib_optval, only: optval
    implicit none
    private

    public :: progress_bar, progress_perc

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