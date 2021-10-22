
submodule(forlab_io) forlab_io_progress_perc

    implicit none

contains

    module subroutine progress_perc_int8(iter, itermax, prefix)

        integer(int8), intent(in) :: iter, itermax
        character(*), intent(in), optional :: prefix

        real(sp) :: percentage
        character(:), allocatable :: prefix_

        prefix_ = optval(prefix, "")
        percentage = real(iter, sp)/real(itermax, sp)*100.0_sp
        write (*, "(a1,A,f6.2,A)", advance="no") achar(13), prefix_, percentage, "%"

    end subroutine progress_perc_int8
    module subroutine progress_perc_int16(iter, itermax, prefix)

        integer(int16), intent(in) :: iter, itermax
        character(*), intent(in), optional :: prefix

        real(sp) :: percentage
        character(:), allocatable :: prefix_

        prefix_ = optval(prefix, "")
        percentage = real(iter, sp)/real(itermax, sp)*100.0_sp
        write (*, "(a1,A,f6.2,A)", advance="no") achar(13), prefix_, percentage, "%"

    end subroutine progress_perc_int16
    module subroutine progress_perc_int32(iter, itermax, prefix)

        integer(int32), intent(in) :: iter, itermax
        character(*), intent(in), optional :: prefix

        real(sp) :: percentage
        character(:), allocatable :: prefix_

        prefix_ = optval(prefix, "")
        percentage = real(iter, sp)/real(itermax, sp)*100.0_sp
        write (*, "(a1,A,f6.2,A)", advance="no") achar(13), prefix_, percentage, "%"

    end subroutine progress_perc_int32
    module subroutine progress_perc_int64(iter, itermax, prefix)

        integer(int64), intent(in) :: iter, itermax
        character(*), intent(in), optional :: prefix

        real(sp) :: percentage
        character(:), allocatable :: prefix_

        prefix_ = optval(prefix, "")
        percentage = real(iter, sp)/real(itermax, sp)*100.0_sp
        write (*, "(a1,A,f6.2,A)", advance="no") achar(13), prefix_, percentage, "%"

    end subroutine progress_perc_int64

end submodule forlab_io_progress_perc
