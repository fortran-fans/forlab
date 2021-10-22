
submodule(forlab_io) forlab_io_progress_bar

    implicit none

contains

    module subroutine progress_bar_int8(iter, itermax, step, symbol)

        integer(int8), intent(in) :: iter, itermax
        integer(int8), intent(in), optional :: step
        character(*), intent(in), optional :: symbol

        integer(int8) :: step_, i, percentage
        character(:), allocatable :: symbol_, bar

        step_ = optval(step, 50_int8)
        symbol_ = optval(symbol, "=")

        bar = "  ["
        do i = 1_int8, step_
            bar = bar//" "
        end do
        bar = bar//"]"

        percentage = real(iter)/real(itermax)*100.0

        do i = 1_int8, floor(percentage/(100.0/step_), int8)
            bar(3_int8 + i:3_int8 + i) = symbol_
        end do

        i = ceiling((step_ + 2_int8)/2.0, int8)
        write (bar(i + 1_int8:i + 3_int8), "(i3)") percentage
        bar(i + 4_int8:i + 4_int8) = "%"

        if (percentage < 100_int8 .and. percentage > 50_int8 - 100_int8/step_) &
            bar(i + 1_int8:i + 1_int8) = symbol_

        write (*, "(a1, A)", advance="no") achar(13), bar

    end subroutine progress_bar_int8
    module subroutine progress_bar_int16(iter, itermax, step, symbol)

        integer(int16), intent(in) :: iter, itermax
        integer(int16), intent(in), optional :: step
        character(*), intent(in), optional :: symbol

        integer(int16) :: step_, i, percentage
        character(:), allocatable :: symbol_, bar

        step_ = optval(step, 50_int16)
        symbol_ = optval(symbol, "=")

        bar = "  ["
        do i = 1_int16, step_
            bar = bar//" "
        end do
        bar = bar//"]"

        percentage = real(iter)/real(itermax)*100.0

        do i = 1_int16, floor(percentage/(100.0/step_), int16)
            bar(3_int16 + i:3_int16 + i) = symbol_
        end do

        i = ceiling((step_ + 2_int16)/2.0, int16)
        write (bar(i + 1_int16:i + 3_int16), "(i3)") percentage
        bar(i + 4_int16:i + 4_int16) = "%"

        if (percentage < 100_int16 .and. percentage > 50_int16 - 100_int16/step_) &
            bar(i + 1_int16:i + 1_int16) = symbol_

        write (*, "(a1, A)", advance="no") achar(13), bar

    end subroutine progress_bar_int16
    module subroutine progress_bar_int32(iter, itermax, step, symbol)

        integer(int32), intent(in) :: iter, itermax
        integer(int32), intent(in), optional :: step
        character(*), intent(in), optional :: symbol

        integer(int32) :: step_, i, percentage
        character(:), allocatable :: symbol_, bar

        step_ = optval(step, 50_int32)
        symbol_ = optval(symbol, "=")

        bar = "  ["
        do i = 1_int32, step_
            bar = bar//" "
        end do
        bar = bar//"]"

        percentage = real(iter)/real(itermax)*100.0

        do i = 1_int32, floor(percentage/(100.0/step_), int32)
            bar(3_int32 + i:3_int32 + i) = symbol_
        end do

        i = ceiling((step_ + 2_int32)/2.0, int32)
        write (bar(i + 1_int32:i + 3_int32), "(i3)") percentage
        bar(i + 4_int32:i + 4_int32) = "%"

        if (percentage < 100_int32 .and. percentage > 50_int32 - 100_int32/step_) &
            bar(i + 1_int32:i + 1_int32) = symbol_

        write (*, "(a1, A)", advance="no") achar(13), bar

    end subroutine progress_bar_int32
    module subroutine progress_bar_int64(iter, itermax, step, symbol)

        integer(int64), intent(in) :: iter, itermax
        integer(int64), intent(in), optional :: step
        character(*), intent(in), optional :: symbol

        integer(int64) :: step_, i, percentage
        character(:), allocatable :: symbol_, bar

        step_ = optval(step, 50_int64)
        symbol_ = optval(symbol, "=")

        bar = "  ["
        do i = 1_int64, step_
            bar = bar//" "
        end do
        bar = bar//"]"

        percentage = real(iter)/real(itermax)*100.0

        do i = 1_int64, floor(percentage/(100.0/step_), int64)
            bar(3_int64 + i:3_int64 + i) = symbol_
        end do

        i = ceiling((step_ + 2_int64)/2.0, int64)
        write (bar(i + 1_int64:i + 3_int64), "(i3)") percentage
        bar(i + 4_int64:i + 4_int64) = "%"

        if (percentage < 100_int64 .and. percentage > 50_int64 - 100_int64/step_) &
            bar(i + 1_int64:i + 1_int64) = symbol_

        write (*, "(a1, A)", advance="no") achar(13), bar

    end subroutine progress_bar_int64

end submodule forlab_io_progress_bar
