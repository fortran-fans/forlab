
submodule (forlab_strings) forlab_strings_progress

    implicit none

contains
    
    module procedure progress_bar
        integer :: i, perc, opt_step
        character(len=:), allocatable :: bar

        opt_step = 50
        if (present(step)) opt_step = step

        ! Initialize the bar
        bar = "  ["
        do i = 1, opt_step
            bar = bar//" "
        end do
        bar = bar//"]"

        ! Compute the percentage
        perc = real(iter)/real(itermax)*100.

        ! Fill the bar
        do i = 1, floor(perc/(100./opt_step))
            bar(3 + i:3 + i) = "="
        end do

        ! Place the percentage
        i = ceiling((opt_step + 2)/2.)
        write (bar(i + 1:i + 3), "(I3)") perc
        bar(i + 4:i + 4) = "%"

        ! Fill the space
        if (perc < 100 .and. perc > 50 - 100/opt_step) bar(i + 1:i + 1) = "="

        ! Return to the beginning of the line and display the bar
        write (*, "(A1, A)", advance="no") char(13), bar
        return
    end procedure progress_bar

    module procedure progress_perc
        real(kind=8) :: perc
        character(len=:), allocatable :: opt_prefix

        opt_prefix = ""
        if (present(prefix)) opt_prefix = prefix

        perc = real(iter)/real(itermax)*100.
        write (*, "(A1, A, F6.2, A)", advance="no") char(13), opt_prefix, perc, "%"
        return
    end procedure progress_perc

end submodule forlab_strings_progress