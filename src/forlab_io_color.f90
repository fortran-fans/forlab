!> Some prepared color to choosed.
!> https://rosettacode.org/wiki/Terminal_control/Coloured_text#Fortran
submodule(forlab_io) forlab_io_color

contains

    module subroutine color(string)
        character(len=*), intent(in), optional :: string

        write (*, "(A)", advance="no") optval(string, achar(27)//'[0m')

    end subroutine color

end submodule forlab_io_color
