submodule (forlab_io) forlab_io_read_line

    use, intrinsic :: iso_fortran_env, only: stdin => input_unit
    implicit none
    character(*), parameter :: nl = new_line("\n")
    integer, parameter :: buffer_len = 4096
    
contains

    !> Read a line from the input unit.
    subroutine read_line(line, unit, iostat)
    
        character(:), allocatable, intent(out) :: line
        integer, intent(in), optional :: unit
        integer, intent(out), optional :: iostat
        
        integer :: unit_, iostat_
        character(len=buffer_len) :: line_
        character(len=buffer_len) :: msg
        
        unit_ = optval(unit, stdin)
        
        line = ""
        read(unit_, "(A)", iostat=iostat_, iomsg=msg) line_
        if (present(iostat)) then
            iostat = iostat_
            if (iostat_ == 0) line = trim(line_)
        else
            if (iostat_ == 0) then
                line = trim(line_)
            else
                error stop trim(msg)
            end if
        end if        
    
    end subroutine read_line
    
    !> Read ASCII file as a string.
    subroutine read_file(string, file, iostat, keep_newline)
        
        character(:), allocatable, intent(out) :: string
        character(*), intent(in) :: file
        integer, intent(out), optional :: iostat
        logical, intent(in), optional :: keep_newline
        
        integer :: iostat_, unit, count, i
        character(:), allocatable :: string_
        logical :: keep_newline_
        
        keep_newline_ = optval(keep_newline, .true.)
        open(newunit=unit, file=file)
        
        string = ""
        count = countlines(file)
        do i = 1, count
            call read_line(string_, unit, iostat_)
            if (keep_newline_) string_ = string_ // nl
            string = string//string_
        end do
        close(unit)
        
    end subroutine read_file

end submodule forlab_io_read_line