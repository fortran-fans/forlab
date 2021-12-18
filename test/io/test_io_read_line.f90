program main

    use forlab_io, only: read_line, read_file
    implicit none
    integer :: unit
    character(:), allocatable :: line
    
    open(newunit=unit, file="test/io/test_io_read_line.f90")
    call read_line(line, unit)
    print *, line, len(line)
    
    call read_file(line, "test/io/test_io_read_line.f90")
    print *, line, len(line)
    
    call read_file(line, "test/io/test_io_read_line.f90", keep_newline=.false.)
    print *, line, len(line)

end program main