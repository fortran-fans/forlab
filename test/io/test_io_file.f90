program test_io_file
    use forlab_io, only: file, disp
    use stdlib_error, only: check
    type(file) :: infile

    infile = file('./test/io/test_io_file.f90', 'r')
    call check(infile%exist(), msg='Error: File not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is: ')
    call check(infile%lines == 14, msg="`test_io_file` failed.")
    call infile%close()

end program test_io_file