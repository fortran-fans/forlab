program test_io_file
    use forlab_io, only: file, disp
    use stdlib_error, only: error_stop
    type(file) :: infile

    infile = file('DP.txt', 'r')
    if(.not.infile%exist()) call error_stop('Error: File not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is: ')
    call infile%close()

end program test_io_file