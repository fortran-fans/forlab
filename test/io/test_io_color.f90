!> This is not a test.
program test_io_color
    use forlab_io, only: color
    use iso_fortran_env, only: compiler_version
    use forlab_color, only: green, red

    call color(green)
    print *, compiler_version()

    print *, red // compiler_version()
    
    call color()
    print *, compiler_version()

end program test_io_color
