program demo_io_color

    use forlab_io, only: color
    use forlab_color, only: red, green
    character(len=*), parameter :: char = "It is a Fortran color: "

    call color(green)
    print *, char // 'green.'

    print *, red // char // 'red.'

    call color()
    print *, char // "default."

end program demo_io_color