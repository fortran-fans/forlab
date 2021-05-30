program main
    use forlab
    implicit none
    real(kind=RPRE), allocatable :: x(:, :)

    x = empty(2, 3)
    print *, x

end program
