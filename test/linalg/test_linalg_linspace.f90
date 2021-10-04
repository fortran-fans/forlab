program test_linalg_linspace
    use forlab_io, only: disp, file
    use forlab_linalg, only: linspace, logspace
    implicit none
    real :: x(4)
    call linspace(x, 1.0, 10.0)
    call disp(x, 'linspace(x, 1.0, 10.0) : ')
    call logspace(x, 1.0, 10.0)
    call disp(x, 'logspace(x, 1.0, 10.0) : ')
end program test_linalg_linspace