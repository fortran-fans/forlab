program main
    use forlab, only: arange, disp
    implicit none
    call disp(arange(1, -1), 'arange_example:')
    call disp(arange(-1, 1), 'arange_example:')
    call disp(arange(1, 1), 'arange_example:')
end program
