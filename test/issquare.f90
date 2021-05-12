
program main
    use forlab
    implicit none
    real(kind=RPRE), dimension(:, :), allocatable :: A

    A = eye(3)
    call disp(size(A, 1), 'size 1:')
    call disp(size(A, 2), 'size 2:')
    call disp(size(A), 'size 3:')
    call disp(issquare(A), 'issquare:')

end program
