
program main
    use forlab, only: issquare, eye, disp
    implicit none
    real, dimension(:, :), allocatable :: A

    A = eye(2, 2)
    call disp(size(A, 1), 'size 1:')
    call disp(size(A, 2), 'size 2:')
    call disp(size(A), 'size 3:')
    call disp(issquare(A), 'eye(2,2) issquare:')

    A = eye(2, 3)
    call disp(size(A, 1), 'size 1:')
    call disp(size(A, 2), 'size 2:')
    call disp(size(A), 'size 3:')
    call disp(issquare(A), 'eye(2,3) issquare:')

end program