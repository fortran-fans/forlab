program test_linalg_i
    use forlab_linalg, only: operator(.i.)
    use forlab_io, only: disp
    ! test inv and solve equation Ax=b, A^(-1) * b
    real :: a(2, 2) = reshape([1, 2, 3, 1], [2, 2])
    real :: b(2) = [1, 1]
    call disp("Test Inv and solve equation Ax=b")
    call disp('A=')
    call disp(a)
    call disp("b=")
    call disp(b)
    call disp("x=")
    call disp(matmul(.i.a, b))
end program test_linalg_i