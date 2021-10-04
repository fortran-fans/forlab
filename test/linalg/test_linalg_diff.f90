program test_linalg_diff
    
    use forlab_linalg, only: diff
    use forlab_linalg, only: linspace, seq
    use forlab_io, only: disp
    implicit none

    real :: x(10)
    integer, allocatable :: i(:)
    
    call linspace(x, 0.0, 9.0)
    call disp("Test_linalg_diff_real : ")
    call disp(x, "Linspace(x) : ")
    call disp(diff(x), "Test_linalg_diff : ")

    call seq(i, 0, 9, 1)
    call disp("Test_linalg_diff_integer : ")
    call disp(i, "Seq(i) : ")
    call disp(diff(i), "Test_linalg_diff : ")
    
end program test_linalg_diff