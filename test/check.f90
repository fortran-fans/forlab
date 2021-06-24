!! fpm test test
program main

    
    block   !! forlab_num2str.fypp
        use forlab, only: setcolor, disp, num2str
        use iso_fortran_env
            call setcolor(2)
            call disp('forlab_num2str.fypp : ')
            call disp('----------------------')
            call setcolor(-1)
            !!! CMPLX
            call disp('num2str(c): ')
            call disp(num2str((1,1))) 
            call disp(num2str((1,1), '(F6.2)'))
            !!! INT
            call disp('num2str(i): ')
            call disp(num2str(2))
            call disp(num2str(2, '(I6)'))
            !!! REAL
            call disp('num2str(r): ')
            call disp(num2str((1.0))) 
            call disp(num2str((1.0), '(F6.2)'))

    endblock


    block   !! forlab_disp.fypp
        use forlab, only: setcolor, disp, num2str, ones
        use iso_fortran_env
        complex :: c(2,2)
        real :: r(2,2)
        integer :: i(2,2)
        call ones(c)
        call ones(r)
        call ones(i)
            call setcolor(3)
            call disp('forlab_disp.fypp : ')
            call disp('----------------------')
            call setcolor(-1)

            !!! CMPLX
            call disp(c, 'disp(ones(c)): ') 
            !!! INT
            call disp(i, 'disp(ones(i)): ')
            !!! REAL
            call disp(r, 'disp(ones(r)): ') 

    endblock
end program
