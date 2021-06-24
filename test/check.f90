!! fpm test test
program main
    block
        use forlab, only: setcolor, disp, num2str
        use iso_fortran_env
            call setcolor(2)
            !!! CMPLX
            call disp(num2str((1,1))) 
            call disp(num2str((1,1), '(F6.2)'))
            !!! INT
            call disp(num2str(2))
            call disp(num2str(2), '(I6)')
            !!! REAL
            call disp(num2str((1.0))) 
            call disp(num2str((1.0), '(F6.2)'))

    endblock

end program
