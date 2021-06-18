!! fpm test test
program main
    block
        use forlab, only: setcolor, disp
        use iso_fortran_env
        do i = 2, 4
            call setcolor(i)
            call disp(compiler_version())
        enddo
    endblock

end program
