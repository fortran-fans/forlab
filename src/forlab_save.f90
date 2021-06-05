submodule(forlab) forlab_save
    use forlab_kinds
    implicit none

contains
    !! REAL Versions
    module subroutine savetxt_1_sp(filename, x)
        character(len=*), intent(in) :: filename
        real(sp), dimension(:), intent(in) :: x
        integer :: i, m
        type(file) :: infile
        integer :: unit

        infile = file(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_sp(filename, A)
        character(len=*), intent(in) :: filename
        real(sp), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savebin_1_sp(filename, x)
        character(len=*), intent(in) :: filename
        real(sp), dimension(:), intent(in) :: x
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end subroutine

    module subroutine savebin_2_sp(filename, A)
        character(len=*), intent(in) :: filename
        real(sp), dimension(:, :), intent(in) :: A
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end subroutine

    module subroutine savebin_3_sp(filename, X)
        character(len=*), intent(in) :: filename
        real(sp), dimension(:, :, :), intent(in) :: X
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_1_dp(filename, x)
        character(len=*), intent(in) :: filename
        real(dp), dimension(:), intent(in) :: x
        integer :: i, m
        type(file) :: infile
        integer :: unit

        infile = file(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_dp(filename, A)
        character(len=*), intent(in) :: filename
        real(dp), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savebin_1_dp(filename, x)
        character(len=*), intent(in) :: filename
        real(dp), dimension(:), intent(in) :: x
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end subroutine

    module subroutine savebin_2_dp(filename, A)
        character(len=*), intent(in) :: filename
        real(dp), dimension(:, :), intent(in) :: A
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end subroutine

    module subroutine savebin_3_dp(filename, X)
        character(len=*), intent(in) :: filename
        real(dp), dimension(:, :, :), intent(in) :: X
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_1_qp(filename, x)
        character(len=*), intent(in) :: filename
        real(qp), dimension(:), intent(in) :: x
        integer :: i, m
        type(file) :: infile
        integer :: unit

        infile = file(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_qp(filename, A)
        character(len=*), intent(in) :: filename
        real(qp), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savebin_1_qp(filename, x)
        character(len=*), intent(in) :: filename
        real(qp), dimension(:), intent(in) :: x
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end subroutine

    module subroutine savebin_2_qp(filename, A)
        character(len=*), intent(in) :: filename
        real(qp), dimension(:, :), intent(in) :: A
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end subroutine

    module subroutine savebin_3_qp(filename, X)
        character(len=*), intent(in) :: filename
        real(qp), dimension(:, :, :), intent(in) :: X
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end subroutine


    !! Integer Versions
    module subroutine savetxt_1_int8(filename, x)
        character(len=*), intent(in) :: filename
        integer(int8), dimension(:), intent(in) :: x
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_int8(filename, A)
        character(len=*), intent(in) :: filename
        integer(int8), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_1_int16(filename, x)
        character(len=*), intent(in) :: filename
        integer(int16), dimension(:), intent(in) :: x
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_int16(filename, A)
        character(len=*), intent(in) :: filename
        integer(int16), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_1_int32(filename, x)
        character(len=*), intent(in) :: filename
        integer(int32), dimension(:), intent(in) :: x
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_int32(filename, A)
        character(len=*), intent(in) :: filename
        integer(int32), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_1_int64(filename, x)
        character(len=*), intent(in) :: filename
        integer(int64), dimension(:), intent(in) :: x
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(x)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) x(i)
        end do
        call infile%close()
        return
    end subroutine

    module subroutine savetxt_2_int64(filename, A)
        character(len=*), intent(in) :: filename
        integer(int64), dimension(:, :), intent(in) :: A
        integer :: i, m
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        m = size(A, 1)
        call infile%open()
        do i = 1, m
            write (infile%unit, *) A(i, :)
        end do
        call infile%close()
        return
    end subroutine

end submodule
