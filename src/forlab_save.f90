submodule(forlab) forlab_save
    use forlab_kinds
    implicit none

contains
    !! REAL Versions
    module procedure savetxt_1_sp
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
    end procedure

    module procedure savetxt_2_sp
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
    end procedure

    module procedure savebin_1_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end procedure

    module procedure savebin_2_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end procedure

    module procedure savebin_3_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end procedure

    module procedure savetxt_1_dp
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
    end procedure

    module procedure savetxt_2_dp
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
    end procedure

    module procedure savebin_1_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end procedure

    module procedure savebin_2_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end procedure

    module procedure savebin_3_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end procedure

    module procedure savetxt_1_qp
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
    end procedure

    module procedure savetxt_2_qp
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
    end procedure

    module procedure savebin_1_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(x)*size(x))
        write (infile%unit, rec=1) x
        call infile%close()
        return
    end procedure

    module procedure savebin_2_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(A)*size(A))
        write (infile%unit, rec=1) A
        call infile%close()
        return
    end procedure

    module procedure savebin_3_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        call infile%open(kind(X)*size(X))
        write (infile%unit, rec=1) X
        call infile%close()
        return
    end procedure


    !! Integer Versions
    module procedure savetxt_1_int8
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
    end procedure

    module procedure savetxt_2_int8
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
    end procedure

    module procedure savetxt_1_int16
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
    end procedure

    module procedure savetxt_2_int16
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
    end procedure

    module procedure savetxt_1_int32
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
    end procedure

    module procedure savetxt_2_int32
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
    end procedure

    module procedure savetxt_1_int64
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
    end procedure

    module procedure savetxt_2_int64
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
    end procedure

end submodule
