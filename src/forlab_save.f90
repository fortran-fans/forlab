submodule(forlab) forlab_save
    !! Version: experimental
    !!
    ! savetxt saves 1 and 2-dimensional arrays to txt files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! call savetxt(filename, x)
    ! call savetxt(filename, A)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! call savetxt(filename, x) saves a vector array x into the txt file
    ! filename.
    !
    ! call savetxt(filename, A) saves a 2-dimensional array A into the txt
    ! file filename.
    !
    ! savebin saves arrays to binary files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! call savebin(filename, x)
    ! call savebin(filename, A)
    ! call savebin(filename, X)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! call savebin(filename, x) saves a vector x into the binary file
    ! filename.
    !
    ! call savebin(filename, A) saves a 2-dimensional array into the binary
    ! file filename.
    !
    ! call savebin(filename, X) saves a 3-dimensional array into the binary
    ! file filename.
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
