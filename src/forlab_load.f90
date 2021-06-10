submodule(forlab) forlab_load
    ! loadbin
    !-----------------------------------------------------------------------
    ! loadbin loads binary files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! x = loadbin(filename)
    ! x = loadbin(filename, kind)
    ! x = loadbin(filename, kind, dim1)
    ! A = loadbin(filename, kind, dim1, dim2)
    ! X = loadbin(filename, kind, dim1, dim2, dim3)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! x = loadbin(filename) loads a 1-dimensional array into x from the
    ! binary file filename treated as 32 bytes floating points.
    !
    ! x = loadbin(filename, kind) loads a 1-dimensional array into x from
    ! the binary file filename.
    !
    ! x = loadbin(filename, kind, dim1) loads a 1-dimensional array into x
    ! from the binary file filename.
    !
    ! A = loadbin(filename, kind, dim1, dim2) loads a 2-dimensional array
    ! into A from the binary file filename.
    !
    ! X = loadbin(filename, kind, dim1, dim2, dim3) loads a 3-dimensional
    ! array into X from the binary file filename.
    !
    ! Notes
    !-----------------------------------------------------------------------
    ! Make sure to use the exact kind:
    !   -   4 for 32 bytes floating points,
    !   -   8 for 64 bytes floating points.


    ! loadtxt
    !-----------------------------------------------------------------------
    ! loadtxt loads txt files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename)
    ! A = loadtxt(filename, dim2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename) loads a 1-dimensional array into x from a txt
    ! file filename.
    !
    ! A = loadtxt(filename, dim2) loads a 2-dimensional array into A from a
    ! txt file filename. dim2 indicates the number of columns of the array.

    use forlab_kinds
    implicit none

contains
    module procedure loadbin_0_sp
        integer :: opt_kind, dim1, fs
        type(File) :: infile
        integer :: unit

        opt_kind = sp

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            inquire (file=filename, size=fs)
            if (mod(fs, opt_kind) .eq. 0) then
                dim1 = fs/opt_kind
                allocate (loadbin_0_sp(dim1))
                call infile%open(opt_kind*dim1)
                read (infile%unit, rec=1) loadbin_0_sp
                call infile%close()
            else
                print *, "Error: in loadbin, file size mismatches kind."
                stop
            end if
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_1_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_1_sp(dim1))
            call infile%open(sp*dim1)
            read (infile%unit, rec=1) loadbin_1_sp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_2_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_2_sp(dim1, dim2))
            call infile%open(sp*dim1*dim2)
            read (infile%unit, rec=1) loadbin_2_sp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_sp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_3_sp(dim1, dim2, dim3))
            call infile%open(sp*dim1*dim2*dim3)
            read (infile%unit, rec=1) loadbin_3_sp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    ! loadtxt
    !-----------------------------------------------------------------------
    ! loadtxt loads txt files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename)
    ! A = loadtxt(filename, dim2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename) loads a 1-dimensional array into x from a txt
    ! file filename.
    !
    ! A = loadtxt(filename, dim2) loads a 2-dimensional array into A from a
    ! txt file filename. dim2 indicates the number of columns of the array.

    module procedure loadtxt_1_sp
        integer :: i, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_1_sp(m))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) loadtxt_1_sp(i)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_sp
        integer(kind=IPRE) :: i, j, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_2_sp(m, dim2))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) (loadtxt_2_sp(i, j), j=1, dim2)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_0_dp
        integer :: opt_kind, dim1, fs
        type(File) :: infile
        integer :: unit

        opt_kind = dp

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            inquire (file=filename, size=fs)
            if (mod(fs, opt_kind) .eq. 0) then
                dim1 = fs/opt_kind
                allocate (loadbin_0_dp(dim1))
                call infile%open(opt_kind*dim1)
                read (infile%unit, rec=1) loadbin_0_dp
                call infile%close()
            else
                print *, "Error: in loadbin, file size mismatches kind."
                stop
            end if
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_1_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_1_dp(dim1))
            call infile%open(dp*dim1)
            read (infile%unit, rec=1) loadbin_1_dp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_2_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_2_dp(dim1, dim2))
            call infile%open(dp*dim1*dim2)
            read (infile%unit, rec=1) loadbin_2_dp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_dp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_3_dp(dim1, dim2, dim3))
            call infile%open(dp*dim1*dim2*dim3)
            read (infile%unit, rec=1) loadbin_3_dp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    ! loadtxt
    !-----------------------------------------------------------------------
    ! loadtxt loads txt files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename)
    ! A = loadtxt(filename, dim2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename) loads a 1-dimensional array into x from a txt
    ! file filename.
    !
    ! A = loadtxt(filename, dim2) loads a 2-dimensional array into A from a
    ! txt file filename. dim2 indicates the number of columns of the array.

    module procedure loadtxt_1_dp
        integer :: i, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_1_dp(m))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) loadtxt_1_dp(i)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_dp
        integer(kind=IPRE) :: i, j, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_2_dp(m, dim2))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) (loadtxt_2_dp(i, j), j=1, dim2)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_0_qp
        integer :: opt_kind, dim1, fs
        type(File) :: infile
        integer :: unit

        opt_kind = qp

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            inquire (file=filename, size=fs)
            if (mod(fs, opt_kind) .eq. 0) then
                dim1 = fs/opt_kind
                allocate (loadbin_0_qp(dim1))
                call infile%open(opt_kind*dim1)
                read (infile%unit, rec=1) loadbin_0_qp
                call infile%close()
            else
                print *, "Error: in loadbin, file size mismatches kind."
                stop
            end if
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_1_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_1_qp(dim1))
            call infile%open(qp*dim1)
            read (infile%unit, rec=1) loadbin_1_qp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_2_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_2_qp(dim1, dim2))
            call infile%open(qp*dim1*dim2)
            read (infile%unit, rec=1) loadbin_2_qp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_qp
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            allocate (loadbin_3_qp(dim1, dim2, dim3))
            call infile%open(qp*dim1*dim2*dim3)
            read (infile%unit, rec=1) loadbin_3_qp
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    ! loadtxt
    !-----------------------------------------------------------------------
    ! loadtxt loads txt files.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename)
    ! A = loadtxt(filename, dim2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! x = loadtxt(filename) loads a 1-dimensional array into x from a txt
    ! file filename.
    !
    ! A = loadtxt(filename, dim2) loads a 2-dimensional array into A from a
    ! txt file filename. dim2 indicates the number of columns of the array.

    module procedure loadtxt_1_qp
        integer :: i, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_1_qp(m))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) loadtxt_1_qp(i)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_qp
        integer(kind=IPRE) :: i, j, m, unit
        type(File) :: infile

        infile = File(unit, trim(filename))
        if (infile%exist()) then
            m = infile%countlines()
            allocate (loadtxt_2_qp(m, dim2))
            call infile%open()
            do i = 1, m
                read (infile%unit, *) (loadtxt_2_qp(i, j), j=1, dim2)
            end do
            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

end submodule
