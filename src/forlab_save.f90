submodule(forlab) forlab_save
    !! Version: experimental
    !!
    !!## Savetxt
    !! savetxt saves 1 and 2-dimensional arrays to txt files.
    !!
    !!### Syntax
    !!    call savetxt(filename, x)
    !!    call savetxt(filename, A)
    !!
    !!### Description
    !! `call savetxt(filename, x)` saves a vector array `x` into the txt file
    !! filename.
    !!
    !! `call savetxt(filename, A)` saves a 2-dimensional array `A` into the txt
    !! file filename.
    !!
    !!## Savebin
    !! savebin saves arrays to binary files.
    !!
    !!### Syntax
    !!    call savebin(filename, x)
    !!    call savebin(filename, A)
    !!    call savebin(filename, X)
    !!
    !!### Description
    !! `call savebin(filename, x)` saves a vector `x` into the binary file
    !! filename.
    !!
    !! `call savebin(filename, A)` saves a 2-dimensional array into the binary
    !! file filename.
    !!
    !! `call savebin(filename, X)` saves a 3-dimensional array into the binary
    !! file filename.
    use forlab_kinds
    implicit none

contains
    module procedure savetxt_1_rsp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rsp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_rsp
        type(File) :: outfile
        character(8), parameter :: type = 'rsp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_rdp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rdp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_rdp
        type(File) :: outfile
        character(8), parameter :: type = 'rdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_rqp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rqp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_rqp
        type(File) :: outfile
        character(8), parameter :: type = 'rqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_iint8
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint8'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_iint8
        type(File) :: outfile
        character(8), parameter :: type = 'iint8'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_iint16
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint16'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_iint16
        type(File) :: outfile
        character(8), parameter :: type = 'iint16'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_iint32
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint32'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_iint32
        type(File) :: outfile
        character(8), parameter :: type = 'iint32'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_iint64
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint64'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_iint64
        type(File) :: outfile
        character(8), parameter :: type = 'iint64'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_csp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'csp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_csp
        type(File) :: outfile
        character(8), parameter :: type = 'csp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_cdp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'cdp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_cdp
        type(File) :: outfile
        character(8), parameter :: type = 'cdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_1_cqp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'cqp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X,1)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_1_cqp
        type(File) :: outfile
        character(8), parameter :: type = 'cqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(1, 4)
        write(outfile%unit) size(X,1)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_rsp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rsp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_rsp
        type(File) :: outfile
        character(8), parameter :: type = 'rsp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_rdp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rdp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_rdp
        type(File) :: outfile
        character(8), parameter :: type = 'rdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_rqp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'rqp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_rqp
        type(File) :: outfile
        character(8), parameter :: type = 'rqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_iint8
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint8'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_iint8
        type(File) :: outfile
        character(8), parameter :: type = 'iint8'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_iint16
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint16'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_iint16
        type(File) :: outfile
        character(8), parameter :: type = 'iint16'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_iint32
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint32'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_iint32
        type(File) :: outfile
        character(8), parameter :: type = 'iint32'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_iint64
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'iint64'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_iint64
        type(File) :: outfile
        character(8), parameter :: type = 'iint64'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_csp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'csp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_csp
        type(File) :: outfile
        character(8), parameter :: type = 'csp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_cdp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'cdp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_cdp
        type(File) :: outfile
        character(8), parameter :: type = 'cdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure

    module procedure savetxt_2_cqp
        integer :: i, m
        type(file) :: outfile
        character(8), parameter :: type = 'cqp'

        outfile = file(trim(filename))
        m = size(x, 1)

        call outfile%open('w t')
        write(outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
        write(outfile%unit, '(A6,A)') 'DATA: ', time_string()

        write(outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X,1), size(X,2)
        write(outfile%unit, '(A)') '---'

        do i = 1, m
            write (outfile%unit, *) X(i, :)
        end do
        call outfile%close()
        return
    end procedure

    module procedure savebin_2_cqp
        type(File) :: outfile
        character(8), parameter :: type = 'cqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(2, 4)
        write(outfile%unit) size(X,1), size(X,2)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_rsp
        type(File) :: outfile
        character(8), parameter :: type = 'rsp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_rdp
        type(File) :: outfile
        character(8), parameter :: type = 'rdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_rqp
        type(File) :: outfile
        character(8), parameter :: type = 'rqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_iint8
        type(File) :: outfile
        character(8), parameter :: type = 'iint8'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_iint16
        type(File) :: outfile
        character(8), parameter :: type = 'iint16'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_iint32
        type(File) :: outfile
        character(8), parameter :: type = 'iint32'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_iint64
        type(File) :: outfile
        character(8), parameter :: type = 'iint64'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_csp
        type(File) :: outfile
        character(8), parameter :: type = 'csp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_cdp
        type(File) :: outfile
        character(8), parameter :: type = 'cdp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    module procedure savebin_3_cqp
        type(File) :: outfile
        character(8), parameter :: type = 'cqp'

        outfile = file(trim(filename))
        call outfile%open('w b')
        write(outfile%unit) type, int(3, 4)
        write(outfile%unit) size(X,1), size(X,2), size(X,3)

        write (outfile%unit) X
        
        call outfile%close()
        return
    end procedure


    character(19) function time_string()
        implicit none
        character(10) :: data, time
        call date_and_time(data, time)
        time_string = data(1:4)//'-'//data(5:6)//'-'//data(7:8)//' '//time(1:2) &
        //':'//time(3:4)//':'//time(5:6)
    end function
end submodule
