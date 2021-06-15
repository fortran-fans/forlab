submodule(forlab) forlab_load
    !! Version: experimental
    !!
    !!## loadbin
    !! loadbin loads binary files.
    !!
    !!### Syntax
    !!    x = loadbin(filename)
    !!    x = loadbin(filename, kind)
    !!    x = loadbin(filename, kind, dim1)
    !!    A = loadbin(filename, kind, dim1, dim2)
    !!    X = loadbin(filename, kind, dim1, dim2, dim3)
    !!
    !!### Description
    !! `x = loadbin(filename)` loads a 1-dimensional array into `x` from the
    !! binary file filename treated as 32 bytes floating points.
    !!
    !! `x = loadbin(filename, kind)` loads a 1-dimensional array into `x` from
    !! the binary file filename.
    !!
    !! `x = loadbin(filename, kind, dim1)` loads a 1-dimensional array into `x`
    !! from the binary file filename.
    !!
    !! `A = loadbin(filename, kind, dim1, dim2)` loads a 2-dimensional array
    !! into `A` from the binary file filename.
    !!
    !! `X = loadbin(filename, kind, dim1, dim2, dim3)` loads a 3-dimensional
    !! array into `X` from the binary file filename.
    !!
    !! Notes
    !!-----------------------------------------------------------------------
    !! Make sure to use the exact kind:
    !!   -   4 for 32 bytes floating points,
    !!   -   8 for 64 bytes floating points.
    !!## loadtxt
    !! loadtxt loads txt files.
    !!
    !!### Syntax
    !!    x = loadtxt(filename)
    !!    A = loadtxt(filename, dim2)
    !!
    !!### Description
    !! `x = loadtxt(filename)` loads a 1-dimensional array into `x` from a txt
    !! file filename.
    !!
    !! `A = loadtxt(filename, dim2)` loads a 2-dimensional array into `A` from a
    !! txt file filename. dim2 indicates the number of columns of the array.
    use forlab_kinds
    use forlab_error, only: error_stop
    implicit none

contains
    module procedure loadbin_1_rsp
        type(File) :: infile
        character(*), parameter :: type = 'rsp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_rsp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rsp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_rdp
        type(File) :: infile
        character(*), parameter :: type = 'rdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_rdp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rdp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_rqp
        type(File) :: infile
        character(*), parameter :: type = 'rqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_rqp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rqp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_iint8
        type(File) :: infile
        character(*), parameter :: type = 'iint8'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_iint8
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint8'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_iint16
        type(File) :: infile
        character(*), parameter :: type = 'iint16'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_iint16
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint16'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_iint32
        type(File) :: infile
        character(*), parameter :: type = 'iint32'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_iint32
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint32'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_iint64
        type(File) :: infile
        character(*), parameter :: type = 'iint64'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_iint64
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint64'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_csp
        type(File) :: infile
        character(*), parameter :: type = 'csp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(sp), allocatable :: rp(:), ip(:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            allocate (rp(nsize(1)))
            allocate (ip(nsize(1)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,sp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_csp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'csp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_cdp
        type(File) :: infile
        character(*), parameter :: type = 'cdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(dp), allocatable :: rp(:), ip(:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            allocate (rp(nsize(1)))
            allocate (ip(nsize(1)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,dp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_cdp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'cdp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_1_cqp
        type(File) :: infile
        character(*), parameter :: type = 'cqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(qp), allocatable :: rp(:), ip(:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 1) then
                call disp('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(1))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1)))

            allocate (rp(nsize(1)))
            allocate (ip(nsize(1)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,qp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_1_cqp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'cqp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(1))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 1) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(1)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,1I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_rsp
        type(File) :: infile
        character(*), parameter :: type = 'rsp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_rsp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rsp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_rdp
        type(File) :: infile
        character(*), parameter :: type = 'rdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_rdp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rdp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_rqp
        type(File) :: infile
        character(*), parameter :: type = 'rqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_rqp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'rqp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_iint8
        type(File) :: infile
        character(*), parameter :: type = 'iint8'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_iint8
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint8'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_iint16
        type(File) :: infile
        character(*), parameter :: type = 'iint16'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_iint16
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint16'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_iint32
        type(File) :: infile
        character(*), parameter :: type = 'iint32'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_iint32
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint32'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_iint64
        type(File) :: infile
        character(*), parameter :: type = 'iint64'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_iint64
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'iint64'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_csp
        type(File) :: infile
        character(*), parameter :: type = 'csp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(sp), allocatable :: rp(:,:), ip(:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            allocate (rp(nsize(1),nsize(2)))
            allocate (ip(nsize(1),nsize(2)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,sp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_csp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'csp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_cdp
        type(File) :: infile
        character(*), parameter :: type = 'cdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(dp), allocatable :: rp(:,:), ip(:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            allocate (rp(nsize(1),nsize(2)))
            allocate (ip(nsize(1),nsize(2)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,dp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_cdp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'cdp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_2_cqp
        type(File) :: infile
        character(*), parameter :: type = 'cqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(qp), allocatable :: rp(:,:), ip(:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 2) then
                call disp('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(2))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2)))

            allocate (rp(nsize(1),nsize(2)))
            allocate (ip(nsize(1),nsize(2)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,qp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadtxt_2_cqp
        integer :: i, data_dim
        type(File) :: infile
        character(*), parameter :: type = 'cqp'
        character(8) :: datatype
        integer, allocatable :: nsize(:)

        allocate(nsize(2))
        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r t')
            read(infile%unit,'(6X,A8,I8)') datatype, data_dim
            if (trim(adjustl(datatype))/=type) then
                call error_stop('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                'stored an array with a '//datatype//'.')
            endif
            if (data_dim /= 2) then
                call error_stop('Error: The program failed to read the ' &
                //num2str(2)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions.')
            endif
            read(infile%unit, *)
            read(infile%unit, '(11X,2I8)') nsize(:)
            read(infile%unit, *)
            
            allocate (X(nsize(1),nsize(2)))
            do i = 1, nsize(1)
                read (infile%unit, *) X(i,:)
            end do
            call infile%close()
        else
            call error_stop("Error: '"//trim(filename)//"' not found")
        end if
        return
    end procedure
    module procedure loadbin_3_rsp
        type(File) :: infile
        character(*), parameter :: type = 'rsp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_rdp
        type(File) :: infile
        character(*), parameter :: type = 'rdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_rqp
        type(File) :: infile
        character(*), parameter :: type = 'rqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_iint8
        type(File) :: infile
        character(*), parameter :: type = 'iint8'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_iint16
        type(File) :: infile
        character(*), parameter :: type = 'iint16'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_iint32
        type(File) :: infile
        character(*), parameter :: type = 'iint32'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_iint64
        type(File) :: infile
        character(*), parameter :: type = 'iint64'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            read (infile%unit) X

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_csp
        type(File) :: infile
        character(*), parameter :: type = 'csp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(sp), allocatable :: rp(:,:,:), ip(:,:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            allocate (rp(nsize(1),nsize(2),nsize(3)))
            allocate (ip(nsize(1),nsize(2),nsize(3)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,sp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_cdp
        type(File) :: infile
        character(*), parameter :: type = 'cdp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(dp), allocatable :: rp(:,:,:), ip(:,:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            allocate (rp(nsize(1),nsize(2),nsize(3)))
            allocate (ip(nsize(1),nsize(2),nsize(3)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,dp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

    module procedure loadbin_3_cqp
        type(File) :: infile
        character(*), parameter :: type = 'cqp'
        integer, allocatable :: nsize(:)
        character(8) :: datatype
        integer :: data_dim
        real(qp), allocatable :: rp(:,:,:), ip(:,:,:)

        infile = File(trim(filename))
        if (infile%exist()) then
            call infile%open('r b')
            read (infile%unit) datatype, data_dim
            if (trim(adjustl(datatype)) /= type) then
                call disp('Error: The program failed to try to read a '&
                //type//' array, but the file '//trim(filename)//&
                ' stored an array with a '//datatype//'.')
                stop
            endif
            if (data_dim /= 3) then
                call disp('Error: The program failed to read the ' &
                //num2str(3)//'-dimensional array. It may be that the file '&
                //trim(filename)//' stores an array of different dimensions '&
                //num2str(data_dim)//'.')
                stop
            endif

            allocate(nsize(3))
            read (infile%unit) nsize(:)
            
            allocate (X(nsize(1),nsize(2),nsize(3)))

            allocate (rp(nsize(1),nsize(2),nsize(3)))
            allocate (ip(nsize(1),nsize(2),nsize(3)))
            read (infile%unit) rp, ip
            X = cmplx(rp,ip,qp)

            call infile%close()
        else
            print *, "Error: '"//trim(filename)//"' not found"
            stop
        end if
        return
    end procedure

end submodule
