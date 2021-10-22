
submodule(forlab_io) forlab_io_bin

    implicit none

contains

    module procedure loadbin_1_rsp
    type(file) :: infile
    character(*), parameter :: type = 'rsp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_rsp
    module procedure loadbin_1_rdp
    type(file) :: infile
    character(*), parameter :: type = 'rdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_rdp
    module procedure loadbin_1_rqp
    type(file) :: infile
    character(*), parameter :: type = 'rqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_rqp
    module procedure loadbin_1_iint8
    type(file) :: infile
    character(*), parameter :: type = 'iint8'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_iint8
    module procedure loadbin_1_iint16
    type(file) :: infile
    character(*), parameter :: type = 'iint16'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_iint16
    module procedure loadbin_1_iint32
    type(file) :: infile
    character(*), parameter :: type = 'iint32'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_iint32
    module procedure loadbin_1_iint64
    type(file) :: infile
    character(*), parameter :: type = 'iint64'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_iint64
    module procedure loadbin_1_csp
    type(file) :: infile
    character(*), parameter :: type = 'csp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(sp), allocatable :: rp(:), ip(:)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        allocate (rp(nsize(1)))
        allocate (ip(nsize(1)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, sp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_csp
    module procedure loadbin_1_cdp
    type(file) :: infile
    character(*), parameter :: type = 'cdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(dp), allocatable :: rp(:), ip(:)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        allocate (rp(nsize(1)))
        allocate (ip(nsize(1)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, dp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_cdp
    module procedure loadbin_1_cqp
    type(file) :: infile
    character(*), parameter :: type = 'cqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(qp), allocatable :: rp(:), ip(:)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 1) then
            call disp('Error: The program failed to read the ' &
                      //to_string(1)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(1))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1)))

        allocate (rp(nsize(1)))
        allocate (ip(nsize(1)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, qp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_1_cqp
    module procedure loadbin_2_rsp
    type(file) :: infile
    character(*), parameter :: type = 'rsp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_rsp
    module procedure loadbin_2_rdp
    type(file) :: infile
    character(*), parameter :: type = 'rdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_rdp
    module procedure loadbin_2_rqp
    type(file) :: infile
    character(*), parameter :: type = 'rqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_rqp
    module procedure loadbin_2_iint8
    type(file) :: infile
    character(*), parameter :: type = 'iint8'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_iint8
    module procedure loadbin_2_iint16
    type(file) :: infile
    character(*), parameter :: type = 'iint16'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_iint16
    module procedure loadbin_2_iint32
    type(file) :: infile
    character(*), parameter :: type = 'iint32'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_iint32
    module procedure loadbin_2_iint64
    type(file) :: infile
    character(*), parameter :: type = 'iint64'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_iint64
    module procedure loadbin_2_csp
    type(file) :: infile
    character(*), parameter :: type = 'csp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(sp), allocatable :: rp(:, :), ip(:, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        allocate (rp(nsize(1), nsize(2)))
        allocate (ip(nsize(1), nsize(2)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, sp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_csp
    module procedure loadbin_2_cdp
    type(file) :: infile
    character(*), parameter :: type = 'cdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(dp), allocatable :: rp(:, :), ip(:, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        allocate (rp(nsize(1), nsize(2)))
        allocate (ip(nsize(1), nsize(2)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, dp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_cdp
    module procedure loadbin_2_cqp
    type(file) :: infile
    character(*), parameter :: type = 'cqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(qp), allocatable :: rp(:, :), ip(:, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 2) then
            call disp('Error: The program failed to read the ' &
                      //to_string(2)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(2))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2)))

        allocate (rp(nsize(1), nsize(2)))
        allocate (ip(nsize(1), nsize(2)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, qp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_2_cqp
    module procedure loadbin_3_rsp
    type(file) :: infile
    character(*), parameter :: type = 'rsp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_rsp
    module procedure loadbin_3_rdp
    type(file) :: infile
    character(*), parameter :: type = 'rdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_rdp
    module procedure loadbin_3_rqp
    type(file) :: infile
    character(*), parameter :: type = 'rqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_rqp
    module procedure loadbin_3_iint8
    type(file) :: infile
    character(*), parameter :: type = 'iint8'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_iint8
    module procedure loadbin_3_iint16
    type(file) :: infile
    character(*), parameter :: type = 'iint16'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_iint16
    module procedure loadbin_3_iint32
    type(file) :: infile
    character(*), parameter :: type = 'iint32'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_iint32
    module procedure loadbin_3_iint64
    type(file) :: infile
    character(*), parameter :: type = 'iint64'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        read (infile%unit) X

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_iint64
    module procedure loadbin_3_csp
    type(file) :: infile
    character(*), parameter :: type = 'csp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(sp), allocatable :: rp(:, :, :), ip(:, :, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        allocate (rp(nsize(1), nsize(2), nsize(3)))
        allocate (ip(nsize(1), nsize(2), nsize(3)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, sp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_csp
    module procedure loadbin_3_cdp
    type(file) :: infile
    character(*), parameter :: type = 'cdp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(dp), allocatable :: rp(:, :, :), ip(:, :, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        allocate (rp(nsize(1), nsize(2), nsize(3)))
        allocate (ip(nsize(1), nsize(2), nsize(3)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, dp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_cdp
    module procedure loadbin_3_cqp
    type(file) :: infile
    character(*), parameter :: type = 'cqp'
    integer, allocatable :: nsize(:)
    character(8) :: datatype
    integer :: data_dim
    real(qp), allocatable :: rp(:, :, :), ip(:, :, :)

    infile = file(filename, 'r b')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit) datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call disp('Error: The program failed to try to read a ' &
                      //type//' array, but the file '//trim(filename)// &
                      ' stored an array with a '//datatype//'.')
            stop
        end if
        if (data_dim /= 3) then
            call disp('Error: The program failed to read the ' &
                      //to_string(3)//'-dimensional array. It may be that the file ' &
                      //trim(filename)//' stores an array of different dimensions ' &
                      //to_string(data_dim)//'.')
            stop
        end if

        allocate (nsize(3))
        read (infile%unit) nsize(:)

        allocate (X(nsize(1), nsize(2), nsize(3)))

        allocate (rp(nsize(1), nsize(2), nsize(3)))
        allocate (ip(nsize(1), nsize(2), nsize(3)))
        read (infile%unit) rp, ip
        X = cmplx(rp, ip, qp)

        call infile%close()
    else
        print *, "Error: '"//trim(filename)//"' not found"
        stop
    end if
    return
    end procedure loadbin_3_cqp

    module procedure savebin_1_rsp
    type(File) :: outfile
    character(8), parameter :: type = 'rsp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_rsp
    module procedure savebin_1_rdp
    type(File) :: outfile
    character(8), parameter :: type = 'rdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_rdp
    module procedure savebin_1_rqp
    type(File) :: outfile
    character(8), parameter :: type = 'rqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_rqp
    module procedure savebin_1_iint8
    type(File) :: outfile
    character(8), parameter :: type = 'iint8'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_iint8
    module procedure savebin_1_iint16
    type(File) :: outfile
    character(8), parameter :: type = 'iint16'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_iint16
    module procedure savebin_1_iint32
    type(File) :: outfile
    character(8), parameter :: type = 'iint32'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_iint32
    module procedure savebin_1_iint64
    type(File) :: outfile
    character(8), parameter :: type = 'iint64'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_1_iint64
    module procedure savebin_1_csp
    type(File) :: outfile
    character(8), parameter :: type = 'csp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_1_csp
    module procedure savebin_1_cdp
    type(File) :: outfile
    character(8), parameter :: type = 'cdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_1_cdp
    module procedure savebin_1_cqp
    type(File) :: outfile
    character(8), parameter :: type = 'cqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(1, 4)
    write (outfile%unit) size(X, 1)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_1_cqp
    module procedure savebin_2_rsp
    type(File) :: outfile
    character(8), parameter :: type = 'rsp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_rsp
    module procedure savebin_2_rdp
    type(File) :: outfile
    character(8), parameter :: type = 'rdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_rdp
    module procedure savebin_2_rqp
    type(File) :: outfile
    character(8), parameter :: type = 'rqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_rqp
    module procedure savebin_2_iint8
    type(File) :: outfile
    character(8), parameter :: type = 'iint8'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_iint8
    module procedure savebin_2_iint16
    type(File) :: outfile
    character(8), parameter :: type = 'iint16'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_iint16
    module procedure savebin_2_iint32
    type(File) :: outfile
    character(8), parameter :: type = 'iint32'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_iint32
    module procedure savebin_2_iint64
    type(File) :: outfile
    character(8), parameter :: type = 'iint64'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_2_iint64
    module procedure savebin_2_csp
    type(File) :: outfile
    character(8), parameter :: type = 'csp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_2_csp
    module procedure savebin_2_cdp
    type(File) :: outfile
    character(8), parameter :: type = 'cdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_2_cdp
    module procedure savebin_2_cqp
    type(File) :: outfile
    character(8), parameter :: type = 'cqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(2, 4)
    write (outfile%unit) size(X, 1), size(X, 2)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_2_cqp
    module procedure savebin_3_rsp
    type(File) :: outfile
    character(8), parameter :: type = 'rsp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_rsp
    module procedure savebin_3_rdp
    type(File) :: outfile
    character(8), parameter :: type = 'rdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_rdp
    module procedure savebin_3_rqp
    type(File) :: outfile
    character(8), parameter :: type = 'rqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_rqp
    module procedure savebin_3_iint8
    type(File) :: outfile
    character(8), parameter :: type = 'iint8'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_iint8
    module procedure savebin_3_iint16
    type(File) :: outfile
    character(8), parameter :: type = 'iint16'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_iint16
    module procedure savebin_3_iint32
    type(File) :: outfile
    character(8), parameter :: type = 'iint32'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_iint32
    module procedure savebin_3_iint64
    type(File) :: outfile
    character(8), parameter :: type = 'iint64'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) X

    call outfile%close()
    return
    end procedure savebin_3_iint64
    module procedure savebin_3_csp
    type(File) :: outfile
    character(8), parameter :: type = 'csp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_3_csp
    module procedure savebin_3_cdp
    type(File) :: outfile
    character(8), parameter :: type = 'cdp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_3_cdp
    module procedure savebin_3_cqp
    type(File) :: outfile
    character(8), parameter :: type = 'cqp'

    outfile = file(filename, 'w b')
    call outfile%open()
    write (outfile%unit) type, int(3, 4)
    write (outfile%unit) size(X, 1), size(X, 2), size(X, 3)

    write (outfile%unit) real(X), imag(X)
            !! Precision

    call outfile%close()
    return
    end procedure savebin_3_cqp

end submodule
