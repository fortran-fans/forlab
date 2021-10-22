
submodule(forlab_io) forlab__io_txt

    use forlab_time, only: time_string
    implicit none

contains

    module procedure loadtxt_1_rsp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rsp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_rsp
    module procedure loadtxt_1_rdp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rdp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_rdp
    module procedure loadtxt_1_rqp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rqp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_rqp
    module procedure loadtxt_1_iint8
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint8'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_iint8
    module procedure loadtxt_1_iint16
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint16'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_iint16
    module procedure loadtxt_1_iint32
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint32'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_iint32
    module procedure loadtxt_1_iint64
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint64'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_iint64
    module procedure loadtxt_1_csp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'csp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_csp
    module procedure loadtxt_1_cdp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'cdp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_cdp
    module procedure loadtxt_1_cqp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'cqp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(1))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 1) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(1)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,1I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_1_cqp
    module procedure loadtxt_2_rsp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rsp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_rsp
    module procedure loadtxt_2_rdp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rdp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_rdp
    module procedure loadtxt_2_rqp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'rqp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_rqp
    module procedure loadtxt_2_iint8
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint8'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_iint8
    module procedure loadtxt_2_iint16
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint16'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_iint16
    module procedure loadtxt_2_iint32
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint32'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_iint32
    module procedure loadtxt_2_iint64
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'iint64'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_iint64
    module procedure loadtxt_2_csp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'csp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_csp
    module procedure loadtxt_2_cdp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'cdp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_cdp
    module procedure loadtxt_2_cqp
    integer :: i, data_dim
    type(File) :: infile
    character(*), parameter :: type = 'cqp'
    character(8) :: datatype
    integer, allocatable :: nsize(:)

    allocate (nsize(2))
    infile = file(filename, 'r t')
    if (infile%exist()) then
        call infile%open()
        read (infile%unit, '(6X,A8,I8)') datatype, data_dim
        if (trim(adjustl(datatype)) /= type) then
            call error_stop('Error: The program failed to try to read a ' &
                            //type//' array, but the file '//trim(filename)// &
                            'stored an array with a '//datatype//'.')
        end if
        if (data_dim /= 2) then
            call error_stop('Error: The program failed to read the ' &
                            //to_string(2)//'-dimensional array. It may be that the file ' &
                            //trim(filename)//' stores an array of different dimensions.')
        end if
        read (infile%unit, *)
        read (infile%unit, '(11X,2I8)') nsize(:)
        read (infile%unit, *)

        allocate (X(nsize(1), nsize(2)))
        do i = 1, nsize(1)
            read (infile%unit, *) X(i, :)
        end do
        call infile%close()
    else
        call error_stop("Error: '"//trim(filename)//"' not found")
    end if
    return
    end procedure loadtxt_2_cqp

    module procedure savetxt_1_rsp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rsp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_rsp
    module procedure savetxt_1_rdp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rdp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_rdp
    module procedure savetxt_1_rqp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rqp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_rqp
    module procedure savetxt_1_iint8
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint8'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_iint8
    module procedure savetxt_1_iint16
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint16'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_iint16
    module procedure savetxt_1_iint32
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint32'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_iint32
    module procedure savetxt_1_iint64
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint64'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_iint64
    module procedure savetxt_1_csp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'csp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_csp
    module procedure savetxt_1_cdp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'cdp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_cdp
    module procedure savetxt_1_cqp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'cqp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(1, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,1(I8))') 'DIMENSION: ', size(X, 1)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i)
    end do
    call outfile%close()
    return
    end procedure savetxt_1_cqp
    module procedure savetxt_2_rsp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rsp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_rsp
    module procedure savetxt_2_rdp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rdp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_rdp
    module procedure savetxt_2_rqp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'rqp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_rqp
    module procedure savetxt_2_iint8
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint8'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_iint8
    module procedure savetxt_2_iint16
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint16'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_iint16
    module procedure savetxt_2_iint32
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint32'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_iint32
    module procedure savetxt_2_iint64
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'iint64'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_iint64
    module procedure savetxt_2_csp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'csp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_csp
    module procedure savetxt_2_cdp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'cdp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_cdp
    module procedure savetxt_2_cqp
    integer :: i, m
    type(file) :: outfile
    character(8), parameter :: type = 'cqp'

    outfile = file(filename, 'w t')
    m = size(x, 1)

    call outfile%open()
    write (outfile%unit, '(A6,A8,I8)') 'TYPE: ', type, int(2, 4)
    write (outfile%unit, '(A6,A)') 'DATA: ', time_string()

    write (outfile%unit, '(A11,2(I8))') 'DIMENSION: ', size(X, 1), size(X, 2)
    write (outfile%unit, '(A)') '---'

    do i = 1, m
        write (outfile%unit, *) X(i, :)
    end do
    call outfile%close()
    return
    end procedure savetxt_2_cqp
end submodule forlab__io_txt
