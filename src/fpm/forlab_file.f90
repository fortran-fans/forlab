module forlab_file
    use forlab_error, only: error_stop
    use forlab_optval, only: optval
    implicit none
    private
    public :: file, countlines, file_exist
    type file
        integer :: unit
        character(:), allocatable :: filename
    contains
        procedure, private :: open1, countlines1, file_exist1
        procedure, public :: close
        generic, public :: open => open1
        generic, public :: countlines => countlines1
        generic, public :: exist => file_exist1
    end type file

    interface file
        procedure init_file
    end interface file

    interface countlines
        procedure countlines2
    end interface countlines

    interface file_exist
        procedure file_exist2
    end interface file_exist
contains

    subroutine open1(self, mode, iostat)
        !! version: experimental
        !!
        !! Opens a file
        !! ([Specification](../page/specs/stdlib_io.html#description_1))
        !!
        !!##### Behavior
        !!
        !!
        !! To open a file to read:
        !!
        !!```fortran
        !! u = open("somefile.txt")        ! The default `mode` is "rt"
        !! u = open("somefile.txt", "r")
        !!```
        !!
        !! To open a file to write:
        !!
        !!```fortran
        !! u = open("somefile.txt", "w")
        !!```
        !!
        !! To append to the end of the file if it exists:
        !!
        !!```fortran
        !! u = open("somefile.txt", "a")
        !!```

        class(file) :: self
        character(*), intent(in), optional :: mode
        integer, intent(out), optional :: iostat

        character(3) :: mode_
        character(:), allocatable :: action_, position_, status_, access_, form_

        mode_ = parse_mode(optval(mode, ""))

        select case (mode_(1:2))
        case ('r')
            action_ = 'read'
            position_ = 'asis'
            status_ = 'old'
        case ('w')
            action_ = 'write'
            position_ = 'asis'
            status_ = 'replace'
        case ('a')
            action_ = 'write'
            position_ = 'append'
            status_ = 'old'
        case ('x')
            action_ = 'write'
            position_ = 'asis'
            status_ = 'new'
        case ('r+')
            action_ = 'readwrite'
            position_ = 'asis'
            status_ = 'old'
        case ('w+')
            action_ = 'readwrite'
            position_ = 'asis'
            status_ = 'replace'
        case ('a+')
            action_ = 'readwrite'
            position_ = 'append'
            status_ = 'old'
        case ('x+')
            action_ = 'readwrite'
            position_ = 'asis'
            status_ = 'new'
        case default
            print *, "Unsupported mode: "//mode_(1:2)
            stop
        end select

        select case (mode_(3:3))
        case ('t')
            form_ = 'formatted'
        case ('b')
            form_ = 'unformatted'
        case default
            call error_stop("Unsupported mode: "//mode_(3:3))
        end select

        access_ = 'stream'

        if (present(iostat)) then
            open (newunit=self%unit, file=self%filename, &
                  action=action_, position=position_, status=status_, &
                  access=access_, form=form_, &
                  iostat=iostat)
        else
            open (newunit=self%unit, file=self%filename, &
                  action=action_, position=position_, status=status_, &
                  access=access_, form=form_)
        end if

    end subroutine open1

    subroutine close (self)
        ! close
        !-----------------------------------------------------------------------
        ! close closes a File object.
        !
        ! Syntax
        !-----------------------------------------------------------------------
        ! call ofile%close()
        !
        ! Description
        !-----------------------------------------------------------------------
        ! call ofile%close() closes the File object ofile.
        class(File) :: self

        close (self%unit)
        return
    end subroutine close

    integer function countlines1(self)
        ! countlines
        !-----------------------------------------------------------------------
        ! countlines counts the number of lines in a txt file.
        !
        ! Syntax
        !-----------------------------------------------------------------------
        ! n = countlines(filename)
        ! n = ofile%countlines()
        !
        ! Description
        !-----------------------------------------------------------------------
        ! n = countlines(filename) returns the number of lines in the txt file
        ! filename.
        !
        ! n = ofile%countlines() returns the number of lines in the txt file
        ! associated to the File object ofile.
        class(File), intent(inout) :: self
        integer :: ierr
        logical :: ok
        countlines1 = 0
        inquire (unit=self%unit, opened=ok)
        if (ok) then
            do
                read (self%unit, *, iostat=ierr)
                if (ierr < 0) exit
                countlines1 = countlines1 + 1
            end do
            rewind(self%unit)
        else
            call self%open()
            do
                read (self%unit, *, iostat=ierr)
                if (ierr < 0) exit
                countlines1 = countlines1 + 1
            end do
            call self%close()
        end if
        if (countlines1 == 0) then
            print *, 'Warn: linecounts is 0 in ', "'"//trim(self%filename)//"'"
        end if
        return
    end function countlines1

    integer function countlines2(filename)
        !! The `countlines2` function returns the number of lines of the file
        character(len=*), intent(in) :: filename
        integer :: ierr
        type(file) :: infile

        infile = File(trim(filename))
        countlines2 = 0
        call infile%open()
        countlines2 = infile%countlines()
        call infile%close()
        return
    end function countlines2

    logical function file_exist1(self)
        ! file_exist
        !-----------------------------------------------------------------------
        ! file_exist determines whether a File object already exists.
        !
        ! Syntax
        !-----------------------------------------------------------------------
        ! exist = ofile % exist()
        !
        ! Description
        !-----------------------------------------------------------------------
        ! call ofile % exist() returns .true. if the File object ofile exists,
        ! .false. otherwise.
        class(File), intent(inout) :: self

        inquire (file=trim(self%filename), exist=file_exist1)
        return
    end function file_exist1

    logical function file_exist2(filename)
        !! The `countlines2` function returns the number of lines of the file
        character(len=*), intent(in) :: filename
        integer :: ierr
        type(File) :: infile

        infile = File(trim(filename))
        file_exist2 = infile%exist()
        return
    end function file_exist2

    type(File) function init_File(filename)
        ! File (constructor)
        !-----------------------------------------------------------------------
        ! File constructs a File object.
        !
        ! Syntax
        !-----------------------------------------------------------------------
        ! ofile = File(filename)
        !
        ! Description
        !-----------------------------------------------------------------------
        ! ofile = File(filename) returns a File object associated to the
        ! file filename with the identifier unit.
        !
        ! Examples
        !-----------------------------------------------------------------------
        ! type(File) :: ofile
        !
        ! ofile = File("myfile.txt")
        ! call ofile%open()
        ! ! ... some operations on this file ...
        ! call ofile%close()
        character(len=*), intent(in) :: filename

        init_File%filename = trim(filename)
        return
    end function init_File

    character(3) function parse_mode(mode) result(mode_)
        character(*), intent(in) :: mode

        integer :: i
        character(:), allocatable :: a
        logical :: lfirst(3)

        mode_ = 'r t'

        if (len_trim(mode) == 0) return
        a = trim(adjustl(mode))

        lfirst = .true.
        do i = 1, len(a)
            if (lfirst(1) &
                .and. (a(i:i) == 'r' .or. a(i:i) == 'w' .or. a(i:i) == 'a' .or. a(i:i) == 'x') &
                ) then
                mode_(1:1) = a(i:i)
                lfirst(1) = .false.
            else if (lfirst(2) .and. a(i:i) == '+') then
                mode_(2:2) = a(i:i)
                lfirst(2) = .false.
            else if (lfirst(3) .and. (a(i:i) == 't' .or. a(i:i) == 'b')) then
                mode_(3:3) = a(i:i)
                lfirst(3) = .false.
            else if (a(i:i) == ' ') then
                cycle
            else if ((.not. lfirst(1)).or.(.not. lfirst(2)).or.(.not. lfirst(3))) then
                call error_stop("Wrong mode: "//trim(a))
            else
                call error_stop("Wrong character: "//a(i:i))
            end if
        end do

    end function parse_mode
end module
