module forlab_file
    implicit none
    public :: file
    type file
        integer :: unit
        character(:), allocatable :: filename
    contains
        procedure, private :: open1, open2, countlines1, file_exist
        procedure, public :: close
        generic, public :: open => open1, open2
        generic, public :: countlines => countlines1
        generic, public :: exist => file_exist
    end type file
    interface file
        module procedure init_file
    end interface file
contains

    ! open
    !-----------------------------------------------------------------------
    ! open opens a File object with sequential or direct access.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! call ofile % open()
    ! call ofile % open(r)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! call ofile % open() open the File object ofile with sequential access.
    !
    ! call ofile % open(r) open the File object ofile with direct access,
    ! where r is the record length.

    subroutine open1(self)
    !! Version: experimental
    !!
    !! Use fortran08 NEWUNIT syntax.
        class(File), intent(inout) :: self
        integer :: ierr

        open (newunit=self%unit, file=self%filename, access="sequential", &
              form="formatted", status="unknown", iostat=ierr)
        if (ierr /= 0) then
            print *, "Error: cannot read '"//trim(self%filename)//"'"
            stop
        end if
        return
    end subroutine open1

    subroutine open2(self, r)
    !! Version: experimental
    !!
        integer, intent(in) :: r
        class(File), intent(inout) :: self
        integer :: ierr

        open (newunit=self%unit, file=self%filename, access="direct", &
              form="unformatted", status="unknown", recl=r, iostat=ierr)
        if (ierr /= 0) then
            print *, "Error: cannot read '"//trim(self%filename)//"'"
            stop
        end if
        return
    end subroutine open2

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

    subroutine close (self)
        class(File) :: self

        close (self%unit)
        return
    end subroutine close

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

    integer function countlines1(self)
        class(File), intent(inout) :: self
        integer :: ierr

        countlines1 = 0
        call self%open()
        do
            read (self%unit, *, iostat=ierr)
            if (ierr < 0) exit
            countlines1 = countlines1 + 1
        end do
        call self%close()
        return
    end function countlines1

    integer function countlines2(filename)
    !! The `countlines2` function returns the number of lines of the file
        character(len=*), intent(in) :: filename
        integer :: ierr
        type(File) :: infile
        integer :: unit

        infile = File(unit, trim(filename))
        countlines2 = 0
        call infile%open()
        do
            read (infile%unit, *, iostat=ierr)
            if (ierr < 0) exit
            countlines2 = countlines2 + 1
        end do
        call infile%close()
        return
    end function countlines2

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

    logical function file_exist(self)
        class(File), intent(inout) :: self

        inquire (file=trim(self%filename), exist=file_exist)
        return
    end function file_exist

    ! File (constructor)
    !-----------------------------------------------------------------------
    ! File constructs a File object.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! ofile = File(unit, filename)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! ofile = File(unit, filename) returns a File object associated to the
    ! file filename with the identifier unit.
    !
    ! Examples
    !-----------------------------------------------------------------------
    ! type(File) :: ofile
    !
    ! ofile = File(10, "myfile.txt")
    ! call ofile%open()
    ! ! ... some operations on this file ...
    ! call ofile%close()

    type(File) function init_File(unit, filename)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: filename

        init_File%unit = unit
        init_File%filename = trim(filename)
        return
    end function init_File
end module
