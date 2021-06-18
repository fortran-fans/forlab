!! Modify from [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)

module forlab_error
    !! Provides support for catching and handling errors  
    !! ([Specification](../page/specs/stdlib_error.html))
    use, intrinsic :: iso_fortran_env, only: stderr => error_unit
    use forlab_optval, only: optval
    implicit none
    private
    public :: error_stop

contains

    subroutine error_stop(msg, code)
        ! Aborts the program with nonzero exit code
        !
        ! The "stop <character>" statement generally has return code 0.
        ! To allow non-zero return code termination with character message,
        ! error_stop() uses the statement "error stop", which by default
        ! has exit code 1 and prints the message to stderr.
        ! An optional integer return code "code" may be specified.
        !
        ! Example
        ! -------
        !
        ! call error_stop("Invalid argument")
        character(*), intent(in) :: msg
        integer, intent(in), optional :: code
        if (present(code)) then
            write (stderr, *) msg
            error stop code
        else
            error stop msg
        end if
    end subroutine

end module forlab_error
