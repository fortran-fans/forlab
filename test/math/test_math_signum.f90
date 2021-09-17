module test_math_signum

    use forlab_math, only: signum, is_close
    use stdlib_error, only: check

contains

    subroutine test_math_signum_real

        real :: x(3) = [-2.0, 0.0, 3.0]
        call check(all(is_close(signum(x), [-1.0, 0.0, 1.0])), "signum for `real` failed.")

    end subroutine test_math_signum_real

    subroutine test_math_signum_integer

        integer :: x(3) = [-2, 0, 3]
        call check(all(signum(x) == [-1, 0, 1]), "signum for `integer` failed.")

    end subroutine test_math_signum_integer

    subroutine test_math_signum_complex

        complex :: x(3) = [(-2.0, 1.0), (0.0, 0.0), (3.0, 4.0)]
        call check(all(is_close(signum(x), [(-0.894427180, 0.447213590), (0.0, 0.0), (0.600000024, 0.800000012)])), &
                   "signum for `complex` failed.")

    end subroutine test_math_signum_complex

end module test_math_signum

program tester

    use test_math_signum
    call test_math_signum_real
    call test_math_signum_integer
    call test_math_signum_complex
    print *, "All tests in `test_math_signum` passed."

end program tester
