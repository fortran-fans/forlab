module test_math

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use forlab_math, only: operator(.c.), all_close, angle, is_close
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    private
    
    public :: collect_math
    
contains

    subroutine collect_math(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [&
        new_unittest("angle_real vaild", test_math_angle_real), &
        new_unittest("cross_int vaild", test_math_cross_int), &
        new_unittest("cross_real vaild", test_math_cross_real) &
        ]
    end subroutine collect_math
    
    subroutine test_math_cross_int(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int8), dimension(3) :: x, y
        x = 1_int8;  y = 2_int8
        
        call check(error, all((x.c.y) == [integer(int8) :: 0, 0, 0]))
        if (allocated(error)) return
        
    end subroutine test_math_cross_int
    
    subroutine test_math_cross_real(error)
        type(error_type), allocatable, intent(out) :: error
        real, dimension(3) :: x, y
        x = 1_int8;  y = 2_int8
        
        call check(error, all_close((x.c.y), [real :: 0, 0, 0]))
        if (allocated(error)) return
        
    end subroutine test_math_cross_real
    
    module subroutine test_math_angle_real(error)
        type(error_type), allocatable, intent(out) :: error
        real, dimension(3) :: x, y
        x = 1_int8;  y = 2_int8
        
        call check(error, is_close(angle(x, y), 0.0))
        if (allocated(error)) return
        
    end subroutine test_math_angle_real
    
end module test_math