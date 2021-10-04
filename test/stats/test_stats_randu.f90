module test_stats_randu
    
    use forlab_stats, only: randu
    use stdlib_error, only: check
    implicit none
    private

    public :: test_stats_randu_integer, test_stats_randu_real
    
contains

    subroutine test_stats_randu_real

        print *, "*<DEBUG>* checking `test_stats_randu_real`.."
        
        call check(randu(start=1.0, end=2.0) <= 2.0, msg="randu(start=1.0, end=2.0) <= 2.0 failed.")
        call check(randu(start=1.0, end=2.0) >= 1.0, msg="randu(start=1.0, end=2.0) >= 1.0 failed.")
        
        call check(all(randu(start=1.0, end=2.0, ndim=3) <= 2.0), msg="randu(start=1.0, end=2.0, ndim=3) <= 2.0 failed.")
        call check(all(randu(start=1.0, end=2.0, ndim=3) >= 1.0), msg="randu(start=1.0, end=2.0, ndim=3) >= 1.0 failed.")

    end subroutine test_stats_randu_real

    subroutine test_stats_randu_integer

        print *, "*<DEBUG>* checking `test_stats_randu_integer`.."

        call check(randu(start=1, end=2) <= 2, msg="randu(start=1, end=2) <= 2 failed.")
        call check(randu(start=1, end=2) >= 1, msg="randu(start=1, end=2) >= 1 failed.")
        
        call check(all(randu(start=1, end=2, ndim=3) <= 2), msg="randu(start=1, end=2, ndim=3) <= 2 failed.")
        call check(all(randu(start=1, end=2, ndim=3) >= 1), msg="randu(start=1, end=2, ndim=3) >= 1 failed.")

    end subroutine test_stats_randu_integer

end module test_stats_randu