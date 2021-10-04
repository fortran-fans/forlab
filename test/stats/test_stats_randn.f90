module test_stats_randn

    use forlab_stats, only: randn, mean
    use stdlib_error, only: check
    implicit none
    private

    public :: test_stats_randn_real

contains

    subroutine test_stats_randn_real

        print *, "*<DEBUG>* checking `test_stats_randn_real`.."

        call check(mean(randn(mean=0.0, std=2.0, ndim=100)) <=  2.0, &
                   msg="mean(randn(mean=0.0, std=2.0, ndim=10) <=  2.0) failed.")
        call check(mean(randn(mean=0.0, std=2.0, ndim=100)) >= -2.0, &
                   msg="mean(randn(mean=0.0, std=2.0, ndim=10) >= -2.0) failed.")

    end subroutine test_stats_randn_real

end module test_stats_randn
