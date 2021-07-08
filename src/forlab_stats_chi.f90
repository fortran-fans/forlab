submodule(forlab_stats) forlab_stats_chi

    implicit none

contains

    module procedure chi2rand_sp
        !! chi2rand generates chi-square random numbers.
        real(sp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure chi2rand_sp
    module procedure chi2rand_dp
        !! chi2rand generates chi-square random numbers.
        real(dp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure chi2rand_dp
    module procedure chi2rand_qp
        !! chi2rand generates chi-square random numbers.
        real(qp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure chi2rand_qp

end submodule forlab_stats_chi
