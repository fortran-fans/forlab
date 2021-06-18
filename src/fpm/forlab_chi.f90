submodule(forlab) forlab_chi
    use forlab_kinds
    implicit none

contains
    ! chi2rand
    !-----------------------------------------------------------------------
    ! chi2rand generates chi-square random numbers.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! r = chi2rand(v)
    ! r = chi2rand(v, dim1)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! r = chi2rand(v) returns a chi-square distributed random number with
    ! v degrees of freedom.
    !
    ! r = chi2rand(v, dim1) returns a dim1 vector of chi-square distributed
    ! random number with v degrees of freedom.
    module procedure chi2rand_sp
        real(sp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure
    module procedure chi2rand_dp
        real(dp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure
    module procedure chi2rand_qp
        real(qp), allocatable :: x_(:)
        allocate(x_(v))
        call randn(x_)
        X = sum(x_**2)
    end procedure
end submodule
