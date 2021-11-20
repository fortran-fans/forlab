
submodule(forlab_linalg) forlab_linalg_linspace

    implicit none

contains

    module procedure linspace_sp
    integer :: i, n
    real(sp) :: by
    n = size(X)
    by = (to - from)/real(n - 1, sp)
    X = from + by*real([(i - 1, i=1, n)], sp)
    return
    end procedure linspace_sp

    module procedure logspace_sp
    call linspace(X, log10(from), log10(to))
    X = 10._sp**X
    return
    end procedure logspace_sp
    module procedure linspace_dp
    integer :: i, n
    real(dp) :: by
    n = size(X)
    by = (to - from)/real(n - 1, dp)
    X = from + by*real([(i - 1, i=1, n)], dp)
    return
    end procedure linspace_dp

    module procedure logspace_dp
    call linspace(X, log10(from), log10(to))
    X = 10._dp**X
    return
    end procedure logspace_dp

end submodule forlab_linalg_linspace
