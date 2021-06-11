


submodule(forlab) forlab_issymmetric
    !! Version: experimental
    !!
    !!## issymmetric
    !! issymmetric determines whether a square matrix is symmetric.
    !!
    !!### Syntax
    !!    bool = issymmetric(A)
    !!
    !!### Description
    !! `bool = issymmetric(A)` returns `.true.` if `A` is symmetric, `.false.`
    !! otherwise.
    !!
    !!### Examples
    !!    A = eye(3)
    !!    bool = issymmetric(A)
    !!        .true.
    use forlab_kinds
    implicit none
contains

    module procedure issymmetric_sp
        integer :: i, j, n
        issymmetric_sp = .true.
        if (.not. issquare(A)) then
            issymmetric_sp = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        issymmetric_sp = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end procedure issymmetric_sp
    module procedure issymmetric_dp
        integer :: i, j, n
        issymmetric_dp = .true.
        if (.not. issquare(A)) then
            issymmetric_dp = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        issymmetric_dp = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end procedure issymmetric_dp
    module procedure issymmetric_qp
        integer :: i, j, n
        issymmetric_qp = .true.
        if (.not. issquare(A)) then
            issymmetric_qp = .false.
            return
        else
            n = size(A, 1)
            do i = 1, n
                do j = 1, n
                    if (A(i, j) .ne. A(j, i)) then
                        issymmetric_qp = .false.
                        return
                    end if
                end do
            end do
        end if
        return
    end procedure issymmetric_qp

end submodule

