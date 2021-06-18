submodule(forlab) forlab_mean
    ! mean
    !-----------------------------------------------------------------------
    ! mean computes the mean value of an array.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! y = mean(x)
    ! x = mean(A)
    ! x = mean(A, 1)
    ! x = mean(A, 2)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! y = mean(x) returns the mean value of the vector x.
    !
    ! x = mean(A) returns a dim2 vector with the mean values of each column
    ! of matrix A.
    !
    ! x = mean(A, 1) (see x = mean(A)).
    !
    ! x = mean(A, 2) returns a dim1 vector with the mean values of each row
    ! of matrix A.
    !
    ! Examples
    !-----------------------------------------------------------------------
    ! x = [ 1., 2., 3. ]
    ! y = mean(x)
    !     2.
    !
    ! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
    !             order = [ 2, 1 ])
    ! x = mean(A)
    !     4.  5.  6.
    ! x = mean(A, 2)
    !     2.  5.  8.
    use forlab_kinds
    implicit none

contains
    module procedure mean_1_sp
        mean_1_sp = sum(x)/size(x)
        return
    end procedure

    module procedure mean_2_sp
        integer :: i, m, n
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (mean_2_sp(n))
            do i = 1, n
                mean_2_sp(i) = mean_1_sp(A(:, i))
            end do
        elseif (dim == 2) then
            allocate (mean_2_sp(m))
            do i = 1, m
                mean_2_sp(i) = mean_1_sp(A(i, :))
            end do
        end if
        return
    end procedure
    module procedure mean_1_dp
        mean_1_dp = sum(x)/size(x)
        return
    end procedure

    module procedure mean_2_dp
        integer :: i, m, n
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (mean_2_dp(n))
            do i = 1, n
                mean_2_dp(i) = mean_1_dp(A(:, i))
            end do
        elseif (dim == 2) then
            allocate (mean_2_dp(m))
            do i = 1, m
                mean_2_dp(i) = mean_1_dp(A(i, :))
            end do
        end if
        return
    end procedure
    module procedure mean_1_qp
        mean_1_qp = sum(x)/size(x)
        return
    end procedure

    module procedure mean_2_qp
        integer :: i, m, n
        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (mean_2_qp(n))
            do i = 1, n
                mean_2_qp(i) = mean_1_qp(A(:, i))
            end do
        elseif (dim == 2) then
            allocate (mean_2_qp(m))
            do i = 1, m
                mean_2_qp(i) = mean_1_qp(A(i, :))
            end do
        end if
        return
    end procedure
end submodule
