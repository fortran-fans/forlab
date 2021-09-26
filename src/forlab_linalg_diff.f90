submodule(forlab_linalg) forlab_linalg_diff

    implicit none

contains

    !> `diff` computes differences of arrays of the real(sp) type.
    pure module function diff_1_sp(x, n) result(result)
        real(sp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        real(sp), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_sp

    !> `diff` computes differences of arrays of the real(sp) type.
    pure module function diff_2_sp(A, n, dim) result(result)
        real(sp), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        real(sp), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_sp
    !> `diff` computes differences of arrays of the real(dp) type.
    pure module function diff_1_dp(x, n) result(result)
        real(dp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        real(dp), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_dp

    !> `diff` computes differences of arrays of the real(dp) type.
    pure module function diff_2_dp(A, n, dim) result(result)
        real(dp), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        real(dp), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_dp
    !> `diff` computes differences of arrays of the real(qp) type.
    pure module function diff_1_qp(x, n) result(result)
        real(qp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        real(qp), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_qp

    !> `diff` computes differences of arrays of the real(qp) type.
    pure module function diff_2_qp(A, n, dim) result(result)
        real(qp), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        real(qp), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_qp
    !> `diff` computes differences of arrays of the integer(int8) type.
    pure module function diff_1_int8(x, n) result(result)
        integer(int8), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        integer(int8), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_int8

    !> `diff` computes differences of arrays of the integer(int8) type.
    pure module function diff_2_int8(A, n, dim) result(result)
        integer(int8), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        integer(int8), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_int8
    !> `diff` computes differences of arrays of the integer(int16) type.
    pure module function diff_1_int16(x, n) result(result)
        integer(int16), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        integer(int16), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_int16

    !> `diff` computes differences of arrays of the integer(int16) type.
    pure module function diff_2_int16(A, n, dim) result(result)
        integer(int16), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        integer(int16), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_int16
    !> `diff` computes differences of arrays of the integer(int32) type.
    pure module function diff_1_int32(x, n) result(result)
        integer(int32), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        integer(int32), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_int32

    !> `diff` computes differences of arrays of the integer(int32) type.
    pure module function diff_2_int32(A, n, dim) result(result)
        integer(int32), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        integer(int32), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_int32
    !> `diff` computes differences of arrays of the integer(int64) type.
    pure module function diff_1_int64(x, n) result(result)
        integer(int64), dimension(:), intent(in) :: x
        integer, intent(in), optional :: n
        integer(int64), dimension(:), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = x
        do i = 1, n_
            result = result(2:) - result(:size(result) - 1)
        end do

    end function diff_1_int64

    !> `diff` computes differences of arrays of the integer(int64) type.
    pure module function diff_2_int64(A, n, dim) result(result)
        integer(int64), dimension(:, :), intent(in) :: A
        integer, intent(in), optional :: n, dim
        integer(int64), dimension(:, :), allocatable :: result
        integer :: n_, i

        n_ = merge(n, 1, present(n))

        result = A
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n_
                result = result(2:, :) - result(:size(result, 1) - 1, :)
            end do
        elseif (dim == 2) then
            do i = 1, n_
                result = result(:, 2:) - result(:, :size(result, 2) - 1)
            end do
        end if

    end function diff_2_int64

end submodule forlab_linalg_diff
