submodule(forlab) forlab_arange
    !! Version: experimental
    !!
    !! arange
    !!
    !! arange returns evenly spaced vector.
    !!
    !!## Syntax
    !!     x = arange(first, last)
    !!
    !!## Description
    !! `x = arange(first, last)` returns an evenly spaced integer vector
    !! starting from first and ending at last.
    !!
    !!## Examples
    !!     x = arange(1, 9)
    !!          1   2   3   4   5   6   7   8   9
    use forlab_kinds
    implicit none

contains
    module procedure arange_int8
        integer(int8) :: i
        if (first <= last) then
            arange_int8 = [(i, i=first, last)]
        else
            arange_int8 = [(i, i=first, last, -1)]
        end if
        return
    end procedure
    module procedure arange_int16
        integer(int16) :: i
        if (first <= last) then
            arange_int16 = [(i, i=first, last)]
        else
            arange_int16 = [(i, i=first, last, -1)]
        end if
        return
    end procedure
    module procedure arange_int32
        integer(int32) :: i
        if (first <= last) then
            arange_int32 = [(i, i=first, last)]
        else
            arange_int32 = [(i, i=first, last, -1)]
        end if
        return
    end procedure
    module procedure arange_int64
        integer(int64) :: i
        if (first <= last) then
            arange_int64 = [(i, i=first, last)]
        else
            arange_int64 = [(i, i=first, last, -1)]
        end if
        return
    end procedure
end submodule
