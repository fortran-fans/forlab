submodule(forlab) forlab_arange
    use forlab_kinds
    implicit none

contains
    module function arange_int8 (first, last)
        integer(int8), dimension(:), allocatable :: arange_int8
        integer(int8), intent(in) :: first, last
        integer(int8) :: i

        if (first <= last) then
            arange_int8 = [(i, i=first, last)]
        else
            arange_int8 = [(i, i=first, last, -1)]
        end if
        return
    end function
    module function arange_int16 (first, last)
        integer(int16), dimension(:), allocatable :: arange_int16
        integer(int16), intent(in) :: first, last
        integer(int16) :: i

        if (first <= last) then
            arange_int16 = [(i, i=first, last)]
        else
            arange_int16 = [(i, i=first, last, -1)]
        end if
        return
    end function
    module function arange_int32 (first, last)
        integer(int32), dimension(:), allocatable :: arange_int32
        integer(int32), intent(in) :: first, last
        integer(int32) :: i

        if (first <= last) then
            arange_int32 = [(i, i=first, last)]
        else
            arange_int32 = [(i, i=first, last, -1)]
        end if
        return
    end function
    module function arange_int64 (first, last)
        integer(int64), dimension(:), allocatable :: arange_int64
        integer(int64), intent(in) :: first, last
        integer(int64) :: i

        if (first <= last) then
            arange_int64 = [(i, i=first, last)]
        else
            arange_int64 = [(i, i=first, last, -1)]
        end if
        return
    end function
end submodule
