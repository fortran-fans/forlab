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
    use forlab_optval, only: optval
    implicit none

contains
    module procedure seq_sp
        real(sp) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_dp
        real(dp) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_qp
        real(qp) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_int8
        integer(int8) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_int16
        integer(int16) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_int32
        integer(int32) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
    module procedure seq_int64
        integer(int64) :: i, by_

        by_ = optval(x=by, 1)

        if (by <= 0) then
            call error_stop('Error: In seq, `by` should be greater than 0.')
        end if

        if (from <= to) then
            X = [(i, i=from, to, by_)]
        else
            X = [(i, i=from, to, -by_)]
        end if
        return
    end procedure
    
end submodule
