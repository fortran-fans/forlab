submodule(forlab_linalg) forlab_linalg_seq

    implicit none

contains

    module procedure seq_sp
    real(sp) :: by_
    integer :: i, n

    by_ = optval(by, 1.0_sp)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = int(abs(to - from)/by_) + 1
    allocate (X(n))
    if (from <= to) then
        X = from + by_*real([(i - 1, i=1, n)], sp)
    else
        X = from - by_*real([(i - 1, i=1, n)], sp)
    end if
    return
    end procedure seq_sp
    module procedure seq_dp
    real(dp) :: by_
    integer :: i, n

    by_ = optval(by, 1.0_dp)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = int(abs(to - from)/by_) + 1
    allocate (X(n))
    if (from <= to) then
        X = from + by_*real([(i - 1, i=1, n)], dp)
    else
        X = from - by_*real([(i - 1, i=1, n)], dp)
    end if
    return
    end procedure seq_dp
    module procedure seq_int8
    integer(int8) :: by_
    integer :: i, n

    by_ = optval(by, 1_int8)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = (to - from)/by_ + 1
    allocate (X(n))
    if (from <= to) then
        X = [(i, i=from, to, by_)]
    else
        X = [(i, i=from, to, -by_)]
    end if
    return
    end procedure seq_int8
    module procedure seq_int16
    integer(int16) :: by_
    integer :: i, n

    by_ = optval(by, 1_int16)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = (to - from)/by_ + 1
    allocate (X(n))
    if (from <= to) then
        X = [(i, i=from, to, by_)]
    else
        X = [(i, i=from, to, -by_)]
    end if
    return
    end procedure seq_int16
    module procedure seq_int32
    integer(int32) :: by_
    integer :: i, n

    by_ = optval(by, 1_int32)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = (to - from)/by_ + 1
    allocate (X(n))
    if (from <= to) then
        X = [(i, i=from, to, by_)]
    else
        X = [(i, i=from, to, -by_)]
    end if
    return
    end procedure seq_int32
    module procedure seq_int64
    integer(int64) :: by_
    integer :: i, n

    by_ = optval(by, 1_int64)

    if (by <= 0) then
        call error_stop('Error: In seq, `by` should be greater than 0.')
    end if

    n = (to - from)/by_ + 1
    allocate (X(n))
    if (from <= to) then
        X = [(i, i=from, to, by_)]
    else
        X = [(i, i=from, to, -by_)]
    end if
    return
    end procedure seq_int64

end submodule forlab_linalg_seq
