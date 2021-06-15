submodule(forlab) forlab_randu
    !! Version: experimental
    !!
    !! randu generates uniformly distributed random numbers.
    !!
    !!## Syntax
    !!    x = randu()
    !!    x = randu(dim1)
    !!    A = randu(dim1, dim2)
    !!    X = randu(dim1, dim2, dim3)
    !!
    !!## Description
    !! `x = randu()` returns a single uniformly distributed random number in
    !! the interval [0,1].
    !!
    !! `x = randu(dim1)` returns a dim1 vector of uniformly distributed random
    !! numbers.
    !!
    !! `A = randu(dim1, dim2)` returns a dim1-by-dim2 matrix of uniformly
    !! distributed random numbers.
    !!
    !! `X = randu(dim1, dim2, dim3)` returns a dim1-by-dim2-by-dim3
    !! 3-dimensional matrix of uniformly distributed random numbers.
    !!
    !!## Examples
    !!    x = randu()
    !!        0.383413825
    !!
    !!    x = randu(5)*2 - 1
    !!        0.640258908  -0.873707294   0.787327528
    use forlab_kinds
    use forlab_optval, only: optval
    implicit none
contains
    module procedure randu_sp
        sp :: from_, to_

        from_ = optval(x=from, 0.0_sp)
        to_ = optval(x=to, 1.0_sp)
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_sp
        real(sp) :: u, v, s
        real(sp) :: mean_, std_ 

        mean_ = optval(x=mean, 0.0_sp)
        std_ = optval(x=std, 1.0_sp)
        do
            call randu(u)
            u = 2._sp*u - 1._sp
            call radnu(v)
            v = 2._sp*v - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_sp*log(s)/s)*std_
        return
    end procedure

    module procedure randu_dp
        dp :: from_, to_

        from_ = optval(x=from, 0.0_dp)
        to_ = optval(x=to, 1.0_dp)
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_dp
        real(dp) :: u, v, s
        real(dp) :: mean_, std_ 

        mean_ = optval(x=mean, 0.0_dp)
        std_ = optval(x=std, 1.0_dp)
        do
            call randu(u)
            u = 2._dp*u - 1._dp
            call radnu(v)
            v = 2._dp*v - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_dp*log(s)/s)*std_
        return
    end procedure

    module procedure randu_qp
        qp :: from_, to_

        from_ = optval(x=from, 0.0_qp)
        to_ = optval(x=to, 1.0_qp)
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_qp
        real(qp) :: u, v, s
        real(qp) :: mean_, std_ 

        mean_ = optval(x=mean, 0.0_qp)
        std_ = optval(x=std, 1.0_qp)
        do
            call randu(u)
            u = 2._qp*u - 1._qp
            call radnu(v)
            v = 2._qp*v - 1._qp
            s = u*u + v*v
            if ((s > 0._qp) .and. (s < 1._qp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_qp*log(s)/s)*std_
        return
    end procedure


end submodule
