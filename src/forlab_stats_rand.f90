!!\TODO: Reduced speed. @2021-6-15

submodule(forlab_stats) forlab_stats_randu

    implicit none

contains

    module procedure randu_rsp
        real(sp) :: from_, to_
        from_ = merge(from, 0.0_sp, present(from))
        to_ = merge(to, 1.0_sp, present(to))
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_sp
        real(sp) :: u, v, s
        real(sp) :: mean_, std_ 

        mean_ = merge(mean, 0.0_sp, present(mean))
        std_ = merge(std, 1.0_sp, present(std))
        do
            call randu(u)
            u = 2._sp*u - 1._sp
            call randu(v)
            v = 2._sp*v - 1._sp
            s = u*u + v*v
            if ((s > 0._sp) .and. (s < 1._sp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_sp*log(s)/s)*std_
        return
    end procedure
    module procedure randu_rdp
        real(dp) :: from_, to_
        from_ = merge(from, 0.0_dp, present(from))
        to_ = merge(to, 1.0_dp, present(to))
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_dp
        real(dp) :: u, v, s
        real(dp) :: mean_, std_ 

        mean_ = merge(mean, 0.0_dp, present(mean))
        std_ = merge(std, 1.0_dp, present(std))
        do
            call randu(u)
            u = 2._dp*u - 1._dp
            call randu(v)
            v = 2._dp*v - 1._dp
            s = u*u + v*v
            if ((s > 0._dp) .and. (s < 1._dp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_dp*log(s)/s)*std_
        return
    end procedure
    module procedure randu_rqp
        real(qp) :: from_, to_
        from_ = merge(from, 0.0_qp, present(from))
        to_ = merge(to, 1.0_qp, present(to))
        call random_number(X)
        X = min(from_, to_) + X*(max(from_, to_) - min(from_, to_))
        return
    end procedure

    module procedure randn_qp
        real(qp) :: u, v, s
        real(qp) :: mean_, std_ 

        mean_ = merge(mean, 0.0_qp, present(mean))
        std_ = merge(std, 1.0_qp, present(std))
        do
            call randu(u)
            u = 2._qp*u - 1._qp
            call randu(v)
            v = 2._qp*v - 1._qp
            s = u*u + v*v
            if ((s > 0._qp) .and. (s < 1._qp)) exit
        end do
        X = mean_ + u*sqrt(-2.0_qp*log(s)/s)*std_
        return
    end procedure
    module procedure randu_iint8
        integer(int8) :: from_, to_
        real :: r
        from_ = merge(from, 0_int8, present(from))
        to_ = merge(to, 1_int8, present(to))
        call random_number(r)
        X = min(from_, to_) + nint(r*real(max(from_, to_) - min(from_, to_)))
        return
    end procedure

    module procedure randu_iint16
        integer(int16) :: from_, to_
        real :: r
        from_ = merge(from, 0_int16, present(from))
        to_ = merge(to, 1_int16, present(to))
        call random_number(r)
        X = min(from_, to_) + nint(r*real(max(from_, to_) - min(from_, to_)))
        return
    end procedure

    module procedure randu_iint32
        integer(int32) :: from_, to_
        real :: r
        from_ = merge(from, 0_int32, present(from))
        to_ = merge(to, 1_int32, present(to))
        call random_number(r)
        X = min(from_, to_) + nint(r*real(max(from_, to_) - min(from_, to_)))
        return
    end procedure

    module procedure randu_iint64
        integer(int64) :: from_, to_
        real :: r
        from_ = merge(from, 0_int64, present(from))
        to_ = merge(to, 1_int64, present(to))
        call random_number(r)
        X = min(from_, to_) + nint(r*real(max(from_, to_) - min(from_, to_)))
        return
    end procedure


end submodule forlab_stats_randu
