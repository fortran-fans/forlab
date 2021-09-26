submodule(forlab_stats) forlab_stats_randu

    implicit none

contains

    module function randu_0_rsp(start, end) result(random)
        real(sp), intent(in) :: start, end
        real(sp) :: random

        call random_number(random)
        random = start + random*(end - start)

    end function randu_0_rsp

    module function randu_1_rsp(start, end, ndim) result(random)
        real(sp), intent(in) :: start, end
        integer, intent(in) :: ndim
        real(sp) :: random(ndim)

        call random_number(random)
        random = start + random*(end - start)

    end function randu_1_rsp
    module function randu_0_rdp(start, end) result(random)
        real(dp), intent(in) :: start, end
        real(dp) :: random

        call random_number(random)
        random = start + random*(end - start)

    end function randu_0_rdp

    module function randu_1_rdp(start, end, ndim) result(random)
        real(dp), intent(in) :: start, end
        integer, intent(in) :: ndim
        real(dp) :: random(ndim)

        call random_number(random)
        random = start + random*(end - start)

    end function randu_1_rdp
    module function randu_0_rqp(start, end) result(random)
        real(qp), intent(in) :: start, end
        real(qp) :: random

        call random_number(random)
        random = start + random*(end - start)

    end function randu_0_rqp

    module function randu_1_rqp(start, end, ndim) result(random)
        real(qp), intent(in) :: start, end
        integer, intent(in) :: ndim
        real(qp) :: random(ndim)

        call random_number(random)
        random = start + random*(end - start)

    end function randu_1_rqp

    module function randu_0_iint8(start, end) result(random)
        integer(int8), intent(in) :: start, end
        integer(int8) :: random

        real :: tmp

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_0_iint8

    module function randu_1_iint8(start, end, ndim) result(random)
        integer(int8), intent(in) :: start, end
        integer, intent(in) :: ndim
        integer(int8) :: random(ndim)

        real :: tmp(ndim)

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_1_iint8
    module function randu_0_iint16(start, end) result(random)
        integer(int16), intent(in) :: start, end
        integer(int16) :: random

        real :: tmp

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_0_iint16

    module function randu_1_iint16(start, end, ndim) result(random)
        integer(int16), intent(in) :: start, end
        integer, intent(in) :: ndim
        integer(int16) :: random(ndim)

        real :: tmp(ndim)

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_1_iint16
    module function randu_0_iint32(start, end) result(random)
        integer(int32), intent(in) :: start, end
        integer(int32) :: random

        real :: tmp

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_0_iint32

    module function randu_1_iint32(start, end, ndim) result(random)
        integer(int32), intent(in) :: start, end
        integer, intent(in) :: ndim
        integer(int32) :: random(ndim)

        real :: tmp(ndim)

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_1_iint32
    module function randu_0_iint64(start, end) result(random)
        integer(int64), intent(in) :: start, end
        integer(int64) :: random

        real :: tmp

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_0_iint64

    module function randu_1_iint64(start, end, ndim) result(random)
        integer(int64), intent(in) :: start, end
        integer, intent(in) :: ndim
        integer(int64) :: random(ndim)

        real :: tmp(ndim)

        call random_number(tmp)
        random = start + nint(tmp*real(end - start))

    end function randu_1_iint64

end submodule forlab_stats_randu
