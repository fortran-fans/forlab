
submodule(forlab_linalg) forlab_linalg_cat

    implicit none

contains

    module procedure horzcat_r_1_sp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_r_1_sp(max(m1, m2), 2))
        call zeros(horzcat_r_1_sp)
        horzcat_r_1_sp(1:m1, 1) = x1
        horzcat_r_1_sp(1:m2, 2) = x2
        return
    end procedure horzcat_r_1_sp

    module procedure horzcat_r_2_sp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_2_sp(max(m1, m2), n1 + n2))
        call zeros(horzcat_r_2_sp)
        horzcat_r_2_sp(1:m1, 1:n1) = A1
        horzcat_r_2_sp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_r_2_sp

    module procedure horzcat_r_21_sp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_r_21_sp(max(m1, m2), n1 + 1))
        call zeros(horzcat_r_21_sp)
        horzcat_r_21_sp(1:m1, 1:n1) = A1
        horzcat_r_21_sp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_r_21_sp

    module procedure horzcat_r_12_sp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_12_sp(max(m1, m2), n2 + 1))
        call zeros(horzcat_r_12_sp)
        horzcat_r_12_sp(1:m1, 1) = x1
        horzcat_r_12_sp(1:m2, 2:) = A2
        return
    end procedure horzcat_r_12_sp
    module procedure horzcat_r_1_dp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_r_1_dp(max(m1, m2), 2))
        call zeros(horzcat_r_1_dp)
        horzcat_r_1_dp(1:m1, 1) = x1
        horzcat_r_1_dp(1:m2, 2) = x2
        return
    end procedure horzcat_r_1_dp

    module procedure horzcat_r_2_dp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_2_dp(max(m1, m2), n1 + n2))
        call zeros(horzcat_r_2_dp)
        horzcat_r_2_dp(1:m1, 1:n1) = A1
        horzcat_r_2_dp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_r_2_dp

    module procedure horzcat_r_21_dp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_r_21_dp(max(m1, m2), n1 + 1))
        call zeros(horzcat_r_21_dp)
        horzcat_r_21_dp(1:m1, 1:n1) = A1
        horzcat_r_21_dp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_r_21_dp

    module procedure horzcat_r_12_dp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_12_dp(max(m1, m2), n2 + 1))
        call zeros(horzcat_r_12_dp)
        horzcat_r_12_dp(1:m1, 1) = x1
        horzcat_r_12_dp(1:m2, 2:) = A2
        return
    end procedure horzcat_r_12_dp
    module procedure horzcat_r_1_qp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_r_1_qp(max(m1, m2), 2))
        call zeros(horzcat_r_1_qp)
        horzcat_r_1_qp(1:m1, 1) = x1
        horzcat_r_1_qp(1:m2, 2) = x2
        return
    end procedure horzcat_r_1_qp

    module procedure horzcat_r_2_qp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_2_qp(max(m1, m2), n1 + n2))
        call zeros(horzcat_r_2_qp)
        horzcat_r_2_qp(1:m1, 1:n1) = A1
        horzcat_r_2_qp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_r_2_qp

    module procedure horzcat_r_21_qp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_r_21_qp(max(m1, m2), n1 + 1))
        call zeros(horzcat_r_21_qp)
        horzcat_r_21_qp(1:m1, 1:n1) = A1
        horzcat_r_21_qp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_r_21_qp

    module procedure horzcat_r_12_qp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_r_12_qp(max(m1, m2), n2 + 1))
        call zeros(horzcat_r_12_qp)
        horzcat_r_12_qp(1:m1, 1) = x1
        horzcat_r_12_qp(1:m2, 2:) = A2
        return
    end procedure horzcat_r_12_qp
    module procedure horzcat_c_1_sp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_c_1_sp(max(m1, m2), 2))
        call zeros(horzcat_c_1_sp)
        horzcat_c_1_sp(1:m1, 1) = x1
        horzcat_c_1_sp(1:m2, 2) = x2
        return
    end procedure horzcat_c_1_sp

    module procedure horzcat_c_2_sp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_2_sp(max(m1, m2), n1 + n2))
        call zeros(horzcat_c_2_sp)
        horzcat_c_2_sp(1:m1, 1:n1) = A1
        horzcat_c_2_sp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_c_2_sp

    module procedure horzcat_c_21_sp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_c_21_sp(max(m1, m2), n1 + 1))
        call zeros(horzcat_c_21_sp)
        horzcat_c_21_sp(1:m1, 1:n1) = A1
        horzcat_c_21_sp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_c_21_sp

    module procedure horzcat_c_12_sp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_12_sp(max(m1, m2), n2 + 1))
        call zeros(horzcat_c_12_sp)
        horzcat_c_12_sp(1:m1, 1) = x1
        horzcat_c_12_sp(1:m2, 2:) = A2
        return
    end procedure horzcat_c_12_sp
    module procedure horzcat_c_1_dp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_c_1_dp(max(m1, m2), 2))
        call zeros(horzcat_c_1_dp)
        horzcat_c_1_dp(1:m1, 1) = x1
        horzcat_c_1_dp(1:m2, 2) = x2
        return
    end procedure horzcat_c_1_dp

    module procedure horzcat_c_2_dp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_2_dp(max(m1, m2), n1 + n2))
        call zeros(horzcat_c_2_dp)
        horzcat_c_2_dp(1:m1, 1:n1) = A1
        horzcat_c_2_dp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_c_2_dp

    module procedure horzcat_c_21_dp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_c_21_dp(max(m1, m2), n1 + 1))
        call zeros(horzcat_c_21_dp)
        horzcat_c_21_dp(1:m1, 1:n1) = A1
        horzcat_c_21_dp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_c_21_dp

    module procedure horzcat_c_12_dp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_12_dp(max(m1, m2), n2 + 1))
        call zeros(horzcat_c_12_dp)
        horzcat_c_12_dp(1:m1, 1) = x1
        horzcat_c_12_dp(1:m2, 2:) = A2
        return
    end procedure horzcat_c_12_dp
    module procedure horzcat_c_1_qp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_c_1_qp(max(m1, m2), 2))
        call zeros(horzcat_c_1_qp)
        horzcat_c_1_qp(1:m1, 1) = x1
        horzcat_c_1_qp(1:m2, 2) = x2
        return
    end procedure horzcat_c_1_qp

    module procedure horzcat_c_2_qp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_2_qp(max(m1, m2), n1 + n2))
        call zeros(horzcat_c_2_qp)
        horzcat_c_2_qp(1:m1, 1:n1) = A1
        horzcat_c_2_qp(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_c_2_qp

    module procedure horzcat_c_21_qp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_c_21_qp(max(m1, m2), n1 + 1))
        call zeros(horzcat_c_21_qp)
        horzcat_c_21_qp(1:m1, 1:n1) = A1
        horzcat_c_21_qp(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_c_21_qp

    module procedure horzcat_c_12_qp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_c_12_qp(max(m1, m2), n2 + 1))
        call zeros(horzcat_c_12_qp)
        horzcat_c_12_qp(1:m1, 1) = x1
        horzcat_c_12_qp(1:m2, 2:) = A2
        return
    end procedure horzcat_c_12_qp
    module procedure horzcat_i_1_int8
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_i_1_int8(max(m1, m2), 2))
        call zeros(horzcat_i_1_int8)
        horzcat_i_1_int8(1:m1, 1) = x1
        horzcat_i_1_int8(1:m2, 2) = x2
        return
    end procedure horzcat_i_1_int8

    module procedure horzcat_i_2_int8
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_2_int8(max(m1, m2), n1 + n2))
        call zeros(horzcat_i_2_int8)
        horzcat_i_2_int8(1:m1, 1:n1) = A1
        horzcat_i_2_int8(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_i_2_int8

    module procedure horzcat_i_21_int8
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_i_21_int8(max(m1, m2), n1 + 1))
        call zeros(horzcat_i_21_int8)
        horzcat_i_21_int8(1:m1, 1:n1) = A1
        horzcat_i_21_int8(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_i_21_int8

    module procedure horzcat_i_12_int8
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_12_int8(max(m1, m2), n2 + 1))
        call zeros(horzcat_i_12_int8)
        horzcat_i_12_int8(1:m1, 1) = x1
        horzcat_i_12_int8(1:m2, 2:) = A2
        return
    end procedure horzcat_i_12_int8
    module procedure horzcat_i_1_int16
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_i_1_int16(max(m1, m2), 2))
        call zeros(horzcat_i_1_int16)
        horzcat_i_1_int16(1:m1, 1) = x1
        horzcat_i_1_int16(1:m2, 2) = x2
        return
    end procedure horzcat_i_1_int16

    module procedure horzcat_i_2_int16
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_2_int16(max(m1, m2), n1 + n2))
        call zeros(horzcat_i_2_int16)
        horzcat_i_2_int16(1:m1, 1:n1) = A1
        horzcat_i_2_int16(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_i_2_int16

    module procedure horzcat_i_21_int16
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_i_21_int16(max(m1, m2), n1 + 1))
        call zeros(horzcat_i_21_int16)
        horzcat_i_21_int16(1:m1, 1:n1) = A1
        horzcat_i_21_int16(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_i_21_int16

    module procedure horzcat_i_12_int16
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_12_int16(max(m1, m2), n2 + 1))
        call zeros(horzcat_i_12_int16)
        horzcat_i_12_int16(1:m1, 1) = x1
        horzcat_i_12_int16(1:m2, 2:) = A2
        return
    end procedure horzcat_i_12_int16
    module procedure horzcat_i_1_int32
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_i_1_int32(max(m1, m2), 2))
        call zeros(horzcat_i_1_int32)
        horzcat_i_1_int32(1:m1, 1) = x1
        horzcat_i_1_int32(1:m2, 2) = x2
        return
    end procedure horzcat_i_1_int32

    module procedure horzcat_i_2_int32
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_2_int32(max(m1, m2), n1 + n2))
        call zeros(horzcat_i_2_int32)
        horzcat_i_2_int32(1:m1, 1:n1) = A1
        horzcat_i_2_int32(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_i_2_int32

    module procedure horzcat_i_21_int32
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_i_21_int32(max(m1, m2), n1 + 1))
        call zeros(horzcat_i_21_int32)
        horzcat_i_21_int32(1:m1, 1:n1) = A1
        horzcat_i_21_int32(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_i_21_int32

    module procedure horzcat_i_12_int32
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_12_int32(max(m1, m2), n2 + 1))
        call zeros(horzcat_i_12_int32)
        horzcat_i_12_int32(1:m1, 1) = x1
        horzcat_i_12_int32(1:m2, 2:) = A2
        return
    end procedure horzcat_i_12_int32
    module procedure horzcat_i_1_int64
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(horzcat_i_1_int64(max(m1, m2), 2))
        call zeros(horzcat_i_1_int64)
        horzcat_i_1_int64(1:m1, 1) = x1
        horzcat_i_1_int64(1:m2, 2) = x2
        return
    end procedure horzcat_i_1_int64

    module procedure horzcat_i_2_int64
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_2_int64(max(m1, m2), n1 + n2))
        call zeros(horzcat_i_2_int64)
        horzcat_i_2_int64(1:m1, 1:n1) = A1
        horzcat_i_2_int64(1:m2, n1 + 1:) = A2
        return
    end procedure horzcat_i_2_int64

    module procedure horzcat_i_21_int64
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(horzcat_i_21_int64(max(m1, m2), n1 + 1))
        call zeros(horzcat_i_21_int64)
        horzcat_i_21_int64(1:m1, 1:n1) = A1
        horzcat_i_21_int64(1:m2, n1 + 1) = x2
        return
    end procedure horzcat_i_21_int64

    module procedure horzcat_i_12_int64
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(horzcat_i_12_int64(max(m1, m2), n2 + 1))
        call zeros(horzcat_i_12_int64)
        horzcat_i_12_int64(1:m1, 1) = x1
        horzcat_i_12_int64(1:m2, 2:) = A2
        return
    end procedure horzcat_i_12_int64
    module procedure vertcat_r_1_sp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_r_1_sp(m1+m2, 1))
        call zeros(vertcat_r_1_sp)
        vertcat_r_1_sp(1:m1, 1) = x1
        vertcat_r_1_sp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_1_sp

    module procedure vertcat_r_2_sp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_2_sp(m1+m2, max(n1,n2)))
        call zeros(vertcat_r_2_sp)
        vertcat_r_2_sp(1:m1, 1:n1) = A1
        vertcat_r_2_sp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_2_sp

    module procedure vertcat_r_21_sp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_r_21_sp(m1+m2, n1))
        call zeros(vertcat_r_21_sp)
        vertcat_r_21_sp(1:m1, 1:n1) = A1
        vertcat_r_21_sp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_21_sp

    module procedure vertcat_r_12_sp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_12_sp(m1+m2, n2))
        call zeros(vertcat_r_12_sp)
        vertcat_r_12_sp(1:m1, 1) = x1
        vertcat_r_12_sp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_12_sp
    module procedure vertcat_r_1_dp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_r_1_dp(m1+m2, 1))
        call zeros(vertcat_r_1_dp)
        vertcat_r_1_dp(1:m1, 1) = x1
        vertcat_r_1_dp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_1_dp

    module procedure vertcat_r_2_dp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_2_dp(m1+m2, max(n1,n2)))
        call zeros(vertcat_r_2_dp)
        vertcat_r_2_dp(1:m1, 1:n1) = A1
        vertcat_r_2_dp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_2_dp

    module procedure vertcat_r_21_dp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_r_21_dp(m1+m2, n1))
        call zeros(vertcat_r_21_dp)
        vertcat_r_21_dp(1:m1, 1:n1) = A1
        vertcat_r_21_dp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_21_dp

    module procedure vertcat_r_12_dp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_12_dp(m1+m2, n2))
        call zeros(vertcat_r_12_dp)
        vertcat_r_12_dp(1:m1, 1) = x1
        vertcat_r_12_dp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_12_dp
    module procedure vertcat_r_1_qp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_r_1_qp(m1+m2, 1))
        call zeros(vertcat_r_1_qp)
        vertcat_r_1_qp(1:m1, 1) = x1
        vertcat_r_1_qp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_1_qp

    module procedure vertcat_r_2_qp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_2_qp(m1+m2, max(n1,n2)))
        call zeros(vertcat_r_2_qp)
        vertcat_r_2_qp(1:m1, 1:n1) = A1
        vertcat_r_2_qp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_2_qp

    module procedure vertcat_r_21_qp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_r_21_qp(m1+m2, n1))
        call zeros(vertcat_r_21_qp)
        vertcat_r_21_qp(1:m1, 1:n1) = A1
        vertcat_r_21_qp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_r_21_qp

    module procedure vertcat_r_12_qp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_r_12_qp(m1+m2, n2))
        call zeros(vertcat_r_12_qp)
        vertcat_r_12_qp(1:m1, 1) = x1
        vertcat_r_12_qp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_r_12_qp
    module procedure vertcat_c_1_sp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_c_1_sp(m1+m2, 1))
        call zeros(vertcat_c_1_sp)
        vertcat_c_1_sp(1:m1, 1) = x1
        vertcat_c_1_sp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_1_sp

    module procedure vertcat_c_2_sp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_2_sp(m1+m2, max(n1,n2)))
        call zeros(vertcat_c_2_sp)
        vertcat_c_2_sp(1:m1, 1:n1) = A1
        vertcat_c_2_sp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_2_sp

    module procedure vertcat_c_21_sp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_c_21_sp(m1+m2, n1))
        call zeros(vertcat_c_21_sp)
        vertcat_c_21_sp(1:m1, 1:n1) = A1
        vertcat_c_21_sp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_21_sp

    module procedure vertcat_c_12_sp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_12_sp(m1+m2, n2))
        call zeros(vertcat_c_12_sp)
        vertcat_c_12_sp(1:m1, 1) = x1
        vertcat_c_12_sp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_12_sp
    module procedure vertcat_c_1_dp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_c_1_dp(m1+m2, 1))
        call zeros(vertcat_c_1_dp)
        vertcat_c_1_dp(1:m1, 1) = x1
        vertcat_c_1_dp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_1_dp

    module procedure vertcat_c_2_dp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_2_dp(m1+m2, max(n1,n2)))
        call zeros(vertcat_c_2_dp)
        vertcat_c_2_dp(1:m1, 1:n1) = A1
        vertcat_c_2_dp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_2_dp

    module procedure vertcat_c_21_dp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_c_21_dp(m1+m2, n1))
        call zeros(vertcat_c_21_dp)
        vertcat_c_21_dp(1:m1, 1:n1) = A1
        vertcat_c_21_dp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_21_dp

    module procedure vertcat_c_12_dp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_12_dp(m1+m2, n2))
        call zeros(vertcat_c_12_dp)
        vertcat_c_12_dp(1:m1, 1) = x1
        vertcat_c_12_dp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_12_dp
    module procedure vertcat_c_1_qp
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_c_1_qp(m1+m2, 1))
        call zeros(vertcat_c_1_qp)
        vertcat_c_1_qp(1:m1, 1) = x1
        vertcat_c_1_qp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_1_qp

    module procedure vertcat_c_2_qp
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_2_qp(m1+m2, max(n1,n2)))
        call zeros(vertcat_c_2_qp)
        vertcat_c_2_qp(1:m1, 1:n1) = A1
        vertcat_c_2_qp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_2_qp

    module procedure vertcat_c_21_qp
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_c_21_qp(m1+m2, n1))
        call zeros(vertcat_c_21_qp)
        vertcat_c_21_qp(1:m1, 1:n1) = A1
        vertcat_c_21_qp(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_c_21_qp

    module procedure vertcat_c_12_qp
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_c_12_qp(m1+m2, n2))
        call zeros(vertcat_c_12_qp)
        vertcat_c_12_qp(1:m1, 1) = x1
        vertcat_c_12_qp(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_c_12_qp
    module procedure vertcat_i_1_int8
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_i_1_int8(m1+m2, 1))
        call zeros(vertcat_i_1_int8)
        vertcat_i_1_int8(1:m1, 1) = x1
        vertcat_i_1_int8(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_1_int8

    module procedure vertcat_i_2_int8
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_2_int8(m1+m2, max(n1,n2)))
        call zeros(vertcat_i_2_int8)
        vertcat_i_2_int8(1:m1, 1:n1) = A1
        vertcat_i_2_int8(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_2_int8

    module procedure vertcat_i_21_int8
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_i_21_int8(m1+m2, n1))
        call zeros(vertcat_i_21_int8)
        vertcat_i_21_int8(1:m1, 1:n1) = A1
        vertcat_i_21_int8(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_21_int8

    module procedure vertcat_i_12_int8
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_12_int8(m1+m2, n2))
        call zeros(vertcat_i_12_int8)
        vertcat_i_12_int8(1:m1, 1) = x1
        vertcat_i_12_int8(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_12_int8
    module procedure vertcat_i_1_int16
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_i_1_int16(m1+m2, 1))
        call zeros(vertcat_i_1_int16)
        vertcat_i_1_int16(1:m1, 1) = x1
        vertcat_i_1_int16(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_1_int16

    module procedure vertcat_i_2_int16
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_2_int16(m1+m2, max(n1,n2)))
        call zeros(vertcat_i_2_int16)
        vertcat_i_2_int16(1:m1, 1:n1) = A1
        vertcat_i_2_int16(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_2_int16

    module procedure vertcat_i_21_int16
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_i_21_int16(m1+m2, n1))
        call zeros(vertcat_i_21_int16)
        vertcat_i_21_int16(1:m1, 1:n1) = A1
        vertcat_i_21_int16(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_21_int16

    module procedure vertcat_i_12_int16
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_12_int16(m1+m2, n2))
        call zeros(vertcat_i_12_int16)
        vertcat_i_12_int16(1:m1, 1) = x1
        vertcat_i_12_int16(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_12_int16
    module procedure vertcat_i_1_int32
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_i_1_int32(m1+m2, 1))
        call zeros(vertcat_i_1_int32)
        vertcat_i_1_int32(1:m1, 1) = x1
        vertcat_i_1_int32(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_1_int32

    module procedure vertcat_i_2_int32
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_2_int32(m1+m2, max(n1,n2)))
        call zeros(vertcat_i_2_int32)
        vertcat_i_2_int32(1:m1, 1:n1) = A1
        vertcat_i_2_int32(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_2_int32

    module procedure vertcat_i_21_int32
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_i_21_int32(m1+m2, n1))
        call zeros(vertcat_i_21_int32)
        vertcat_i_21_int32(1:m1, 1:n1) = A1
        vertcat_i_21_int32(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_21_int32

    module procedure vertcat_i_12_int32
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_12_int32(m1+m2, n2))
        call zeros(vertcat_i_12_int32)
        vertcat_i_12_int32(1:m1, 1) = x1
        vertcat_i_12_int32(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_12_int32
    module procedure vertcat_i_1_int64
        integer :: m1, m2

        m1 = size(x1)
        m2 = size(x2)
        allocate(vertcat_i_1_int64(m1+m2, 1))
        call zeros(vertcat_i_1_int64)
        vertcat_i_1_int64(1:m1, 1) = x1
        vertcat_i_1_int64(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_1_int64

    module procedure vertcat_i_2_int64
        integer :: m1, n1, m2, n2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_2_int64(m1+m2, max(n1,n2)))
        call zeros(vertcat_i_2_int64)
        vertcat_i_2_int64(1:m1, 1:n1) = A1
        vertcat_i_2_int64(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_2_int64

    module procedure vertcat_i_21_int64
        integer :: m1, n1, m2

        m1 = size(A1, 1)
        n1 = size(A1, 2)
        m2 = size(x2)
        allocate(vertcat_i_21_int64(m1+m2, n1))
        call zeros(vertcat_i_21_int64)
        vertcat_i_21_int64(1:m1, 1:n1) = A1
        vertcat_i_21_int64(m1+1:m1+m2, 1) = x2
        return
    end procedure vertcat_i_21_int64

    module procedure vertcat_i_12_int64
        integer :: m1, m2, n2

        m1 = size(x1)
        m2 = size(A2, 1)
        n2 = size(A2, 2)
        allocate(vertcat_i_12_int64(m1+m2, n2))
        call zeros(vertcat_i_12_int64)
        vertcat_i_12_int64(1:m1, 1) = x1
        vertcat_i_12_int64(m1+1:m1+m2, 1:n2) = A2
        return
    end procedure vertcat_i_12_int64

end submodule forlab_linalg_cat
