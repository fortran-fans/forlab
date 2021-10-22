
submodule(forlab_linalg) forlab_linalg_cat

    implicit none

contains

    module procedure horzcat_r_1_sp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_r_1_sp

    module procedure horzcat_r_2_sp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_r_2_sp

    module procedure horzcat_r_21_sp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_r_21_sp

    module procedure horzcat_r_12_sp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_r_12_sp
    module procedure horzcat_r_1_dp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_r_1_dp

    module procedure horzcat_r_2_dp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_r_2_dp

    module procedure horzcat_r_21_dp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_r_21_dp

    module procedure horzcat_r_12_dp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_r_12_dp
    module procedure horzcat_r_1_qp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_r_1_qp

    module procedure horzcat_r_2_qp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_r_2_qp

    module procedure horzcat_r_21_qp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_r_21_qp

    module procedure horzcat_r_12_qp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_r_12_qp
    module procedure horzcat_c_1_sp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_c_1_sp

    module procedure horzcat_c_2_sp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_c_2_sp

    module procedure horzcat_c_21_sp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_c_21_sp

    module procedure horzcat_c_12_sp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_c_12_sp
    module procedure horzcat_c_1_dp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_c_1_dp

    module procedure horzcat_c_2_dp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_c_2_dp

    module procedure horzcat_c_21_dp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_c_21_dp

    module procedure horzcat_c_12_dp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_c_12_dp
    module procedure horzcat_c_1_qp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_c_1_qp

    module procedure horzcat_c_2_qp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_c_2_qp

    module procedure horzcat_c_21_qp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_c_21_qp

    module procedure horzcat_c_12_qp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_c_12_qp
    module procedure horzcat_i_1_int8
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_i_1_int8

    module procedure horzcat_i_2_int8
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_i_2_int8

    module procedure horzcat_i_21_int8
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_i_21_int8

    module procedure horzcat_i_12_int8
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_i_12_int8
    module procedure horzcat_i_1_int16
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_i_1_int16

    module procedure horzcat_i_2_int16
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_i_2_int16

    module procedure horzcat_i_21_int16
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_i_21_int16

    module procedure horzcat_i_12_int16
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_i_12_int16
    module procedure horzcat_i_1_int32
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_i_1_int32

    module procedure horzcat_i_2_int32
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_i_2_int32

    module procedure horzcat_i_21_int32
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_i_21_int32

    module procedure horzcat_i_12_int32
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_i_12_int32
    module procedure horzcat_i_1_int64
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(max(m1, m2), 2)
    result(1:m1, 1) = x1
    result(1:m2, 2) = x2

    end procedure horzcat_i_1_int64

    module procedure horzcat_i_2_int64
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(max(m1, m2), n1 + n2)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1:) = A2

    end procedure horzcat_i_2_int64

    module procedure horzcat_i_21_int64
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(max(m1, m2), n1 + 1)
    result(1:m1, 1:n1) = A1
    result(1:m2, n1 + 1) = x2
    return
    end procedure horzcat_i_21_int64

    module procedure horzcat_i_12_int64
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(max(m1, m2), n2 + 1)
    result(1:m1, 1) = x1
    result(1:m2, 2:) = A2

    end procedure horzcat_i_12_int64
    module procedure vertcat_r_1_sp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_r_1_sp

    module procedure vertcat_r_2_sp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_2_sp

    module procedure vertcat_r_21_sp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_r_21_sp

    module procedure vertcat_r_12_sp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_12_sp
    module procedure vertcat_r_1_dp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_r_1_dp

    module procedure vertcat_r_2_dp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_2_dp

    module procedure vertcat_r_21_dp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_r_21_dp

    module procedure vertcat_r_12_dp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_12_dp
    module procedure vertcat_r_1_qp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_r_1_qp

    module procedure vertcat_r_2_qp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_2_qp

    module procedure vertcat_r_21_qp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_r_21_qp

    module procedure vertcat_r_12_qp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_r_12_qp
    module procedure vertcat_c_1_sp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_c_1_sp

    module procedure vertcat_c_2_sp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_2_sp

    module procedure vertcat_c_21_sp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_c_21_sp

    module procedure vertcat_c_12_sp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_12_sp
    module procedure vertcat_c_1_dp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_c_1_dp

    module procedure vertcat_c_2_dp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_2_dp

    module procedure vertcat_c_21_dp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_c_21_dp

    module procedure vertcat_c_12_dp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_12_dp
    module procedure vertcat_c_1_qp
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_c_1_qp

    module procedure vertcat_c_2_qp
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_2_qp

    module procedure vertcat_c_21_qp
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_c_21_qp

    module procedure vertcat_c_12_qp
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_c_12_qp
    module procedure vertcat_i_1_int8
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_i_1_int8

    module procedure vertcat_i_2_int8
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_2_int8

    module procedure vertcat_i_21_int8
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_i_21_int8

    module procedure vertcat_i_12_int8
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_12_int8
    module procedure vertcat_i_1_int16
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_i_1_int16

    module procedure vertcat_i_2_int16
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_2_int16

    module procedure vertcat_i_21_int16
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_i_21_int16

    module procedure vertcat_i_12_int16
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_12_int16
    module procedure vertcat_i_1_int32
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_i_1_int32

    module procedure vertcat_i_2_int32
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_2_int32

    module procedure vertcat_i_21_int32
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_i_21_int32

    module procedure vertcat_i_12_int32
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_12_int32
    module procedure vertcat_i_1_int64
    integer :: m1, m2

    m1 = size(x1)
    m2 = size(x2)
    result = zeros(m1 + m2, 1)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1) = x2

    end procedure vertcat_i_1_int64

    module procedure vertcat_i_2_int64
    integer :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    result = zeros(m1 + m2, max(n1, n2))
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_2_int64

    module procedure vertcat_i_21_int64
    integer :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)
    result = zeros(m1 + m2, n1)
    result(1:m1, 1:n1) = A1
    result(m1 + 1:m1 + m2, 1) = x2
    return
    end procedure vertcat_i_21_int64

    module procedure vertcat_i_12_int64
    integer :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)
    result = zeros(m1 + m2, n2)
    result(1:m1, 1) = x1
    result(m1 + 1:m1 + m2, 1:n2) = A2

    end procedure vertcat_i_12_int64

end submodule forlab_linalg_cat
