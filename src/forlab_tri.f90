

submodule(forlab) forlab_tri
    !! Version: expermental
    !!
    !! tril extracts the lower triangular part of a matrix.
    !!
    !!## Syntax
    !!
    !!      B = tril(A)
    !!      B = tril(A, k)
    !!
    !!## Description
    !!
    !!  `B = tril(A)` returns the lower triangular part of matrix A.
    !!
    !!  `B = tril(A, k)` returns the elements on and below the kth diagonal of
    !!      matrix X:
    !!
    !!      -   k = 0 is the main diagonal,
    !!      -   k > 0 is above the main diagonal,
    !!      -   k < 0 is below the main diagonal.
    !!
    !!## Examples
    !!
    !!      A = ones(4, 4)
    !!      B = tril(A, -1)
    !!          0.  0.  0.  0.
    !!          1.  0.  0.  0.
    !!          1.  1.  0.  0.
    !!          1.  1.  1.  0.
    !! Version: expermental
    !!
    !! triu extracts the upper triangular part of a matrix.
    !!
    !!## Syntax
    !!
    !!      B = triu(A)
    !!      B = triu(A, k)
    !!
    !!## Description
    !!
    !!  `B = triu(A)` returns the upper triangular part of matrix A.
    !!
    !!  `B = triu(A, k)` returns the elements on and above the kth diagonal of
    !!
    !!      matrix X:
    !!      -   k = 0 is the main diagonal,
    !!      -   k > 0 is above the main diagonal,
    !!      -   k < 0 is below the main diagonal.
    !!
    !!## Examples
    !!
    !!      A = ones(4, 4)
    !!      B = triu(A, -1)
    !!          1.  1.  1.  1.
    !!          1.  1.  1.  1.
    !!          0.  1.  1.  1.
    !!          0.  0.  1.  1.
    use forlab_kinds
    implicit none
contains
    module procedure tril_int8
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_int8 = A
        do i = 1, min(m, n)
            tril_int8(:i - opt_k - 1, i) = 0_int8
        end do
    end procedure tril_int8

    module procedure tril_int16
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_int16 = A
        do i = 1, min(m, n)
            tril_int16(:i - opt_k - 1, i) = 0_int16
        end do
    end procedure tril_int16

    module procedure tril_int32
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_int32 = A
        do i = 1, min(m, n)
            tril_int32(:i - opt_k - 1, i) = 0_int32
        end do
    end procedure tril_int32

    module procedure tril_int64
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_int64 = A
        do i = 1, min(m, n)
            tril_int64(:i - opt_k - 1, i) = 0_int64
        end do
    end procedure tril_int64


    module procedure tril_sp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_sp = A
        do i = 1, min(m, n)
            tril_sp(:i - opt_k - 1, i) = 0.0_sp
        end do
    end procedure tril_sp

    module procedure tril_dp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_dp = A
        do i = 1, min(m, n)
            tril_dp(:i - opt_k - 1, i) = 0.0_dp
        end do
    end procedure tril_dp

    module procedure tril_qp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_qp = A
        do i = 1, min(m, n)
            tril_qp(:i - opt_k - 1, i) = 0.0_qp
        end do
    end procedure tril_qp

    module procedure tril_csp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_csp = A
        do i = 1, min(m, n)
            tril_csp(:i - opt_k - 1, i) =cmplx(0.0_sp,0.0_sp,kind=sp)
        end do
    end procedure tril_csp


    module procedure tril_cdp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_cdp = A
        do i = 1, min(m, n)
            tril_cdp(:i - opt_k - 1, i) =cmplx(0.0_dp,0.0_dp,kind=dp)
        end do
    end procedure tril_cdp


    module procedure tril_cqp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        tril_cqp = A
        do i = 1, min(m, n)
            tril_cqp(:i - opt_k - 1, i) =cmplx(0.0_qp,0.0_qp,kind=qp)
        end do
    end procedure tril_cqp


    module procedure triu_int8
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_int8 = A
        do i = 1, min(m, n)
            triu_int8(i - opt_k + 1:, i) = 0_int8
        end do
    end procedure triu_int8

    module procedure triu_int16
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_int16 = A
        do i = 1, min(m, n)
            triu_int16(i - opt_k + 1:, i) = 0_int16
        end do
    end procedure triu_int16

    module procedure triu_int32
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_int32 = A
        do i = 1, min(m, n)
            triu_int32(i - opt_k + 1:, i) = 0_int32
        end do
    end procedure triu_int32

    module procedure triu_int64
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_int64 = A
        do i = 1, min(m, n)
            triu_int64(i - opt_k + 1:, i) = 0_int64
        end do
    end procedure triu_int64


    module procedure triu_sp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_sp = A
        do i = 1, min(m, n)
            triu_sp(i - opt_k + 1:, i) = 0.0_sp
        end do
    end procedure triu_sp

    module procedure triu_dp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_dp = A
        do i = 1, min(m, n)
            triu_dp(i - opt_k + 1:, i) = 0.0_dp
        end do
    end procedure triu_dp

    module procedure triu_qp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_qp = A
        do i = 1, min(m, n)
            triu_qp(i - opt_k + 1:, i) = 0.0_qp
        end do
    end procedure triu_qp

    module procedure triu_csp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_csp = A
        do i = 1, min(m, n)
            triu_csp(i - opt_k + 1:, i) =cmplx(0.0_sp,0.0_sp,kind=sp)
        end do
    end procedure triu_csp

    module procedure triu_cdp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_cdp = A
        do i = 1, min(m, n)
            triu_cdp(i - opt_k + 1:, i) =cmplx(0.0_dp,0.0_dp,kind=dp)
        end do
    end procedure triu_cdp

    module procedure triu_cqp
        integer::opt_k,i,m,n
        opt_k = 0
        if (present(k)) opt_k = k
        m = size(A, 1)
        n = size(A, 2)
        triu_cqp = A
        do i = 1, min(m, n)
            triu_cqp(i - opt_k + 1:, i) =cmplx(0.0_qp,0.0_qp,kind=qp)
        end do
    end procedure triu_cqp


end submodule
