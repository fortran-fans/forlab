

submodule(forlab_linalg) forlab_linalg_eig

    implicit none

contains

    module procedure eig_sp
        integer :: opt_itermax, iter, i, j, k, n
        integer, dimension(:), allocatable :: idx
        real(sp) :: threshold, gapj, termi, termj, h, term, t, &
            theta, c, s, tau, g
        real(sp), dimension(:), allocatable :: bw, zw
        real(sp), dimension(:, :), allocatable :: B
        real(sp), parameter::zero=0.0_sp,one=1.0_sp,half=0.5_sp
        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax

        if (.not. is_symmetric(A)) then
            stop "Error: in eig(A), A is not symmetric."
        else
            if (allocated(V)) deallocate (V)
            if (allocated(d)) deallocate (d)

            B = A
            n = size(B, 1)
            allocate(V(n, n))
            call eye(V)
            d = diag(B)
            bw = d
            allocate(zw(n))
            call zeros(zw)

            iter = 0
            do while (iter .lt. opt_itermax)
            iter = iter + 1

            threshold = sqrt(sum(triu(B, 1)**2))/(4*n)
            if (threshold .eq. zero) exit

            do i = 1, n
            do j = i + 1, n
            gapj = 10_sp*abs(B(i, j))
            termi = gapj + abs(d(i))
            termj = gapj + abs(d(j))

            if ((iter .gt. 4) .and. (termi .eq. abs(d(i))) &
                .and. (termj .eq. abs(d(j)))) then
                B(i, j) = zero
            elseif (threshold .le. abs(B(i, j))) then
                h = d(j) - d(i)
                term = abs(h) + gapj

                if (term .eq. abs(h)) then
                    t = B(i, j)/h
                else
                    theta = half*h/B(i, j)
                    t = one/(abs(theta) + sqrt(one + theta*theta))
                    if (theta .lt. zero) t = -t
                end if

                c = one/sqrt(one + t*t)
                s = t*c
                tau = s/(one + c)
                h = t*B(i, j)

                zw(i) = zw(i) - h
                zw(j) = zw(j) + h
                d(i) = d(i) - h
                d(j) = d(j) + h
                B(i, j) = zero

                do k = 1, i - 1
                g = B(k, i)
                h = B(k, j)
                B(k, i) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = i + 1, j - 1
                g = B(i, k)
                h = B(k, j)
                B(i, k) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = j + 1, n
                g = B(i, k)
                h = B(j, k)
                B(i, k) = g - s*(h + g*tau)
                B(j, k) = h + s*(g - h*tau)
                end do

                do k = 1, n
                g = V(k, i)
                h = V(k, j)
                v(k, i) = g - s*(h + g*tau)
                v(k, j) = h + s*(g - h*tau)
                end do

            end if
            end do
            end do

            bw = bw + zw
            d = bw
            zw = zero
            end do
            idx = argsort(d, 1)
            d = d(idx)
            V = V(:, idx)
        end if

    end procedure eig_sp
    module procedure eig_dp
        integer :: opt_itermax, iter, i, j, k, n
        integer, dimension(:), allocatable :: idx
        real(dp) :: threshold, gapj, termi, termj, h, term, t, &
            theta, c, s, tau, g
        real(dp), dimension(:), allocatable :: bw, zw
        real(dp), dimension(:, :), allocatable :: B
        real(dp), parameter::zero=0.0_dp,one=1.0_dp,half=0.5_dp
        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax

        if (.not. is_symmetric(A)) then
            stop "Error: in eig(A), A is not symmetric."
        else
            if (allocated(V)) deallocate (V)
            if (allocated(d)) deallocate (d)

            B = A
            n = size(B, 1)
            allocate(V(n, n))
            call eye(V)
            d = diag(B)
            bw = d
            allocate(zw(n))
            call zeros(zw)

            iter = 0
            do while (iter .lt. opt_itermax)
            iter = iter + 1

            threshold = sqrt(sum(triu(B, 1)**2))/(4*n)
            if (threshold .eq. zero) exit

            do i = 1, n
            do j = i + 1, n
            gapj = 10_dp*abs(B(i, j))
            termi = gapj + abs(d(i))
            termj = gapj + abs(d(j))

            if ((iter .gt. 4) .and. (termi .eq. abs(d(i))) &
                .and. (termj .eq. abs(d(j)))) then
                B(i, j) = zero
            elseif (threshold .le. abs(B(i, j))) then
                h = d(j) - d(i)
                term = abs(h) + gapj

                if (term .eq. abs(h)) then
                    t = B(i, j)/h
                else
                    theta = half*h/B(i, j)
                    t = one/(abs(theta) + sqrt(one + theta*theta))
                    if (theta .lt. zero) t = -t
                end if

                c = one/sqrt(one + t*t)
                s = t*c
                tau = s/(one + c)
                h = t*B(i, j)

                zw(i) = zw(i) - h
                zw(j) = zw(j) + h
                d(i) = d(i) - h
                d(j) = d(j) + h
                B(i, j) = zero

                do k = 1, i - 1
                g = B(k, i)
                h = B(k, j)
                B(k, i) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = i + 1, j - 1
                g = B(i, k)
                h = B(k, j)
                B(i, k) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = j + 1, n
                g = B(i, k)
                h = B(j, k)
                B(i, k) = g - s*(h + g*tau)
                B(j, k) = h + s*(g - h*tau)
                end do

                do k = 1, n
                g = V(k, i)
                h = V(k, j)
                v(k, i) = g - s*(h + g*tau)
                v(k, j) = h + s*(g - h*tau)
                end do

            end if
            end do
            end do

            bw = bw + zw
            d = bw
            zw = zero
            end do
            idx = argsort(d, 1)
            d = d(idx)
            V = V(:, idx)
        end if

    end procedure eig_dp
    module procedure eig_qp
        integer :: opt_itermax, iter, i, j, k, n
        integer, dimension(:), allocatable :: idx
        real(qp) :: threshold, gapj, termi, termj, h, term, t, &
            theta, c, s, tau, g
        real(qp), dimension(:), allocatable :: bw, zw
        real(qp), dimension(:, :), allocatable :: B
        real(qp), parameter::zero=0.0_qp,one=1.0_qp,half=0.5_qp
        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax

        if (.not. is_symmetric(A)) then
            stop "Error: in eig(A), A is not symmetric."
        else
            if (allocated(V)) deallocate (V)
            if (allocated(d)) deallocate (d)

            B = A
            n = size(B, 1)
            allocate(V(n, n))
            call eye(V)
            d = diag(B)
            bw = d
            allocate(zw(n))
            call zeros(zw)

            iter = 0
            do while (iter .lt. opt_itermax)
            iter = iter + 1

            threshold = sqrt(sum(triu(B, 1)**2))/(4*n)
            if (threshold .eq. zero) exit

            do i = 1, n
            do j = i + 1, n
            gapj = 10_qp*abs(B(i, j))
            termi = gapj + abs(d(i))
            termj = gapj + abs(d(j))

            if ((iter .gt. 4) .and. (termi .eq. abs(d(i))) &
                .and. (termj .eq. abs(d(j)))) then
                B(i, j) = zero
            elseif (threshold .le. abs(B(i, j))) then
                h = d(j) - d(i)
                term = abs(h) + gapj

                if (term .eq. abs(h)) then
                    t = B(i, j)/h
                else
                    theta = half*h/B(i, j)
                    t = one/(abs(theta) + sqrt(one + theta*theta))
                    if (theta .lt. zero) t = -t
                end if

                c = one/sqrt(one + t*t)
                s = t*c
                tau = s/(one + c)
                h = t*B(i, j)

                zw(i) = zw(i) - h
                zw(j) = zw(j) + h
                d(i) = d(i) - h
                d(j) = d(j) + h
                B(i, j) = zero

                do k = 1, i - 1
                g = B(k, i)
                h = B(k, j)
                B(k, i) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = i + 1, j - 1
                g = B(i, k)
                h = B(k, j)
                B(i, k) = g - s*(h + g*tau)
                B(k, j) = h + s*(g - h*tau)
                end do

                do k = j + 1, n
                g = B(i, k)
                h = B(j, k)
                B(i, k) = g - s*(h + g*tau)
                B(j, k) = h + s*(g - h*tau)
                end do

                do k = 1, n
                g = V(k, i)
                h = V(k, j)
                v(k, i) = g - s*(h + g*tau)
                v(k, j) = h + s*(g - h*tau)
                end do

            end if
            end do
            end do

            bw = bw + zw
            d = bw
            zw = zero
            end do
            idx = argsort(d, 1)
            d = d(idx)
            V = V(:, idx)
        end if

    end procedure eig_qp

end submodule forlab_linalg_eig
