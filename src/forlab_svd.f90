submodule(forlab) forlab_svd
    use forlab_kinds
contains
    module procedure svd_sp
        integer :: m, n, i, its, i1, j, k, kk, k1, l, ll, l1, mn
        integer, dimension(:), allocatable :: idx
        real(sp) :: c, f, g, h, s, scale, tst1, tst2, x, y, z
        real(sp), dimension(:), allocatable :: rv1
        real(sp), dimension(:, :), allocatable :: opt_u, opt_v
        logical :: outu,outv,opt_d,outierr
        real(sp),parameter::zero=0.0_sp,one=1.0_sp
        outu = .false.
        outv = .false.
        opt_d = .true.
        outierr = .false.
        m = size(a, 1)
        n = size(a, 2)
        if (.not. allocated(w)) allocate (w(n))
        allocate (rv1(n), opt_u(m, n), opt_v(n, n))
        opt_u = a
        if (present(d)) opt_d = d
        if (present(u)) outu = .true.
        if (present(v)) outv = .true.
        if (present(ierr)) outierr = .true.
        ! Householder reduction to bidiagonal form
        !==========================================
        g = zero
        scale = zero
        x = zero
        do i = 1, n
        l = i + 1
        rv1(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if (i <= m) then
            scale = sum(abs(opt_u(i:m, i)))
            if (scale /= zero) then
                opt_u(i:m, i) = opt_u(i:m, i)/scale
                s = sum(opt_u(i:m, i)**2)
                f = opt_u(i, i)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, i) = f - g
                if (i /= n) then
                    do j = l, n
                    s = dot_product(opt_u(i:m, i), opt_u(i:m, j))
                    opt_u(i:m, j) = opt_u(i:m, j) + s*opt_u(i:m, i)/h
                    end do
                end if
                opt_u(i:m, i) = scale*opt_u(i:m, i)
            end if
        end if
        w(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if ((i <= m) .and. (i /= n)) then
            scale = sum(abs(opt_u(i, l:n)))
            if (scale /= zero) then
                opt_u(i, l:n) = opt_u(i, l:n)/scale
                s = sum(opt_u(i, l:n)**2)
                f = opt_u(i, l)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, l) = f - g
                rv1(l:n) = opt_u(i, l:n)/h
                if (i /= m) then
                    do j = l, m
                    s = dot_product(opt_u(j, l:n), opt_u(i, l:n))
                    opt_u(j, l:n) = opt_u(j, l:n) + s*rv1(l:n)
                    end do
                end if
                opt_u(i, l:n) = scale*opt_u(i, l:n)
            end if
        end if
        x = max(x, abs(w(i)) + abs(rv1(i)))
        end do
        ! Accumulation of right-hand transformations
        !============================================
        if (outv) then
            do i = n, 1, -1
            if (i /= n) then
                if (g /= zero) then
                    opt_v(l:n, i) = (opt_u(i, l:n)/opt_u(i, l))/g
                    do j = l, n
                    s = dot_product(opt_u(i, l:n), opt_v(l:n, j))
                    opt_v(l:n, j) = opt_v(l:n, j) + s*opt_v(l:n, i)
                    end do
                end if
                opt_v(i, l:n) = zero
                opt_v(l:n, i) = zero
            end if
            opt_v(i, i) = one
            g = rv1(i)
            l = i
            end do
        end if
        ! Accumulation of left-hand transformations
        !===========================================
        if (outu) then
            mn = min(m, n)
            do i = min(m, n), 1, -1
            l = i + 1
            g = w(i)
            if (i /= n) opt_u(i, l:n) = zero
            if (g /= zero) then
                if (i /= mn) then
                    do j = l, n
                    s = dot_product(opt_u(l:m, i), opt_u(l:m, j))
                    f = (s/opt_u(i, i))/g
                    opt_u(i:m, j) = opt_u(i:m, j) + f*opt_u(i:m, i)
                    end do
                end if
                opt_u(i:m, i) = opt_u(i:m, i)/g
            else
                opt_u(i:m, i) = zero
            end if
            opt_u(i, i) = opt_u(i, i) + one
            end do
        end if
        ! Diagonalization of the bidiagonal form
        !========================================
        tst1 = x
        do kk = 1, n
        k1 = n - kk
        k = k1 + 1
        its = 0
        ! Test for splitting
        !====================
        520         continue
        do ll = 1, k
        l1 = k - ll
        l = l1 + 1
        tst2 = tst1 + abs(rv1(l))
        if (tst2 == tst1) goto 565
        tst2 = tst1 + abs(w(l1))
        if (tst2 == tst1) exit
        end do
        ! Cancellation of rv1(l) if L greater than 1
        !============================================
        c = zero
        s = one
        do i = l, k
        f = s*rv1(i)
        rv1(i) = c*rv1(i)
        tst2 = tst1 + abs(f)
        if (tst2 == tst1) goto 565
        g = w(i)
        h = pythag_sp(f, g)
        w(i) = h
        c = g/h
        s = -f/h
        if (outu) then
            do j = 1, m
            y = opt_u(j, l1)
            z = opt_u(j, i)
            opt_u(j, l1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        ! Test for convergence
        !======================
        565         continue
        z = w(k)
        if (l == k) goto 650
        ! Shift from bottom 2 by 2 minor
        !================================
        if (its >= 30) then
            if (outierr) ierr = k
            return
        end if
        its = its + 1
        x = w(l)
        y = w(k1)
        g = rv1(k1)
        h = rv1(k)
        f = one/2*(((g + z)/h)*((g - z)/y) + y/h - h/y)
        g = pythag_sp(f,one)
        f = x - (z/x)*z + (h/x)*(y/(f + sign(g, f)) - h)
        ! Next QR transformation
        !========================
        c = one
        s = one
        do i1 = l, k1
        i = i1 + 1
        g = rv1(i)
        y = w(i)
        h = s*g
        g = c*g
        z = pythag_sp(f, h)
        rv1(i1) = z
        c = f/z
        s = h/z
        f = x*c + g*s
        g = -x*s + g*c
        h = y*s
        y = y*c
        if (outv) then
            do j = 1, n
            x = opt_v(j, i1)
            z = opt_v(j, i)
            opt_v(j, i1) = x*c + z*s
            opt_v(j, i) = -x*s + z*c
            end do
        end if
        z = pythag_sp(f, h)
        w(i1) = z
        ! Rotation can be arbitrary if Z is zero
        !========================================
        if (z /= zero) then
            c = f/z
            s = h/z
        end if
        f = c*g + s*y
        x = -s*g + c*y
        if (outu) then
            do j = 1, m
            y = opt_u(j, i1)
            z = opt_u(j, i)
            opt_u(j, i1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        rv1(l) = zero
        rv1(k) = f
        w(k) = x
        go to 520
        ! Convergence
        !=============
        650         continue
        if (z <= zero) then
            w(k) = -z
            if (outv) then
                opt_v(1:n, k) = -opt_v(1:n, k)
            end if
        end if
        end do
        ! Sort singular values
        !======================
        if (opt_d) then
            idx = argsort(w, 2)
            w = w(idx)
            if (present(u)) u = opt_u(:, idx)
            if (present(v)) v = opt_v(:, idx)
        else
            if (present(u)) u = opt_u
            if (present(v)) v = opt_v
        end if
        return
    end procedure svd_sp

    function pythag_sp(x1, x2)result(pythag)
        real(sp) ,intent(in) :: x1, x2
        real(sp):: r, s, t, u
        real(sp)::pythag
        real(sp),parameter::zero=0.0_sp,one=1.0_sp
        pythag = max(abs(x1), abs(x2))
        if (pythag /= zero) then
            r = (min(abs(x1), abs(x2))/pythag)**2
            do
            t = 4*one + r
            if (t == 4*one) exit
            s = r/t
            u = one + 2*s
            pythag = u*pythag
            r = (s/u)**2*r
            end do
        end if
        return
    end function pythag_sp

    module procedure svd_dp
        integer :: m, n, i, its, i1, j, k, kk, k1, l, ll, l1, mn
        integer, dimension(:), allocatable :: idx
        real(dp) :: c, f, g, h, s, scale, tst1, tst2, x, y, z
        real(dp), dimension(:), allocatable :: rv1
        real(dp), dimension(:, :), allocatable :: opt_u, opt_v
        logical :: outu,outv,opt_d,outierr
        real(dp),parameter::zero=0.0_dp,one=1.0_dp
        outu = .false.
        outv = .false.
        opt_d = .true.
        outierr = .false.
        m = size(a, 1)
        n = size(a, 2)
        if (.not. allocated(w)) allocate (w(n))
        allocate (rv1(n), opt_u(m, n), opt_v(n, n))
        opt_u = a
        if (present(d)) opt_d = d
        if (present(u)) outu = .true.
        if (present(v)) outv = .true.
        if (present(ierr)) outierr = .true.
        ! Householder reduction to bidiagonal form
        !==========================================
        g = zero
        scale = zero
        x = zero
        do i = 1, n
        l = i + 1
        rv1(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if (i <= m) then
            scale = sum(abs(opt_u(i:m, i)))
            if (scale /= zero) then
                opt_u(i:m, i) = opt_u(i:m, i)/scale
                s = sum(opt_u(i:m, i)**2)
                f = opt_u(i, i)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, i) = f - g
                if (i /= n) then
                    do j = l, n
                    s = dot_product(opt_u(i:m, i), opt_u(i:m, j))
                    opt_u(i:m, j) = opt_u(i:m, j) + s*opt_u(i:m, i)/h
                    end do
                end if
                opt_u(i:m, i) = scale*opt_u(i:m, i)
            end if
        end if
        w(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if ((i <= m) .and. (i /= n)) then
            scale = sum(abs(opt_u(i, l:n)))
            if (scale /= zero) then
                opt_u(i, l:n) = opt_u(i, l:n)/scale
                s = sum(opt_u(i, l:n)**2)
                f = opt_u(i, l)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, l) = f - g
                rv1(l:n) = opt_u(i, l:n)/h
                if (i /= m) then
                    do j = l, m
                    s = dot_product(opt_u(j, l:n), opt_u(i, l:n))
                    opt_u(j, l:n) = opt_u(j, l:n) + s*rv1(l:n)
                    end do
                end if
                opt_u(i, l:n) = scale*opt_u(i, l:n)
            end if
        end if
        x = max(x, abs(w(i)) + abs(rv1(i)))
        end do
        ! Accumulation of right-hand transformations
        !============================================
        if (outv) then
            do i = n, 1, -1
            if (i /= n) then
                if (g /= zero) then
                    opt_v(l:n, i) = (opt_u(i, l:n)/opt_u(i, l))/g
                    do j = l, n
                    s = dot_product(opt_u(i, l:n), opt_v(l:n, j))
                    opt_v(l:n, j) = opt_v(l:n, j) + s*opt_v(l:n, i)
                    end do
                end if
                opt_v(i, l:n) = zero
                opt_v(l:n, i) = zero
            end if
            opt_v(i, i) = one
            g = rv1(i)
            l = i
            end do
        end if
        ! Accumulation of left-hand transformations
        !===========================================
        if (outu) then
            mn = min(m, n)
            do i = min(m, n), 1, -1
            l = i + 1
            g = w(i)
            if (i /= n) opt_u(i, l:n) = zero
            if (g /= zero) then
                if (i /= mn) then
                    do j = l, n
                    s = dot_product(opt_u(l:m, i), opt_u(l:m, j))
                    f = (s/opt_u(i, i))/g
                    opt_u(i:m, j) = opt_u(i:m, j) + f*opt_u(i:m, i)
                    end do
                end if
                opt_u(i:m, i) = opt_u(i:m, i)/g
            else
                opt_u(i:m, i) = zero
            end if
            opt_u(i, i) = opt_u(i, i) + one
            end do
        end if
        ! Diagonalization of the bidiagonal form
        !========================================
        tst1 = x
        do kk = 1, n
        k1 = n - kk
        k = k1 + 1
        its = 0
        ! Test for splitting
        !====================
        520         continue
        do ll = 1, k
        l1 = k - ll
        l = l1 + 1
        tst2 = tst1 + abs(rv1(l))
        if (tst2 == tst1) goto 565
        tst2 = tst1 + abs(w(l1))
        if (tst2 == tst1) exit
        end do
        ! Cancellation of rv1(l) if L greater than 1
        !============================================
        c = zero
        s = one
        do i = l, k
        f = s*rv1(i)
        rv1(i) = c*rv1(i)
        tst2 = tst1 + abs(f)
        if (tst2 == tst1) goto 565
        g = w(i)
        h = pythag_dp(f, g)
        w(i) = h
        c = g/h
        s = -f/h
        if (outu) then
            do j = 1, m
            y = opt_u(j, l1)
            z = opt_u(j, i)
            opt_u(j, l1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        ! Test for convergence
        !======================
        565         continue
        z = w(k)
        if (l == k) goto 650
        ! Shift from bottom 2 by 2 minor
        !================================
        if (its >= 30) then
            if (outierr) ierr = k
            return
        end if
        its = its + 1
        x = w(l)
        y = w(k1)
        g = rv1(k1)
        h = rv1(k)
        f = one/2*(((g + z)/h)*((g - z)/y) + y/h - h/y)
        g = pythag_dp(f,one)
        f = x - (z/x)*z + (h/x)*(y/(f + sign(g, f)) - h)
        ! Next QR transformation
        !========================
        c = one
        s = one
        do i1 = l, k1
        i = i1 + 1
        g = rv1(i)
        y = w(i)
        h = s*g
        g = c*g
        z = pythag_dp(f, h)
        rv1(i1) = z
        c = f/z
        s = h/z
        f = x*c + g*s
        g = -x*s + g*c
        h = y*s
        y = y*c
        if (outv) then
            do j = 1, n
            x = opt_v(j, i1)
            z = opt_v(j, i)
            opt_v(j, i1) = x*c + z*s
            opt_v(j, i) = -x*s + z*c
            end do
        end if
        z = pythag_dp(f, h)
        w(i1) = z
        ! Rotation can be arbitrary if Z is zero
        !========================================
        if (z /= zero) then
            c = f/z
            s = h/z
        end if
        f = c*g + s*y
        x = -s*g + c*y
        if (outu) then
            do j = 1, m
            y = opt_u(j, i1)
            z = opt_u(j, i)
            opt_u(j, i1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        rv1(l) = zero
        rv1(k) = f
        w(k) = x
        go to 520
        ! Convergence
        !=============
        650         continue
        if (z <= zero) then
            w(k) = -z
            if (outv) then
                opt_v(1:n, k) = -opt_v(1:n, k)
            end if
        end if
        end do
        ! Sort singular values
        !======================
        if (opt_d) then
            idx = argsort(w, 2)
            w = w(idx)
            if (present(u)) u = opt_u(:, idx)
            if (present(v)) v = opt_v(:, idx)
        else
            if (present(u)) u = opt_u
            if (present(v)) v = opt_v
        end if
        return
    end procedure svd_dp

    function pythag_dp(x1, x2)result(pythag)
        real(dp) ,intent(in) :: x1, x2
        real(dp):: r, s, t, u
        real(dp)::pythag
        real(dp),parameter::zero=0.0_dp,one=1.0_dp
        pythag = max(abs(x1), abs(x2))
        if (pythag /= zero) then
            r = (min(abs(x1), abs(x2))/pythag)**2
            do
            t = 4*one + r
            if (t == 4*one) exit
            s = r/t
            u = one + 2*s
            pythag = u*pythag
            r = (s/u)**2*r
            end do
        end if
        return
    end function pythag_dp

    module procedure svd_qp
        integer :: m, n, i, its, i1, j, k, kk, k1, l, ll, l1, mn
        integer, dimension(:), allocatable :: idx
        real(qp) :: c, f, g, h, s, scale, tst1, tst2, x, y, z
        real(qp), dimension(:), allocatable :: rv1
        real(qp), dimension(:, :), allocatable :: opt_u, opt_v
        logical :: outu,outv,opt_d,outierr
        real(qp),parameter::zero=0.0_qp,one=1.0_qp
        outu = .false.
        outv = .false.
        opt_d = .true.
        outierr = .false.
        m = size(a, 1)
        n = size(a, 2)
        if (.not. allocated(w)) allocate (w(n))
        allocate (rv1(n), opt_u(m, n), opt_v(n, n))
        opt_u = a
        if (present(d)) opt_d = d
        if (present(u)) outu = .true.
        if (present(v)) outv = .true.
        if (present(ierr)) outierr = .true.
        ! Householder reduction to bidiagonal form
        !==========================================
        g = zero
        scale = zero
        x = zero
        do i = 1, n
        l = i + 1
        rv1(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if (i <= m) then
            scale = sum(abs(opt_u(i:m, i)))
            if (scale /= zero) then
                opt_u(i:m, i) = opt_u(i:m, i)/scale
                s = sum(opt_u(i:m, i)**2)
                f = opt_u(i, i)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, i) = f - g
                if (i /= n) then
                    do j = l, n
                    s = dot_product(opt_u(i:m, i), opt_u(i:m, j))
                    opt_u(i:m, j) = opt_u(i:m, j) + s*opt_u(i:m, i)/h
                    end do
                end if
                opt_u(i:m, i) = scale*opt_u(i:m, i)
            end if
        end if
        w(i) = scale*g
        g = zero
        s = zero
        scale = zero
        if ((i <= m) .and. (i /= n)) then
            scale = sum(abs(opt_u(i, l:n)))
            if (scale /= zero) then
                opt_u(i, l:n) = opt_u(i, l:n)/scale
                s = sum(opt_u(i, l:n)**2)
                f = opt_u(i, l)
                g = -sign(sqrt(s), f)
                h = f*g - s
                opt_u(i, l) = f - g
                rv1(l:n) = opt_u(i, l:n)/h
                if (i /= m) then
                    do j = l, m
                    s = dot_product(opt_u(j, l:n), opt_u(i, l:n))
                    opt_u(j, l:n) = opt_u(j, l:n) + s*rv1(l:n)
                    end do
                end if
                opt_u(i, l:n) = scale*opt_u(i, l:n)
            end if
        end if
        x = max(x, abs(w(i)) + abs(rv1(i)))
        end do
        ! Accumulation of right-hand transformations
        !============================================
        if (outv) then
            do i = n, 1, -1
            if (i /= n) then
                if (g /= zero) then
                    opt_v(l:n, i) = (opt_u(i, l:n)/opt_u(i, l))/g
                    do j = l, n
                    s = dot_product(opt_u(i, l:n), opt_v(l:n, j))
                    opt_v(l:n, j) = opt_v(l:n, j) + s*opt_v(l:n, i)
                    end do
                end if
                opt_v(i, l:n) = zero
                opt_v(l:n, i) = zero
            end if
            opt_v(i, i) = one
            g = rv1(i)
            l = i
            end do
        end if
        ! Accumulation of left-hand transformations
        !===========================================
        if (outu) then
            mn = min(m, n)
            do i = min(m, n), 1, -1
            l = i + 1
            g = w(i)
            if (i /= n) opt_u(i, l:n) = zero
            if (g /= zero) then
                if (i /= mn) then
                    do j = l, n
                    s = dot_product(opt_u(l:m, i), opt_u(l:m, j))
                    f = (s/opt_u(i, i))/g
                    opt_u(i:m, j) = opt_u(i:m, j) + f*opt_u(i:m, i)
                    end do
                end if
                opt_u(i:m, i) = opt_u(i:m, i)/g
            else
                opt_u(i:m, i) = zero
            end if
            opt_u(i, i) = opt_u(i, i) + one
            end do
        end if
        ! Diagonalization of the bidiagonal form
        !========================================
        tst1 = x
        do kk = 1, n
        k1 = n - kk
        k = k1 + 1
        its = 0
        ! Test for splitting
        !====================
        520         continue
        do ll = 1, k
        l1 = k - ll
        l = l1 + 1
        tst2 = tst1 + abs(rv1(l))
        if (tst2 == tst1) goto 565
        tst2 = tst1 + abs(w(l1))
        if (tst2 == tst1) exit
        end do
        ! Cancellation of rv1(l) if L greater than 1
        !============================================
        c = zero
        s = one
        do i = l, k
        f = s*rv1(i)
        rv1(i) = c*rv1(i)
        tst2 = tst1 + abs(f)
        if (tst2 == tst1) goto 565
        g = w(i)
        h = pythag_qp(f, g)
        w(i) = h
        c = g/h
        s = -f/h
        if (outu) then
            do j = 1, m
            y = opt_u(j, l1)
            z = opt_u(j, i)
            opt_u(j, l1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        ! Test for convergence
        !======================
        565         continue
        z = w(k)
        if (l == k) goto 650
        ! Shift from bottom 2 by 2 minor
        !================================
        if (its >= 30) then
            if (outierr) ierr = k
            return
        end if
        its = its + 1
        x = w(l)
        y = w(k1)
        g = rv1(k1)
        h = rv1(k)
        f = one/2*(((g + z)/h)*((g - z)/y) + y/h - h/y)
        g = pythag_qp(f,one)
        f = x - (z/x)*z + (h/x)*(y/(f + sign(g, f)) - h)
        ! Next QR transformation
        !========================
        c = one
        s = one
        do i1 = l, k1
        i = i1 + 1
        g = rv1(i)
        y = w(i)
        h = s*g
        g = c*g
        z = pythag_qp(f, h)
        rv1(i1) = z
        c = f/z
        s = h/z
        f = x*c + g*s
        g = -x*s + g*c
        h = y*s
        y = y*c
        if (outv) then
            do j = 1, n
            x = opt_v(j, i1)
            z = opt_v(j, i)
            opt_v(j, i1) = x*c + z*s
            opt_v(j, i) = -x*s + z*c
            end do
        end if
        z = pythag_qp(f, h)
        w(i1) = z
        ! Rotation can be arbitrary if Z is zero
        !========================================
        if (z /= zero) then
            c = f/z
            s = h/z
        end if
        f = c*g + s*y
        x = -s*g + c*y
        if (outu) then
            do j = 1, m
            y = opt_u(j, i1)
            z = opt_u(j, i)
            opt_u(j, i1) = y*c + z*s
            opt_u(j, i) = -y*s + z*c
            end do
        end if
        end do
        rv1(l) = zero
        rv1(k) = f
        w(k) = x
        go to 520
        ! Convergence
        !=============
        650         continue
        if (z <= zero) then
            w(k) = -z
            if (outv) then
                opt_v(1:n, k) = -opt_v(1:n, k)
            end if
        end if
        end do
        ! Sort singular values
        !======================
        if (opt_d) then
            idx = argsort(w, 2)
            w = w(idx)
            if (present(u)) u = opt_u(:, idx)
            if (present(v)) v = opt_v(:, idx)
        else
            if (present(u)) u = opt_u
            if (present(v)) v = opt_v
        end if
        return
    end procedure svd_qp

    function pythag_qp(x1, x2)result(pythag)
        real(qp) ,intent(in) :: x1, x2
        real(qp):: r, s, t, u
        real(qp)::pythag
        real(qp),parameter::zero=0.0_qp,one=1.0_qp
        pythag = max(abs(x1), abs(x2))
        if (pythag /= zero) then
            r = (min(abs(x1), abs(x2))/pythag)**2
            do
            t = 4*one + r
            if (t == 4*one) exit
            s = r/t
            u = one + 2*s
            pythag = u*pythag
            r = (s/u)**2*r
            end do
        end if
        return
    end function pythag_qp

end submodule
