
submodule(forlab) forlab_inv
    use forlab_kinds

contains
        function inv_rsp (A)
        !! inv0 computes the real matrix inverse.
            real(sp), dimension(:, :), allocatable :: inv_rsp
            real(sp), dimension(:, :), intent(in) :: A
            integer :: i, j, k, m
            real(sp) :: D
            real(sp), dimension(:), allocatable :: x, y, e
            real(sp), dimension(:, :), allocatable :: L, U

            if (issquare(A)) then
                m = size(A, 1)
                if (m .le. 3) then
                    D = det(A)
                else
                    D = det(A, L, U)
                end if
                if (D .ne. 0.) then
                    inv_rsp = zeros(m, m)
                    if (m .eq. 2) then
                        inv_rsp (1, 1) = A(2, 2)
                        inv_rsp (1, 2) = -A(1, 2)
                        inv_rsp (2, 1) = -A(2, 1)
                        inv_rsp (2, 2) = A(1, 1)
                        inv_rsp = inv_rsp/D
                    elseif (m .eq. 3) then
                        inv_rsp (1, 1) = A(2, 2)*A(3, 3) - A(2, 3)*A(3, 2)
                        inv_rsp (1, 2) = A(1, 3)*A(3, 2) - A(1, 2)*A(3, 3)
                        inv_rsp (1, 3) = A(1, 2)*A(2, 3) - A(1, 3)*A(2, 2)
                        inv_rsp (2, 1) = A(2, 3)*A(3, 1) - A(2, 1)*A(3, 3)
                        inv_rsp (2, 2) = A(1, 1)*A(3, 3) - A(1, 3)*A(3, 1)
                        inv_rsp (2, 3) = A(1, 3)*A(2, 1) - A(1, 1)*A(2, 3)
                        inv_rsp (3, 1) = A(2, 1)*A(3, 2) - A(2, 2)*A(3, 1)
                        inv_rsp (3, 2) = A(1, 2)*A(3, 1) - A(1, 1)*A(3, 2)
                        inv_rsp (3, 3) = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
                        inv_rsp = inv_rsp/D
                    else
                        do k = 1, m
                            x = zeros(m)
                            y = zeros(m)
                            e = zeros(m)
                            e(k) = 1.
                            y(1) = e(1)

                            ! Forward substitution: Ly = e
                            !==============================
                            do i = 2, m
                                y(i) = e(i)
                                do j = 1, i - 1
                                    y(i) = y(i) - y(j)*L(i, j)
                                end do
                            end do

                            ! Back substitution: Ux = y
                            !===========================
                            x(m) = y(m)/U(m, m)
                            do i = m - 1, 1, -1
                                x(i) = y(i)
                                do j = m, i + 1, -1
                                    x(i) = x(i) - x(j)*U(i, j)
                                end do
                                x(i) = x(i)/U(i, i)
                            end do

                            ! The column k of the inverse is x
                            !==================================
                            inv_rsp (:, k) = x
                        end do
                    end if
                else
                    stop "Error: in det(A), A is not inversible (= 0)."
                end if
            else
                stop "Error: in inv(A), A should be square."
            end if
            return
        end function

        function inv_rdp (A)
        !! inv0 computes the real matrix inverse.
            real(dp), dimension(:, :), allocatable :: inv_rdp
            real(dp), dimension(:, :), intent(in) :: A
            integer :: i, j, k, m
            real(dp) :: D
            real(dp), dimension(:), allocatable :: x, y, e
            real(dp), dimension(:, :), allocatable :: L, U

            if (issquare(A)) then
                m = size(A, 1)
                if (m .le. 3) then
                    D = det(A)
                else
                    D = det(A, L, U)
                end if
                if (D .ne. 0.) then
                    inv_rdp = zeros(m, m)
                    if (m .eq. 2) then
                        inv_rdp (1, 1) = A(2, 2)
                        inv_rdp (1, 2) = -A(1, 2)
                        inv_rdp (2, 1) = -A(2, 1)
                        inv_rdp (2, 2) = A(1, 1)
                        inv_rdp = inv_rdp/D
                    elseif (m .eq. 3) then
                        inv_rdp (1, 1) = A(2, 2)*A(3, 3) - A(2, 3)*A(3, 2)
                        inv_rdp (1, 2) = A(1, 3)*A(3, 2) - A(1, 2)*A(3, 3)
                        inv_rdp (1, 3) = A(1, 2)*A(2, 3) - A(1, 3)*A(2, 2)
                        inv_rdp (2, 1) = A(2, 3)*A(3, 1) - A(2, 1)*A(3, 3)
                        inv_rdp (2, 2) = A(1, 1)*A(3, 3) - A(1, 3)*A(3, 1)
                        inv_rdp (2, 3) = A(1, 3)*A(2, 1) - A(1, 1)*A(2, 3)
                        inv_rdp (3, 1) = A(2, 1)*A(3, 2) - A(2, 2)*A(3, 1)
                        inv_rdp (3, 2) = A(1, 2)*A(3, 1) - A(1, 1)*A(3, 2)
                        inv_rdp (3, 3) = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
                        inv_rdp = inv_rdp/D
                    else
                        do k = 1, m
                            x = zeros(m)
                            y = zeros(m)
                            e = zeros(m)
                            e(k) = 1.
                            y(1) = e(1)

                            ! Forward substitution: Ly = e
                            !==============================
                            do i = 2, m
                                y(i) = e(i)
                                do j = 1, i - 1
                                    y(i) = y(i) - y(j)*L(i, j)
                                end do
                            end do

                            ! Back substitution: Ux = y
                            !===========================
                            x(m) = y(m)/U(m, m)
                            do i = m - 1, 1, -1
                                x(i) = y(i)
                                do j = m, i + 1, -1
                                    x(i) = x(i) - x(j)*U(i, j)
                                end do
                                x(i) = x(i)/U(i, i)
                            end do

                            ! The column k of the inverse is x
                            !==================================
                            inv_rdp (:, k) = x
                        end do
                    end if
                else
                    stop "Error: in det(A), A is not inversible (= 0)."
                end if
            else
                stop "Error: in inv(A), A should be square."
            end if
            return
        end function

        function inv_rqp (A)
        !! inv0 computes the real matrix inverse.
            real(qp), dimension(:, :), allocatable :: inv_rqp
            real(qp), dimension(:, :), intent(in) :: A
            integer :: i, j, k, m
            real(qp) :: D
            real(qp), dimension(:), allocatable :: x, y, e
            real(qp), dimension(:, :), allocatable :: L, U

            if (issquare(A)) then
                m = size(A, 1)
                if (m .le. 3) then
                    D = det(A)
                else
                    D = det(A, L, U)
                end if
                if (D .ne. 0.) then
                    inv_rqp = zeros(m, m)
                    if (m .eq. 2) then
                        inv_rqp (1, 1) = A(2, 2)
                        inv_rqp (1, 2) = -A(1, 2)
                        inv_rqp (2, 1) = -A(2, 1)
                        inv_rqp (2, 2) = A(1, 1)
                        inv_rqp = inv_rqp/D
                    elseif (m .eq. 3) then
                        inv_rqp (1, 1) = A(2, 2)*A(3, 3) - A(2, 3)*A(3, 2)
                        inv_rqp (1, 2) = A(1, 3)*A(3, 2) - A(1, 2)*A(3, 3)
                        inv_rqp (1, 3) = A(1, 2)*A(2, 3) - A(1, 3)*A(2, 2)
                        inv_rqp (2, 1) = A(2, 3)*A(3, 1) - A(2, 1)*A(3, 3)
                        inv_rqp (2, 2) = A(1, 1)*A(3, 3) - A(1, 3)*A(3, 1)
                        inv_rqp (2, 3) = A(1, 3)*A(2, 1) - A(1, 1)*A(2, 3)
                        inv_rqp (3, 1) = A(2, 1)*A(3, 2) - A(2, 2)*A(3, 1)
                        inv_rqp (3, 2) = A(1, 2)*A(3, 1) - A(1, 1)*A(3, 2)
                        inv_rqp (3, 3) = A(1, 1)*A(2, 2) - A(1, 2)*A(2, 1)
                        inv_rqp = inv_rqp/D
                    else
                        do k = 1, m
                            x = zeros(m)
                            y = zeros(m)
                            e = zeros(m)
                            e(k) = 1.
                            y(1) = e(1)

                            ! Forward substitution: Ly = e
                            !==============================
                            do i = 2, m
                                y(i) = e(i)
                                do j = 1, i - 1
                                    y(i) = y(i) - y(j)*L(i, j)
                                end do
                            end do

                            ! Back substitution: Ux = y
                            !===========================
                            x(m) = y(m)/U(m, m)
                            do i = m - 1, 1, -1
                                x(i) = y(i)
                                do j = m, i + 1, -1
                                    x(i) = x(i) - x(j)*U(i, j)
                                end do
                                x(i) = x(i)/U(i, i)
                            end do

                            ! The column k of the inverse is x
                            !==================================
                            inv_rqp (:, k) = x
                        end do
                    end if
                else
                    stop "Error: in det(A), A is not inversible (= 0)."
                end if
            else
                stop "Error: in inv(A), A should be square."
            end if
            return
        end function



        function inv_csp (A)
        !! inv computes the complex matrix inverse.
            complex(sp), dimension(:, :), allocatable :: inv_csp
            complex(sp), dimension(:, :), intent(in) :: A
            real(sp), dimension(:, :), allocatable :: ar, ai
        !! AR stores the real part, AI stores the imaginary part
            integer :: flag, n
            real(sp) :: d, p, t, q, s, b
            integer, dimension(:), allocatable :: is, js
            integer :: i, j, k

            if (issquare(A)) then
                n = size(A, 1)
                inv_csp = zeros(n, n)
                ar = zeros(n, n)
                ai = zeros(n, n)
                allocate(is(n),js(n))
                is = 0
                js = 0
                forall (i=1:n, j=1:n)
                    ar(i, j) = real(A(i, j)); ai(i, j) = imag(A(i, j))
                end forall
                flag = 1
                do k = 1, n
                    d = 0.0
                    do i = k, n
                        do j = k, n
                            p = ar(i, j)*ar(i, j) + ai(i, j)*ai(i, j)
                            if (p .gt. d) then
                                d = p
                                is(k) = i
                                js(k) = j
                            end if
                        end do
                    end do
                    if (d + 1.0 .eq. 1.0) then
                        flag = 0
                        stop 'ERROR: A is not inversible (= 0)'
                    end if
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(is(k), j)
                        ar(is(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(is(k), j)
                        ai(is(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, js(k))
                        ar(i, js(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, js(k))
                        ai(i, js(k)) = t
                    end do
                    ar(k, k) = ar(k, k)/d
                    ai(k, k) = -ai(k, k)/d
                    do j = 1, n
                        if (j .ne. k) then
                            p = ar(k, j)*ar(k, k)
                            q = ai(k, j)*ai(k, k)
                            s = (ar(k, j) + ai(k, j))*(ar(k, k) + ai(k, k))
                            ar(k, j) = p - q
                            ai(k, j) = s - p - q
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            do j = 1, n
                                if (j .ne. k) then
                                    p = ar(k, j)*ar(i, k)
                                    q = ai(k, j)*ai(i, k)
                                    s = (ar(k, j) + ai(k, j))*(ar(i, k) + ai(i, k))
                                    t = p - q
                                    b = s - p - q
                                    ar(i, j) = ar(i, j) - t
                                    ai(i, j) = ai(i, j) - b
                                end if
                            end do
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            p = ar(i, k)*ar(k, k)
                            q = ai(i, k)*ai(k, k)
                            s = (ar(i, k) + ai(i, k))*(ar(k, k) + ai(k, k))
                            ar(i, k) = q - p
                            ai(i, k) = p + q - s
                        end if
                    end do
                end do
                do k = n, 1, -1
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(js(k), j)
                        ar(js(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(js(k), j)
                        ai(js(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, is(k))
                        ar(i, is(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, is(k))
                        ai(i, is(k)) = t
                    end do
                end do
                forall (i=1:n, j=1:n)
                    inv_csp (i, j) = dcmplx(ar(i, j), ai(i, j))
                end forall
            else
                stop 'Error: in inv(A), A should be square.'
            end if
            return
        end function

        function inv_cdp (A)
        !! inv computes the complex matrix inverse.
            complex(dp), dimension(:, :), allocatable :: inv_cdp
            complex(dp), dimension(:, :), intent(in) :: A
            real(dp), dimension(:, :), allocatable :: ar, ai
        !! AR stores the real part, AI stores the imaginary part
            integer :: flag, n
            real(dp) :: d, p, t, q, s, b
            integer, dimension(:), allocatable :: is, js
            integer :: i, j, k

            if (issquare(A)) then
                n = size(A, 1)
                inv_cdp = zeros(n, n)
                ar = zeros(n, n)
                ai = zeros(n, n)
                allocate(is(n),js(n))
                is = 0
                js = 0
                forall (i=1:n, j=1:n)
                    ar(i, j) = real(A(i, j)); ai(i, j) = imag(A(i, j))
                end forall
                flag = 1
                do k = 1, n
                    d = 0.0
                    do i = k, n
                        do j = k, n
                            p = ar(i, j)*ar(i, j) + ai(i, j)*ai(i, j)
                            if (p .gt. d) then
                                d = p
                                is(k) = i
                                js(k) = j
                            end if
                        end do
                    end do
                    if (d + 1.0 .eq. 1.0) then
                        flag = 0
                        stop 'ERROR: A is not inversible (= 0)'
                    end if
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(is(k), j)
                        ar(is(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(is(k), j)
                        ai(is(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, js(k))
                        ar(i, js(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, js(k))
                        ai(i, js(k)) = t
                    end do
                    ar(k, k) = ar(k, k)/d
                    ai(k, k) = -ai(k, k)/d
                    do j = 1, n
                        if (j .ne. k) then
                            p = ar(k, j)*ar(k, k)
                            q = ai(k, j)*ai(k, k)
                            s = (ar(k, j) + ai(k, j))*(ar(k, k) + ai(k, k))
                            ar(k, j) = p - q
                            ai(k, j) = s - p - q
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            do j = 1, n
                                if (j .ne. k) then
                                    p = ar(k, j)*ar(i, k)
                                    q = ai(k, j)*ai(i, k)
                                    s = (ar(k, j) + ai(k, j))*(ar(i, k) + ai(i, k))
                                    t = p - q
                                    b = s - p - q
                                    ar(i, j) = ar(i, j) - t
                                    ai(i, j) = ai(i, j) - b
                                end if
                            end do
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            p = ar(i, k)*ar(k, k)
                            q = ai(i, k)*ai(k, k)
                            s = (ar(i, k) + ai(i, k))*(ar(k, k) + ai(k, k))
                            ar(i, k) = q - p
                            ai(i, k) = p + q - s
                        end if
                    end do
                end do
                do k = n, 1, -1
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(js(k), j)
                        ar(js(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(js(k), j)
                        ai(js(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, is(k))
                        ar(i, is(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, is(k))
                        ai(i, is(k)) = t
                    end do
                end do
                forall (i=1:n, j=1:n)
                    inv_cdp (i, j) = dcmplx(ar(i, j), ai(i, j))
                end forall
            else
                stop 'Error: in inv(A), A should be square.'
            end if
            return
        end function

        function inv_cqp (A)
        !! inv computes the complex matrix inverse.
            complex(qp), dimension(:, :), allocatable :: inv_cqp
            complex(qp), dimension(:, :), intent(in) :: A
            real(qp), dimension(:, :), allocatable :: ar, ai
        !! AR stores the real part, AI stores the imaginary part
            integer :: flag, n
            real(qp) :: d, p, t, q, s, b
            integer, dimension(:), allocatable :: is, js
            integer :: i, j, k

            if (issquare(A)) then
                n = size(A, 1)
                inv_cqp = zeros(n, n)
                ar = zeros(n, n)
                ai = zeros(n, n)
                allocate(is(n),js(n))
                is = 0
                js = 0
                forall (i=1:n, j=1:n)
                    ar(i, j) = real(A(i, j)); ai(i, j) = imag(A(i, j))
                end forall
                flag = 1
                do k = 1, n
                    d = 0.0
                    do i = k, n
                        do j = k, n
                            p = ar(i, j)*ar(i, j) + ai(i, j)*ai(i, j)
                            if (p .gt. d) then
                                d = p
                                is(k) = i
                                js(k) = j
                            end if
                        end do
                    end do
                    if (d + 1.0 .eq. 1.0) then
                        flag = 0
                        stop 'ERROR: A is not inversible (= 0)'
                    end if
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(is(k), j)
                        ar(is(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(is(k), j)
                        ai(is(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, js(k))
                        ar(i, js(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, js(k))
                        ai(i, js(k)) = t
                    end do
                    ar(k, k) = ar(k, k)/d
                    ai(k, k) = -ai(k, k)/d
                    do j = 1, n
                        if (j .ne. k) then
                            p = ar(k, j)*ar(k, k)
                            q = ai(k, j)*ai(k, k)
                            s = (ar(k, j) + ai(k, j))*(ar(k, k) + ai(k, k))
                            ar(k, j) = p - q
                            ai(k, j) = s - p - q
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            do j = 1, n
                                if (j .ne. k) then
                                    p = ar(k, j)*ar(i, k)
                                    q = ai(k, j)*ai(i, k)
                                    s = (ar(k, j) + ai(k, j))*(ar(i, k) + ai(i, k))
                                    t = p - q
                                    b = s - p - q
                                    ar(i, j) = ar(i, j) - t
                                    ai(i, j) = ai(i, j) - b
                                end if
                            end do
                        end if
                    end do
                    do i = 1, n
                        if (i .ne. k) then
                            p = ar(i, k)*ar(k, k)
                            q = ai(i, k)*ai(k, k)
                            s = (ar(i, k) + ai(i, k))*(ar(k, k) + ai(k, k))
                            ar(i, k) = q - p
                            ai(i, k) = p + q - s
                        end if
                    end do
                end do
                do k = n, 1, -1
                    do j = 1, n
                        t = ar(k, j)
                        ar(k, j) = ar(js(k), j)
                        ar(js(k), j) = t
                        t = ai(k, j)
                        ai(k, j) = ai(js(k), j)
                        ai(js(k), j) = t
                    end do
                    do i = 1, n
                        t = ar(i, k)
                        ar(i, k) = ar(i, is(k))
                        ar(i, is(k)) = t
                        t = ai(i, k)
                        ai(i, k) = ai(i, is(k))
                        ai(i, is(k)) = t
                    end do
                end do
                forall (i=1:n, j=1:n)
                    inv_cqp (i, j) = dcmplx(ar(i, j), ai(i, j))
                end forall
            else
                stop 'Error: in inv(A), A should be square.'
            end if
            return
        end function
end submodule
