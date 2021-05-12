function inv1(A)
    use forlab
    !! inv1 computes the complex matrix inverse.
    complex(kind=rpre), dimension(:, :), allocatable :: inv1
    complex(kind=RPRE), dimension(:, :), intent(in) :: A
    real(kind=RPRE), dimension(:, :), allocatable :: ar, ai
    !! AR stores the real part, AI stores the imaginary part
    integer(kind=ipre) :: flag, n
    real(kind=RPRE) :: d, p, t, q, s, b
    integer(kind=ipre), dimension(:), allocatable :: is, js

    if (issquare(A)) then
        n = size(A, 1)
        inv1 = zeros(n, n)
        ar = zeros(n, n)
        ai = zeros(n, n)
        is = zeros(n)
        js = zeros(n)
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
                stop 'error: A is not inversible (= 0)'
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
            inv1(i, j) = dcmplx(ar(i, j), ai(i, j))
        end forall
    else
        stop 'Error: in inv(A), A should be square.'
    end if
    return

end function

program main
    implicit none
    complex(8) :: c(2, 2)
    c = 0.0
    c(1, 1) = (1, 1)
    c(1, 2) = (1, 1)

    print *, inv1(c)
end program
