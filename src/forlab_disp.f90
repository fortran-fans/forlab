submodule(forlab) forlab_disp
    use forlab_kinds
        real(sp), parameter :: nearzero_sp = 1.0e-10_sp
        real(dp), parameter :: nearzero_dp = 1.0e-10_dp
        real(qp), parameter :: nearzero_qp = 1.0e-10_qp
    character(len=*), parameter :: fmt_r = "( *( g11.4, 1x ) )"
    character(len=*), parameter :: fmt_c = "( *( g11.4, a1, g11.4, a1, 1x ) )"
    interface filter
            module procedure filter_sp
            module procedure filter_dp
            module procedure filter_qp
    end interface
contains

        module subroutine disp_rsp0(x, string)
            real(sp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, filter(x)
            return
        end subroutine

        module subroutine disp_rsp1(x, string)
            real(sp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (filter(x(i)), i=1, m)

            return
        end subroutine

        module subroutine disp_rsp2(A, string)
            real(sp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print fmt_r, (filter(A(i, j)), j=1, n)
            end do
            return
        end subroutine

        module subroutine disp_rsp3(X, dim, string)
            real(sp), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_rdp0(x, string)
            real(dp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, filter(x)
            return
        end subroutine

        module subroutine disp_rdp1(x, string)
            real(dp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (filter(x(i)), i=1, m)

            return
        end subroutine

        module subroutine disp_rdp2(A, string)
            real(dp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print fmt_r, (filter(A(i, j)), j=1, n)
            end do
            return
        end subroutine

        module subroutine disp_rdp3(X, dim, string)
            real(dp), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_rqp0(x, string)
            real(qp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, filter(x)
            return
        end subroutine

        module subroutine disp_rqp1(x, string)
            real(qp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (filter(x(i)), i=1, m)

            return
        end subroutine

        module subroutine disp_rqp2(A, string)
            real(qp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print fmt_r, (filter(A(i, j)), j=1, n)
            end do
            return
        end subroutine

        module subroutine disp_rqp3(X, dim, string)
            real(qp), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_csp0(x, string)
            complex(sp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            if (imag(x) .ge. 0.0d0) then
                ! print *, num2str(real(x))//" + "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "+", filter(abs(imag(x))), "i"
            else
                ! print *, num2str(real(x))//" - "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "-", filter(abs(imag(x))), "i"
            end if
            return
        end subroutine

        module subroutine disp_csp1(x, string)
            complex(sp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            do i = 1, m
                call disp_csp0(x(i))
            end do
            return
        end subroutine
        module subroutine disp_csp2(A, string)
            complex(sp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                do j = 1, n
                    call disp_csp0(A(i, j))
                end do
                print *, ''
            end do
            return
        end subroutine
        module subroutine disp_cdp0(x, string)
            complex(dp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            if (imag(x) .ge. 0.0d0) then
                ! print *, num2str(real(x))//" + "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "+", filter(abs(imag(x))), "i"
            else
                ! print *, num2str(real(x))//" - "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "-", filter(abs(imag(x))), "i"
            end if
            return
        end subroutine

        module subroutine disp_cdp1(x, string)
            complex(dp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            do i = 1, m
                call disp_cdp0(x(i))
            end do
            return
        end subroutine
        module subroutine disp_cdp2(A, string)
            complex(dp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                do j = 1, n
                    call disp_cdp0(A(i, j))
                end do
                print *, ''
            end do
            return
        end subroutine
        module subroutine disp_cqp0(x, string)
            complex(qp), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            if (imag(x) .ge. 0.0d0) then
                ! print *, num2str(real(x))//" + "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "+", filter(abs(imag(x))), "i"
            else
                ! print *, num2str(real(x))//" - "//num2str(abs(imag(x)))//"i"
                print fmt_c, filter(real(x)), "-", filter(abs(imag(x))), "i"
            end if
            return
        end subroutine

        module subroutine disp_cqp1(x, string)
            complex(qp), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            do i = 1, m
                call disp_cqp0(x(i))
            end do
            return
        end subroutine
        module subroutine disp_cqp2(A, string)
            complex(qp), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                do j = 1, n
                    call disp_cqp0(A(i, j))
                end do
                print *, ''
            end do
            return
        end subroutine
    module subroutine disp_l0(x, string)
        logical, intent(in) :: x
        character(len=*), intent(in), optional :: string

        if (present(string)) print *, trim(string)
        print fmt_r, x
        return
    end subroutine
    module subroutine disp_l1(x, string)
        logical, dimension(:), intent(in) :: x
        character(len=*), intent(in), optional :: string
        integer :: i, m

        m = size(x)
        if (present(string)) print *, trim(string)
        print fmt_r, (x(i), i=1, m)
        return
    end subroutine disp_l1
    module subroutine disp_l2(A, string)
        logical, dimension(:, :), intent(in) :: A
        character(len=*), intent(in), optional :: string
        integer :: i, j, m, n

        m = size(A, 1)
        n = size(A, 2)
        if (present(string)) print *, trim(string)
        do i = 1, m
            print fmt_r, (A(i, j), j=1, n)
        end do
        return
    end subroutine disp_l2
        module subroutine disp_iint80(x, string)
            integer(int8), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, x
            return
        end subroutine
        module subroutine disp_iint81(x, string)
            integer(int8), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (x(i), i=1, m)

            return
        end subroutine
        module subroutine disp_iint82(A, string)
            integer(int8), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print *, (A(i, j), j=1, n)
            end do
            return
        end subroutine
        module subroutine disp_iint83(X, dim, string)
            integer(int8), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
                !! \fixme: dim precision
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_iint160(x, string)
            integer(int16), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, x
            return
        end subroutine
        module subroutine disp_iint161(x, string)
            integer(int16), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (x(i), i=1, m)

            return
        end subroutine
        module subroutine disp_iint162(A, string)
            integer(int16), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print *, (A(i, j), j=1, n)
            end do
            return
        end subroutine
        module subroutine disp_iint163(X, dim, string)
            integer(int16), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
                !! \fixme: dim precision
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_iint320(x, string)
            integer(int32), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, x
            return
        end subroutine
        module subroutine disp_iint321(x, string)
            integer(int32), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (x(i), i=1, m)

            return
        end subroutine
        module subroutine disp_iint322(A, string)
            integer(int32), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print *, (A(i, j), j=1, n)
            end do
            return
        end subroutine
        module subroutine disp_iint323(X, dim, string)
            integer(int32), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
                !! \fixme: dim precision
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine
        module subroutine disp_iint640(x, string)
            integer(int64), intent(in) :: x
            character(len=*), intent(in), optional :: string

            if (present(string)) print *, trim(string)
            print fmt_r, x
            return
        end subroutine
        module subroutine disp_iint641(x, string)
            integer(int64), dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
            integer :: i, m

            m = size(x)
            if (present(string)) print *, trim(string)
            print fmt_r, (x(i), i=1, m)

            return
        end subroutine
        module subroutine disp_iint642(A, string)
            integer(int64), dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
            integer :: i, j, m, n

            m = size(A, 1)
            n = size(A, 2)
            if (present(string)) print *, trim(string)
            do i = 1, m
                print *, (A(i, j), j=1, n)
            end do
            return
        end subroutine
        module subroutine disp_iint643(X, dim, string)
            integer(int64), dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
                !! \fixme: dim precision
            character(len=*), intent(in), optional :: string
            integer :: i, dim1, dim2, dim3

            dim1 = size(X, 1)
            dim2 = size(X, 2)
            dim3 = size(X, 3)
            if (present(string)) print *, trim(string)
            if ((.not. present(dim)) .or. (dim .eq. 1)) then
                do i = 1, dim1
                    print *, "Slice ("//num2str(i)//",:,:):"
                    call disp(X(i, :, :))
                end do
            elseif (dim .eq. 2) then
                do i = 1, dim2
                    print *, "Slice (:,"//num2str(i)//",:):"
                    call disp(X(:, i, :))
                end do
            elseif (dim .eq. 3) then
                do i = 1, dim3
                    print *, "Slice (:,:,"//num2str(i)//"):"
                    call disp(X(:, :, i))
                end do
            end if
            return
        end subroutine

        elemental module function filter_sp (x)
            !! Filter near-zero. Note: elemental
            implicit none
            real(sp), intent(in) :: x
            real(sp) :: filter_sp
            filter_sp = x
            if (abs(x) < nearzero_sp) filter_sp = 0.0
        end function
        elemental module function filter_dp (x)
            !! Filter near-zero. Note: elemental
            implicit none
            real(dp), intent(in) :: x
            real(dp) :: filter_dp
            filter_dp = x
            if (abs(x) < nearzero_dp) filter_dp = 0.0
        end function
        elemental module function filter_qp (x)
            !! Filter near-zero. Note: elemental
            implicit none
            real(qp), intent(in) :: x
            real(qp) :: filter_qp
            filter_qp = x
            if (abs(x) < nearzero_qp) filter_qp = 0.0
        end function
end submodule
