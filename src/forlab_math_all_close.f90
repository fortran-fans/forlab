
submodule (forlab_math) forlab_math_all_close

    implicit none
    character(*), parameter :: error_1 = "*<ERROR>* The ranks of `a` and `b` in `all_close` are not equal."
    character(*), parameter :: error_2 = "*<ERROR>* The rank of `a` in `all_close` is too large to be supported."
    
contains


    logical module function all_close_rsp(a, b, rel_tol, abs_tol) result(close)

        real(sp), intent(in) :: a(..), b(..)
        real(sp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_rsp
    logical module function all_close_rdp(a, b, rel_tol, abs_tol) result(close)

        real(dp), intent(in) :: a(..), b(..)
        real(dp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_rdp
    logical module function all_close_rqp(a, b, rel_tol, abs_tol) result(close)

        real(qp), intent(in) :: a(..), b(..)
        real(qp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_rqp
    logical module function all_close_csp(a, b, rel_tol, abs_tol) result(close)

        complex(sp), intent(in) :: a(..), b(..)
        real(sp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_csp
    logical module function all_close_cdp(a, b, rel_tol, abs_tol) result(close)

        complex(dp), intent(in) :: a(..), b(..)
        real(dp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_cdp
    logical module function all_close_cqp(a, b, rel_tol, abs_tol) result(close)

        complex(qp), intent(in) :: a(..), b(..)
        real(qp), intent(in), optional :: rel_tol, abs_tol

        select rank(a)

        rank(1)
            select rank(b)
            rank(1)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(2)
            select rank(b)
            rank(2)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(3)
            select rank(b)
            rank(3)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select
        rank(4)
            select rank(b)
            rank(4)
                close = all(is_close(a, b, rel_tol, abs_tol))
            rank default
                error stop error_1
            end select

        rank default
            error stop error_2
        end select

    end function all_close_cqp

end submodule forlab_math_all_close