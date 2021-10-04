
submodule(forlab_math) forlab_math_is_close

contains


    elemental module function is_close_rsp(a, b, rel_tol, abs_tol) result(close)
        real(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = abs(a - b) <= max( abs(optval(rel_tol, 1.0e-9_sp)*max(abs(a), abs(b))), &
                                   abs(optval(abs_tol, 0.0_sp)) )

    end function is_close_rsp
    elemental module function is_close_rdp(a, b, rel_tol, abs_tol) result(close)
        real(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = abs(a - b) <= max( abs(optval(rel_tol, 1.0e-9_dp)*max(abs(a), abs(b))), &
                                   abs(optval(abs_tol, 0.0_dp)) )

    end function is_close_rdp
    elemental module function is_close_rqp(a, b, rel_tol, abs_tol) result(close)
        real(qp), intent(in) :: a, b
        real(qp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = abs(a - b) <= max( abs(optval(rel_tol, 1.0e-9_qp)*max(abs(a), abs(b))), &
                                   abs(optval(abs_tol, 0.0_qp)) )

    end function is_close_rqp

    elemental module function is_close_csp(a, b, rel_tol, abs_tol) result(close)
        complex(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = is_close_rsp(a%re, b%re, rel_tol, abs_tol) .and. &
                is_close_rsp(a%im, b%im, rel_tol, abs_tol)

    end function is_close_csp
    elemental module function is_close_cdp(a, b, rel_tol, abs_tol) result(close)
        complex(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = is_close_rdp(a%re, b%re, rel_tol, abs_tol) .and. &
                is_close_rdp(a%im, b%im, rel_tol, abs_tol)

    end function is_close_cdp
    elemental module function is_close_cqp(a, b, rel_tol, abs_tol) result(close)
        complex(qp), intent(in) :: a, b
        real(qp), intent(in), optional :: rel_tol, abs_tol
        logical :: close

        close = is_close_rqp(a%re, b%re, rel_tol, abs_tol) .and. &
                is_close_rqp(a%im, b%im, rel_tol, abs_tol)

    end function is_close_cqp

end submodule forlab_math_is_close