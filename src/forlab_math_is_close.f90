
submodule(forlab_math) forlab_math_is_close

contains


    elemental module function is_close_rsp(a, b, rtol, atol) result(result)
        real(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rtol, atol
        logical :: result

        result = abs(a - b) <= abs(optval(rtol, 1.0e-5_sp)*b) + &
                               abs(optval(atol, 1.0e-8_sp))

    end function is_close_rsp
    elemental module function is_close_rdp(a, b, rtol, atol) result(result)
        real(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rtol, atol
        logical :: result

        result = abs(a - b) <= abs(optval(rtol, 1.0e-5_dp)*b) + &
                               abs(optval(atol, 1.0e-8_dp))

    end function is_close_rdp
    elemental module function is_close_rqp(a, b, rtol, atol) result(result)
        real(qp), intent(in) :: a, b
        real(qp), intent(in), optional :: rtol, atol
        logical :: result

        result = abs(a - b) <= abs(optval(rtol, 1.0e-5_qp)*b) + &
                               abs(optval(atol, 1.0e-8_qp))

    end function is_close_rqp

    elemental module function is_close_csp(a, b, rtol, atol) result(result)
        complex(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rtol, atol
        logical :: result

        result = is_close_rsp(a%re, b%re, rtol, atol) .and. &
                 is_close_rsp(a%im, b%im, rtol, atol)

    end function is_close_csp
    elemental module function is_close_cdp(a, b, rtol, atol) result(result)
        complex(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rtol, atol
        logical :: result

        result = is_close_rdp(a%re, b%re, rtol, atol) .and. &
                 is_close_rdp(a%im, b%im, rtol, atol)

    end function is_close_cdp
    elemental module function is_close_cqp(a, b, rtol, atol) result(result)
        complex(qp), intent(in) :: a, b
        real(qp), intent(in), optional :: rtol, atol
        logical :: result

        result = is_close_rqp(a%re, b%re, rtol, atol) .and. &
                 is_close_rqp(a%im, b%im, rtol, atol)

    end function is_close_cqp

end submodule forlab_math_is_close