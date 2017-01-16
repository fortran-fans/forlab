program example_fit

  use forlab, only : IPRE, RPRE, fminbnd

  implicit none

  real(kind = RPRE) :: a, b, c, eps

  a = -5.d0
  b = 15.d0
  c = fminbnd(sphere, a, b)
  print *, c

  print *
  stop

contains

  real(kind = RPRE) function sphere(x)
    real(kind = RPRE), intent(in) :: x
    sphere = (x - 1)**2
    return
  end function sphere

end program example_fit
