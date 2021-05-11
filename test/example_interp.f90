!=======================================================================
! Created by
!     Keurfon Luu <keurfon.luu@mines-paristech.fr>
!     MINES ParisTech - Centre de Géosciences
!     PSL - Research University
!=======================================================================

program example_interp

  use forlab, only: IPRE, RPRE, zeros, linspace, meshgrid, interp2, spline2, &
                    bspline2, savetxt

  implicit none

  integer(kind = IPRE) :: i, j, nx, ny
  real(kind = RPRE), dimension(:), allocatable :: ax, ay, axq, ayq
  real(kind = RPRE), dimension(:,:), allocatable :: A, XX, YY, ZZ, xq, yq
  character(len = :), allocatable :: outdir

  ! Output directory
  outdir = "examples/interp/"
  call system("mkdir -p " // outdir)

  ! Create an undersampled grid to interpolate (Rosenbrock)
  nx = 10
  ny = 20
  A = zeros(ny, nx)
  ax = linspace(-5.12, 5.12, nx)
  ay = linspace(-5.12, 5.12, ny)
  do i = 1, ny
    do j = 1, nx
      A(i,j) = rosenbrock( [ ay(i), ax(j) ] )
    end do
  end do

  ! Compute the true grid
  print *, "Computing true grid:"

  nx = 100
  ny = 200
  ZZ = zeros(ny, nx)
  axq = linspace(-5.12, 5.12, nx)
  ayq = linspace(-5.12, 5.12, ny)
  do i = 1, ny
    do j = 1, nx
      ZZ(i,j) = rosenbrock( [ ayq(i), axq(j) ] )
    end do
  end do

  call savetxt(outdir // "true.txt", ZZ)
  print *, "True grid saved in " // outdir

  ! Bilinear interpolation
  print *; print *, "Bilinear interpolation:"
  call meshgrid(axq, ayq, XX, YY)
  ZZ = interp2(ax, ay, A, XX, YY)

  call savetxt(outdir // "linear2.txt", ZZ)
  print *, "Bilinear interpolation grid saved in " // outdir

  ! Bicubic spline interpolation
  print *; print *, "Bicubic spline interpolation:"
  ZZ = spline2(ax, ay, A, XX, YY)

  call savetxt(outdir // "spline2.txt", ZZ)
  print *, "Bicubic spline interpolation grid saved in " // outdir

  ! 2D B-spline interpolation
  print *; print *, "2D B-spline interpolation:"
  ZZ = bspline2(ax, ay, A, XX, YY, 4)

  call savetxt(outdir // "bspline2.txt", ZZ)
  print *, "2D B-spline interpolation grid saved in " // outdir

  print *; print *, "Run script /utils/view_interp.py to check results."

  print *

contains

  real(kind = RPRE) function rosenbrock(x)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: nd
    real(kind = RPRE) :: sum1, sum2

    nd = size(x)
    sum1 = sum( ( x(2:) - x(:nd-1)**2 )**2 )
    sum2 = sum( ( 1.0d0 - x(:nd-1) )**2 )
    rosenbrock = 100.0d0 * sum1 + sum2
    return
  end function rosenbrock

end program example_interp
