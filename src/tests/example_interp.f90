program example_interp

  use forlab

  implicit none

  integer(kind = IPRE) :: i, j, k, m, n, zn, ve, d(7), niter, nx, ny
  real(kind = RPRE) :: x, lat, lon, east, north
  integer(kind = IPRE), dimension(:), allocatable :: idx
  real(kind = RPRE), dimension(:), allocatable :: b, w, ax, axq, ay, ayq, az, y, z, ad, f, xi
  real(kind = RPRE), dimension(:,:), allocatable :: A, C, L, U1, U2, V, XX, YY, ZZ, xq, yq, zq
  real(kind = RPRE), dimension(:,:,:), allocatable :: av
  real(kind = RPRE), dimension(:,:,:,:), allocatable :: tt
  real(kind = RPRE), dimension(8) :: data = [ 1., 1., 1., 1., 0., 0., 0., 0. ]
  character(len = 1) :: zl
  complex(kind = RPRE), dimension(:), allocatable :: cz

  character(len = :), allocatable :: outdir

  ! Output directory
  outdir = "examples/interp/"
  call system("mkdir -p " // outdir)

  ! Create an undersampled grid to interpolate (Rosenbrock)
  n = 20
  A = zeros(n, n)
  ax = linspace(-5.12, 5.12, n)
  ay = linspace(-5.12, 5.12, n)
  do i = 1, n
    do j = 1, n
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
