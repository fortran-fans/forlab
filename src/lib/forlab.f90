!=======================================================================
! Forlab
!-----------------------------------------------------------------------
! Forlab aims to provide a package of functions for scientific
! computing in Fortran.
!
! Created by
!     Keurfon Luu <keurfon.luu@mines-paristech.fr>
!     MINES ParisTech - Centre de Géosciences
!     PSL - Research University
!
! Notes
!-----------------------------------------------------------------------
! When changing precision (IPRE and/or RPRE), the whole program needs to
! be recompiled.
!=======================================================================

module forlab

#ifdef do_mpi
  use mpi
#endif

  implicit none

!=======================================================================
! Parameters
!=======================================================================

  integer, public, parameter :: IPRE = 4
  integer, public, parameter :: RPRE = 4
  integer, public, parameter :: CLEN = 512
  real(kind = 8), public, parameter :: pi = 3.141592653589793238460d0
  real(kind = 8), public, save :: tic_time

!=======================================================================
! Functions
!=======================================================================

  private
  public :: File, acosd, asind, atand, argmax, argmin, argsort, arange, &
    angle, bsplrep1, bsplrep2, bspline1, bspline2, chol, cosd, countlines, &
    cov, cumsum, chi2cdf, chi2pdf, chi2inv, chi2rand, check_directory, &
    det, diag, disp, deg2utm, datenum, datevec, datestr, deboor, diff, &
    eye, eig, find, flip, fliplr, flipud, fminbnd, gammainc, horzcat, &
    hann, interp1, interp2, interp3, inv, ismember, isoutlier, issquare, &
    isleap, issymmetric, kurtosis, k2test, kde, loadtxt, loadbin, linspace, &
    mean, median, mad, meshgrid, nextpow2, norm, normpdf, num2str, ones, &
    pascal, prctile, progress_bar, progress_perc, rng, randu, randn, &
    randi, randperm, repmat, rms, savetxt, savebin, sind, sort, solve, &
    svd, svdsolve, std, spline1, spline2, skewness, signum, sinc, &
    split_argument, tand, tic, toc, trace, tril, triu, utm2deg, vertcat, &
    var, zeros, dbindex, gmm, kmeans, mbkmeans, silhouette
#ifdef do_mpi
  public :: mpi_rpre
#endif

!=======================================================================
! Object File
!=======================================================================

  type File
    integer(kind = IPRE) :: unit
    character(len = CLEN) :: filename
  contains
    generic, public :: open => open1, open2
    procedure, private :: open1, open2
    procedure, public :: close
    generic, public :: countlines => countlines1
    procedure, private :: countlines1
  end type File

!=======================================================================
! Abstract function
!=======================================================================

  abstract interface
    real(kind = RPRE) function func1d(x)
      import :: RPRE
      real(kind = RPRE), intent(in) :: x
    end function func1d
  end interface

!=======================================================================
! Polymorphic functions and subroutines
!=======================================================================

  !---------------------------------------------------------------------
  ! Function acosd
  !---------------------------------------------------------------------
  interface acosd
    module procedure acosd0, acosd1, acosd2, acosd3
  end interface acosd

  !---------------------------------------------------------------------
  ! Function angle
  !---------------------------------------------------------------------
  interface angle
    module procedure angle0, angle1
  end interface angle

  !---------------------------------------------------------------------
  ! Function argmax
  !---------------------------------------------------------------------
  interface argmax
    module procedure argmax1, argmax2, argmax3
  end interface argmax

  !---------------------------------------------------------------------
  ! Function argmin
  !---------------------------------------------------------------------
  interface argmin
    module procedure argmin1, argmin2, argmin3
  end interface argmin

  !---------------------------------------------------------------------
  ! Function asind
  !---------------------------------------------------------------------
  interface asind
    module procedure asind0, asind1, asind2, asind3
  end interface asind

  !---------------------------------------------------------------------
  ! Function atand
  !---------------------------------------------------------------------
  interface atand
    module procedure atand0, atand1, atand2, atand3
  end interface atand

  !---------------------------------------------------------------------
  ! Function bspline1
  !---------------------------------------------------------------------
  interface bspline1
    module procedure bspline1_1
  end interface bspline1

  !---------------------------------------------------------------------
  ! Function bspline2
  !---------------------------------------------------------------------
  interface bspline2
    module procedure bspline2_1, bspline2_2
  end interface bspline2

  !---------------------------------------------------------------------
  ! Function chi2cdf
  !---------------------------------------------------------------------
  interface chi2cdf
    module procedure chi2cdf0, chi2cdf1_0, chi2cdf1_1
  end interface chi2cdf

  !---------------------------------------------------------------------
  ! Function chi2inv
  !---------------------------------------------------------------------
  interface chi2inv
    module procedure chi2inv0, chi2inv1_0, chi2inv1_1
  end interface chi2inv

  !---------------------------------------------------------------------
  ! Function chi2pdf
  !---------------------------------------------------------------------
  interface chi2pdf
    module procedure chi2pdf0, chi2pdf1_0, chi2pdf1_1
  end interface chi2pdf

  !---------------------------------------------------------------------
  ! Function chi2rand
  !---------------------------------------------------------------------
  interface chi2rand
    module procedure chi2rand0, chi2rand1
  end interface chi2rand

  !---------------------------------------------------------------------
  ! Function cosd
  !---------------------------------------------------------------------
  interface cosd
    module procedure cosd0, cosd1, cosd2, cosd3
  end interface cosd

  !---------------------------------------------------------------------
  ! Function countlines
  !---------------------------------------------------------------------
  interface countlines
    module procedure countlines2
  end interface countlines

  !---------------------------------------------------------------------
  ! Function cov
  !---------------------------------------------------------------------
  interface cov
    module procedure cov1_1, cov1_2, cov2_1, cov2_2
  end interface cov

  !---------------------------------------------------------------------
  ! Function cumsum
  !---------------------------------------------------------------------
  interface cumsum
    module procedure cumsum1, cumsum2
  end interface cumsum

  !---------------------------------------------------------------------
  ! Function datenum
  !---------------------------------------------------------------------
  interface datenum
    module procedure datenum0
  end interface datenum

  !---------------------------------------------------------------------
  ! Function datestr
  !---------------------------------------------------------------------
  interface datestr
    module procedure datestr0_0
  end interface datestr

  !---------------------------------------------------------------------
  ! Function datevec
  !---------------------------------------------------------------------
  interface datevec
    module procedure datevec0
  end interface datevec

  !---------------------------------------------------------------------
  ! Function dbindex
  !---------------------------------------------------------------------
  interface dbindex
    module procedure dbindex1, dbindex2
  end interface dbindex

  !---------------------------------------------------------------------
  ! Function deg2utm
  !---------------------------------------------------------------------
  interface deg2utm
    module procedure deg2utm0, deg2utm1
  end interface deg2utm

  !---------------------------------------------------------------------
  ! Function diag
  !---------------------------------------------------------------------
  interface diag
    module procedure diag1, diag2
  end interface diag

  !---------------------------------------------------------------------
  ! Function diff
  !---------------------------------------------------------------------
  interface diff
    module procedure diff1, diff2
  end interface diff

  !---------------------------------------------------------------------
  ! Subroutine disp
  !---------------------------------------------------------------------
  interface disp
    module procedure disp_i0, disp_r0, disp_c0, disp_i1, disp_r1, &
      disp_c1, disp_i2, disp_r2, disp_i3, disp_r3
  end interface disp

  !---------------------------------------------------------------------
  ! Function File
  !---------------------------------------------------------------------
  interface File
    module procedure init_File
  end interface File

  !---------------------------------------------------------------------
  ! Function find
  !---------------------------------------------------------------------
  interface find
    module procedure find1, find2, find3
  end interface find

  !---------------------------------------------------------------------
  ! Function flip
  !---------------------------------------------------------------------
  interface flip
    module procedure flip_i1, flip_r1, flip_i2, flip_r2, flip_i3, &
      flip_r3
  end interface flip

  !---------------------------------------------------------------------
  ! Function flipud
  !---------------------------------------------------------------------
  interface flipud
    module procedure flipud_i1, flipud_r1, flipud_i2, flipud_r2
  end interface flipud

  !---------------------------------------------------------------------
  ! Function fliplr
  !---------------------------------------------------------------------
  interface fliplr
    module procedure fliplr_i1, fliplr_r1, fliplr_i2, fliplr_r2
  end interface fliplr

  !---------------------------------------------------------------------
  ! Function gammainc
  !---------------------------------------------------------------------
  interface gammainc
    module procedure gammainc0, gammainc1_0
  end interface gammainc

  !---------------------------------------------------------------------
  ! Function gmm
  !---------------------------------------------------------------------
  interface gmm
    module procedure gmm1, gmm2
  end interface gmm

  !---------------------------------------------------------------------
  ! Function horzcat
  !---------------------------------------------------------------------
  interface horzcat
    module procedure horzcat_i1, horzcat_r1, horzcat_i2, horzcat_r2, horzcat_i12, &
      horzcat_r12, horzcat_i21, horzcat_r21
  end interface horzcat

  !---------------------------------------------------------------------
  ! Function interp1
  !---------------------------------------------------------------------
  interface interp1
    module procedure interp1_0, interp1_1
  end interface interp1

  !---------------------------------------------------------------------
  ! Function interp2
  !---------------------------------------------------------------------
  interface interp2
    module procedure interp2_0, interp2_1, interp2_2
  end interface interp2

  !---------------------------------------------------------------------
  ! Function interp3
  !---------------------------------------------------------------------
  interface interp3
    module procedure interp3_0, interp3_1
  end interface interp3

  !---------------------------------------------------------------------
  ! Function ismember
  !---------------------------------------------------------------------
  interface ismember
    module procedure ismember_i0i1, ismember_i0r1, ismember_i0i2, &
      ismember_i0r2, ismember_i0i3, ismember_i0r3, ismember_r0i1, &
      ismember_r0r1, ismember_r0i2, ismember_r0r2, ismember_r0i3, &
      ismember_r0r3
  end interface ismember

  !---------------------------------------------------------------------
  ! Function kde
  !---------------------------------------------------------------------
  interface kde
    module procedure kde1, kde2
  end interface kde

  !---------------------------------------------------------------------
  ! Function kmeans
  !---------------------------------------------------------------------
  interface kmeans
    module procedure kmeans1, kmeans2
  end interface kmeans

  !---------------------------------------------------------------------
  ! Function kurtosis
  !---------------------------------------------------------------------
  interface kurtosis
    module procedure kurtosis1, kurtosis2
  end interface kurtosis

  !---------------------------------------------------------------------
  ! Function linspace
  !---------------------------------------------------------------------
  interface linspace
    module procedure linspace_r8r8, linspace_r4r4, linspace_i4i4, &
      linspace_r8i4, linspace_r4i4, linspace_i4r8, linspace_i4r4
  end interface linspace

  !---------------------------------------------------------------------
  ! Function loadbin
  !---------------------------------------------------------------------
  interface loadbin
    module procedure loadbin0, loadbin1, loadbin2, loadbin3
  end interface loadbin

  !---------------------------------------------------------------------
  ! Function loadtxt
  !---------------------------------------------------------------------
  interface loadtxt
    module procedure loadtxt1, loadtxt2
  end interface loadtxt

  !---------------------------------------------------------------------
  ! Function log2
  !---------------------------------------------------------------------
  interface log2
    module procedure log2_i0, log2_r0, log2_i1, log2_r1
  end interface log2

  !---------------------------------------------------------------------
  ! Function mad
  !---------------------------------------------------------------------
  interface mad
    module procedure mad1, mad2
  end interface mad

  !---------------------------------------------------------------------
  ! Function mbkmeans
  !---------------------------------------------------------------------
  interface mbkmeans
    module procedure mbkmeans1, mbkmeans2
  end interface mbkmeans

  !---------------------------------------------------------------------
  ! Function median
  !---------------------------------------------------------------------
  interface median
    module procedure median1, median2
  end interface median

  !---------------------------------------------------------------------
  ! Function mean
  !---------------------------------------------------------------------
  interface mean
    module procedure mean1, mean2
  end interface mean

  !---------------------------------------------------------------------
  ! Subroutine meshgrid
  !---------------------------------------------------------------------
  interface meshgrid
    module procedure meshgrid2
  end interface meshgrid

  !---------------------------------------------------------------------
  ! Function nextpow2
  !---------------------------------------------------------------------
  interface nextpow2
    module procedure nextpow2_0, nextpow2_1
  end interface nextpow2

  !---------------------------------------------------------------------
  ! Function norm
  !---------------------------------------------------------------------
  interface norm
    module procedure norm1, norm2
  end interface norm

  !---------------------------------------------------------------------
  ! Function normpdf
  !---------------------------------------------------------------------
  interface normpdf
    module procedure normpdf0, normpdf1, normpdf2
  end interface normpdf

  !---------------------------------------------------------------------
  ! Function num2str
  !---------------------------------------------------------------------
  interface num2str
    module procedure num2str_i4, num2str_i8, num2str_r4, num2str_r8
  end interface num2str

  !---------------------------------------------------------------------
  ! Function ones
  !---------------------------------------------------------------------
  interface ones
    module procedure ones1, ones2, ones3
  end interface ones

  !---------------------------------------------------------------------
  ! Function prctile
  !---------------------------------------------------------------------
  interface prctile
    module procedure prctile0, prctile1
  end interface prctile

  !---------------------------------------------------------------------
  ! Function randi
  !---------------------------------------------------------------------
  interface randi
    module procedure randi0_0, randi0_1, randi1_0, randi1_1, randi2_0, &
      randi2_1, randi3_0, randi3_1
  end interface randi

  !---------------------------------------------------------------------
  ! Function randu
  !---------------------------------------------------------------------
  interface randu
    module procedure randu0, randu1, randu2, randu3
  end interface randu

  !---------------------------------------------------------------------
  ! Function randn
  !---------------------------------------------------------------------
  interface randn
    module procedure randn0, randn1, randn2, randn3
  end interface randn

  !---------------------------------------------------------------------
  ! Function repmat
  !---------------------------------------------------------------------
  interface repmat
    module procedure repmat1, repmat2
  end interface repmat

  !---------------------------------------------------------------------
  ! Function rms
  !---------------------------------------------------------------------
  interface rms
    module procedure rms1, rms2
  end interface rms

  !---------------------------------------------------------------------
  ! Subroutine savebin
  !---------------------------------------------------------------------
  interface savebin
    module procedure savebin1_r4, savebin1_r8, savebin2_r4, savebin2_r8, &
      savebin3_r4, savebin3_r8
  end interface savebin

  !---------------------------------------------------------------------
  ! Subroutine savetxt
  !---------------------------------------------------------------------
  interface savetxt
    module procedure savetxt1_i4, savetxt1_r4, savetxt1_i8, savetxt1_r8, &
      savetxt2_i4, savetxt2_r4, savetxt2_i8, savetxt2_r8
  end interface savetxt

  !---------------------------------------------------------------------
  ! Function signum
  !---------------------------------------------------------------------
  interface signum
    module procedure signum0, signum1, signum2
  end interface signum

  !---------------------------------------------------------------------
  ! Function sinc
  !---------------------------------------------------------------------
  interface sinc
    module procedure sinc0, sinc1
  end interface sinc

  !---------------------------------------------------------------------
  ! Function silhouette
  !---------------------------------------------------------------------
  interface silhouette
    module procedure silhouette1, silhouette2
  end interface silhouette

  !---------------------------------------------------------------------
  ! Function sind
  !---------------------------------------------------------------------
  interface sind
    module procedure sind0, sind1, sind2, sind3
  end interface sind

  !---------------------------------------------------------------------
  ! Function skewness
  !---------------------------------------------------------------------
  interface skewness
    module procedure skewness1, skewness2
  end interface skewness

  !---------------------------------------------------------------------
  ! Function spline1
  !---------------------------------------------------------------------
  interface spline1
    module procedure spline1_0, spline1_1
  end interface spline1

  !---------------------------------------------------------------------
  ! Function spline2
  !---------------------------------------------------------------------
  interface spline2
    module procedure spline2_1, spline2_2
  end interface spline2

  !---------------------------------------------------------------------
  ! Function std
  !---------------------------------------------------------------------
  interface std
    module procedure std1, std2
  end interface std

  !---------------------------------------------------------------------
  ! Function tand
  !---------------------------------------------------------------------
  interface tand
    module procedure tand0, tand1, tand2, tand3
  end interface tand

  !---------------------------------------------------------------------
  ! Function tril
  !---------------------------------------------------------------------
  interface tril
    module procedure tril_i, tril_r, tril_c
  end interface tril

  !---------------------------------------------------------------------
  ! Function triu
  !---------------------------------------------------------------------
  interface triu
    module procedure triu_i, triu_r, triu_c
  end interface triu

  !---------------------------------------------------------------------
  ! Function utm2deg
  !---------------------------------------------------------------------
  interface utm2deg
    module procedure utm2deg0, utm2deg1
  end interface utm2deg

  !---------------------------------------------------------------------
  ! Function var
  !---------------------------------------------------------------------
  interface var
    module procedure var1, var2
  end interface var

  !---------------------------------------------------------------------
  ! Function vertcat
  !---------------------------------------------------------------------
  interface vertcat
    module procedure vertcat_r1, vertcat_r2, vertcat_c2, vertcat_r12, &
      vertcat_r21
  end interface vertcat

  !---------------------------------------------------------------------
  ! Function zeros
  !---------------------------------------------------------------------
  interface zeros
    module procedure zeros1, zeros2, zeros3
  end interface zeros

!=======================================================================
! End of declaration of interfaces
!=======================================================================

contains

!=======================================================================
! acosd
!-----------------------------------------------------------------------
! acosd computes the inverse cosine in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = acosd(x)
!
! Description
!-----------------------------------------------------------------------
! y = acosd(x) returns the inverse cosine of the elements in x in
! degrees. For real elements of x in the domain [-1,1], acosd returns
! values in the range [0,180]. For values of x outside this range,
! acosd returns NaN (Not a Number).
!
! Examples
!-----------------------------------------------------------------------
! y = acosd(1.)
!     1.
!
! y = acosd(2.)
!     NaN
!
! x = [ -1., 0., 1. ]
! y = acosd(x)
!     180.  90.   0.
!=======================================================================

  real(kind = RPRE) function acosd0(x)
    real(kind = RPRE), intent(in) :: x

    acosd0 = acos(x)*180.0d0/pi
    return
  end function acosd0

  function acosd1(x)
    real(kind = RPRE), dimension(:), allocatable :: acosd1
    real(kind = RPRE), dimension(:), intent(in) :: x

    acosd1 = acos(x)*180.0d0/pi
    return
  end function acosd1

  function acosd2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: acosd2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    acosd2 = acos(A)*180.0d0/pi
    return
  end function acosd2

  function acosd3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: acosd3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    acosd3 = acos(X)*180.0d0/pi
    return
  end function acosd3

!=======================================================================
! angle
!-----------------------------------------------------------------------
! angle compute the phase angle.
!
! Syntax
!-----------------------------------------------------------------------
! p = angle(z)
! P = angle(Z)
!
! Description
!-----------------------------------------------------------------------
! p = angle(z) returns the phase angle in radians of the complex
! number z.
!
! P = angle(Z) returns the phase angles in radians of each complex
! numbers in vector Z.
!=======================================================================

  real(kind = RPRE) function angle0(z)
    complex(kind = RPRE), intent(in) :: z

    angle0 = imag(log(z))
    return
  end function angle0

  function angle1(Z)
    real(kind = RPRE), dimension(:), allocatable :: angle1
    complex(kind = RPRE), dimension(:), intent(in) :: Z
    integer(kind = IPRE) :: i, n

    n = size(Z)
    angle1 = zeros(n)
    do i = 1, n
      angle1(i) = angle0(Z(i))
    end do
    return
  end function angle1

!=======================================================================
! arange
!-----------------------------------------------------------------------
! arange returns evenly spaced vector.
!
! Syntax
!-----------------------------------------------------------------------
! x = arange(first, last)
!
! Description
!-----------------------------------------------------------------------
! x = arange(first, last) returns an evenly spaced integer vector
! starting from first and ending at last.
!
! Examples
!-----------------------------------------------------------------------
! x = arange(1, 9)
!     1   2   3   4   5   6   7   8   9
!=======================================================================

  function arange(first, last)
    integer(kind = IPRE), dimension(:), allocatable :: arange
    integer(kind = IPRE), intent(in) :: first, last
    integer(kind = IPRE) :: i

    arange = [ ( i, i = first, last ) ]
    return
  end function arange

!=======================================================================
! argmax
!-----------------------------------------------------------------------
! argmax computes the indices of the maximum value of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = argmax(x)
! y = argmax(A)
! y = argmax(X)
!
! Description
!-----------------------------------------------------------------------
! y = argmax(x) returns the index of the maximum value of the vector x.
!
! y = argmax(A) returns a 2-elements vector with the indices of the
! maximum value of the matrix A.
!
! y = argmax(X) returns a 3-elements vector with the indices of the
! maximum value of the 3-dimensional matrix X.
!
! Notes
!-----------------------------------------------------------------------
! argmax ignores NaN values.
!=======================================================================

  integer(kind = IPRE) function argmax1(x)
    real(kind = RPRE), dimension(:), intent(in) :: x

    argmax1 = maxloc(x, 1, .not. isnan(x))
    return
  end function argmax1

  function argmax2(A)
    integer(kind = IPRE) :: argmax2(2)
    real(kind = IPRE), dimension(:,:), intent(in) :: A

    argmax2 = maxloc(A, .not. isnan(A))
    return
  end function argmax2

  function argmax3(X)
    integer(kind = IPRE) :: argmax3(3)
    real(kind = IPRE), dimension(:,:,:), intent(in) :: X

    argmax3 = maxloc(X, .not. isnan(X))
    return
  end function argmax3

!=======================================================================
! argmin
!-----------------------------------------------------------------------
! argmin computes the indices of the minimum value of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = argmin(x)
! y = argmin(A)
! y = argmin(X)
!
! Description
!-----------------------------------------------------------------------
! y = argmin(x) returns the index of the minimum value of the vector x.
!
! y = argmin(A) returns a 2-elements vector with the indices of the
! minimum value of the matrix A.
!
! y = argmin(X) returns a 3-elements vector with the indices of the
! minimum value of the 3-dimensional matrix X.
!
! Notes
!-----------------------------------------------------------------------
! argmin ignores NaN values.
!=======================================================================

  integer(kind = IPRE) function argmin1(x)
    real(kind = RPRE), dimension(:), intent(in) :: x

    argmin1 = minloc(x, 1, .not. isnan(x))
    return
  end function argmin1

  function argmin2(A)
    integer(kind = IPRE) :: argmin2(2)
    real(kind = IPRE), dimension(:,:), intent(in) :: A

    argmin2 = minloc(A, .not. isnan(A))
    return
  end function argmin2

  function argmin3(X)
    integer(kind = IPRE) :: argmin3(3)
    real(kind = IPRE), dimension(:,:,:), intent(in) :: X

    argmin3 = minloc(X, .not. isnan(X))
    return
  end function argmin3

!=======================================================================
! argsort
!-----------------------------------------------------------------------
! argsort generates the indices that would sort an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = argsort(x)
! y = argsort(x, 1)
! y = argsort(x, 2)
!
! Description
!-----------------------------------------------------------------------
! y = argsort(x) returns the indices that would sort an array in
! ascending order.
!
! y = argsort(x, 1) (see y = argsort(x)).
!
! y = argsort(x, 2) returns the indices that would sort an array in
! descending order.
!
! Notes
!-----------------------------------------------------------------------
! x(argsort(x), order) returns the same result as sort(x, order).
!=======================================================================

  function argsort(x, order)
    integer(kind = IPRE), dimension(:), allocatable :: argsort
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: order
    integer(kind = IPRE) :: i, n
    real(kind = RPRE), dimension(:), allocatable :: xsort

    n = size(x)
    xsort = x
    argsort = [ ( i, i = 1, n ) ]
    if ((.not. present(order)) .or. (order .eq. 1)) then
      call quickargsort(xsort, argsort, n, 1)
    elseif (order .eq. 2) then
      call quickargsort(xsort, argsort, n, 2)
    end if
    return

  contains

    !-------------------------------------------------------------------
    ! quickargsort
    !-------------------------------------------------------------------
    recursive subroutine quickargsort(x, idx, n, order)
      real(kind = RPRE), dimension(n), intent(inout) :: x
      integer(kind = IPRE), dimension(n), intent(inout) :: idx
      integer(kind = IPRE), intent(in) :: n, order
      integer(kind = IPRE) :: left, right, marker
      real(kind = RPRE) :: pivot, tmp

      if (n .gt. 1) then
        left = 0
        right = n + 1
        pivot = x(randi(n))

        select case(order)
          case(1)
            do while ( left .lt. right )
              left = left + 1
              right = right - 1
              do while ( x(left) .lt. pivot )
                left = left + 1
              end do
              do while ( x(right) .gt. pivot )
                right = right - 1
              end do
              if ( left .lt. right ) then
                tmp = x(left)
                x(left) = x(right)
                x(right) = tmp
                tmp = idx(left)
                idx(left) = idx(right)
                idx(right) = tmp
              end if
            end do
          case(2)
            do while ( left .lt. right )
              left = left + 1
              right = right - 1
              do while ( x(left) .gt. pivot )
                left = left + 1
              end do
              do while ( x(right) .lt. pivot )
                right = right - 1
              end do
              if ( left .lt. right ) then
                tmp = x(left)
                x(left) = x(right)
                x(right) = tmp
                tmp = idx(left)
                idx(left) = idx(right)
                idx(right) = tmp
              end if
            end do
        end select

        if ( left .eq. right ) then
          marker = left + 1
        else
          marker = left
        end if

        call quickargsort(x(:marker-1), idx(:marker-1), marker-1, order)
        call quickargsort(x(marker:), idx(marker:), n-marker+1, order)
      end if
      return
    end subroutine quickargsort

  end function argsort

!=======================================================================
! asind
!-----------------------------------------------------------------------
! asind computes the inverse sine in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = asind(x)
!
! Description
!-----------------------------------------------------------------------
! y = asind(x) returns the inverse sine of the elements in x in degrees.
! For real elements of x in the domain [-1,1], asind returns values in
! the range [-90,90]. For values of x outside this range, asind returns
! NaN (Not a Number).
!
! Examples
!-----------------------------------------------------------------------
! y = asind(1.)
!     90.
!
! y = asind(2.)
!     NaN
!
! x = [ -1., 0., 1. ]
! y = asind(x)
!     -90.  0.  90.
!=======================================================================

  real(kind = RPRE) function asind0(x)
    real(kind = RPRE), intent(in) :: x

    asind0 = asin(x)*180.0d0/pi
    return
  end function asind0

  function asind1(x)
    real(kind = RPRE), dimension(:), allocatable :: asind1
    real(kind = RPRE), dimension(:), intent(in) :: x

    asind1 = asin(x)*180.0d0/pi
    return
  end function asind1

  function asind2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: asind2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    asind2 = asin(A)*180.0d0/pi
    return
  end function asind2

  function asind3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: asind3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    asind3 = asin(X)*180.0d0/pi
    return
  end function asind3

!=======================================================================
! atand
!-----------------------------------------------------------------------
! atand computes the inverse tangent in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = atand(x)
!
! Description
!-----------------------------------------------------------------------
! y = atand(x) returns the inverse tangent of the elements in x in
! degrees. For real elements of x in the domain [-Inf,Inf], atand
! returns values in the range [-90,90].
!
! Examples
!-----------------------------------------------------------------------
! y = atand(0.)
!     0.
!
! y = atand(50.)
!     88.8542328
!
! x = [ -50., 0., 50. ]
! y = atand(x)
!     -88.8542328   0.  88.8542328
!=======================================================================

  real(kind = RPRE) function atand0(x)
    real(kind = RPRE), intent(in) :: x

    atand0 = atan(x)*180.0d0/pi
    return
  end function atand0

  function atand1(x)
    real(kind = RPRE), dimension(:), allocatable :: atand1
    real(kind = RPRE), dimension(:), intent(in) :: x

    atand1 = atan(x)*180.0d0/pi
    return
  end function atand1

  function atand2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: atand2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    atand2 = atan(A)*180.0d0/pi
    return
  end function atand2

  function atand3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: atand3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    atand3 = atan(X)*180.0d0/pi
    return
  end function atand3

!=======================================================================
! bsplrep1
!-----------------------------------------------------------------------
! bsplrep1 computes the B-spline representation C(t) of a set of
! 1-dimensional control points with a uniform knot vector.
!
! Syntax
!-----------------------------------------------------------------------
! call bsplrep1(x, y, xq, yq)
! call bsplrep1(x, y, xq, yq, order)
! call bsplrep1(x, y, xq, yq, order, n1)
!
! Description
!-----------------------------------------------------------------------
! call bsplrep1(x, y, xq, yq) returns the B-spline representation C(t)
! given the control points defined by x and y using a cubic spline
! (order 4 / degree 3).
!
! call bsplrep1(x, y, xq, yq, order) returns the B-spline representation
! C(t) given the control points defined by x and y and the order.
!
! call bsplrep1(x, y, xq, yq, order, n1) returns the n1-points B-spline
! representation C(t) given the control points defined by x and y, and
! the order.
!=======================================================================

  subroutine bsplrep1(x, y, xq, yq, order, n1)
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: xq, yq
    integer(kind = IPRE), intent(in), optional :: order, n1
    integer(kind = IPRE) :: opt_n1, i, iq, j, k, n
    real(kind = RPRE) :: w
    integer(kind = IPRE), dimension(:), allocatable :: x0
    real(kind = RPRE), dimension(:), allocatable :: t, y1

    n = size(x)
    k = 4
    opt_n1 = 100
    if (present(order)) k = order
    if (present(n1)) opt_n1 = n1

    if (k .gt. n) then
      print *, "Error: in bsplrep1, order k should be less than the " &
        // "number of control points (" // num2str(k) // " > " &
        // num2str(n) // ")."
      stop
    end if

    xq = zeros(opt_n1)
    yq = zeros(opt_n1)
    t = [ zeros(k-1), linspace(0, 1, n-k+2), ones(k-1) ]
    y1 = linspace(0, 1, opt_n1)

    do iq = 1, opt_n1
      x0 = find(y1(iq) .ge. t)
      j = min( n, x0(size(x0)) )
      do i = j-k+1, j
        w = deboor(i, k, y1(iq), t)
        xq(iq) = xq(iq) + x(i) * w
        yq(iq) = yq(iq) + y(i) * w
      end do
    end do
    return
  end subroutine bsplrep1

!=======================================================================
! bsplrep2
!-----------------------------------------------------------------------
! bsplrep2 computes the B-spline surface representation S(t) of a set of
! 2-dimensional control points with uniform knot vectors.
!
! Syntax
!-----------------------------------------------------------------------
! call bsplrep2(x, y, z, xq, yq, zq)
! call bsplrep2(x, y, z, xq, yq, zq, order)
! call bsplrep2(x, y, z, xq, yq, zq, order, n1, n2)
!
! Description
!-----------------------------------------------------------------------
! call bsplrep2(x, y, z, xq, yq, zq) returns the B-spline surface
! representation S(t) = (xq, yq, zq) given the control points defined by
! x, y and z using a cubic spline (order 4 / degree 3).
!
! call bsplrep2(x, y, z, xq, yq, zq, order) returns the B-spline surface
! representation S(t) = (xq, yq, zq) given the control points defined by
! x, y and z, and the order.
!
! call bsplrep2(x, y, z, xq, yq, zq, order, n1, n2) returns the
! n1-by-b2-points B-spline surface representation S(t) = (xq, yq, zq)
! given the control points defined by x, y and z, and the order.
!=======================================================================

  subroutine bsplrep2(x, y, z, xq, yq, zq, order, n1, n2)
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: z
    real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: xq, yq, zq
    integer(kind = IPRE), intent(in), optional :: order, n1, n2
    integer(kind = IPRE) :: opt_n1, opt_n2, i1, i2, iq1, iq2, j1, j2, k, m, n
    real(kind = RPRE) :: w1, w2
    integer(kind = IPRE), dimension(:), allocatable :: x0, y0
    real(kind = RPRE), dimension(:), allocatable :: t1, t2, y1, y2

    m = size(x)
    n = size(y)
    k = 4
    opt_n1 = 100
    opt_n2 = 100
    if (present(order)) k = order
    if (present(n1)) opt_n1 = n1
    if (present(n2)) opt_n2 = n2

    if (k .gt. min(m, n)) then
      print *, "Error: in bsplrep2, order k should be less than the " &
        // "number of control points (" // num2str(k) // " > " &
        // num2str(min(m, n)) // ")."
      stop
    end if

    xq = zeros(opt_n1, opt_n2)
    yq = zeros(opt_n1, opt_n2)
    zq = zeros(opt_n1, opt_n2)
    t1 = [ zeros(k-1), linspace(0, 1, m-k+2), ones(k-1) ]
    t2 = [ zeros(k-1), linspace(0, 1, n-k+2), ones(k-1) ]
    y1 = linspace(0, 1, opt_n1)
    y2 = linspace(0, 1, opt_n2)

    do iq1 = 1, opt_n1
      x0 = find(y1(iq1) .ge. t1)
      j1 = min( m, x0(size(x0)) )
      do iq2 = 1, opt_n2
        y0 = find(y2(iq2) .ge. t2)
        j2 = min( n, y0(size(y0)) )
        do i1 = j1-k+1, j1
          w1 = deboor(i1, k, y1(iq1), t1)
          do i2 = j2-k+1, j2
            w2 = deboor(i2, k, y2(iq2), t2)
            xq(iq1,iq2) = xq(iq1,iq2) + x(i1) * w1 * w2
            yq(iq1,iq2) = yq(iq1,iq2) + y(i2) * w1 * w2
            zq(iq1,iq2) = zq(iq1,iq2) + z(i1,i2) * w1 * w2
          end do
        end do
      end do
    end do
    return
  end subroutine bsplrep2

!=======================================================================
! bspline1
!-----------------------------------------------------------------------
! bspline1 approximates a set of 1-dimensional control points with
! spline curves in B-spline form.
!
! Syntax
!-----------------------------------------------------------------------
! yq = bspline1(x, y, xq)
! yq = bspline1(x, y, xq, order)
!
! Description
!-----------------------------------------------------------------------
! yq = bspline1(x, y, xq) returns the approximated vector yq at the
! query points in xq using a cubic spline (order 4 / degree 3).
!
! yq = bspline1(x, y, xq, order) returns the approximated vector yq at
! the query points in xq with spline curves given the order.
!=======================================================================

  function bspline1_1(x, y, xq, order, n1) result(yq)
    real(kind = RPRE), dimension(:), allocatable :: yq
    real(kind = RPRE), dimension(:), intent(in) :: x, y, xq
    integer(kind = IPRE), intent(in), optional :: order, n1
    integer(kind = IPRE) :: k, n, opt_n1
    real(kind = RPRE), dimension(:), allocatable :: bspl_x, bspl_y

    n = size(x)
    k = 4
    opt_n1 = 100
    if (present(order)) k = order
    if (present(n1)) opt_n1 = n1
    if (k .gt. n) then
      print *, "Error: in bspline1, order k should be less than the " &
        // "number of control points (" // num2str(k) // " > " &
        // num2str(n) // ")."
      stop
    end if

    call bsplrep1(x, y, bspl_x, bspl_y, k, opt_n1)
    yq = spline1(bspl_x, bspl_y, xq)
    return
  end function bspline1_1

!=======================================================================
! bspline2
!-----------------------------------------------------------------------
! bspline2 approximates a set of 2-dimensional control points with
! spline curves in B-spline form.
!
! Syntax
!-----------------------------------------------------------------------
! zq = bspline2(x, y, z, xq, yq)
! zq = bspline2(x, y, z, xq, yq, order)
! ZQ = bspline2(x, y, z, XQ, YQ)
! ZQ = bspline2(x, y, z, XQ, YQ, order)
!
! Description
!-----------------------------------------------------------------------
! zq = bspline2(x, y, z, xq, yq) returns the approximated vector zq at
! the query points in xq and yq using a cubic spline (degree 3).
!
! zq = bspline2(x, y, z, xq, yq, order) returns the approximated vector
! zq at the query points in xq and yq with spline curves given the
! order.
!
! ZQ = bspline2(x, y, Z, XQ, YQ) returns the evaluated matrix ZQ given
! mesh type grids XQ and YQ using a bicubic spline (degree 3). ZQ is of
! the same shape as XQ and YQ.
!
! ZQ = bspline2(x, y, Z, XQ, YQ, order) returns the evaluated matrix ZQ
! given mesh type grids XQ and YQ with spline curves given the order. ZQ
! is of the same shape as XQ and YQ.
!=======================================================================

  function bspline2_1(x, y, z, xq, yq, order, n1, n2) result(zq)
    real(kind = RPRE), dimension(:), allocatable :: zq
    real(kind = RPRE), dimension(:), intent(in) :: x, y, xq, yq
    real(kind = RPRE), dimension(:,:), intent(in) :: z
    integer(kind = IPRE), intent(in), optional :: order, n1, n2
    integer(kind = IPRE) :: k, m, n, nq, opt_n1, opt_n2
    real(kind = RPRE), dimension(:,:), allocatable :: bspl_x, bspl_y, bspl_z

    m = size(x)
    n = size(y)
    k = 4
    opt_n1 = 100
    opt_n2 = 100
    if (present(order)) k = order
    if (present(n1)) opt_n1 = n1
    if (present(n2)) opt_n2 = n2
    if (k .gt. min(m, n)) then
      print *, "Error: in bspline2, order k should be less than the " &
        // "number of control points (" // num2str(k) // " > " &
        // num2str(min(m, n)) // ")."
      stop
    end if

    call bsplrep2(x, y, z, bspl_x, bspl_y, bspl_z, k, opt_n1, opt_n2)
    zq = spline2(bspl_x(:,1), bspl_y(1,:), bspl_z, xq, yq)

    ! TODO:
    ! Bilinear interpolation on irregular grid by mapping physical grid
    ! to logical grid:
    ! https://www.particleincell.com/2012/quad-interpolation/

    return
  end function bspline2_1

  function bspline2_2(x, y, z, xq, yq, order) result(zq)
    real(kind = RPRE), dimension(:,:), allocatable :: zq
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: z, xq, yq
    integer(kind = IPRE), intent(in), optional :: order
    integer(kind = IPRE) :: m, n, k

    m = size(xq, 1)
    n = size(xq, 2)
    k = 4
    if (present(order)) k = order
    zq = reshape( bspline2_1(x, y, z, [ xq ], [ yq ], k), shape = [ m, n ] )
    return
  end function bspline2_2

!=======================================================================
! check_directory
!-----------------------------------------------------------------------
! check_directory appends '/' do a directory name.
!
! Syntax
!-----------------------------------------------------------------------
! call check_directory(dirname)
!
! Description
!-----------------------------------------------------------------------
! call check_directory(dirname) returns a directory name dirname that
! ends with '/'.
!=======================================================================

  subroutine check_directory(dirname)
    character(len = :), allocatable, intent(inout) :: dirname
    integer(kind = IPRE) :: i

    i = len_trim(dirname)
    if (dirname(i:i) .ne. "/") dirname = trim(dirname) // "/"
    return
  end subroutine check_directory

!=======================================================================
! chi2cdf
!-----------------------------------------------------------------------
! chi2cdf computes the chi-square cumulative distribution function.
!
! Syntax
!-----------------------------------------------------------------------
! p = chi2cdf(x, v)
!
! Description
!-----------------------------------------------------------------------
! p = chi2cdf(x, v) returns the chi-square cdf at each of the values
! in x.
!=======================================================================

  real(kind = RPRE) function chi2cdf0(x, v)
    real(kind = RPRE), intent(in) :: x
    integer(kind = IPRE), intent(in) :: v

    chi2cdf0 = gammainc(real(x/2., RPRE), real(v/2., RPRE))
    return
  end function chi2cdf0

  function chi2cdf1_0(X, v)
    real(kind = RPRE), dimension(:), allocatable :: chi2cdf1_0
    real(kind = RPRE), dimension(:), intent(in) :: X
    integer(kind = IPRE), intent(in) :: v
    integer(kind = IPRE) :: i, n

    n = size(X)
    chi2cdf1_0 = zeros(n)
    do i = 1, n
      chi2cdf1_0(i) = chi2cdf0(X(i), v)
    end do
    return
  end function chi2cdf1_0

  function chi2cdf1_1(X, V)
    real(kind = RPRE), dimension(:), allocatable :: chi2cdf1_1
    real(kind = RPRE), dimension(:), intent(in) :: X
    integer(kind = IPRE), dimension(:), intent(in) :: V
    integer(kind = IPRE) :: i, n

    n = size(X)
    chi2cdf1_1 = zeros(n)
    do i = 1, n
      chi2cdf1_1(i) = chi2cdf0(X(i), V(i))
    end do
    return
  end function chi2cdf1_1

!=======================================================================
! chi2inv
!-----------------------------------------------------------------------
! chi2inv computes the chi-square inverse cumulative distribution
! function.
!
! Syntax
!-----------------------------------------------------------------------
! x = chi2inv(p, v)
!
! Description
!-----------------------------------------------------------------------
! x = chi2inv(p, v) returns the chi-square inverse cdf at each of the
! values in p.
!=======================================================================

  real(kind = RPRE) function chi2inv0(p, v)
    real(kind = RPRE), intent(in), target :: p
    integer(kind = IPRE), intent(in), target :: v
    real(kind = RPRE) :: a, b
    real(kind = RPRE), pointer, save :: p_ptr
    integer(kind = IPRE), pointer, save :: v_ptr

    if ( p .le. 0. .or. p .ge. 1. ) then
      print *, "Error: in chi2inv0(p, v), p should be between 0 and 1"
      stop
    end if
    if ( v .le. 0 ) then
      print *, "Error: in chi2inv0(p, v), v should be greater than 0"
      stop
    end if

    p_ptr => p
    v_ptr => v
    a = 0.
    b = real(v, RPRE)
    do while ( chi2cdf(b, v) .lt. p )
      b = b * b
    end do
    chi2inv0 = fminbnd(chi2func, a, b)
    return
  contains

    real(kind = RPRE) function chi2func(x)
      real(kind = RPRE), intent(in) :: x
      chi2func = abs( chi2cdf0(x, v_ptr) - p_ptr )
      return
    end function chi2func

  end function chi2inv0

  function chi2inv1_0(P, v)
    real(kind = RPRE), dimension(:), allocatable :: chi2inv1_0
    real(kind = RPRE), dimension(:), intent(in) :: P
    integer(kind = IPRE), intent(in) :: v
    integer(kind = IPRE) :: i, n

    n = size(P)
    chi2inv1_0 = zeros(n)
    do i = 1, n
      chi2inv1_0(i) = chi2inv0(P(i), v)
    end do
    return
  end function chi2inv1_0

  function chi2inv1_1(P, V)
    real(kind = RPRE), dimension(:), allocatable :: chi2inv1_1
    real(kind = RPRE), dimension(:), intent(in) :: P
    integer(kind = IPRE), dimension(:), intent(in) :: V
    integer(kind = IPRE) :: i, n

    n = size(P)
    chi2inv1_1 = zeros(n)
    do i = 1, n
      chi2inv1_1(i) = chi2inv0(P(i), V(i))
    end do
    return
  end function chi2inv1_1

!=======================================================================
! chi2pdf
!-----------------------------------------------------------------------
! chi2pdf computes the chi-square probability distribution function.
!
! Syntax
!-----------------------------------------------------------------------
! y = chi2pdf(x, v)
!
! Description
!-----------------------------------------------------------------------
! y = chi2pdf(x, v) returns the chi-square pdf at each of the values
! in x.
!=======================================================================

  real(kind = RPRE) function chi2pdf0(x, v)
    real(kind = RPRE), intent(in) :: x
    integer(kind = IPRE), intent(in) :: v
    real(kind = RPRE) :: v2

    if ( x .gt. 0. ) then
      v2 = 0.5 * real(v, RPRE)
      chi2pdf0 = 1. / (2.*gamma(v2)) * (x/2)**(v2-1.) * exp(-x/2.)
    else
      chi2pdf0 = 0.
    end if
    return
  end function chi2pdf0

  function chi2pdf1_0(X, v)
    real(kind = RPRE), dimension(:), allocatable :: chi2pdf1_0
    real(kind = RPRE), dimension(:), intent(in) :: X
    integer(kind = IPRE), intent(in) :: v
    integer(kind = IPRE) :: i, n

    n = size(X)
    chi2pdf1_0 = zeros(n)
    do i = 1, n
      chi2pdf1_0(i) = chi2pdf0(X(i), v)
    end do
    return
  end function chi2pdf1_0

  function chi2pdf1_1(X, V)
    real(kind = RPRE), dimension(:), allocatable :: chi2pdf1_1
    real(kind = RPRE), dimension(:), intent(in) :: X
    integer(kind = IPRE), dimension(:), intent(in) :: V
    integer(kind = IPRE) :: i, n

    n = size(X)
    chi2pdf1_1 = zeros(n)
    do i = 1, n
      chi2pdf1_1(i) = chi2pdf0(X(i), V(i))
    end do
    return
  end function chi2pdf1_1

!=======================================================================
! chi2rand
!-----------------------------------------------------------------------
! chi2rand generates chi-square random numbers.
!
! Syntax
!-----------------------------------------------------------------------
! r = chi2rand(v)
! r = chi2rand(v, dim1)
!
! Description
!-----------------------------------------------------------------------
! r = chi2rand(v) returns a chi-square distributed random number with
! v degrees of freedom.
!
! r = chi2rand(v, dim1) returns a dim1 vector of chi-square distributed
! random number with v degrees of freedom.
!=======================================================================

  real(kind = RPRE) function chi2rand0(v)
    integer(kind = IPRE), intent(in) :: v
    chi2rand0 = sum(randn(v)**2)
    return
  end function chi2rand0

  function chi2rand1(v, dim1)
    real(kind = RPRE), dimension(:), allocatable :: chi2rand1
    integer(kind = IPRE), intent(in) :: v, dim1
    chi2rand1 = sum(randn(dim1, v)**2, dim = 2)
    return
  end function chi2rand1

!=======================================================================
! chol
!-----------------------------------------------------------------------
! chol computes Cholesky's decomposition of a symmetric positive
! definite matrix.
!
! Syntax
!-----------------------------------------------------------------------
! L = chol(A)
!
! Description
!-----------------------------------------------------------------------
! L = chol(A) returns a lower triangular matrix L satisfying the
! equation A = L*Lt.
!=======================================================================

  function chol(A) result(L)
    real(kind = RPRE), dimension(:,:), allocatable :: L
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, k, n
    real(kind = RPRE) :: sum1, sum2
    real(kind = RPRE), dimension(:), allocatable :: d
    real(kind = RPRE), dimension(:,:), allocatable :: V

    call eig(A, V, d)
    if ( all(d .gt. 0.0d0 ) ) then
      n = size(A, 1)
      L = zeros(n, n)
      L(1,1) = sqrt(A(1,1))
      do i = 2, n
        L(i,1) = A(i,1) / L(1,1)
      end do

      do i = 2, n
        do k = 1, i
          sum1 = 0.0d0
          sum2 = 0.0d0
          do j = 1, k-1
            if ( i .eq. k ) then
              sum1 = sum1 + ( L(k,j) * L(k,j) )
              L(k,k) = sqrt(A(k,k) - sum1)
            elseif ( i .gt. k ) then
              sum2 = sum2 + ( L(i,j) * L(k,j) )
              L(i,k) = ( 1.0d0 / L(k,k) ) * ( A(i,k) - sum2 )
            else
              L(i,k) = 0.0d0
            end if
          end do
        end do
      end do
    else
      stop "Error: in chol(A), A should be positive definite."
    end if
    return
  end function chol

!=======================================================================
! close
!-----------------------------------------------------------------------
! close closes a File object.
!
! Syntax
!-----------------------------------------------------------------------
! call ofile%close()
!
! Description
!-----------------------------------------------------------------------
! call ofile%close() closes the File object ofile.
!=======================================================================

  subroutine close(self)
    class(File) :: self

    close(self%unit)
    return
  end subroutine close

!=======================================================================
! cosd
!-----------------------------------------------------------------------
! cosd computes the cosine of argument in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = cosd(x)
!
! Description
!-----------------------------------------------------------------------
! y = cosd(x) returns the cosine of the elements in x, which are
! expressed in degrees.
!
! Examples
!-----------------------------------------------------------------------
! y = cosd(0.)
!     1.
!
! x = [ 0., 90., 180., 270. ]
! y = cosd(x)
!     1.  0. -1.  0.
!=======================================================================

  real(kind = RPRE) function cosd0(x)
    real(kind = RPRE), intent(in) :: x

    cosd0 = cos(x*pi/180.0d0)
    return
  end function cosd0

  function cosd1(x)
    real(kind = RPRE), dimension(:), allocatable :: cosd1
    real(kind = RPRE), dimension(:), intent(in) :: x

    cosd1 = cos(x*pi/180.0d0)
    return
  end function cosd1

  function cosd2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: cosd2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    cosd2 = cos(A*pi/180.0d0)
    return
  end function cosd2

  function cosd3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: cosd3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    cosd3 = cos(X*pi/180.0d0)
    return
  end function cosd3

!=======================================================================
! countlines
!-----------------------------------------------------------------------
! countlines counts the number of lines in a txt file.
!
! Syntax
!-----------------------------------------------------------------------
! n = countlines(filename)
! n = ofile%countlines()
!
! Description
!-----------------------------------------------------------------------
! n = countlines(filename) returns the number of lines in the txt file
! filename.
!
! n = ofile%countlines() returns the number of lines in the txt file
! associated to the File object ofile.
!=======================================================================

  integer(kind = IPRE) function countlines1(self)
    class(File), intent(inout) :: self
    integer(kind = IPRE) :: ierr

    countlines1 = 0
    call self%open()
    do
      read(self%unit, *, iostat = ierr)
      if (ierr .lt. 0) exit
      countlines1 = countlines1 + 1
    end do
    call self%close()
    return
  end function countlines1

  integer(kind = IPRE) function countlines2(filename)
    character(len = *), intent(in) :: filename
    integer(kind = IPRE) :: ierr
    type(File) :: infile

    infile = File(999, trim(filename))
    countlines2 = 0
    call infile%open()
    do
      read(infile%unit, *, iostat = ierr)
      if (ierr .lt. 0) exit
      countlines2 = countlines2 + 1
    end do
    call infile%close()
    return
  end function countlines2

!=======================================================================
! cov
!-----------------------------------------------------------------------
! cov computes the covariance.
!
! Syntax
!-----------------------------------------------------------------------
! c = cov(x)
! c = cov(x, w)
! C = cov(X)
! C = cov(X, w)
! C = cov(x, y)
! C = cov(x, y, w)
! C = cov(X, Y)
! C = cov(X, Y, w)
!
! Description
!-----------------------------------------------------------------------
! c = cov(x) returns the covariance of vector x normalized by the number
! of observations minus 1.
!
! c = cov(x, w) returns the covariance of vector x with the
! normalization option w.
!   -   0 (default) normalize by N-1,
!   -   1 normalize by N.
!
! C = cov(X) returns the the covariance matrix with the corresponding
! column variances along the diagonal normalized by the number of
! observations minus 1.
!
! C = cov(X, w) returns the the covariance matrix with the corresponding
! column variances along the diagonal with the normalization option w.
!
! C = cov(x, y) returns the 2-by-2 covariance matrix normalized by the
! number of observations minus 1.
!
! C = cov(x, y, w) returns the 2-by-2 covariance matrix with the
! normalization option w.
!
! C = cov(X, Y) returns the the 2-by-2 covariance matrix normalized by
! the number of observations minus 1. X and Y are treated as column
! vectors.
!
! C = cov(X, Y, w) returns the the 2-by-2 covariance matrix with the
! normalisation option w. X and Y are treated as column vectors.
!=======================================================================

  real(kind = RPRE) function cov1_1(x, w)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w
    real(kind = RPRE), dimension(:), allocatable :: tmp

    opt_w = 0
    if (present(w)) opt_w = w

    tmp = x - mean(x)
    select case(opt_w)
      case(0)
        cov1_1 = dot_product(tmp, tmp) / (size(x) - 1)
      case(1)
        cov1_1 = dot_product(tmp, tmp) / size(x)
    end select
    return
  end function cov1_1

  function cov1_2(X, w)
    real(kind = RPRE), dimension(:,:), allocatable :: cov1_2
    real(kind = RPRE), dimension(:,:), intent(in) :: X
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w
    real(kind = RPRE), dimension(:,:), allocatable :: tmp

    opt_w = 0
    if (present(w)) opt_w = w

    tmp = X - repmat(mean(X, 1), size(X, 1), 2)
    select case(opt_w)
      case(0)
        cov1_2 = matmul(transpose(tmp), tmp) / (size(X, 1) - 1)
      case(1)
        cov1_2 = matmul(transpose(tmp), tmp) / size(X, 1)
    end select
    return
  end function cov1_2

  function cov2_1(x, y, w)
    real(kind = RPRE), dimension(:,:), allocatable :: cov2_1
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w
    real(kind = RPRE), dimension(:,:), allocatable :: tmp

    opt_w = 0
    if (present(w)) opt_w = w

    cov2_1 = cov1_2(horzcat(x, y), opt_w)
    return
  end function cov2_1

  function cov2_2(X, Y, w)
    real(kind = RPRE), dimension(:,:), allocatable :: cov2_2
    real(kind = RPRE), dimension(:,:), intent(in) :: X, Y
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w

    opt_w = 0
    if (present(w)) opt_w = w

    if ( all( shape(X) .eq. shape(Y) ) ) then
      cov2_2 = cov1_2(horzcat([ X ], [ Y ]), opt_w)
    else
      stop "Error: in cov(X, Y), X and Y should have the same shape."
    end if
    return
  end function cov2_2

!=======================================================================
! cumsum
!-----------------------------------------------------------------------
! cumsum computes the cumulative sum of a vector or array.
!
! Syntax
!-----------------------------------------------------------------------
! y = cumsum(x)
! B = cumsum(A)
! B = cumsum(A, dim)
!
! Description
!-----------------------------------------------------------------------
! y = cumsum(x) returns a vector containing the cumulative sum of the
! elements of A.
!
! B = cumsum(A) returns a matrix containing the cumulative sums for each
! column of A.
!
! B = cumsum(A) returns a matrix containing the cumulative sums for each
! elements A along the specified dimension dim.
!=======================================================================

  function cumsum1(x)
    real(kind = RPRE), dimension(:), allocatable :: cumsum1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, n
    real(kind = RPRE), dimension(:), allocatable :: xsort

    n = size(x)
    xsort = sort(x, 1)
    cumsum1 = [ ( sum(xsort(1:i)), i = 1, n ) ]
    return
  end function cumsum1

  function cumsum2(A, dim)
    real(kind = RPRE), dimension(:,:), allocatable :: cumsum2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: i, m, n

    m = size(A, 1)
    n = size(A, 2)
    cumsum2 = zeros(m, n)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      do i = 1, n
        cumsum2(:,i) = cumsum1(A(:,i))
      end do
    elseif (dim .eq. 2) then
      do i = 1, m
        cumsum2(i,:) = cumsum1(A(i,:))
      end do
    end if
    return
  end function cumsum2

!=======================================================================
! datenum
!-----------------------------------------------------------------------
! datenum converts the datetime values into serial date numbers (since
! 0000-01-01 00:00:00).
!
! Syntax
!-----------------------------------------------------------------------
! d = datenum(year, month, day)
! d = datenum(year, month, day, hour, minute, second)
! d = datenum(year, month, day, hour, minute, second, microsecond)
!
! Description
!-----------------------------------------------------------------------
! d = datenum(year, month, day) returns an integer serial date number
! given by year, month and day.
!
! d = datenum(year, month, day, hour, minute, second) returns a floating
! serial date number given by year, month, day, hour, minute and second.
!
! d = datenum(year, month, day, hour, minute, second, microsecond)
! returns a floating serial date number given by year, month, day, hour,
! minute, second and microsecond.
!
! Examples
!-----------------------------------------------------------------------
! d = datenum(2016, 1, 1)
!     736330.
!
! d = datenum(2016, 1, 17, 22, 28, 30, 250000)
!     736346.93646122678
!
! Notes
!-----------------------------------------------------------------------
! Use double precision for accuracy.
!=======================================================================

  real(kind = 8) function datenum0(year, month, day, hour, minute, &
                                      second, microsecond)
    integer(kind = IPRE), intent(in) :: year, month, day
    integer(kind = IPRE), intent(in), optional :: hour, minute, second, microsecond
    integer(kind = IPRE) :: i, days_per_month(12)

    if ((month .lt. 1) .and. (month .gt. 12)) then
      print *, "Error: month should be between 1 and 12 (" // num2str(month) // ")."
    end if
    if ((day .lt. 1) .and. (day .gt. 31)) then
      print *, "Error: day should be between 1 and 31 (" // num2str(day) // ")."
    end if
    if ((present(hour)) .and. (hour .lt. 0) .and. (hour .gt. 23)) then
      print *, "Error: hour should be between 0 and 23 (" // num2str(hour) // ")."
    end if
    if ((present(minute)) .and. (minute .lt. 0) .and. (minute .gt. 59)) then
      print *, "Error: minute should be between 0 and 59 (" // num2str(minute) // ")."
    end if
    if ((present(second)) .and. (second .lt. 0) .and. (second .gt. 59)) then
      print *, "Error: second should be between 0 and 59 (" // num2str(second) // ")."
    end if
    if ((present(microsecond)) .and. (microsecond .lt. 0) .and. (microsecond .ge. 1.0d+6)) then
      print *, "Error: microsecond should be between 0 and 999,999 (" // num2str(microsecond) // ")."
    end if
    days_per_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
    datenum0 = 0
    do i = 0, year-1
      if (isleap(i)) then
        datenum0 = datenum0 + 366
      else
        datenum0 = datenum0 + 365
      end if
    end do
    datenum0 = datenum0 + sum(days_per_month(:month-1))
    if (isleap(year) .and. (month .gt. 2)) datenum0 = datenum0 + 1
    datenum0 = datenum0 + day
    if (present(hour)) datenum0 = datenum0 + real(hour, kind = 8) / 24.0d0
    if (present(minute)) datenum0 = datenum0 + real(minute, kind = 8) / (24.0d0*60.0d0)
    if (present(second)) datenum0 = datenum0 + real(second, kind = 8) / (24.0d0*60.0d0*60.0d0)
    if (present(microsecond)) datenum0 = datenum0 + real(microsecond, kind = 8) / (24.0d0*60.0d0*60.0d0*1.0d+6)
    return
  end function datenum0

!=======================================================================
! datestr
!-----------------------------------------------------------------------
! datestr creates a string of a datetime.
!
! Syntax
!-----------------------------------------------------------------------
! dstr = datestr(t)
!
! Description
!-----------------------------------------------------------------------
! dstr = datestr(t) returns a string of the serial date number t.
!
! Examples
!-----------------------------------------------------------------------
! t = datenum([ 1991, 9, 14, 16, 5, 0, 0])
! dstr = datestr(t)
!     14-Sep-1991 16:04:59:999998
!
! Notes
!-----------------------------------------------------------------------
! Output string may slightly differ from the input date vector in
! datenum due to accuracy in the calculation of the serial date number.
!=======================================================================

  function datestr0_0(t)
    character(len = :), allocatable :: datestr0_0
    real(kind = 8), intent(in) :: t
    integer(kind = IPRE) :: d(7)
    character(len = CLEN) :: dstr
    character(len = 3) :: months_in_letters(12)

    d = datevec(t)

    ! Day
    !=====
    if (d(3) .lt. 10) then
      dstr = "0" // num2str(d(3)) // "-"
    else
      dstr = num2str(d(3)) // "-"
    end if

    ! Month
    !=======
    months_in_letters = [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ]
    dstr = trim(dstr) // months_in_letters(d(2)) // "-"

    ! Year
    !======
    dstr = trim(dstr) // num2str(d(1))

    if ((d(4) .eq. 0) .and. (d(5) .eq. 0) &
        .and. (d(6) .eq. 0) .and. (d(7) .eq. 0)) then
      datestr0_0 = trim(dstr)
      return

    else

      ! Hour
      !======
      if (d(4) .lt. 10) then
        dstr = trim(dstr) // " " // "0" // num2str(d(4)) // ":"
      else
        dstr = trim(dstr) // " " // num2str(d(4)) // ":"
      end if

      ! Minute
      !========
      if (d(5) .lt. 10) then
        dstr = trim(dstr) // "0" // num2str(d(5)) // ":"
      else
        dstr = trim(dstr) // num2str(d(5)) // ":"
      end if

      ! Second
      !========
      if (d(6) .lt. 10) then
        dstr = trim(dstr) // "0" // num2str(d(6)) // "."
      else
        dstr = trim(dstr) // num2str(d(6)) // "."
      end if

      ! Microsecond
      !=============
      if (d(7) .lt. 10) then
        dstr = trim(dstr) // "00000" // num2str(d(7))
      elseif (d(7) .lt. 100) then
        dstr = trim(dstr) // "0000" // num2str(d(7))
      elseif (d(7) .lt. 1000) then
        dstr = trim(dstr) // "000" // num2str(d(7))
      elseif (d(7) .lt. 10000) then
        dstr = trim(dstr) // "00" // num2str(d(7))
      elseif (d(7) .lt. 100000) then
        dstr = trim(dstr) // "0" // num2str(d(7))
      else
        dstr = trim(dstr) // num2str(d(7))
      end if

      datestr0_0 = trim(dstr)
    end if
    return
  end function datestr0_0

!=======================================================================
! datevec
!-----------------------------------------------------------------------
! datevec converts date and time to vector of components.
!
! Syntax
!-----------------------------------------------------------------------
! d = datevec(t)
!
! Description
!-----------------------------------------------------------------------
! d = datevec(t) returns a 7-elements datetime vector given the serial
! date number t.
!
! Examples
!-----------------------------------------------------------------------
! d = datevec(727455.67013888888d0)
!     1991   9   14   16   4   59   999998
!
! Notes
!-----------------------------------------------------------------------
! Input is in double precision for accuracy.
!=======================================================================

  function datevec0(t)
    integer(kind = IPRE) :: datevec0(7)
    real(kind = 8), intent(in) :: t
    integer(kind = IPRE) :: i, days_per_month(12)
    real(kind = 8) :: tmp, dateres

    datevec0 = 0

    ! Year
    !======
    tmp = 0.0d0
    do
      if (isleap(datevec0(1))) then
        tmp = tmp + 366.0d0
      else
        tmp = tmp + 365.0d0
      end if
      if (tmp .lt. floor(t)) then
        datevec0(1) = datevec0(1) + 1
      else
        exit
      end if
    end do
    dateres = floor(t) - datenum(datevec0(1), 1, 1)

    ! Month
    !=======
    tmp = 0.0d0
    days_per_month = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
    do i = 1, 12
      tmp = tmp + days_per_month(i)
      if ((isleap(datevec0(1))) .and. (i .eq. 2)) tmp = tmp + 1
      if (tmp .ge. dateres) then
        datevec0(2) = i
        exit
      end if
    end do
    dateres = floor(t) - datenum(datevec0(1), datevec0(2), 1)

    ! Day
    !=====
    datevec0(3) = floor(dateres) + 1
    dateres = t - floor(t)

    ! Hour
    !======
    datevec0(4) = floor(dateres * 24.0d0)
    dateres = dateres - datevec0(4) / 24.0d0

    ! Minute
    !========
    datevec0(5) = floor(dateres * 24.0d0 * 60.0d0)
    dateres = dateres - datevec0(5) / (24.0d0 * 60.0d0)

    ! Second
    !========
    datevec0(6) = floor(dateres * 24.0d0 * 60.0d0 * 60.0d0)
    dateres = dateres - datevec0(6) / (24.0d0 * 60.0d0 * 60.0d0)

    ! Microsecond
    !=============
    datevec0(7) = floor(dateres * 24.0d0 * 60.0d0 * 60.0d0 * 1.0d+6)

    return
  end function datevec0

!=======================================================================
! dbindex
!-----------------------------------------------------------------------
! dbindex computes the Davies-Bouldin index for evaluating clutering
! algorithms.
!
! Syntax
!-----------------------------------------------------------------------
! db = dbindex(x, cluster, means)
! db = dbindex(x, cluster, means, p, q)
! db = dbindex(X, cluster, means)
! db = dbindex(X, cluster, means, p, q)
!
! Description
!-----------------------------------------------------------------------
! db = dbindex(x, cluster, means) returns the Davies-Bouldin index using
! the Euclidian distance.
!
! db = dbindex(x, cluster, means, p, q) returns the Davies-Bouldin index
! using the metric defined by p and q.
!
! db = dbindex(X, cluster, means) returns the Davies-Bouldin index using
! the Euclidian distance, each row of array X being an observation and
! each column a parameter.
!
! db = dbindex(X, cluster, means, p, q) returns the Davies-Bouldin index
! using the metric defined by p and q, each row of array X being an
! observation and each column a parameter.
!
! Notes
!-----------------------------------------------------------------------
! After Davies D. L. and Bouldin D. W. (1979): "A Cluster Separation
! Measure".
!=======================================================================

  real(kind = RPRE) function dbindex1(x, cluster, means, p, q) result(db)
    real(kind = RPRE), dimension(:), intent(in) :: x, means
    integer(kind = IPRE), dimension(:), intent(in) :: cluster
    real(kind = RPRE), intent(in), optional :: p, q
    integer(kind = IPRE) :: K, n
    real(kind = RPRE) :: opt_p, opt_q
    real(kind = RPRE), dimension(:,:), allocatable :: A, mu

    K = size(means)
    if ( K .eq. 1 ) then
      print *, "Warning: in dbindex, the Davies-Bouldin index cannot " &
        // "be defined for K = 1."
      db = 1.0d0
      return
    end if

    opt_p = 2.
    opt_q = 2.
    if (present(p)) opt_p = p
    if (present(q)) opt_q = q

    n = size(x)
    A = reshape( x, shape = [ n, 1 ], order = [ 1, 2 ] )
    mu = reshape( x, shape = [ K, 1 ], order = [ 1, 2 ] )
    db = dbindex2(A, cluster, mu, opt_p, opt_q)
    return
  end function dbindex1

  real(kind = RPRE) function dbindex2(X, cluster, means, p, q) result(db)
    real(kind = RPRE), dimension(:,:), intent(in) :: X
    integer(kind = IPRE), dimension(:), intent(in) :: cluster
    real(kind = RPRE), dimension(:,:), intent(in) :: means
    real(kind = RPRE), intent(in), optional :: p, q
    integer(kind = IPRE) :: i, j, K
    real(kind = RPRE) :: opt_p, opt_q, Mij
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:), allocatable :: S
    real(kind = RPRE), dimension(:,:), allocatable :: R

    K = size(means, 1)
    if ( K .eq. 1 ) then
      print *, "Warning: in dbindex, the Davies-Bouldin index cannot " &
        // "be defined for K = 1."
      db = 1.0d0
      return
    end if

    opt_p = 2.
    opt_q = 2.
    if (present(p)) opt_p = p
    if (present(q)) opt_q = q

    ! Measure the scattering within each cluster
    !============================================
    S = zeros(K)
    do i = 1, K
      idx = find( cluster .eq. i )
      do j = 1, size(idx)
        S(i) = S(i) + norm(X(idx(j),:) - means(i,:), opt_q)**2
      end do
      S(i) = sqrt( S(i) / real(size(idx), RPRE) )
    end do

    ! Measure the similarity function R between each cluster
    !========================================================
    R = zeros(K, K)
    do i = 1, K-1
      do j = i+1, K
        Mij = norm(means(i,:) - means(j,:), opt_p)  ! Distance between clusters i and j
        R(i,j) = ( S(i) + S(j) ) / Mij
        R(j,i) = R(i,j)
      end do
    end do

    ! Compute the Davies-Bouldin index
    !==================================
    db = mean( [ ( maxval(R(i,:)), i = 1, K ) ] )

    return
  end function dbindex2

!=======================================================================
! deboor
!-----------------------------------------------------------------------
! deboor evaluates recursively the spline polynomial basis using
! Cox-de-Boor algorithm.
!
! Syntax
!-----------------------------------------------------------------------
! db = deboor(i, k, x, t)
!
! Description
!-----------------------------------------------------------------------
! db = deboor(i, k, x, t) returns the polynomial basis given the control
! point index i, the polynomial order k (degree k-1), the evaluated
! point x, and the knots vector t.
!=======================================================================

  recursive function deboor(i, k, x, t) result(db)
    real(kind = RPRE) :: db
    integer(kind = IPRE), intent(in) :: i, k
    real(kind = RPRE), intent(in) :: x
    real(kind = RPRE), dimension(:), intent(in) :: t
    real(kind = RPRE) :: A1, A2

    if (k .eq. 1) then
      if (x .ne. t(size(t))) then
        if ( (x .ge. t(i)) .and. (x .lt. t(i+1)) ) then
          db = 1.0d0
        else
          db = 0.0d0
        end if
      else
        if ( (x .ge. t(i)) .and. (x .le. t(i+1)) ) then
          db = 1.0d0
        else
          db = 0.0d0
        end if
      end if
    else
      if (t(i+k-1) - t(i) .ne. 0.0d0) then
        A1 = (x - t(i)) / (t(i+k-1) - t(i))
      else
        A1 = 0.0d0
      end if
      if (t(i+k) - t(i+1) .ne. 0.0d0) then
        A2 = (t(i+k) - x) / (t(i+k) - t(i+1))
      else
        A2 = 0.0d0
      end if
      db = A1 * deboor(i, k-1, x, t) + A2 * deboor(i+1, k-1, x, t)
    end if
    return
  end function deboor

!=======================================================================
! deg2utm
!-----------------------------------------------------------------------
! deg2utm converts latitude / longitude (in degrees) coordinates to
! UTM-WGS84 coordinates.
!
! Syntax
!-----------------------------------------------------------------------
! call deg2utm(lat, lon, east, north, zn, zl)
!
! Description
!-----------------------------------------------------------------------
! call deg2utm(lat, lon, east, north, zn, zl) converts the coordinates
! in lat and lon (in degrees) to UTM-WGS84 coordinates. It outputs the
! easting east, northing north, zone number zn, and zone letter zl.
!
! Notes
!-----------------------------------------------------------------------
! This function has been translated and adapted from the Python's
! module utm: https://pypi.python.org/pypi/utm
!=======================================================================

  subroutine deg2utm0(lat, lon, east, north, zn, zl)
    real(kind = RPRE), intent(in) :: lat, lon
    real(kind = RPRE), intent(out) :: east, north
    integer(kind = IPRE), intent(out) :: zn
    character(len = 1), intent(out) :: zl

    real(kind = 8), parameter :: K0 = 0.9996d0
    real(kind = 8), parameter :: E = 0.00669438d0
    real(kind = 8), parameter :: R = 6378137
    real(kind = 8) :: E_P2, m, n, c, a
    real(kind = 8) :: M1, M2, M3, M4
    real(kind = 8) :: lat_rad, lat_sin, lat_cos, lat_tan, lat_tan2, &
      lat_tan4, lon_rad, central_lon, central_lon_rad

    E_P2 = E / (1.0d0 - E)
    M1 = (1 - E / 4 - 3 * E**2 / 64 - 5 * E**3 / 256)
    M2 = (3 * E / 8 + 3 * E**2 / 32 + 45 * E**3 / 1024)
    M3 = (15 * E**2 / 256 + 45 * E**3 / 1024)
    M4 = (35 * E**3 / 3072)

    lat_rad = lat*pi/180.0d0
    lat_sin = sin(lat_rad)
    lat_cos = cos(lat_rad)
    lat_tan = lat_sin / lat_cos
    lat_tan2 = lat_tan**2
    lat_tan4 = lat_tan2**2

    zn = zone_number(lat, lon)
    zl = zone_letter(lat)

    lon_rad = lon*pi/180.0d0
    central_lon = central_longitude(zn)
    central_lon_rad = central_lon*pi/180.0d0

    n = R / sqrt(1 - E * lat_sin**2)
    c = E_P2 * lat_cos**2
    a = lat_cos * (lon_rad - central_lon_rad)
    m = R * (M1 * lat_rad &
          -  M2 * sin(2 * lat_rad) &
          +  M3 * sin(4 * lat_rad) &
          -  M4 * sin(6 * lat_rad))
    east = K0 * n * (a + &
                     a**3 / 6 * (1 - lat_tan2 + c) + &
                     a**5 / 120 * (5 - 18 * lat_tan2 + lat_tan4 + 72 * c - 58 * E_P2)) + 500000
    north = K0 * (m + n * lat_tan * (a**2 / 2 + &
                                     a**4 / 24 * (5 - lat_tan2 + 9 * c + 4 * c**2) + &
                                     a**6 / 720 * (61 - 58 * lat_tan2 + lat_tan4 + 600 * c - 330 * E_P2)))
    if (lat .lt. 0.0d0) north = north + 10000000
    return

  contains

    !-------------------------------------------------------------------
    ! zone_number
    !-------------------------------------------------------------------
    integer(kind = IPRE) function zone_number(lat, lon)
      real(kind = RPRE), intent(in) :: lat, lon
      if ((lat .ge. 56.0d0) .and. (lat .le. 64.0d0) &
          .and. (lon .ge. 3.0d0) .and. (lon .le. 12.0d0)) then
        zone_number = 32
        return
      end if

      if ((lat .ge. 72.0d0) .and. (lat .le. 84.0d0) &
          .and. (lon .ge. 0.0d0)) then
        if (lon .le. 9.0d0) then
          zone_number = 31
          return
        elseif (lon .le. 21.0d0) then
          zone_number = 33
          return
        elseif (lon .le. 42.0d0) then
          zone_number = 37
          return
        end if
      end if

      zone_number = int((lon + 180.0d0) / 6.0d0) + 1
      return
    end function zone_number

    !-------------------------------------------------------------------
    ! zone_letter
    !-------------------------------------------------------------------
    character(len = 1) function zone_letter(lat)
      real(kind = RPRE), intent(in) :: lat
      character(len = *), parameter :: ZONE_LETTERS = "OXWVUTSRQPNMLKJHGFEDC"
      integer(kind = IPRE) :: ZONE_LATS(21) = [ 84, 72, 64, 56, 48, 40, 32, 24, 16, 8, 0, &
                                                -8, -16, -24, -32, -40, -48, -56, -64, -72, -80 ]
      integer(kind = IPRE) :: i
      do i = 1, 21
        if (lat .ge. ZONE_LATS(i)) then
          zone_letter = ZONE_LETTERS(i:i)
          exit
        end if
      end do
      return
    end function zone_letter

    !-------------------------------------------------------------------
    ! central_longitude
    !-------------------------------------------------------------------
    real(kind = RPRE) function central_longitude(zn)
      integer(kind = IPRE), intent(in) :: zn
      central_longitude = (zn - 1) * 6 - 180 + 3
      return
    end function central_longitude

  end subroutine deg2utm0

  subroutine deg2utm1(lat, lon, east, north, zn, zl)
    real(kind = RPRE), dimension(:), intent(in) :: lat, lon
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: east, north
    integer(kind = IPRE), dimension(:), allocatable, intent(out) :: zn
    character(len = 1), dimension(:), allocatable, intent(out) :: zl
    integer(kind = IPRE) :: i, n

    n = size(lat)
    allocate(east(n), north(n), zn(n), zl(n))
    do i = 1, n
      call deg2utm(lat(i), lon(i), east(i), north(i), zn(i), zl(i))
    end do
    return
  end subroutine deg2utm1

!=======================================================================
! det
!-----------------------------------------------------------------------
! det computes the matrix determinant.
!
! Syntax
!-----------------------------------------------------------------------
! x = det(A)
! x = det(A, L, U)
!
! Description
!-----------------------------------------------------------------------
! x = det(A) returns the determinant of the square matrix A, as the
! product of the diagonal elements of the upper triangular matrix from
! the LU factorization of A.
!
! x = det(A, L, U) returns the determinant of the square matrix A and
! outputs the LU factorization matrices of A used for the calculation.
!
! Examples
!-----------------------------------------------------------------------
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 0. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! x = det(A)
!     27.
!=======================================================================

  real(kind = RPRE) function det(A, outL, outU)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:,:), allocatable, intent(inout), optional :: outL, outU
    real(kind = RPRE), dimension(:,:), allocatable :: L, U
    integer(kind = IPRE) :: m

    if (issquare(A)) then
      m = size(A, 1)
      if (m .eq. 2) then
        det = A(1,1)*A(2,2) - A(1,2)*A(2,1)
      elseif (m .eq. 3) then
        det = A(1,1)*A(2,2)*A(3,3) &
              + A(2,1)*A(3,2)*A(1,3) &
              + A(3,1)*A(1,2)*A(2,3) &
              - A(1,1)*A(3,2)*A(2,3) &
              - A(3,1)*A(2,2)*A(1,3) &
              - A(2,1)*A(1,2)*A(3,3)
      else
        call lu(A, L, U)
        det = product(diag(U))
        if (present(outL)) outL = L
        if (present(outU)) outU = U
      end if
    else
      stop "Error: in det(A), A should be square."
    end if
    return
  end function det

!=======================================================================
! diag
!-----------------------------------------------------------------------
! diag creates diagonal matrix or get the diagonal of a matrix.
!
! Syntax
!-----------------------------------------------------------------------
! x = diag(A)
! A = diag(x)
!
! Description
!-----------------------------------------------------------------------
! x = diag(A) returns the main diagonal of matrix A.
!
! A = diag(x) returns a square diagonal matrix with the elements of x on
! the main diagonal.
!
! Examples
!-----------------------------------------------------------------------
! A = eye(3)
! x = diag(A)
!     1.  1.  1.
!
! x = [ 1., 2., 3. ]
! A = diag(x)
!     1.  0.  0.
!     0.  2.  0.
!     0.  0.  3.
!=======================================================================

  function diag1(A)
    real(kind = RPRE), dimension(:), allocatable :: diag1
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, n

    n = min(size(A, 1), size(A, 2))
    allocate(diag1(n))
    do i = 1, n
      diag1(i) = A(i,i)
    end do
    return
  end function diag1

  function diag2(x)
    real(kind = RPRE), dimension(:,:), allocatable :: diag2
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, n

    n = size(x)
    diag2 = zeros(n, n)
    do i = 1, n
      diag2(i,i) = x(i)
    end do
    return
  end function diag2

!=======================================================================
! diff
!-----------------------------------------------------------------------
! diff computes differences of arrays
!
! Syntax
!-----------------------------------------------------------------------
! y = diff(x)
! y = diff(x, n)
! B = diff(A)
! B = diff(A, n)
! B = diff(A, dim)
! B = diff(A, n, dim)
!
! Description
!-----------------------------------------------------------------------
! y = diff(x) returns differences between adjacent elements of vector x.
!
! y = diff(x, n) returns the nth difference by applying the diff(x)
! operator recursively n times.
!
! B = diff(A) returns differences between adjacent elements of array A
! along the first dimension.
!
! B = diff(A, n) returns the nth difference by applying the diff(A)
! operator recursively n times.
!
! B = diff(A, dim) returns differences between adjacent elements of
! array A along the dimension given by dim.
!
! B = diff(A, n, dim) returns the nth difference along the dimension
! given by dim by applying the diff(A, dim) operator recursively
! n times.
!=======================================================================

  function diff1(x, n)
    real(kind = RPRE), dimension(:), allocatable :: diff1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: n
    integer(kind = IPRE) :: opt_n, i

    opt_n = 1
    if (present(n)) opt_n = n

    diff1 = x
    do i = 1, opt_n
      diff1 = diff1(2:) - diff1(:size(diff1)-1)
    end do
    return
  end function diff1

  function diff2(A, n, dim)
    real(kind = RPRE), dimension(:,:), allocatable :: diff2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: n, dim
    integer(kind = IPRE) :: opt_n, i

    opt_n = 1
    if (present(n)) opt_n = n

    diff2 = A
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      do i = 1, opt_n
        diff2 = diff2(2:,:) - diff2(:size(diff2,1)-1,:)
      end do
    elseif (dim .eq. 2) then
      do i = 1, opt_n
        diff2 = diff2(:,2:) - diff2(:,:size(diff2,2)-1)
      end do
    end if
    return
  end function diff2

!=======================================================================
! disp
!-----------------------------------------------------------------------
! disp displays the value of a variable.
!
! Syntax
!-----------------------------------------------------------------------
! call disp(x)
! call disp(x, string)
! call disp(A)
! call disp(A, string)
! call disp(X)
! call disp(X, 1)
! call disp(X, 1, string)
! call disp(X, 2)
! call disp(X, 2, string)
! call disp(X, 3)
! call disp(X, 3, string)
!
! Description
!-----------------------------------------------------------------------
! call disp(x) displays the scalar or the vector x.
!
! call disp(x, string) displays the scalar or the vector x preceded by
! string.
!
! call disp(A) displays the matrix A.
!
! call disp(A, string) displays the matrix A preceded by string.
!
! call disp(X) displays the 3-dimensional matrix X along the axis 1.
!
! call disp(X, 1) (see call disp(X)).
!
! call disp(X, 1, string) displays the 3-dimensional matrix X along the
! axis 1 preceded by string.
!
! call disp(X, 2) displays the 3-dimensional matrix X along the axis 2.
!
! call disp(X, 2, string) displays the 3-dimensional matrix X along the
! axis 2 preceded by string.
!
! call disp(X, 3) displays the 3-dimensional matrix X along the axis 3.
!
! call disp(X, 3, string) displays the 3-dimensional matrix X along the
! axis 3 preceded by string.
!
! Examples
!-----------------------------------------------------------------------
! x = [ 1. 2. 3. ]
! call disp(x)
!     1.
!     2.
!     3.
! call disp(x, "x = ")
!     x =
!         1.
!         2.
!         3.
!
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! call disp(A, "Matrix A is")
!     Matrix A is
!         1.  2.  3.
!         4.  5.  6.
!         7.  8.  9.
!=======================================================================

  subroutine disp_i0(x, string)
    integer(kind = IPRE), intent(in) :: x
    character(len = *), intent(in), optional :: string

    if (present(string)) print *, trim(string)
    print *, x
    return
  end subroutine disp_i0

  subroutine disp_r0(x, string)
    real(kind = RPRE), intent(in) :: x
    character(len = *), intent(in), optional :: string

    if (present(string)) print *, trim(string)
    print *, x
    return
  end subroutine disp_r0

  subroutine disp_c0(x, string)
    complex(kind = RPRE), intent(in) :: x
    character(len = *), intent(in), optional :: string

    if (present(string)) print *, trim(string)
    if (imag(x) .ge. 0.0d0) then
      print *, num2str(real(x)) // " + " // num2str(abs(imag(x))) // "i"
    else
      print *, num2str(real(x)) // " - " // num2str(abs(imag(x))) // "i"
    end if
    return
  end subroutine disp_c0

  subroutine disp_i1(x, string)
    integer(kind = IPRE), dimension(:), intent(in) :: x
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, m

    m = size(x)
    if (present(string)) print *, trim(string)
    do i = 1, m
      print *, x(i)
    end do
    return
  end subroutine disp_i1

  subroutine disp_r1(x, string)
    real(kind = RPRE), dimension(:), intent(in) :: x
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, m

    m = size(x)
    if (present(string)) print *, trim(string)
    do i = 1, m
      print *, x(i)
    end do
    return
  end subroutine disp_r1

  subroutine disp_c1(x, string)
    complex(kind = RPRE), dimension(:), intent(in) :: x
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, m

    m = size(x)
    if (present(string)) print *, trim(string)
    do i = 1, m
      call disp_c0(x(i))
    end do
    return
  end subroutine disp_c1

  subroutine disp_i2(A, string)
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, j, m, n

    m = size(A, 1)
    n = size(A, 2)
    if (present(string)) print *, trim(string)
    do i = 1, m
      print *, (A(i,j), j = 1, n)
    end do
    return
  end subroutine disp_i2

  subroutine disp_r2(A, string)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, j, m, n

    m = size(A, 1)
    n = size(A, 2)
    if (present(string)) print *, trim(string)
    do i = 1, m
      print *, (A(i,j), j = 1, n)
    end do
    return
  end subroutine disp_r2

  subroutine disp_i3(X, dim, string)
    integer(kind = IPRE), dimension(:,:,:), intent(in) :: X
    integer(kind = IPRE), intent(in), optional :: dim
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, dim1, dim2, dim3

    dim1 = size(X, 1)
    dim2 = size(X, 2)
    dim3 = size(X, 3)
    if (present(string)) print *, trim(string)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      do i = 1, dim1
        print *, "Slice (" // num2str(i) // ",:,:):"
        call disp(X(i,:,:))
      end do
    elseif (dim .eq. 2) then
      do i = 1, dim2
        print *, "Slice (:," // num2str(i) // ",:):"
        call disp(X(:,i,:))
      end do
    elseif (dim .eq. 3) then
      do i = 1, dim3
        print * , "Slice (:,:," // num2str(i) // "):"
        call disp(X(:,:,i))
      end do
    end if
    return
  end subroutine disp_i3

  subroutine disp_r3(X, dim, string)
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X
    integer(kind = IPRE), intent(in), optional :: dim
    character(len = *), intent(in), optional :: string
    integer(kind = IPRE) :: i, dim1, dim2, dim3

    dim1 = size(X, 1)
    dim2 = size(X, 2)
    dim3 = size(X, 3)
    if (present(string)) print *, trim(string)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      do i = 1, dim1
        print *, "Slice (" // num2str(i) // ",:,:):"
        call disp(X(i,:,:))
      end do
    elseif (dim .eq. 2) then
      do i = 1, dim2
        print *, "Slice (:," // num2str(i) // ",:):"
        call disp(X(:,i,:))
      end do
    elseif (dim .eq. 3) then
      do i = 1, dim3
        print * , "Slice (:,:," // num2str(i) // "):"
        call disp(X(:,:,i))
      end do
    end if
    return
  end subroutine disp_r3

!=======================================================================
! eig
!-----------------------------------------------------------------------
! eig computes eigenvalues and eigenvectors of symmetric matrix using
! Jacobi algorithm.
!
! Syntax
!-----------------------------------------------------------------------
! call eig(A, V, d)
! call eig(A, V, d, itermax)
!
! Description
!-----------------------------------------------------------------------
! call eig(A, V, d) returns the eigenvalues of the symmetric matrix A
! in the vector d and the associated eigenvectors in the matrix V.
!
! call eig(A, V, d) returns eigenvalues and eigenvectors with a maximum
! of itermax iterations.
!=======================================================================

  subroutine eig(A, V, d, itermax)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: V
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: d
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE) :: opt_itermax, iter, i, j, k, n
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE) :: threshold, gapj, termi, termj, h, term, t, &
      theta, c, s, tau, g
    real(kind = RPRE), dimension(:), allocatable :: bw, zw
    real(kind = RPRE), dimension(:,:), allocatable :: B

    opt_itermax = 1000
    if (present(itermax)) opt_itermax = itermax

    if (.not. issymmetric(A)) then
      stop "Error: in eig(A), A is not symmetric."
    else
      if (allocated(V)) deallocate(V)
      if (allocated(d)) deallocate(d)

      B = A
      n = size(B, 1)
      V = eye(n)
      d = diag(B)
      bw = d
      zw = zeros(n)

      iter = 0
      do while (iter .lt. opt_itermax)
        iter = iter + 1

        threshold = sqrt(sum(triu(B, 1)**2)) / (4.0*n)
        if (threshold .eq. 0.0d0) exit

        do i = 1, n
          do j = i+1, n
            gapj = 10.0d0 * abs(B(i,j))
            termi = gapj + abs(d(i))
            termj = gapj + abs(d(j))

            if ( ( iter .gt. 4 ) .and. ( termi .eq. abs(d(i)) ) &
                 .and. ( termj .eq. abs(d(j)) ) ) then
              B(i,j) = 0.0d0
            elseif ( threshold .le. abs(B(i,j)) ) then
              h = d(j) - d(i)
              term = abs(h) + gapj

              if ( term .eq. abs(h) ) then
                t = B(i,j) / h
              else
                theta = 0.5d0 * h / B(i,j)
                t = 1.0d0 / ( abs(theta) + sqrt(1.0d0 + theta*theta) )
                if (theta .lt. 0.0d0) t = -t
              end if

              c = 1.0d0 / sqrt(1.0d0 + t*t)
              s = t * c
              tau = s / (1.0d0 + c)
              h = t * B(i,j)

              zw(i) = zw(i) - h
              zw(j) = zw(j) + h
              d(i) = d(i) - h
              d(j) = d(j) + h
              B(i,j) = 0.0d0

              do k = 1, i-1
                g = B(k,i)
                h = B(k,j)
                B(k,i) = g - s * (h + g * tau)
                B(k,j) = h + s * (g - h * tau)
              end do

              do k = i+1, j-1
                g = B(i,k)
                h = B(k,j)
                B(i,k) = g - s * (h + g * tau)
                B(k,j) = h + s * (g - h * tau)
              end do

              do k = j+1, n
                g = B(i,k)
                h = B(j,k)
                B(i,k) = g - s * (h + g * tau)
                B(j,k) = h + s * (g - h * tau)
              end do

              do k = 1, n
                g = V(k,i)
                h = V(k,j)
                v(k,i) = g - s * (h + g * tau)
                v(k,j) = h + s * (g - h * tau)
              end do

            end if
          end do
        end do

        bw = bw + zw
        d = bw
        zw = 0.0d0
      end do

      idx = argsort(d, 1)
      d = d(idx)
      V = V(:,idx)
    end if

    return
  end subroutine eig

!=======================================================================
! eye
!-----------------------------------------------------------------------
! eye creates the identity matrix.
!
! Syntax
!-----------------------------------------------------------------------
! I = eye(dim1)
! I = eye(dim1, dim2)
!
! Description
!-----------------------------------------------------------------------
! I = eye(dim1) returns an dim1-by-dim1 matrix with ones on the main
! diagonal and zeros elsewhere.
!
! I = eye(dim1, dim2) returns a dim1-by-dim2 matrix with ones on the
! main diagonal and zeros elsewhere.
!
! Examples
!-----------------------------------------------------------------------
! I = eye(3)
!     1.  0.  0.
!     0.  1.  0.
!     0.  0.  1.
!
! I = eye(3, 4)
!     1.  0.  0.  0.
!     0.  1.  0.  0.
!     0.  0.  1.  0.
!
! I = eye(4, 3)
!     1.  0.  0.
!     0.  1.  0.
!     0.  0.  1.
!     0.  0.  0.
!=======================================================================

  function eye(dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: eye
    integer(kind = IPRE), intent(in) :: dim1
    integer(kind = IPRE), intent(in), optional :: dim2
    integer(kind = IPRE) :: i

    if (.not. present(dim2)) then
      eye = zeros(dim1, dim1)
      do i = 1, dim1
        eye(i,i) = 1.0d0
      end do
    else
      eye = zeros(dim1, dim2)
      do i = 1, min(dim1, dim2)
        eye(i,i) = 1.0d0
      end do
    end if
    return
  end function eye

!=======================================================================
! File (constructor)
!-----------------------------------------------------------------------
! File constructs a File object.
!
! Syntax
!-----------------------------------------------------------------------
! ofile = File(unit, filename)
!
! Description
!-----------------------------------------------------------------------
! ofile = File(unit, filename) returns a File object associated to the
! file filename with the identifier unit.
!
! Examples
!-----------------------------------------------------------------------
! type(File) :: ofile
!
! ofile = File(10, "myfile.txt")
! call ofile%open()
! ! ... some operations on this file ...
! call ofile%close()
!=======================================================================

  type(File) function init_File(unit, filename)
    integer(kind = IPRE), intent(in) :: unit
    character(len = *), intent(in) :: filename

    init_File%unit = unit
    init_File%filename = trim(filename)
    return
  end function init_File

!=======================================================================
! find
!-----------------------------------------------------------------------
! find finds indices of values in arrays that satisfy input condition.
!
! Syntax
!-----------------------------------------------------------------------
! x = find(condition)
! A = find(condition)
! X = find(condition)
!
! Description
!-----------------------------------------------------------------------
! x = find(condition) returns a vector with the indices of the values
! that satisfy condition.
!
! A = find(condition) returns a 2-columns array with the indices of the
! values that satisfy condition.
!
! X = find(condition) returns a 3-columns array with the indices of the
! values that satisfy condition.
!
! Examples
!-----------------------------------------------------------------------
! x = [ 2., 1., 8., 7., 4., 6., 9., 3., 5. ]
! y = find(x .ge. 5.)
!     3   4   6   7   9
!
! A = diag([1., 2., 3.])
! y = find(A .ne. 0.)
!     1   1
!     2   2
!     3   3
!=======================================================================

  function find1(bool)
    integer(kind = IPRE), dimension(:), allocatable :: find1
    logical, dimension(:), intent(in) :: bool
    integer(kind = IPRE) :: i, j, n

    n = count(bool)
    if (n .ne. 0) then
      find1 = zeros(n)
      j = 1
      do i = 1, size(bool)
        if (bool(i)) then
          find1(j) = i
          j = j + 1
        end if
      end do
    else
      find1 = zeros(0)
    end if
    return
  end function find1

  function find2(bool)
    integer(kind = IPRE), dimension(:,:), allocatable :: find2
    logical, dimension(:,:), intent(in) :: bool
    integer(kind = IPRE) :: i, j, k, n

    n = count(bool)
    if (n .ne. 0) then
      find2 = zeros(n, 2)
      k = 1
      do i = 1, size(bool, 1)
        do j = 1, size(bool, 2)
          if (bool(i,j)) then
            find2(k,1) = i
            find2(k,2) = j
            k = k + 1
          end if
        end do
      end do
    else
      find2 = zeros(0, 2)
    end if
    return
  end function find2

  function find3(bool)
    integer(kind = IPRE), dimension(:,:), allocatable :: find3
    logical, dimension(:,:,:), intent(in) :: bool
    integer(kind = IPRE) :: i, j, k, l, n

    n = count(bool)
    if (n .ne. 0) then
      find3 = zeros(n, 3)
      l = 1
      do i = 1, size(bool, 1)
        do j = 1, size(bool, 2)
          do k = 1, size(bool, 3)
            if (bool(i,j,k)) then
              find3(l,1) = i
              find3(l,2) = j
              find3(l,3) = k
              l = l + 1
            end if
          end do
        end do
      end do
    else
      find3 = zeros(0, 3)
    end if
    return
  end function find3

!=======================================================================
! fminbnd
!-----------------------------------------------------------------------
! fminbnd solves a 1-dimensional problem defined by
!   min_x f(x) | a < x < b
! with x, a, b finite scalars, and f(x) a function that returns a
! scalar. The golden section search algorithm is used.
!
! Syntax
!-----------------------------------------------------------------------
! x = fminbnd(fitness, a, b)
! x = fminbnd(fitness, a, b, eps)
!
! Description
!-----------------------------------------------------------------------
! x = fminbnd(fitness, a, b) returns the scalar x that is a local
! minimizer of the function fitness in the interval a < x < b.
!
! x = fminbnd(fitness, a, b, eps) returns the local minimizer x with the
! convergence tolerance specified by eps.
!=======================================================================

  real(kind = RPRE) function fminbnd(fitness, a, b, eps)
    procedure(func1d) :: fitness
    real(kind = RPRE), intent(in) :: a, b
    real(kind = RPRE), intent(in), optional :: eps
    real(kind = RPRE) :: opt_eps, x1, x2, x3, x4
    real(kind = RPRE), parameter :: gr = 0.6180339887498949d0

    opt_eps = 1.0d-4
    if (present(eps)) opt_eps = eps

    x1 = a
    x2 = b
    x3 = x2 - gr * (x2 - x1)
    x4 = x1 + gr * (x2 - x1)
    do while ( abs(x3 - x4) .gt. opt_eps )
      if ( fitness(x3) .lt. fitness(x4) ) then
        x2 = x4
      else
        x1 = x3
      end if
      x3 = x2 - gr * (x2 - x1)
      x4 = x1 + gr * (x2 - x1)
    end do
    fminbnd = 0.5d0 * (x1 + x2)
    return
  end function fminbnd

!=======================================================================
! flip
!-----------------------------------------------------------------------
! flip reverses order of elements of arrays.
!
! Syntax
!-----------------------------------------------------------------------
! y = flip(x)
! B = flip(A)
! B = flip(A, dim)
! Y = flip(X)
! Y = flip(X, dim)
!
! Description
!-----------------------------------------------------------------------
! y = flip(x) returns the vector x in reversed order.
!
! B = flip(A) returns the matrix A with its rows flipped in the
! up-to-down direction.
!
! B = flip(A, dim) returns the matrix A with its elements in reversed
! order along the dimension dim.
!
! Y = flip(X) returns the 3-dimensional array X with its elements in
! reversed order along the first dimension.
!
! Y = flip(X, dim) returns the 3-dimensional array X with its elements
! in reversed order along the dimension dim.
!
! Examples
!-----------------------------------------------------------------------
! x = linspace(1, 9, 9)
! y = flip(x)
!     9.    8.    7.    6.    5.    4.    3.    2.    1.
!
! A = eye(3)
! B = flip(A, 1)
!     0.    0.    1.
!     0.    1.    0.
!     1.    0.    0.
! C = flip(A, 2)
!     0.    0.    1.
!     0.    1.    0.
!     1.    0.    0.
!=======================================================================

  function flip_i1(x)
    integer(kind = IPRE), dimension(:), allocatable :: flip_i1
    integer(kind = IPRE), dimension(:), intent(in) :: x

    flip_i1 = flipud(x)
    return
  end function flip_i1

  function flip_r1(x)
    real(kind = RPRE), dimension(:), allocatable :: flip_r1
    real(kind = RPRE), dimension(:), intent(in) :: x

    flip_r1 = flipud(x)
    return
  end function flip_r1

  function flip_i2(A, dim)
    integer(kind = IPRE), dimension(:,:), allocatable :: flip_i2
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim

    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      flip_i2 = flipud(A)
    elseif (dim .eq. 2) then
      flip_i2 = fliplr(A)
    end if
    return
  end function flip_i2

  function flip_r2(A, dim)
    real(kind = RPRE), dimension(:,:), allocatable :: flip_r2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim

    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      flip_r2 = flipud(A)
    elseif (dim .eq. 2) then
      flip_r2 = fliplr(A)
    end if
    return
  end function flip_r2

  function flip_i3(X, dim)
    integer(kind = IPRE), dimension(:,:,:), allocatable :: flip_i3
    integer(kind = IPRE), dimension(:,:,:), intent(in) :: X
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: n

    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      n = size(X, 1)
      flip_i3 = X(n:1:-1,:,:)
    elseif (dim .eq. 2) then
      n = size(X, 2)
      flip_i3 = X(:,n:1:-1,:)
    elseif (dim .eq. 3) then
      n = size(X, 3)
      flip_i3 = X(:,:,n:1:-1)
    end if
    return
  end function flip_i3

  function flip_r3(X, dim)
    real(kind = IPRE), dimension(:,:,:), allocatable :: flip_r3
    real(kind = IPRE), dimension(:,:,:), intent(in) :: X
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: n

    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      n = size(X, 1)
      flip_r3 = X(n:1:-1,:,:)
    elseif (dim .eq. 2) then
      n = size(X, 2)
      flip_r3 = X(:,n:1:-1,:)
    elseif (dim .eq. 3) then
      n = size(X, 3)
      flip_r3 = X(:,:,n:1:-1)
    end if
    return
  end function flip_r3

!=======================================================================
! fliplr
!-----------------------------------------------------------------------
! fliplr reverses vector and matrix left to right.
!
! Syntax
!-----------------------------------------------------------------------
! y = fliplr(x)
! B = fliplr(A)
!
! Description
!-----------------------------------------------------------------------
! y = fliplr(x) returns the vector x in reversed order.
!
! B = fliplr(A) returns the matrix A with its columns flipped in the
! left-to-right direction.
!
! Examples
!-----------------------------------------------------------------------
! x = linspace(1, 9, 9)
! y = fliplr(x)
!     9.    8.    7.    6.    5.    4.    3.    2.    1.
!
! A = eye(3)
! B = fliplr(A)
!     0.    0.    1.
!     0.    1.    0.
!     1.    0.    0.
!=======================================================================

  function fliplr_i1(x)
    integer(kind = IPRE), dimension(:), allocatable :: fliplr_i1
    integer(kind = IPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: n

    n = size(x)
    fliplr_i1 = x(n:1:-1)
    return
  end function fliplr_i1

  function fliplr_r1(x)
    real(kind = RPRE), dimension(:), allocatable :: fliplr_r1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: n

    n = size(x)
    fliplr_r1 = x(n:1:-1)
    return
  end function fliplr_r1

  function fliplr_i2(A)
    integer(kind = IPRE), dimension(:,:), allocatable :: fliplr_i2
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: n

    n = size(A, 2)
    fliplr_i2 = A(:,n:1:-1)
    return
  end function fliplr_i2

  function fliplr_r2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: fliplr_r2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: n

    n = size(A, 2)
    fliplr_r2 = A(:,n:1:-1)
    return
  end function fliplr_r2

!=======================================================================
! flipud
!-----------------------------------------------------------------------
! flipud reverses vector and matrix up to down.
!
! Syntax
!-----------------------------------------------------------------------
! y = flipud(x)
! B = flipud(A)
!
! Description
!-----------------------------------------------------------------------
! y = flipud(x) returns the vector x in reversed order.
!
! B = flipud(A) returns the matrix A with its rows flipped in the
! up-to-down direction.
!
! Examples
!-----------------------------------------------------------------------
! x = linspace(1, 9, 9)
! y = flipud(x)
!     9.    8.    7.    6.    5.    4.    3.    2.    1.
!
! A = eye(3)
! B = flipud(A)
!     0.    0.    1.
!     0.    1.    0.
!     1.    0.    0.
!=======================================================================

  function flipud_i1(x)
    integer(kind = IPRE), dimension(:), allocatable :: flipud_i1
    integer(kind = IPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: n

    n = size(x)
    flipud_i1 = x(n:1:-1)
    return
  end function flipud_i1

  function flipud_r1(x)
    real(kind = RPRE), dimension(:), allocatable :: flipud_r1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: n

    n = size(x)
    flipud_r1 = x(n:1:-1)
    return
  end function flipud_r1

  function flipud_i2(A)
    integer(kind = IPRE), dimension(:,:), allocatable :: flipud_i2
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: n

    n = size(A, 1)
    flipud_i2 = A(n:1:-1,:)
    return
  end function flipud_i2

  function flipud_r2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: flipud_r2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: n

    n = size(A, 1)
    flipud_r2 = A(n:1:-1,:)
    return
  end function flipud_r2

!=======================================================================
! gammainc
!-----------------------------------------------------------------------
! gammainc returns the incomplete gamma function.
!
! Syntax
!-----------------------------------------------------------------------
! y = gammainc(x, a)
!
! Description
!-----------------------------------------------------------------------
! y = gammainc(x, a) returns the incomplete gamma function of
! corresponding elements of x and a.
!=======================================================================

  real(kind = RPRE) function gammainc0(x, a)
    real(kind = RPRE), intent(in) :: x, a

    if ( x .lt. 0. .or. a .le. 0.) then
      print *, "Error: in gammainc, x < 0 and/or a <= 0"
      stop
    end if
    if ( x .lt. a+1. ) then
      gammainc0 = gser(x, a)
    else
      gammainc0 = 1. - gcf(x, a)
    end if

    return
  contains

    real(kind = RPRE) function gser(x, a)
      real(kind = RPRE), intent(in) :: x, a
      integer(kind = IPRE), parameter :: itermax = 100
      real(kind = RPRE), parameter :: eps = 3.e-7
      integer(kind = IPRE) :: n
      real(kind = RPRE) :: gln, ap, del, s

      gln = log(gamma(a))
      if ( x .le. 0. ) then
        gser = 0.
      else
        ap = a
        s = 1. / a
        del = s
        do n = 1, itermax
          ap = ap + 1.
          del = del * x / ap
          s = s + del
          if ( abs(del) .lt. abs(s)*eps ) exit
        end do
        gser = s * exp(-x + a * log(x) - gln)
      end if
      return
    end function gser

    real(kind = RPRE) function gcf(x, a)
      real(kind = RPRE), intent(in) :: x, a
      integer(kind = IPRE), parameter :: itermax = 100
      real(kind = RPRE), parameter :: eps = 3.e-7, fpmin = 1.e-30
      integer(kind = IPRE) :: i
      real(kind = RPRE) :: an, b, c, d, del, h, gln

      gln = log(gamma(a))
      b = x + 1. - a
      c = 1. / fpmin
      d = 1. / b
      h = d
      do i = 1, itermax
        an = -i * (i - a)
        b = b + 2
        d = an * d + b
        if ( abs(d) .lt. fpmin ) d = fpmin
        c = b + an / c
        if ( abs(c) .lt. fpmin ) c = fpmin
        d = 1. / d
        del = d * c
        h = h * del
        if ( abs(del-1.) .lt. eps ) exit
      end do
      gcf = h * exp(-x + a * log(x) - gln)
      return
    end function gcf

  end function gammainc0

  function gammainc1_0(X, a)
    real(kind = RPRE), dimension(:), allocatable :: gammainc1_0
    real(kind = RPRE), dimension(:), intent(in) :: X
    real(kind = RPRE), intent(in) :: a
    integer(kind = IPRE) :: i, n

    n = size(X)
    gammainc1_0 = zeros(n)
    do i = 1, n
      gammainc1_0(i) = gammainc0(X(i), a)
    end do
    return
  end function

!=======================================================================
! gmm
!-----------------------------------------------------------------------
! gmm performs Gaussian Mixture Modelling using Expectation-Maximization
! algorithm.
!
! Syntax
!-----------------------------------------------------------------------
! idx = gmm(x, K, [options = ])
! idx = gmm(A, K, [options = ])
!
! Description
!-----------------------------------------------------------------------
! idx = gmm(x, K, [options = ]) returns the cluster indices of each
! element in vector x.
!
! idx = gmm(A, K, [options = ]) returns the cluster indices of each
! rows in matrix A.
!
! Options
!-----------------------------------------------------------------------
! means               Output centroids
! stdev / covar       Output standard deviation / covariance matrix
! prob                Output probabilities
! itermax = 1000      Maximum number of iterations
! niter               Output number of iterations
!=======================================================================

  function gmm1(x, K, means, stdev, prob, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: prob, means, stdev
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter
    integer(kind = IPRE) :: opt_itermax, i, j, n, iter
    real(kind = RPRE), dimension(:), allocatable :: phi, mu, mu_prev, sigma
    real(kind = RPRE), dimension(:,:), allocatable :: w, pdf, pdf_w

    n = size(x)

    opt_itermax = 1000
    if (present(itermax)) opt_itermax = itermax

    ! Initialization
    !================
    phi = ones(K) / real(K)     ! Equal initial probabilities for each cluster
    mu = x(randperm(n, K))      ! Random initial means
    sigma = ones(K) * std(x)    ! Covariance matrices for each variable

    ! Loop until convergence
    !========================
    w = zeros(n, K)
    iter = 0
    do while ( iter .lt. opt_itermax )
      iter = iter + 1

      ! Expectation
      !=============
      pdf = zeros(n, K)
      do j = 1, K
        pdf(:,j) = normpdf(x, mu(j), sigma(j))
      end do

      pdf_w = pdf * repmat(phi, n, 2)
      w = pdf_w / repmat(sum(pdf_w, dim = 2), K)

      ! Maximization
      !==============
      mu_prev = mu
      do j = 1, K
        phi(j) = mean(w(:,j))
        mu(j) = dot_product(w(:,j), x) / sum(w(:,j))
        sigma(j) = dot_product(w(:,j), (x - mu(j))**2) / sum(w(:,j))
        sigma(j) = sqrt(sigma(j))
      end do

      if ( norm(mu - mu_prev) .lt. 1.0d-10 ) exit
    end do

    idx = zeros(n)
    do i = 1, n
      idx(i:i) = maxloc(pdf(i,:))
    end do

    if (present(niter)) niter = iter
    if (present(means)) means = mu
    if (present(stdev)) stdev = sigma
    if (present(prob)) prob = phi

    return
  end function gmm1

  function gmm2(A, K, means, covar, prob, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: prob
    real(kind = RPRE), dimension(:,:), allocatable, intent(inout), optional :: means
    real(kind = RPRE), dimension(:,:,:), allocatable, intent(inout), optional :: covar
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter

    integer(kind = IPRE) :: opt_itermax, i, j, n, p, iter
    real(kind = RPRE), dimension(:), allocatable :: phi
    real(kind = RPRE), dimension(:,:), allocatable :: mu, mu_prev, w, pdf, &
      pdf_w, tmp
    real(kind = RPRE), dimension(:,:,:), allocatable :: sigma

    n = size(A, 1)
    p = size(A, 2)

    opt_itermax = 1000
    if (present(itermax)) opt_itermax = itermax

    ! Initialization
    !================
    phi = ones(K) / real(K)     ! Equal initial probabilities for each cluster
    mu = A(randperm(n, K),:)    ! Random initial means
    sigma = zeros(p, p, K)      ! Covariance matrices for each variable

    tmp = cov(A)
    do j = 1, K
      sigma(:,:,j) = tmp
    end do

    ! Loop until convergence
    !========================
    w = zeros(n, K)
    iter = 0
    do while ( iter .lt. opt_itermax )
      iter = iter + 1

      ! Expectation
      !=============
      pdf = zeros(n, K)
      do j = 1, K
        pdf(:,j) = normpdf(A, mu(j,:), sigma(:,:,j))
      end do

      pdf_w = pdf * repmat(phi, n, 2)
      w = pdf_w / repmat(sum(pdf_w, dim = 2), K)

      ! Maximization
      !==============
      mu_prev = mu
      do j = 1, K
        phi(j) = mean(w(:,j))
        mu(j,:) = matmul(w(:,j), A) / sum(w(:,j))
        tmp = A - repmat(mu(j,:), n, 2)

        sigma(:,:,j) = zeros(p, p)
        do i = 1, n
          sigma(:,:,j) = sigma(:,:,j) &
                         + w(i,j) * matmul(transpose(tmp(i:i,:)), tmp(i:i,:)) &
                                  / sum(w(:,j))
        end do
      end do

      if ( means_residuals(mu, mu_prev) .lt. 1.0d-10 ) exit
    end do

    idx = zeros(n)
    do i = 1, n
      idx(i:i) = maxloc(pdf(i,:))
    end do

    if (present(niter)) niter = iter
    if (present(means)) means = mu
    if (present(covar)) covar = sigma
    if (present(prob)) prob = phi

    return
  contains

    !-------------------------------------------------------------------
    ! means_residuals
    !-------------------------------------------------------------------
    function means_residuals(means1, means2) result(eps)
      real(kind = RPRE) :: eps
      real(kind = RPRE), dimension(:,:), intent(in) :: means1, means2
      real(kind = RPRE), dimension(:,:), allocatable :: means
      integer(kind = IPRE) :: k

      eps = 0.0d0
      means = abs( means2 - means1 )
      do k = 1, p
        eps = eps + sum(means(:,k))**2
      end do
      eps = sqrt(eps)
      return
    end function means_residuals

  end function gmm2

!=======================================================================
! horzcat
!-----------------------------------------------------------------------
! horzcat concatenates arrays horizontally.
!
! Syntax
!-----------------------------------------------------------------------
! A = horzcat(x1, x2)
! A = horzcat(A1, A2)
! B = horzcat(x1, A2)
! B = horzcat(A1, x2)
!
! Description
!-----------------------------------------------------------------------
! A = horzcat(x1, x2) concatenates the vectors x1 and x2 treated as
! column vectors along the dimension 1. If the length of x1 and x2 are
! not equal, empty elements will be filled with zeros.
!
! A = horzcat(A1, A2) concatenates the matrices A1 and A2 along the
! dimension 1. If the first dimensions of A1 and A2 are not equal, empty
! elements will be filled with zeros.
!
! B = horzcat(x1, A2) concatenates the vector x treated as column vector
! and the matrix A along the dimension 1. If the length of x and the
! first dimension of A are not equal, empty elements will be filled with
! zeros.
!
! B = horzcat(A1, x2) concatenates the matrix A and the vector x treated
! as column vector along the dimension 1. If the first dimension o A and
! the length of x are not equal, empty elements will be filled with
! zeros.
!
! Examples
!-----------------------------------------------------------------------
! A1 = reshape([ 1., 2., 3., 4. ], [ 2, 2 ], order = [ 2, 1 ])
! A2 = reshape([ 5., 6., 7., 8. ], [ 2, 2 ], order = [ 2, 1 ])
! A = horzcat(A1, A2)
!     1.  2.  5.  6.
!     3.  4.  7.  8.
!=======================================================================

  function horzcat_i1(x1, x2)
    integer(kind = IPRE), dimension(:,:), allocatable :: horzcat_i1
    integer(kind = IPRE), dimension(:), intent(in) :: x1, x2
    integer(kind = IPRE) :: m1, m2

    m1 = size(x1)
    m2 = size(x2)

    horzcat_i1 = zeros(max(m1, m2), 2)
    horzcat_i1(1:m1,1) = x1
    horzcat_i1(1:m2,2) = x2
    return
  end function horzcat_i1

  function horzcat_r1(x1, x2)
    real(kind = RPRE), dimension(:,:), allocatable :: horzcat_r1
    real(kind = RPRE), dimension(:), intent(in) :: x1, x2
    integer(kind = IPRE) :: m1, m2

    m1 = size(x1)
    m2 = size(x2)

    horzcat_r1 = zeros(max(m1, m2), 2)
    horzcat_r1(1:m1,1) = x1
    horzcat_r1(1:m2,2) = x2
    return
  end function horzcat_r1

  function horzcat_i2(A1, A2)
    integer(kind = IPRE), dimension(:,:), allocatable :: horzcat_i2
    integer(kind = IPRE), dimension(:,:), intent(in) :: A1, A2
    integer(kind = IPRE) :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    horzcat_i2 = zeros(max(m1, m2), n1+n2)
    horzcat_i2(1:m1,1:n1) = A1
    horzcat_i2(1:m2,n1+1:) = A2
    return
  end function horzcat_i2

  function horzcat_r2(A1, A2)
    real(kind = RPRE), dimension(:,:), allocatable :: horzcat_r2
    real(kind = RPRE), dimension(:,:), intent(in) :: A1, A2
    integer(kind = IPRE) :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    horzcat_r2 = zeros(max(m1, m2), n1+n2)
    horzcat_r2(1:m1,1:n1) = A1
    horzcat_r2(1:m2,n1+1:) = A2
    return
  end function horzcat_r2

  function horzcat_i12(x1, A2)
    integer(kind = IPRE), dimension(:,:), allocatable :: horzcat_i12
    integer(kind = IPRE), dimension(:), intent(in) :: x1
    integer(kind = IPRE), dimension(:,:), intent(in) :: A2
    integer(kind = IPRE) :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    horzcat_i12 = zeros(max(m1, m2), n2+1)
    horzcat_i12(1:m1,1) = x1
    horzcat_i12(1:m2,2:) = A2
    return
  end function horzcat_i12

  function horzcat_r12(x1, A2)
    real(kind = RPRE), dimension(:,:), allocatable :: horzcat_r12
    real(kind = RPRE), dimension(:), intent(in) :: x1
    real(kind = RPRE), dimension(:,:), intent(in) :: A2
    integer(kind = IPRE) :: m1, m2, n2

    m1 = size(x1)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    horzcat_r12 = zeros(max(m1, m2), n2+1)
    horzcat_r12(1:m1,1) = x1
    horzcat_r12(1:m2,2:) = A2
    return
  end function horzcat_r12

  function horzcat_i21(A1, x2)
    integer(kind = IPRE), dimension(:,:), allocatable :: horzcat_i21
    integer(kind = IPRE), dimension(:,:), intent(in) :: A1
    integer(kind = IPRE), dimension(:), intent(in) :: x2
    integer(kind = IPRE) :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)

    horzcat_i21 = zeros(max(m1, m2), n1+1)
    horzcat_i21(1:m1,1:n1) = A1
    horzcat_i21(1:m2,n1+1) = x2
    return
  end function horzcat_i21

  function horzcat_r21(A1, x2)
    real(kind = RPRE), dimension(:,:), allocatable :: horzcat_r21
    real(kind = RPRE), dimension(:,:), intent(in) :: A1
    real(kind = RPRE), dimension(:), intent(in) :: x2
    integer(kind = IPRE) :: m1, n1, m2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(x2)

    horzcat_r21 = zeros(max(m1, m2), n1+1)
    horzcat_r21(1:m1,1:n1) = A1
    horzcat_r21(1:m2,n1+1) = x2
    return
  end function horzcat_r21

!=======================================================================
! hann
!-----------------------------------------------------------------------
! hann defines a Hanning window.
!
! Syntax
!-----------------------------------------------------------------------
! w = hann(n)
!
! Description
!-----------------------------------------------------------------------
! w = hann(n) returns an n-point symmetric Hanning window.
!=======================================================================

  function hann(n)
    real(kind = RPRE), dimension(:), allocatable :: hann
    integer(kind = IPRE), intent(in) :: n

    hann = 0.5d0 * ( 1 - cos( 2.0d0 * pi * linspace(0, n-1, n) / n ) )
    return
  end function hann

!=======================================================================
! interp1
!-----------------------------------------------------------------------
! interp1 performs a linear interpolation.
!
! Syntax
!-----------------------------------------------------------------------
! vq = interp1(x, v, xq)
!
! Description
!-----------------------------------------------------------------------
! vq = interp1(x, v, xq) returns the evaluated vector yq at the query
! points in xq using a linear interpolation.
!=======================================================================

  function interp1_0(x, v, xq) result(vq)
    real(kind = RPRE) :: vq
    real(kind = RPRE), intent(in) :: xq
    real(kind = RPRE), dimension(:), intent(in) :: x, v
    integer(kind = IPRE) :: i, x1, x2, ix(2)
    real(kind = RPRE) :: vn, xr(2), vr(2)

    x1 = minloc(xq - x, 1, mask = xq .ge. x)
    x2 = maxloc(xq - x, 1, mask = xq .lt. x)
    if ( x2 .ne. 0 ) then
      vn = abs( (x(x2) - x(x1)) )
      xr = x( [ x1, x2 ] )
      vr = v( [ x1, x2 ] )
      vq = vr(1) * ( xr(2) - xq ) + vr(2) * ( xq - xr(1) )
      vq = vq / vn
    else
      vq = v(size(v))
    end if
    return
  end function interp1_0

  function interp1_1(x, v, xq) result(vq)
    real(kind = RPRE), dimension(:), allocatable :: vq
    real(kind = RPRE), dimension(:), intent(in) :: xq, x, v
    integer(kind = IPRE) :: i, n

    n = size(xq)
    vq = zeros(n)
    do i = 1, n
      vq(i) = interp1_0(x, v, xq(i))
    end do
    return
  end function interp1_1

!=======================================================================
! interp2
!-----------------------------------------------------------------------
! interp2 performs a bilinear interpolation.
!
! Syntax
!-----------------------------------------------------------------------
! vq = interp2(x, y, V, xq, yq)
! VQ = interp2(x, y, V, XQ, YQ)
!
! Description
!-----------------------------------------------------------------------
! vq = interp2(x, y, V, xq, yq) returns the evaluated vector vq at the
! query points in xq and yq using a bilinear interpolation.
!
! VQ = interp2(x, y, V, XQ, YQ) returns the evaluated matrix VQ given
! mesh type grids XQ and YQ using a bilinear interpolation. VQ is of the
! same shape as XQ and YQ.
!=======================================================================

  function interp2_0(x, y, v, xq, yq) result(vq)
    real(kind = RPRE) :: vq
    real(kind = RPRE), intent(in) :: xq, yq
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: v
    integer(kind = IPRE) :: i, x1, y1, x2, y2, ix(4), iy(4)
    real(kind = RPRE) :: vn, xr(2), yr(2), N(4), vr(4)

    x1 = minloc(xq - x, 1, mask = xq .ge. x)
    y1 = minloc(yq - y, 1, mask = yq .ge. y)
    x2 = maxloc(xq - x, 1, mask = xq .lt. x)
    y2 = maxloc(yq - y, 1, mask = yq .lt. y)
    vn = abs( (x(x2) - x(x1)) &
            * (y(y2) - y(y1)) )
    xr = x( [ x1, x2 ] )
    yr = y( [ y1, y2 ] )
    ix = [ 2, 1, 2, 1 ]
    iy = [ 2, 2, 1, 1 ]
    do i = 1, 4
      N(i) = abs( (xr(ix(i)) - xq) * (yr(iy(i)) - yq) )
    end do
    vr = reshape(v( [ x1, x2 ], &
                    [ y1, y2 ] ), shape = [ 4 ])
    vq = dot_product(vr, N/vn)
    return
  end function interp2_0

  function interp2_1(x, y, v, xq, yq) result(vq)
    real(kind = RPRE), dimension(:), allocatable :: vq
    real(kind = RPRE), dimension(:), intent(in) :: xq, yq, x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: v
    integer(kind = IPRE) :: i, n

    n = size(xq)
    vq = zeros(n)
    do i = 1, n
      vq(i) = interp2_0(x, y, v, xq(i), yq(i))
    end do
    return
  end function interp2_1

  function interp2_2(x, y, v, xq, yq) result(vq)
    real(kind = RPRE), dimension(:,:), allocatable :: vq
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: v, xq, yq
    integer(kind = IPRE) :: m, n

    m = size(xq, 1)
    n = size(xq, 2)
    vq = reshape( interp2_1(y, x, v, [ yq ], [ xq ]), shape = [ m, n ] )
    return
  end function interp2_2

!=======================================================================
! interp3
!-----------------------------------------------------------------------
! interp3 performs a trilinear interpolation.
!
! Syntax
!-----------------------------------------------------------------------
! vq = interp3(x, y, z, v, xq, yq, zq)
!
! Description
!-----------------------------------------------------------------------
! vq = interp3(x, y, z, v, xq, yq, zq) returns the evaluated vector vq
! at the query points in xq, yq and zq using a trilinear interpolation.
!=======================================================================

  function interp3_0(x, y, z, v, xq, yq, zq) result(vq)
    real(kind = RPRE) :: vq
    real(kind = RPRE), intent(in) :: xq, yq, zq
    real(kind = RPRE), dimension(:), intent(in) :: x, y, z
    real(kind = RPRE), dimension(:,:,:), intent(in) :: v
    integer(kind = IPRE) :: i, x1, y1, z1, x2, y2, z2, &
      ix(8), iy(8), iz(8)
    real(kind = RPRE) :: vn, xr(2), yr(2), zr(2), N(8), vr(8)

    x1 = minloc(xq - x, 1, mask = xq .ge. x)
    y1 = minloc(yq - y, 1, mask = yq .ge. y)
    z1 = minloc(zq - z, 1, mask = zq .ge. z)
    x2 = maxloc(xq - x, 1, mask = xq .lt. x)
    y2 = maxloc(yq - y, 1, mask = yq .lt. y)
    z2 = maxloc(zq - z, 1, mask = zq .lt. z)
    vn = abs( (x(x2) - x(x1)) &
            * (y(y2) - y(y1)) &
            * (z(z2) - z(z1)) )
    xr = x( [ x1, x2 ] )
    yr = y( [ y1, y2 ] )
    zr = z( [ z1, z2 ] )
    ix = [ 2, 1, 2, 1, 2, 1, 2, 1 ]
    iy = [ 2, 2, 1, 1, 2, 2, 1, 1 ]
    iz = [ 2, 2, 2, 2, 1, 1, 1, 1 ]
    do i = 1, 8
      N(i) = abs( (xr(ix(i)) - xq) * (yr(iy(i)) - yq) * (zr(iz(i)) - zq) )
    end do
    vr = reshape(v( [ x1, x2 ], &
                    [ y1, y2 ], &
                    [ z1, z2 ] ), shape = [ 8 ])
    vq = dot_product(vr, N/vn)
    return
  end function interp3_0

  function interp3_1(x, y, z, v, xq, yq, zq) result(vq)
    real(kind = RPRE), dimension(:), allocatable :: vq
    real(kind = RPRE), dimension(:), intent(in) :: xq, yq, zq, x, y, z
    real(kind = RPRE), dimension(:,:,:), intent(in) :: v
    integer(kind = IPRE) :: i, n

    n = size(xq)
    vq = zeros(n)
    do i = 1, n
      vq(i) = interp3_0(x, y, z, v, xq(i), yq(i), zq(i))
    end do
    return
  end function interp3_1

!=======================================================================
! inv
!-----------------------------------------------------------------------
! inv computes the matrix inverse.
!
! Syntax
!-----------------------------------------------------------------------
! B = inv(A)
!
! Description
!-----------------------------------------------------------------------
! B = inv(A) returns the inverse of the matrix A if A is inversible
! (det(A) /= 0.).
!
! Examples
!-----------------------------------------------------------------------
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 0. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! B = inv(A)
!     -1.77777779   0.888888896  -0.111111112
!      1.55555558  -0.777777791   0.222222224
!     -0.11111112   0.222222224  -0.111111112
!=======================================================================

  function inv(A)
    real(kind = RPRE), dimension(:,:), allocatable :: inv
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, k, m
    real(kind = RPRE) :: D
    real(kind = RPRE), dimension(:), allocatable :: x, y, e
    real(kind = RPRE), dimension(:,:), allocatable :: L, U

    if (issquare(A)) then
      m = size(A, 1)
      if (m .le. 3) then
        D = det(A)
      else
        D = det(A, L, U)
      end if
      if (D .ne. 0.) then
        inv = zeros(m, m)
        if (m .eq. 2) then
          inv(1,1) = A(2,2)
          inv(1,2) = -A(1,2)
          inv(2,1) = -A(2,1)
          inv(2,2) = A(1,1)
          inv = inv/D
        elseif (m .eq. 3) then
          inv(1,1) = A(2,2)*A(3,3) - A(2,3)*A(3,2)
          inv(1,2) = A(1,3)*A(3,2) - A(1,2)*A(3,3)
          inv(1,3) = A(1,2)*A(2,3) - A(1,3)*A(2,2)
          inv(2,1) = A(2,3)*A(3,1) - A(2,1)*A(3,3)
          inv(2,2) = A(1,1)*A(3,3) - A(1,3)*A(3,1)
          inv(2,3) = A(1,3)*A(2,1) - A(1,1)*A(2,3)
          inv(3,1) = A(2,1)*A(3,2) - A(2,2)*A(3,1)
          inv(3,2) = A(1,2)*A(3,1) - A(1,1)*A(3,2)
          inv(3,3) = A(1,1)*A(2,2) - A(1,2)*A(2,1)
          inv = inv/D
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
              do j = 1, i-1
                y(i) = y(i) - y(j)*L(i,j)
              end do
            end do

            ! Back substitution: Ux = y
            !===========================
            x(m) = y(m)/U(m,m)
            do i = m-1, 1, -1
              x(i) = y(i)
              do j = m, i+1, -1
                x(i) = x(i) - x(j)*U(i,j)
              end do
              x(i) = x(i)/U(i,i)
            end do

            ! The column k of the inverse is x
            !==================================
            inv(:,k) = x
          end do
        end if
      else
        stop "Error: in det(A), A is not inversible (= 0)."
      end if
    else
      stop "Error: in inv(A), A should be square."
    end if
    return
  end function inv

!=======================================================================
! isleap
!-----------------------------------------------------------------------
! isleap determines whether a year is a leap year.
!
! Syntax
!-----------------------------------------------------------------------
! bool = isleap(year)
!
! Description
!-----------------------------------------------------------------------
! bool = isleap(year) returns .true. if year is a leap year, .false.
! otherwise.
!
! Examples
!-----------------------------------------------------------------------
! bool = isleap(2016)
!     .true.
!=======================================================================

  logical function isleap(year)
    integer(kind = IPRE), intent(in) :: year
    if ( (mod(year, 400) .eq. 0) .or. &
        ((mod(year, 4) .eq. 0) .and. (mod(year, 100) .ne. 0)) ) then
      isleap = .true.
    else
      isleap = .false.
    end if
    return
  end function isleap

!=======================================================================
! ismember
!-----------------------------------------------------------------------
! ismember determines whether a value is present in an array.
!
! Syntax
!-----------------------------------------------------------------------
! bool = ismember(x, y)
! bool = ismember(x, A)
! bool = ismember(x, Y)
!
! Description
!-----------------------------------------------------------------------
! bool = ismember(x, y) returns .true. if x is present in the
! 1-dimensional array y, .false. otherwise.
!
! bool = ismember(x, A) returns .true. if x is present in the
! 2-dimensional array A, .false. otherwise.
!
! bool = ismember(x, Y) returns .true. if x is present in the
! 3-dimensional array Y, .false. otherwise.
!
! Examples
!-----------------------------------------------------------------------
! y = [ 1., 2., 3., 4., 5. ]
! bool = ismember(3., y)
!     .true.
! bool = ismember(6., y)
!     .false.
!=======================================================================

  logical function ismember_i0i1(x, y)
    integer(kind = IPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:), intent(in) :: y
    integer(kind = IPRE) :: i, dim1

    ismember_i0i1 = .false.
    dim1 = size(y)
    do i = 1, dim1
      if (x .eq. y(i)) then
        ismember_i0i1 = .true.
        return
      end if
    end do
    return
  end function ismember_i0i1

  logical function ismember_i0r1(x, y)
    integer(kind = IPRE), intent(in) :: x
    real(kind = RPRE), dimension(:), intent(in) :: y
    integer(kind = IPRE) :: i, dim1

    ismember_i0r1 = .false.
    dim1 = size(y)
    do i = 1, dim1
      if (x .eq. y(i)) then
        ismember_i0r1 = .true.
        return
      end if
    end do
    return
  end function ismember_i0r1

  logical function ismember_i0i2(x, A)
    integer(kind = IPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, dim1, dim2

    ismember_i0i2 = .false.
    dim1 = size(A, 1)
    dim2 = size(A, 2)
    do i = 1, dim1
      do j = 1, dim2
        if (x .eq. A(i,j)) then
          ismember_i0i2 = .true.
          return
        end if
      end do
    end do
    return
  end function ismember_i0i2

  logical function ismember_i0r2(x, A)
    integer(kind = IPRE), intent(in) :: x
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, dim1, dim2

    ismember_i0r2 = .false.
    dim1 = size(A, 1)
    dim2 = size(A, 2)
    do i = 1, dim1
      do j = 1, dim2
        if (x .eq. A(i,j)) then
          ismember_i0r2 = .true.
          return
        end if
      end do
    end do
    return
  end function ismember_i0r2

  logical function ismember_i0i3(x, Y)
    integer(kind = IPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:,:,:), intent(in) :: Y
    integer(kind = IPRE) :: i, j, k, dim1, dim2, dim3

    ismember_i0i3 = .false.
    dim1 = size(Y, 1)
    dim2 = size(Y, 2)
    dim3 = size(Y, 3)
    do i = 1, dim1
      do j = 1, dim2
        do k = 1, dim2
          if (x .eq. Y(i,j,k)) then
            ismember_i0i3 = .true.
            return
          end if
        end do
      end do
    end do
    return
  end function ismember_i0i3

  logical function ismember_i0r3(x, Y)
    integer(kind = IPRE), intent(in) :: x
    real(kind = RPRE), dimension(:,:,:), intent(in) :: Y
    integer(kind = IPRE) :: i, j, k, dim1, dim2, dim3

    ismember_i0r3 = .false.
    dim1 = size(Y, 1)
    dim2 = size(Y, 2)
    dim3 = size(Y, 3)
    do i = 1, dim1
      do j = 1, dim2
        do k = 1, dim2
          if (x .eq. Y(i,j,k)) then
            ismember_i0r3 = .true.
            return
          end if
        end do
      end do
    end do
    return
  end function ismember_i0r3

  logical function ismember_r0i1(x, y)
    real(kind = RPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:), intent(in) :: y
    integer(kind = IPRE) :: i, dim1

    ismember_r0i1 = .false.
    dim1 = size(y)
    do i = 1, dim1
      if (x .eq. y(i)) then
        ismember_r0i1 = .true.
        return
      end if
    end do
    return
  end function ismember_r0i1

  logical function ismember_r0r1(x, y)
    real(kind = RPRE), intent(in) :: x
    real(kind = RPRE), dimension(:), intent(in) :: y
    integer(kind = IPRE) :: i, dim1

    ismember_r0r1 = .false.
    dim1 = size(y)
    do i = 1, dim1
      if (x .eq. y(i)) then
        ismember_r0r1 = .true.
        return
      end if
    end do
    return
  end function ismember_r0r1

  logical function ismember_r0i2(x, A)
    real(kind = RPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, dim1, dim2

    ismember_r0i2 = .false.
    dim1 = size(A, 1)
    dim2 = size(A, 2)
    do i = 1, dim1
      do j = 1, dim2
        if (x .eq. A(i,j)) then
          ismember_r0i2 = .true.
          return
        end if
      end do
    end do
    return
  end function ismember_r0i2

  logical function ismember_r0r2(x, A)
    real(kind = RPRE), intent(in) :: x
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, dim1, dim2

    ismember_r0r2 = .false.
    dim1 = size(A, 1)
    dim2 = size(A, 2)
    do i = 1, dim1
      do j = 1, dim2
        if (x .eq. A(i,j)) then
          ismember_r0r2 = .true.
          return
        end if
      end do
    end do
    return
  end function ismember_r0r2

  logical function ismember_r0i3(x, Y)
    real(kind = RPRE), intent(in) :: x
    integer(kind = IPRE), dimension(:,:,:), intent(in) :: Y
    integer(kind = IPRE) :: i, j, k, dim1, dim2, dim3

    ismember_r0i3 = .false.
    dim1 = size(Y, 1)
    dim2 = size(Y, 2)
    dim3 = size(Y, 3)
    do i = 1, dim1
      do j = 1, dim2
        do k = 1, dim2
          if (x .eq. Y(i,j,k)) then
            ismember_r0i3 = .true.
            return
          end if
        end do
      end do
    end do
    return
  end function ismember_r0i3

  logical function ismember_r0r3(x, Y)
    real(kind = RPRE), intent(in) :: x
    real(kind = RPRE), dimension(:,:,:), intent(in) :: Y
    integer(kind = IPRE) :: i, j, k, dim1, dim2, dim3

    ismember_r0r3 = .false.
    dim1 = size(Y, 1)
    dim2 = size(Y, 2)
    dim3 = size(Y, 3)
    do i = 1, dim1
      do j = 1, dim2
        do k = 1, dim2
          if (x .eq. Y(i,j,k)) then
            ismember_r0r3 = .true.
            return
          end if
        end do
      end do
    end do
    return
  end function ismember_r0r3

!=======================================================================
! isoutlier
!-----------------------------------------------------------------------
! isoutlier determines outliers in a vector.
!
! Syntax
!-----------------------------------------------------------------------
! bool = isoutlier(x)
!
! Description
!-----------------------------------------------------------------------
! bool = isoutlier(x) returns a logical vector with .true. if x(i) is
! an outlier, .false. otherwise.
!=======================================================================

  function isoutlier(x, m)
    logical, dimension(:), allocatable :: isoutlier
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: m
    integer(kind = IPRE) :: opt_m

    opt_m = 3
    if (present(m)) opt_m = m
    isoutlier = abs(x - median(x)) .gt. opt_m*mad(x, 2)
    return
  end function isoutlier

!=======================================================================
! issquare
!-----------------------------------------------------------------------
! issquare determines whether a matrix is square.
!
! Syntax
!-----------------------------------------------------------------------
! bool = issquare(A)
!
! Description
!-----------------------------------------------------------------------
! bool = issquare(A) returns .true. if A is square, .false. otherwise.
!
! Examples
!-----------------------------------------------------------------------
! A = eye(3)
! bool = issquare(A)
!     .true.
!
! A = eye(3, 4)
! bool = issquare(A)
!     .false.
!=======================================================================

  logical function issquare(A)
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    issquare = .false.
    if (size(A, 1) .eq. size(A, 2)) issquare = .true.
    return
  end function issquare

!=======================================================================
! issymmetric
!-----------------------------------------------------------------------
! issymmetric determines whether a square matrix is symmetric.
!
! Syntax
!-----------------------------------------------------------------------
! bool = issymmetric(A)
!
! Description
!-----------------------------------------------------------------------
! bool = issymmetric(A) returns .true. if A is symmetric, .false.
! otherwise.
!
! Examples
!-----------------------------------------------------------------------
! A = eye(3)
! bool = issymmetric(A)
!     .true.
!=======================================================================

  logical function issymmetric(A)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, n

    issymmetric = .true.
    if (.not. issquare(A)) then
      issymmetric = .false.
      return
    else
      n = size(A, 1)
      do i = 1, n
        do j = 1, n
          if ( A(i,j) .ne. A(j,i) ) then
            issymmetric = .false.
            return
          end if
        end do
      end do
    end if
    return
  end function issymmetric

!=======================================================================
! k2test
!-----------------------------------------------------------------------
! k2test performs the D'Agostino-Pearson's K2 test to assess normality
! of a distribution.
!
! Syntax
!-----------------------------------------------------------------------
! p = k2test(x)
!
! Description
!-----------------------------------------------------------------------
! p = k2test(x) returns the p-value for the null hypothesis that the
! data in vector x comes from a normal distribution. According to
! Fisher, the null hypothesis is highly rejectable for p-values lower
! that 0.05.
!
! Examples
!-----------------------------------------------------------------------
! x = randn(1000)
! p1 = k2test(x)
!     0.551972866     ! > 0.05
!
! y = randu(1000)
! p2 = k2test(y)
!     0.000000000     ! < 0.05
!
! Notes
!-----------------------------------------------------------------------
! The K2 statistic has approximately a chi-squared distribution with
! k = 2 degrees of freedom when the population is normally distributed.
! The CDF of the chi-squared with 2 degrees of freedom can be written:
!     F(x,2) = 1 - exp(-x/2)
!=======================================================================

  function k2test(x) result(p)
    real(kind = RPRE) :: p
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = 8) :: n
    real(kind = 8) :: b1, b2, K2
    real(kind = 8) :: Y, beta2, W2, delta, alpha, Z1
    real(kind = 8) :: E, v2, xx, beta1, A, Z2

    n = size(x)
    b1 = skewness(x)
    b2 = kurtosis(x)

    ! Skewness test
    !===============
    Y = b1 * sqrt( (n+1.)*(n+3.) / ( 6.*(n-2.) ) )
    beta2 = 3. * (n*n+27.*n-70.)*(n+1.)*(n+3.) / ( (n-2.)*(n+5.)*(n+7.)*(n+9.) )
    W2 = -1. + sqrt( 2.*(beta2-1.) )
    delta = 1. / sqrt( 0.5*log(W2) )
    alpha = sqrt( 2. / (W2-1.) )
    Z1 = delta * log( Y/alpha + sqrt( (Y/alpha)**2 + 1.) )

    ! Kurtosis test
    !===============
    E = 3. * (n-1.) / (n+1.)
    v2 = 24. * n*(n-2.)*(n-3.) / ( (n+1.)**2*(n+3.)*(n+5.) )
    xx = (b2-E) / sqrt(v2)
    beta1 = 6. * (n**2-5.*n+2.) / ( (n+7.)*(n+9.) ) &
            * sqrt( 6. * (n+3.)*(n+5.) / ( n*(n-2.)*(n-3.) ) )
    A = 6. + 8./beta1 * ( 2./beta1 + sqrt( 1. + 4./(beta1*beta1) ) )
    Z2 = ( (1.-2./(9.*A)) - ( (1.-2./A) / (1.+xx*sqrt( 2./(A-4.) )) )**(1./3.) ) &
         / sqrt( 2./(9.*A) )

    ! Omnibus test
    !==============
    K2 = Z1*Z1 + Z2*Z2
    p = exp(-0.5*K2)

    return
  end function k2test

!=======================================================================
! kde
!-----------------------------------------------------------------------
! kde computes the kernel density estimation assuming Gaussian kernels
! for univariate and bivariate data. The default bandwidth is calculated
! using Silverman's rule of thumb.
!
! Syntax
!-----------------------------------------------------------------------
! call kde(x, f, xi)
! call kde(x, f, xi, bw)
! call kde(A, f, xi, yi)
! call kde(A, f, xi, yi, H)
!
! Description
!-----------------------------------------------------------------------
! call kde(x, f, xi) returns the probability density estimation f at
! points xi for the sample data in the vector x.
!
! call kde(x, f, xi, bw) returns the PDE f at points xi using the kernel
! bandwidth bw.
!
! call kde(A, f, xi, yi) returns the PDE f at points xi and yi.
!
! call kde(A, f, xi, yi, H) returns the PDE f at points xi and yi using
! the bandwidth H.
!=======================================================================

  subroutine kde1(x, f, xi, bw)
    real(kind = RPRE), dimension(:), intent(in) :: x
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: f
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: xi
    real(kind = RPRE), intent(in), optional :: bw
    integer(kind = IPRE) :: ix, j, n, nx
    real(kind = RPRE) :: opt_bw
    real(kind = RPRE), dimension(:), allocatable :: opt_xi

    n = size(x)
    opt_bw = ( 4.*std(x)**5 / (3.*n) )**0.2
    if (present(bw)) opt_bw = bw
    if (present(xi) .and. allocated(xi)) then
      nx = size(xi)
      opt_xi = xi
    else
      nx = 100
      opt_xi = linspace(minval(x)-3*opt_bw, maxval(x)+3*opt_bw, nx)
    end if

    f = zeros(nx)
    do ix = 1, nx
      do j = 1, n
        f(ix) = f(ix) + exp( -0.5 * ( ( opt_xi(ix) - x(j) ) / opt_bw )**2 )
      end do
    end do
    f = 0.3989422804014327 * f / ( n * opt_bw )

    if (present(xi) .and. .not. allocated(xi)) xi = opt_xi
    return
  end subroutine kde1

  subroutine kde2(A, f, xi, yi, H)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: f
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: xi, yi
    real(kind = RPRE), dimension(:,:), intent(in), optional :: H
    integer(kind = IPRE) :: ix, iy, j, n, nx, ny
    real(kind = RPRE) :: opt_H(2,2), invH(2,2), x(2)
    real(kind = RPRE), dimension(:), allocatable :: opt_xi, opt_yi

    n = size(A, 1)
    if (present(H)) then
      opt_H = H
    else
      opt_H = cov(A) * n**(-1./3.)    ! Squared
    end if
    if (present(xi) .and. allocated(xi)) then
      nx = size(xi)
      opt_xi = xi
    else
      nx = 100
      opt_xi = linspace(minval(A(:,1))-3*opt_H(1,1), maxval(A(:,1))+3*opt_H(1,1), nx)
    end if
    if (present(yi) .and. allocated(yi)) then
      ny = size(yi)
      opt_yi = yi
    else
      ny = 100
      opt_yi = linspace(minval(A(:,2))-3*opt_H(2,2), maxval(A(:,2))+3*opt_H(2,2), ny)
    end if

    invH = inv(opt_H)
    f = zeros(nx, ny)
    do ix = 1, nx
      do iy = 1, ny
        do j = 1, n
          x = [ opt_xi(ix), opt_yi(iy) ] - [ A(j,:) ]
          f(ix,iy) = f(ix,iy) + exp( -0.5 * dot_product(matmul(x, invH), x) )
        end do
      end do
    end do
    f = f / ( sqrt( det( real(2.*pi, RPRE) * opt_H ) ) * real(n, RPRE) )

    if (present(xi) .and. .not. allocated(xi)) xi = opt_xi
    if (present(yi) .and. .not. allocated(yi)) yi = opt_yi
    return
  end subroutine kde2

!=======================================================================
! kurtosis
!-----------------------------------------------------------------------
! kurtosis computes vector and matrix kurtosis.
!
! Syntax
!-----------------------------------------------------------------------
! y = kurtosis(x)
! y = kurtosis(x, flag)
! x = kurtosis(A)
! x = kurtosis(A, flag)
! x = kurtosis(A, 1)
! x = kurtosis(A, flag, 1)
! x = kurtosis(A, 2)
! x = kurtosis(A, flag, 2)
!
! Description
!-----------------------------------------------------------------------
! y = kurtosis(x) returns the kurtosis of the vector x.
!
! y = kurtosis(x, w) returns the kurtosis of the vector x given the
! flag. By default, flag is 1. If flag is 0, the function corrects for
! the systematic bias due to the size of the sample.
!
! x = kurtosis(A) returns a dim2 vector with the kurtosis of each
! column of matrix A.
!
! x = kurtosis(A, flag) returns a dim2 vector given the flag.
!
! x = kurtosis(A, 1) (see x = kurtosis(A)).
!
! x = kurtosis(A, flag, 1) (see x = kurtosis(A, flag))
!
! x = kurtosis(A, 2) returns a dim1 vector with the kurtosis of
! each row of matrix A.
!
! x = kurtosis(A, flag, 2) returns a dim1 vector given the flag.
!=======================================================================

  real(kind = RPRE) function kurtosis1(x, flag)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: flag
    integer(kind = IPRE) :: opt_flag, n

    opt_flag = 1
    if (present(flag)) opt_flag = flag

    n = size(x)
    kurtosis1 = ( sum( ( x - mean(x) )**4 ) / n ) / ( var(x, 1)**2 )
    if (opt_flag .eq. 0) then
      kurtosis1 = (n-1)/((n-2)*(n-3)) * ((n+1)*kurtosis1-3*(n-1)) + 3
    end if
    return
  end function kurtosis1

  function kurtosis2(A, flag, dim)
    real(kind = RPRE), dimension(:), allocatable :: kurtosis2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: flag, dim
    integer(kind = IPRE) :: opt_flag, i, m, n

    opt_flag = 1
    if (present(flag)) opt_flag = flag

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(kurtosis2(n))
      do i = 1, n
        kurtosis2(i) = kurtosis1(A(:,i), opt_flag)
      end do
    elseif (dim .eq. 2) then
      allocate(kurtosis2(m))
      do i = 1, m
        kurtosis2(i) = kurtosis1(A(i,:), opt_flag)
      end do
    end if
    return
  end function kurtosis2

!=======================================================================
! linspace
!-----------------------------------------------------------------------
! linspace creates a linearly spaced vector.
!
! Syntax
!-----------------------------------------------------------------------
! x = linspace(x1, x2, n)
!
! Description
!-----------------------------------------------------------------------
! x = linspace(x1, x2, n) returns a vector of n evenly spaced points
! between x1 and x2.
!
! Examples
!-----------------------------------------------------------------------
! x = linspace(0, 10, 11)
!     0.  1.  2.  3.  4.  5.  6.  7.  8.  9.  10.
!=======================================================================

  function linspace_r8r8(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_r8r8
    real(kind = 8), intent(in) :: first, last
    integer(kind = IPRE), intent(in) :: n
    integer(kind = IPRE) :: i
    real(kind = 8) :: step

    allocate(linspace_r8r8(n))
    step = ( last - first ) / ( n-1 )
    linspace_r8r8 = first + step * real([ ( i-1, i = 1, n ) ], RPRE)
    return
  end function linspace_r8r8

  function linspace_r4r4(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_r4r4
    real(kind = 4), intent(in) :: first, last
    integer(kind = IPRE), intent(in) :: n

    linspace_r4r4 = linspace(real(first, kind = 8), real(last, kind = 8), n)
    return
  end function linspace_r4r4

  function linspace_i4i4(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_i4i4
    integer(kind = 4), intent(in) :: first, last
    integer(kind = IPRE), intent(in) :: n

    linspace_i4i4 = linspace(real(first, kind = 8), real(last, kind = 8), n)
    return
  end function linspace_i4i4

  function linspace_r8i4(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_r8i4
    real(kind = 8), intent(in) :: first
    integer(kind = 4), intent(in) :: last
    integer(kind = IPRE), intent(in) :: n

    linspace_r8i4 = linspace(first, real(last, kind = 8), n)
    return
  end function linspace_r8i4

  function linspace_r4i4(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_r4i4
    real(kind = 4), intent(in) :: first
    integer(kind = 4), intent(in) :: last
    integer(kind = IPRE), intent(in) :: n

    linspace_r4i4 = linspace(real(first, kind = 8), real(last, kind = 8), n)
    return
  end function linspace_r4i4

  function linspace_i4r8(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_i4r8
    integer(kind = 4), intent(in) :: first
    real(kind = 8), intent(in) :: last
    integer(kind = IPRE), intent(in) :: n

    linspace_i4r8 = linspace(real(first, kind = 8), last, n)
    return
  end function linspace_i4r8

  function linspace_i4r4(first, last, n)
    real(kind = RPRE), dimension(:), allocatable :: linspace_i4r4
    integer(kind = 4), intent(in) :: first
    real(kind = 4), intent(in) :: last
    integer(kind = IPRE), intent(in) :: n

    linspace_i4r4 = linspace(real(first, kind = 8), real(last, kind = 8), n)
    return
  end function linspace_i4r4

!=======================================================================
! loadbin
!-----------------------------------------------------------------------
! loadbin loads binary files.
!
! Syntax
!-----------------------------------------------------------------------
! x = loadbin(filename)
! x = loadbin(filename, kind)
! x = loadbin(filename, kind, dim1)
! A = loadbin(filename, kind, dim1, dim2)
! X = loadbin(filename, kind, dim1, dim2, dim3)
!
! Description
!-----------------------------------------------------------------------
! x = loadbin(filename) loads a 1-dimensional array into x from the
! binary file filename treated as 32 bytes floating points.
!
! x = loadbin(filename, kind) loads a 1-dimensional array into x from
! the binary file filename.
!
! x = loadbin(filename, kind, dim1) loads a 1-dimensional array into x
! from the binary file filename.
!
! A = loadbin(filename, kind, dim1, dim2) loads a 2-dimensional array
! into A from the binary file filename.
!
! X = loadbin(filename, kind, dim1, dim2, dim3) loads a 3-dimensional
! array into X from the binary file filename.
!
! Notes
!-----------------------------------------------------------------------
! Make sure to use the exact kind:
!   -   4 for 32 bytes floating points,
!   -   8 for 64 bytes floating points.
!=======================================================================

  function loadbin0(filename, kind)
    real(kind = RPRE), dimension(:), allocatable :: loadbin0
    character(len = *), intent(in) :: filename
    integer(kind = IPRE), intent(in), optional :: kind
    integer(kind = IPRE) :: opt_kind, dim1, fs
    real(kind = 4), dimension(:), allocatable :: tmp4
    real(kind = 8), dimension(:), allocatable :: tmp8
    type(File) :: infile

    opt_kind = 4
    if (present(kind)) opt_kind = kind

    infile = File(999, trim(filename))
    inquire(file = filename, size = fs)
    select case(opt_kind)
      case(4)
        if ( mod(fs, 4) .eq. 0 ) then
          dim1 = fs / 4
          allocate(tmp4(dim1), loadbin0(dim1))
          call infile%open(4*dim1)
          read(infile%unit, rec = 1) tmp4
          call infile%close()
          loadbin0 = tmp4
        else
          print *, "Error: in loadbin, file size mismatches kind."
          stop
        end if
      case(8)
        if ( mod(fs, 8) .eq. 0 ) then
          dim1 = fs / 8
          allocate(tmp8(dim1), loadbin0(dim1))
          call infile%open(8*dim1)
          read(infile%unit, rec = 1) tmp8
          call infile%close()
          loadbin0 = tmp8
        else
          print *, "Error: in loadbin, file size mismatches kind."
          stop
        end if
    end select
    return
  end function loadbin0

  function loadbin1(filename, kind, dim1)
    real(kind = RPRE), dimension(:), allocatable :: loadbin1
    character(len = *), intent(in) :: filename
    integer(kind = IPRE), intent(in) :: kind, dim1
    real(kind = 4), dimension(:), allocatable :: tmp4
    real(kind = 8), dimension(:), allocatable :: tmp8
    type(File) :: infile

    allocate(loadbin1(dim1))
    infile = File(999, trim(filename))
    select case(kind)
      case(4)
        allocate(tmp4(dim1))
        call infile%open(4*dim1)
        read(infile%unit, rec = 1) tmp4
        call infile%close()
        loadbin1 = tmp4
      case(8)
        allocate(tmp8(dim1))
        call infile%open(8*dim1)
        read(infile%unit, rec = 1) tmp8
        call infile%close()
        loadbin1 = tmp8
    end select
    return
  end function loadbin1

  function loadbin2(filename, kind, dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: loadbin2
    character(len = *), intent(in) :: filename
    integer(kind = IPRE), intent(in) :: kind, dim1, dim2
    real(kind = 4), dimension(:,:), allocatable :: tmp4
    real(kind = 8), dimension(:,:), allocatable :: tmp8
    type(File) :: infile

    allocate(loadbin2(dim1, dim2))
    infile = File(999, trim(filename))
    select case(kind)
      case(4)
        allocate(tmp4(dim1, dim2))
        call infile%open(4*dim1*dim2)
        read(infile%unit, rec = 1) tmp4
        call infile%close()
        loadbin2 = tmp4
      case(8)
        allocate(tmp8(dim1, dim2))
        call infile%open(8*dim1*dim2)
        read(infile%unit, rec = 1) tmp8
        call infile%close()
        loadbin2 = tmp8
    end select
    return
  end function loadbin2

  function loadbin3(filename, kind, dim1, dim2, dim3)
    real(kind = RPRE), dimension(:,:,:), allocatable :: loadbin3
    character(len = *), intent(in) :: filename
    integer(kind = IPRE), intent(in) :: kind, dim1, dim2, dim3
    real(kind = 4), dimension(:,:,:), allocatable :: tmp4
    real(kind = 8), dimension(:,:,:), allocatable :: tmp8
    type(File) :: infile

    allocate(loadbin3(dim1, dim2, dim3))
    infile = File(999, trim(filename))
    select case(kind)
      case(4)
        allocate(tmp4(dim1, dim2, dim3))
        call infile%open(4*dim1*dim2*dim3)
        read(infile%unit, rec = 1) tmp4
        call infile%close()
        loadbin3 = tmp4
      case(8)
        allocate(tmp8(dim1, dim2, dim3))
        call infile%open(8*dim1*dim2*dim3)
        read(infile%unit, rec = 1) tmp8
        call infile%close()
        loadbin3 = tmp8
    end select
    return
  end function loadbin3

!=======================================================================
! loadtxt
!-----------------------------------------------------------------------
! loadtxt loads txt files.
!
! Syntax
!-----------------------------------------------------------------------
! x = loadtxt(filename)
! A = loadtxt(filename, dim2)
!
! Description
!-----------------------------------------------------------------------
! x = loadtxt(filename) loads a 1-dimensional array into x from a txt
! file filename.
!
! A = loadtxt(filename, dim2) loads a 2-dimensional array into A from a
! txt file filename. dim2 indicates the number of columns of the array.
!=======================================================================

  function loadtxt1(filename)
    real(kind = RPRE), dimension(:), allocatable :: loadtxt1
    character(len = *), intent(in) :: filename
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = infile%countlines()
    allocate(loadtxt1(m))
    call infile%open()
    do i = 1, m
      read(infile%unit,*) loadtxt1(i)
    end do
    call infile%close()
    return
  end function loadtxt1

  function loadtxt2(filename, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: loadtxt2
    character(len = *), intent(in) :: filename
    integer(kind = IPRE), intent(in) :: dim2
    integer(kind = IPRE) :: i, j, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = infile%countlines()
    allocate(loadtxt2(m, dim2))
    call infile%open()
    do i = 1, m
      read(infile%unit,*) (loadtxt2(i,j), j = 1, dim2)
    end do
    call infile%close()
    return
  end function loadtxt2

!=======================================================================
! log2
!-----------------------------------------------------------------------
! log2 computes the base 2 logarithm.
!
! Syntax
!-----------------------------------------------------------------------
! y = log2(x)
! Y = log2(X)
!
! Description
!-----------------------------------------------------------------------
! y = log2(x) returns the base 2 logarithm of x.
!
! Y = log2(X) returns a vector Y with the base 2 logarithm of the
! elements in X.
!
! Examples
!-----------------------------------------------------------------------
! y = log2(arange(1, 10))
!     0.000 1.000 1.585 2.000 2.322 2.585 2.807 3.000 3.170 3.322
!
! m = mod(log2([ 16, 17 ]), 1.)
!     0.  0.0875
!
! Note
!-----------------------------------------------------------------------
! The latter example shows how log2 can be used to tell whether a number
! is a power of 2. Fortran intrinsic binary substraction function iand
! can also be used for this purpose.
!=======================================================================

  real(kind = RPRE) function log2_i0(x)
    integer(kind = IPRE), intent(in) :: x

    log2_i0 = log(real(x)) / log(2.0d0)
    return
  end function log2_i0

  real(kind = RPRE) function log2_r0(x)
    real(kind = RPRE), intent(in) :: x

    log2_r0 = log(x) / log(2.0d0)
    return
  end function log2_r0

  function log2_i1(x)
    real(kind = RPRE), dimension(:), allocatable :: log2_i1
    integer(kind = IPRE), dimension(:), intent(in) :: x

    log2_i1 = log(real(x)) / log(2.0d0)
    return
  end function log2_i1

  function log2_r1(x)
    real(kind = RPRE), dimension(:), allocatable :: log2_r1
    real(kind = RPRE), dimension(:), intent(in) :: x

    log2_r1 = log(x) / log(2.0d0)
    return
  end function log2_r1

!=======================================================================
! lsweight
!-----------------------------------------------------------------------
! lsweight computes the least-square inversion weights.
!
! Syntax
!-----------------------------------------------------------------------
! W = lsweight(r, "none")
! W = lsweight(r, "biweight")
!
! Description
!-----------------------------------------------------------------------
! W = lsweight(r, "none") returns the identity matrix.
!
! W = lsweight(r, "biweight") returns the weights using a biweight norm.
!=======================================================================

  function lsweight(r, ntype)
    real(kind = RPRE), dimension(:,:), allocatable :: lsweight
    real(kind = RPRE), dimension(:), intent(in) :: r
    character(len = *), intent(in) :: ntype
    integer(kind = IPRE) :: i, n
    real(kind = RPRE) :: eps

    eps = 4.685d0 * mad(r, 2) / 0.6745d0
    n = size(r)
    if ((eps .eq. 0.0d0) .or. (ntype .eq. "none")) then
      lsweight = eye(n)
    elseif (ntype .eq. "biweight") then
      lsweight = zeros(n, n)
      do i = 1, n
        if (abs(r(i)) .le. eps) lsweight(i,i) = (1.0d0 - (r(i)/eps)**2)**2
      end do
    end if
    return
  end function lsweight

!=======================================================================
! lu
!-----------------------------------------------------------------------
! lu computes the LU matrix factorization.
!
! Syntax
!-----------------------------------------------------------------------
! call lu(A, L, U)
!
! Description
!-----------------------------------------------------------------------
! call lu(A, L, U) returns the LU matrix factorization of the input
! square m-by-m matrix A. The output matrices are:
!   -   L is a m-by-m lower triangular matrix with ones on the diagonal,
!   -   U is a m-by-m upper triangular matrix.
!
! Examples
!-----------------------------------------------------------------------
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! call lu(A, L, U)
! call disp(L)
!     1.  0.  0.
!     4.  1.  0.
!     7.  2.  1.
! call disp(U)
!     1.  2.  3.
!     0. -3. -6.
!     0.  0.  0.
! call disp(matmul(L, U))
!     1.  2.  3.
!     4.  5.  6.
!     7.  8.  9.
!=======================================================================

  subroutine lu(A, L, U)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: L, U
    integer(kind = IPRE) :: i, j, k, m

    if (issquare(A)) then
      m = size(A, 1)
      if (.not. allocated(L)) L = eye(m)
      if (.not. allocated(U)) U = zeros(m, m)

      do i = 1, m
        do j = 1, m
          U(i,j) = A(i,j)
          do k = 1, i-1
            U(i,j) = U(i,j) - L(i,k)*U(k,j)
          end do
        end do
        do j = i+1, m
          L(j,i) = A(j,i)
          do k = 1, i-1
            L(j,i) = L(j,i) - L(j,k)*U(k,i)
          end do
          L(j,i) = L(j,i)/U(i,i)
        end do
      end do
    else
      stop "Error: in A = LU, A should be square."
    end if
    return
  end subroutine lu

!=======================================================================
! kmeans
!-----------------------------------------------------------------------
! kmeans performs K-means clustering.
!
! Syntax
!-----------------------------------------------------------------------
! idx = kmeans(x, K, [options = ])
! idx = kmeans(A, K, [options = ])
!
! Description
!-----------------------------------------------------------------------
! idx = kmeans(x, K, [options = ]) returns the cluster indices of each
! element in vector x.
!
! idx = kmeans(A, K, [options = ]) returns the cluster indices of each
! rows in matrix A.
!
! Options
!-----------------------------------------------------------------------
! means               Output centroids
! init                Initial centroids
! itermax = 1000      Maximum number of iterations
! niter               Output number of iterations
!
! Notes
!-----------------------------------------------------------------------
! By default, initials centroids are randomly chosen among data points.
!=======================================================================

  function kmeans1(x, K, means, init, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), dimension(:), intent(in), optional :: init
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: means
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter
    integer(kind = IPRE) :: opt_itermax, n, iter
    real(kind = RPRE), dimension(:,:), allocatable :: m1
    real(kind = RPRE), dimension(:,:), allocatable :: opt_init, A

    n = size(x)

    opt_itermax = 100
    if (present(itermax)) opt_itermax = itermax
    if (present(init)) then
      opt_init = reshape( init, shape = [ K, 1 ], order = [ 1, 2 ])
    else
      opt_init = reshape( x(randperm(n, K)), shape = [ K, 1 ], order = [ 1, 2 ])
    end if

    A = reshape( x, shape = [ n, 1 ], order = [ 1, 2 ] )
    idx = kmeans2(A, K, m1, opt_init, opt_itermax, iter)

    if (present(niter)) niter = iter
    if (present(means)) means = [ m1 ]

    return
  end function kmeans1

  function kmeans2(A, K, means, init, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), dimension(:,:), intent(in), optional :: init
    real(kind = RPRE), dimension(:,:), allocatable, intent(inout), optional :: means
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter
    integer(kind = IPRE) :: opt_itermax, i, n, p, iter
    real(kind = RPRE), dimension(:,:), allocatable :: opt_init, m, m1

    n = size(A, 1)
    p = size(A, 2)

    opt_itermax = 1000
    if (present(itermax)) opt_itermax = itermax
    if (present(init)) then
      opt_init = init
    else
      opt_init = A(randperm(n, K),:)
    end if

    ! Initialization
    !================
    m = opt_init
    idx = update_index(A, m)
    m1 = update_means(A, idx)

    ! Loop until convergence
    !========================
    iter = 0
    do while ( ( means_residuals(m, m1) .gt. 1.0d-10 ) &
               .and. ( iter .lt. opt_itermax ) )
      iter = iter + 1
      m = m1
      idx = update_index(A, m)
      m1 = update_means(A, idx)
    end do

    if (present(niter)) niter = iter
    if (present(means)) means = m1

    return
  contains

    !-------------------------------------------------------------------
    ! update_index
    !-------------------------------------------------------------------
    function update_index(A, means) result(idx)
      integer(kind = IPRE), dimension(:), allocatable :: idx
      real(kind = RPRE), dimension(:,:), intent(in) :: A, means
      integer(kind = IPRE) :: i, j, b(1)
      real(kind = RPRE), dimension(:), allocatable :: dist

      idx = zeros(n)
      do i = 1, n
        dist = zeros(K)
        do j = 1, K
          dist(j) = norm(A(i,:) - means(j,:))
        end do
        b = minloc(dist)
        idx(i) = b(1)
      end do
      return
    end function update_index

    !-------------------------------------------------------------------
    ! update_means
    !-------------------------------------------------------------------
    function update_means(A, idx) result(means)
      real(kind = RPRE), dimension(:,:), allocatable :: means
      real(kind = RPRE), dimension(:,:), intent(in) :: A
      integer(kind = IPRE), dimension(:), intent(in) :: idx
      integer(kind = IPRE) :: j

      means = zeros(K, p)
      do j = 1, K
        means(j,:) = mean(A(find(idx .eq. j),:))
      end do
      return
    end function update_means

    !-------------------------------------------------------------------
    ! means_residuals
    !-------------------------------------------------------------------
    function means_residuals(means1, means2) result(eps)
      real(kind = RPRE) :: eps
      real(kind = RPRE), dimension(:,:), intent(in) :: means1, means2
      real(kind = RPRE), dimension(:,:), allocatable :: means
      integer(kind = IPRE) :: k

      eps = 0.0d0
      means = abs( means2 - means1 )
      do k = 1, p
        eps = eps + sum(means(:,k))**2
      end do
      eps = sqrt(eps)
      return
    end function means_residuals

  end function kmeans2

!=======================================================================
! mad
!-----------------------------------------------------------------------
! mad computes the mean-absolute-deviation or the median-absolute
! -deviation of a vector.
!
! Syntax
!-----------------------------------------------------------------------
! y = mad(x)
! y = mad(x, 1)
! y = mad(x, 2)
! x = mad(A)
! x = mad(A, 1, 1)
! x = mad(A, 1, 2)
! x = mad(A, 2, 1)
! x = mad(A, 2, 2)
!
! Description
!-----------------------------------------------------------------------
! y = mad(x) returns the mean-absolute-deviation of the vector x.
!
! y = mad(x, 1) (see y = mad(x)).
!
! y = mad(x, 2) returns the median-absolute-deviation of the vector x.
!
! x = mad(A) returns a dim2 vector with the mean-absolute-deviation of
! each column of matrix A.
!
! x = mad(A, 1, 1) (see x = mad(A)).
!
! x = mad(A, 1, 2) returns a dim2 vector with the median-absolute
! -deviation of each column of matrix A.
!
! x = mad(A, 2, 1) returns a dim1 vector with the mean-absolute
! -deviation of each row of matrix A.
!
! x = mad(A, 2, 2) returns a dim1 vector with the median-absolute
! -deviation of each row of matrix A.
!=======================================================================

  real(kind = RPRE) function mad1(x, method)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: method

    if ((.not. present(method)) .or. (method .eq. 1)) then
      mad1 = mean(abs(x - mean(x)))
    elseif (method .eq. 2) then
      mad1 = median(abs(x - median(x)))
    end if
    return
  end function mad1

  function mad2(A, dim, method)
    real(kind = RPRE), dimension(:), allocatable :: mad2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim, method
    integer(kind = IPRE) :: i, m, n

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(mad2(n))
      if ((.not. present(method)) .or. (method .eq. 1)) then
        do i = 1, n
          mad2(i) = mad(A(:,i), 1)
        end do
      elseif (method .eq. 2) then
        do i = 1, n
          mad2(i) = mad(A(:,i), 2)
        end do
      end if
    elseif (dim .eq. 2) then
      allocate(mad2(m))
      if ((.not. present(method)) .or. (method .eq. 1)) then
        do i = 1, m
          mad2(i) = mad(A(i,:), 1)
        end do
      elseif (method .eq. 2) then
        do i = 1, m
          mad2(i) = mad(A(i,:), 2)
        end do
      end if
    end if
    return
  end function mad2

!=======================================================================
! mbkmeans
!-----------------------------------------------------------------------
! mbkmeans performs Mini-batch K-means clustering.
!
! Syntax
!-----------------------------------------------------------------------
! idx = mbkmeans(x, K, [options = ])
! idx = mbkmeans(A, K, [options = ])
!
! Description
!-----------------------------------------------------------------------
! idx = mbkmeans(x, K, [options = ]) returns the cluster indices of each
! element in vector x.
!
! idx = mbkmeans(A, K, [options = ]) returns the cluster indices of each
! rows in matrix A.
!
! Options
!-----------------------------------------------------------------------
! perc = 0.2          Size of the batch (percentage)
! means               Output centroids
! init                Initial centroids
! itermax = 50        Maximum number of iterations
! niter               Output number of iterations
!=======================================================================

  function mbkmeans1(x, K, perc, means, init, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), intent(in), optional :: perc
    real(kind = RPRE), dimension(:), intent(in), optional :: init
    real(kind = RPRE), dimension(:), allocatable, intent(inout), optional :: means
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter
    integer(kind = IPRE) :: opt_itermax, n, iter
    real(kind = RPRE) :: opt_perc
    real(kind = RPRE), dimension(:,:), allocatable :: m1
    real(kind = RPRE), dimension(:,:), allocatable :: opt_init, A

    n = size(x)

    opt_itermax = 50
    opt_perc = 0.2d0
    if (present(itermax)) opt_itermax = itermax
    if (present(perc)) opt_perc = perc
    if (present(init)) then
      opt_init = reshape( init, shape = [ K, 1 ], order = [ 1, 2 ])
    else
      opt_init = reshape( x(randperm(n, K)), shape = [ K, 1 ], order = [ 1, 2 ])
    end if

    A = reshape( x, shape = [ n, 1 ], order = [ 1, 2 ] )
    idx = mbkmeans2(A, K, perc, m1, opt_init, opt_itermax, iter)

    if (present(niter)) niter = iter
    if (present(means)) means = [ m1 ]

    return
  end function mbkmeans1

  function mbkmeans2(A, K, perc, means, init, itermax, niter) result(idx)
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in) :: K
    real(kind = RPRE), intent(in), optional :: perc
    real(kind = RPRE), dimension(:,:), intent(in), optional :: init
    real(kind = RPRE), dimension(:,:), allocatable, intent(inout), optional :: means
    integer(kind = IPRE), intent(in), optional :: itermax
    integer(kind = IPRE), intent(inout), optional :: niter
    integer(kind = IPRE) :: opt_itermax, n, p, bs, iter
    real(kind = RPRE) :: opt_perc
    integer(kind = IPRE), dimension(:), allocatable :: v
    real(kind = RPRE), dimension(:,:), allocatable :: opt_init, m, m1, B

    n = size(A, 1)
    p = size(A, 2)

    opt_itermax = 50
    opt_perc = 0.2d0
    if (present(itermax)) opt_itermax = itermax
    if (present(perc)) opt_perc = perc
    if (present(init)) then
      opt_init = init
    else
      opt_init = A(randperm(n, K),:)
    end if

    ! Initialization
    !================
    bs = nint(opt_perc*n)   ! Batch size
    m = opt_init            ! Initial centroids
    v = zeros(K)            ! Per-center counter

    ! Iterate until convergence
    !===========================
    do iter = 1, opt_itermax
      B = A(randperm(n, bs),:)        ! Batch
      m1 = m                          ! Previous means
      idx = cache_means(B, m)         ! Cache means
      call update_means(m, v, B, idx) ! Update means with gradient
      if ( means_residuals(m, m1) .lt. 1.0d-2 ) exit
    end do

    idx = cache_means(A, m)

    if (present(niter)) niter = iter - 1
    if (present(means)) means = m

    return
  contains

    !-------------------------------------------------------------------
    ! cache_means
    !-------------------------------------------------------------------
    function cache_means(A, means) result(idx)
      integer(kind = IPRE), dimension(:), allocatable :: idx
      real(kind = RPRE), dimension(:,:), intent(in) :: A, means
      integer(kind = IPRE) :: i, j, n, b(1)
      real(kind = RPRE), dimension(:), allocatable :: dist

      n = size(A, 1)
      idx = zeros(n)
      dist = zeros(K)
      do i = 1, n
        dist = 0.0d0
        do j = 1, K
          dist(j) = norm(A(i,:) - means(j,:))
        end do
        b = minloc(dist)
        idx(i) = b(1)
      end do
      return
    end function cache_means

    !-------------------------------------------------------------------
    ! update_means
    !-------------------------------------------------------------------
    subroutine update_means(means, v, A, idx)
      real(kind = RPRE), dimension(:,:), intent(inout) :: means
      integer(kind = IPRE), dimension(:), intent(inout) :: v
      real(kind = RPRE), dimension(:,:), intent(in) :: A
      integer(kind = IPRE), dimension(:), intent(in) :: idx
      integer(kind = IPRE) :: i, n, c
      real(kind = RPRE) :: eta

      n = size(A, 1)
      do i = 1, n
        c = idx(i)
        v(c) = v(c) + 1
        eta = 1.0d0 / real(v(c), RPRE)
        means(c,:) = ( 1.0d0 - eta ) * means(c,:) + eta * A(i,:)
      end do
      return
    end subroutine update_means

    !-------------------------------------------------------------------
    ! means_residuals
    !-------------------------------------------------------------------
    function means_residuals(means1, means2) result(eps)
      real(kind = RPRE) :: eps
      real(kind = RPRE), dimension(:,:), intent(in) :: means1, means2
      real(kind = RPRE), dimension(:,:), allocatable :: means
      integer(kind = IPRE) :: k

      eps = 0.0d0
      means = abs( means2 - means1 )
      do k = 1, p
        eps = eps + sum(means(:,k))**2
      end do
      eps = sqrt(eps)
      return
    end function means_residuals

  end function mbkmeans2

!=======================================================================
! mean
!-----------------------------------------------------------------------
! mean computes the mean value of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = mean(x)
! x = mean(A)
! x = mean(A, 1)
! x = mean(A, 2)
!
! Description
!-----------------------------------------------------------------------
! y = mean(x) returns the mean value of the vector x.
!
! x = mean(A) returns a dim2 vector with the mean values of each column
! of matrix A.
!
! x = mean(A, 1) (see x = mean(A)).
!
! x = mean(A, 2) returns a dim1 vector with the mean values of each row
! of matrix A.
!
! Examples
!-----------------------------------------------------------------------
! x = [ 1., 2., 3. ]
! y = mean(x)
!     2.
!
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! x = mean(A)
!     4.  5.  6.
! x = mean(A, 2)
!     2.  5.  8.
!=======================================================================

  real(kind = RPRE) function mean1(x)
    real(kind = RPRE), dimension(:), intent(in) :: x

    mean1 = sum(x)/size(x)
    return
  end function mean1

  function mean2(A, dim)
    real(kind = RPRE), dimension(:), allocatable :: mean2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: i, m, n

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(mean2(n))
      do i = 1, n
        mean2(i) = mean(A(:,i))
      end do
    elseif (dim .eq. 2) then
      allocate(mean2(m))
      do i = 1, m
        mean2(i) = mean(A(i,:))
      end do
    end if
    return
  end function mean2

!=======================================================================
! median
!-----------------------------------------------------------------------
! median computes the median value of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = median(x)
! x = median(A)
! x = median(A, 1)
! x = median(A, 2)
!
! Description
!-----------------------------------------------------------------------
! y = median(x) returns the median value of the vector x.
!
! x = median(A) returns a dim2 vector with the median values of each
! column of matrix A.
!
! x = median(A, 1) (see x = median(A)).
!
! x = median(A, 2) returns a dim1 vector with the median values of each
! row of matrix A.
!
! Examples
!-----------------------------------------------------------------------
! x = [ 3., 1., 2. ]
! y = median(x)
!     2.
!
! x = [ 3., 4., 1., 2. ]
! y = median(x)
!     2.5
!
! A = reshape([ 3., 7., 1., 8., 2., 5., 4., 9., 6. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! x = median(A)
!     4.  7.  5.
! x = median(A, 2)
!     3.  5.  6.
!=======================================================================

  real(kind = RPRE) function median1(x)
    real(kind = RPRE), dimension(:), intent(in) :: x
    real(kind = RPRE), dimension(:), allocatable :: x_sort
    integer(kind = IPRE) :: i, n

    n = size(x)
    x_sort = sort(x)
    i = ceiling(real(n/2.0d0))
    if (mod(n, 2) .eq. 0) then
      median1 = (x_sort(i) + x_sort(i+1))/2
    else
      median1 = x_sort(i)
    end if
    return
  end function median1

  function median2(A, dim)
    real(kind = RPRE), dimension(:), allocatable :: median2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: i, m, n

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(median2(n))
      do i = 1, n
        median2(i) = median(A(:,i))
      end do
    elseif (dim .eq. 2) then
      allocate(median2(m))
      do i = 1, m
        median2(i) = median(A(i,:))
      end do
    end if
    return
  end function median2

!=======================================================================
! meshgrid
!-----------------------------------------------------------------------
! meshgrid generates rectangular grid in 2 dimensions.
!
! Syntax
!-----------------------------------------------------------------------
! call meshgrid(ax, ay, X, Y)
!
! Description
!-----------------------------------------------------------------------
! call meshgrid(ax, ay, X, Y) returns replicated grid vectors of ax and
! ay that form a full grid.
!
! Examples
!-----------------------------------------------------------------------
! ax = linspace(1, 3, 3)
! ay = linspace(10, 14, 5)
! call meshgrid(ax, ay, X, Y)
! X =
!     1.   2.   3.
!     1.   2.   3.
!     1.   2.   3.
!     1.   2.   3.
!     1.   2.   3.
! Y =
!    10.  10.  10.
!    11.  11.  11.
!    12.  12.  12.
!    13.  13.  13.
!    14.  14.  14.
!=======================================================================

  subroutine meshgrid2(ax, ay, x, y)
    real(kind = RPRE), dimension(:), intent(in) :: ax, ay
    real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: x, y
    integer(kind = IPRE) :: i, m, n

    m = size(ax)
    n = size(ay)
    if (.not. allocated(x)) allocate(x(n, m))
    if (.not. allocated(y)) allocate(y(n, m))
    do i = 1, n
      x(i,:) = ax
    end do
    do i = 1, m
      y(:,i) = ay
    end do
    return
  end subroutine meshgrid2

!=======================================================================
! mpi_rpre
!-----------------------------------------------------------------------
! mpi_rpre returns either MPI_REAL or MPI_DOUBLE depending on RPRE.
!
! Notes
!-----------------------------------------------------------------------
! When calling MPI functions, use mpi_rpre instead of MPI_REAL or
! MPI_DOUBLE.
!=======================================================================

#ifdef do_mpi
  integer(kind = 4) function mpi_rpre()
    select case(RPRE)
    case(4)
      mpi_rpre = mpi_real
    case(8)
      mpi_rpre = mpi_double
    end select
    return
  end function mpi_rpre
#endif

!=======================================================================
! nextpow2
!-----------------------------------------------------------------------
! nextpow2 computes the exponent of the next higher power of 2.
!
! Syntax
!-----------------------------------------------------------------------
! p = nextpow2(x)
! P = nextpow2(X)
!
! Description
!-----------------------------------------------------------------------
! p = nextpow2(x) returns the exponent for the smallest power of two
! that satisfy 2**p .le. abs(x).
!
! P = nextpow2(X) returns the next power of 2 of each element in
! vector x.
!
! Examples
!-----------------------------------------------------------------------
! y = nextpow2(15)
!     4
!
! x = [ 1, -2, 3, -4, 5, 9, 519 ]
! y = nextpow2(x)
!     0   1   2   2   3   4   10
!=======================================================================

  function nextpow2_0(x) result(pow)
    integer(kind = IPRE) :: pow
    integer(kind = IPRE), intent(in) :: x

    pow = ceiling( log( real( abs(x) ) ) / log(2.) )
    return
  end function nextpow2_0

  function nextpow2_1(x) result(pow)
    integer(kind = IPRE), dimension(:), allocatable :: pow
    integer(kind = IPRE), dimension(:), intent(in) :: x

    pow = ceiling( log( real( abs(x) ) ) / log(2.) )
    return
  end function nextpow2_1

!=======================================================================
! norm
!-----------------------------------------------------------------------
! norm computes vector and matrix norms.
!
! Syntax
!-----------------------------------------------------------------------
! y = norm(x)
! y = norm(x, p)
! x = norm(A)
! x = norm(A, p)
!
! Description
!-----------------------------------------------------------------------
! y = norm(x) returns the 2-norm or Euclidian norm of vector x.
!
! y = norm(x, p) returns the p-norm of vector x, where p is any positive
! real value.
!
! x = norm(A) returns the 2-norm of matrix A (largest singular value).
!
! x = norm(A, p) returns the p-norm of matrix A, where p is {1, 2}.
!
! Examples
!-----------------------------------------------------------------------
! x = [ 1., 2., 3. ]
! y = norm(x)
!     3.74165750
! y = norm(x, 3.)
!     3.30192733
!=======================================================================

  real(kind = RPRE) function norm1(x, p)
    real(kind = RPRE), dimension(:), intent(in) :: x
    real(kind = RPRE), intent(in), optional :: p

    if ((.not. present(p)) .or. (p .eq. 2.)) then
      norm1 = sqrt(sum(abs(x)**2))
    elseif (p .eq. 1.) then
      norm1 = sum(abs(x))
    else
      norm1 = (sum(abs(x)**p))**(1.0d0/p)
    end if
    return
  end function norm1

  real(kind = RPRE) function norm2(A, p)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: p
    real(kind = RPRE), dimension(:), allocatable :: w

    if ((.not. present(p)) .or. (p .eq. 2.)) then
      call svd(A, w)
      norm2 = maxval(w)
    elseif (p .eq. 1.) then
      norm2 = maxval(sum(abs(A), dim = 2))
    end if
    return
  end function norm2

!=======================================================================
! normpdf
!-----------------------------------------------------------------------
! normpdf computes the normal probability density function.
!
! Syntax
!-----------------------------------------------------------------------
! y = normpdf(x, mu, sigma)
! y = normpdf(X, mu, sigma)
!
! Description
!-----------------------------------------------------------------------
! y = normpdf(x, mu, sigma) returns the PDF at each of the points in
! vector x using the normal distribution with mean mu and standard
! deviation sigma.
!
! y = normpdf(X, mu, sigma) returns the PDF at each of the
! multidimensional points in matrix X using the normal distribution with
! mean mu and covariance matrix sigma.
!=======================================================================

  real(kind = RPRE) function normpdf0(x, mu, sigma) result(pdf)
    real(kind = RPRE), intent(in) :: x, mu, sigma

    pdf = exp( -0.5d0 * ( x - mu )**2 / sigma**2 )
    pdf = pdf / ( sigma * sqrt(2.0d0 * pi) )
    return
  end function normpdf0

  function normpdf1(x, mu, sigma) result(pdf)
    real(kind = RPRE), dimension(:), allocatable :: pdf
    real(kind = RPRE), dimension(:), intent(in) :: x
    real(kind = RPRE), intent(in) :: mu, sigma

    pdf = exp( -0.5d0 * ( x - mu )**2 / sigma**2 )
    pdf = pdf / ( sigma * sqrt(2.0d0 * pi) )
    return
  end function normpdf1

  function normpdf2(X, mu, sigma) result(pdf)
    real(kind = RPRE), dimension(:), allocatable :: pdf
    real(kind = RPRE), dimension(:,:), intent(in) :: X, sigma
    real(kind = RPRE), dimension(:), intent(in) :: mu
    integer(kind = IPRE) :: n
    real(kind = RPRE), dimension(:,:), allocatable :: tmp

    n = size(X, 2)
    tmp = X - repmat(mu, size(X, 1), 2)
    pdf = exp( -0.5d0 *sum(matmul(tmp, inv(sigma)) * tmp, dim = 2) )
    pdf = pdf / sqrt( (2.0d0 * pi)**n * det(sigma) )
    return
  end function normpdf2

!=======================================================================
! num2str
!-----------------------------------------------------------------------
! num2str converts numbers to strings.
!
! Syntax
!-----------------------------------------------------------------------
! str = num2str(x)
! str = num2str(x, fmt)
!
! Description
!-----------------------------------------------------------------------
! str = num2str(x) converts x into a string.
!
! str = num2str(x, fmt) converts x into a string with the format fmt.
!
! Examples
!-----------------------------------------------------------------------
! print *, "Percentage: " // num2str(50.431, "(F6.2)") // "%"
!     Percentage: 50.43%
!=======================================================================

  function num2str_i4(x, fmt)
    character(len = :), allocatable :: num2str_i4
    integer(kind = 4), intent(in) :: x
    character(len = *), intent(in), optional :: fmt
    character(len = CLEN) :: xstr

    if (present(fmt)) then
      write(xstr, fmt) x
    else
      write(xstr, *) x
    end if
    xstr = adjustl(xstr)
    num2str_i4 = trim(xstr)
    return
  end function num2str_i4

  function num2str_i8(x, fmt)
    character(len = :), allocatable :: num2str_i8
    integer(kind = 8), intent(in) :: x
    character(len = *), intent(in), optional :: fmt
    character(len = CLEN) :: xstr

    if (present(fmt)) then
      write(xstr, fmt) x
    else
      write(xstr, *) x
    end if
    xstr = adjustl(xstr)
    num2str_i8 = trim(xstr)
    return
  end function num2str_i8

  function num2str_r4(x, fmt)
    character(len = :), allocatable :: num2str_r4
    real(kind = 4), intent(in) :: x
    character(len = *), intent(in), optional :: fmt
    character(len = CLEN) :: xstr

    if (present(fmt)) then
      write(xstr, fmt) x
    else
      write(xstr, *) x
    end if
    xstr = adjustl(xstr)
    num2str_r4 = trim(xstr)
    return
  end function num2str_r4

  function num2str_r8(x, fmt)
    character(len = :), allocatable :: num2str_r8
    real(kind = 8), intent(in) :: x
    character(len = *), intent(in), optional :: fmt
    character(len = CLEN) :: xstr

    if (present(fmt)) then
      write(xstr, fmt) x
    else
      write(xstr, *) x
    end if
    xstr = adjustl(xstr)
    num2str_r8 = trim(xstr)
    return
  end function num2str_r8

!=======================================================================
! ones
!-----------------------------------------------------------------------
! ones creates array all of ones.
!
! Syntax
!-----------------------------------------------------------------------
! x = ones(dim1)
! A = ones(dim1, dim2)
! X = ones(dim1, dim2, dim3)
!
! Description
!-----------------------------------------------------------------------
! x = ones(dim1) returns a dim1 vector of ones.
!
! A = ones(dim1, dim2) returns a dim1-by-dim2 matrix of ones.
!
! X = ones(dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of ones.
!
! Examples
!-----------------------------------------------------------------------
! x = ones(3)
! x =
!     1.  1.  1.
!
! A = ones(3, 3)
! A =
!     1.  1.  1.
!     1.  1.  1.
!     1.  1.  1.
!=======================================================================

  function ones1(dim1)
    real(kind = RPRE), dimension(:), allocatable :: ones1
    integer(kind = IPRE), intent(in) :: dim1
    integer(kind = IPRE) :: ierr

    allocate(ones1(dim1), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in ones, could not allocate array."
      stop
    else
      ones1 = 1.0d0
    end if
    return
  end function ones1

  function ones2(dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: ones2
    integer(kind = IPRE), intent(in) :: dim1, dim2
    integer(kind = IPRE) :: ierr

    allocate(ones2(dim1, dim2), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in ones, could not allocate array."
      stop
    else
      ones2 = 1.0d0
    end if
    return
  end function ones2

  function ones3(dim1, dim2, dim3)
    real(kind = RPRE), dimension(:,:,:), allocatable :: ones3
    integer(kind = IPRE), intent(in) :: dim1, dim2, dim3
    integer(kind = IPRE) :: ierr

    allocate(ones3(dim1, dim2, dim3), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in ones, could not allocate array."
      stop
    else
      ones3 = 1.0d0
    end if
    return
  end function ones3

!=======================================================================
! open
!-----------------------------------------------------------------------
! open opens a File object with sequential or direct access.
!
! Syntax
!-----------------------------------------------------------------------
! call ofile%open()
! call ofile%open(r)
!
! Description
!-----------------------------------------------------------------------
! call ofile%open() open the File object ofile with sequential access.
!
! call ofile%open(r) open the File object ofile with direct access,
! where r is the record length.
!=======================================================================

  subroutine open1(self)
    class(File), intent(inout) :: self
    integer(kind = IPRE) :: ierr

    open(unit = self%unit, file = self%filename, access = "sequential", &
      form = "formatted", status = "unknown", iostat = ierr)
    if (ierr .ne. 0) then
      print *, "Error: Cannot read "//self%filename//" ."
      stop
    end if
    return
  end subroutine open1

  subroutine open2(self, r)
    class(File), intent(inout) :: self
    integer(kind = IPRE), intent(in) :: r
    integer(kind = IPRE) :: ierr

    open(unit = self%unit, file = self%filename, access = "direct", &
      form = "unformatted", status = "unknown", recl = r, iostat = ierr)
    if (ierr .ne. 0) then
      print *, "Error: Cannot read "//self%filename//" ."
      stop
    end if
    return
  end subroutine open2

!=======================================================================
! pascal
!-----------------------------------------------------------------------
! pascal computes the Pascal's matrix.
!
! Syntax
!-----------------------------------------------------------------------
! A = pascal(n)
!
! Description
!-----------------------------------------------------------------------
! A = pascal(n) returns the Pascal's matrix of order n.
!
! Example
!-----------------------------------------------------------------------
! A = pascal(5)
!     1   1   1   1   1
!     1   2   3   4   5
!     1   3   6  10  15
!     1   4  10  20  35
!     1   5  15  35  70
!=======================================================================

  function pascal(n)
    integer(kind = IPRE), dimension(:,:), allocatable :: pascal
    integer(kind = IPRE), intent(in) :: n
    integer(kind = IPRE) :: i, j

    pascal = ones(n, n)
    do i = 2, n
      do j = 2, n
        pascal(i,j) = pascal(i-1,j) + pascal(i,j-1)
      end do
    end do
    return
  end function pascal

!=======================================================================
! prctile
!-----------------------------------------------------------------------
! prctile computes the percentiles of a data set.
!
! Syntax
!-----------------------------------------------------------------------
! q = prctile(x, p)
! Q = prctile(x, P)
!
! Description
!-----------------------------------------------------------------------
! q = prctile(x, p) returns the p-th percentile of vector x.
!
! Q = prctile(x, P) returns each percentile of vector x contained in
! vector P.
!=======================================================================

  real(kind = RPRE) function prctile0(x, p)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in) :: p
    real(kind = RPRE) :: tmp(1)

    tmp = prctile1(x, [ p ])
    prctile0 = tmp(1)
    return
  end function prctile0

  function prctile1(x, p)
    real(kind = RPRE), dimension(:), allocatable :: prctile1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), dimension(:), intent(in) :: p
    integer(kind = IPRE) :: i, nx, np, idx
    real(kind = RPRE), dimension(:), allocatable :: xsort, ap

    nx = size(x)
    np = size(p)
    prctile1 = zeros(np)
    xsort = sort(x)
    do i = 1, np
      if ( p(i) .le. 50.0d0/real(nx, RPRE) ) then
        prctile1(i) = xsort(1)
      elseif ( p(i) .ge. 100.0d0 * ( ( nx - 0.5d0 ) / real(nx, RPRE) ) ) then
        prctile1(i) = xsort(nx)
      else
        ap = 100.0d0 * ( linspace(1, nx, nx) - 0.5d0 ) / real(nx, RPRE)
        idx = maxval( find( p(i) .gt. ap ) )
        prctile1(i) = xsort(idx) &
                      + ( xsort(idx+1) - xsort(idx) ) * ( p(i) - ap(idx) ) &
                      / ( ap(idx+1) - ap(idx) )
      end if
    end do
    return
  end function prctile1

!=======================================================================
! progress_bar
!-----------------------------------------------------------------------
! Display a progression bar.
!
! Syntax
!-----------------------------------------------------------------------
! call progress_bar(iter, itermax, step)
!
! Description
!-----------------------------------------------------------------------
! call progress_bar(iter, itermax, step) displays a progress bar given
! the current iteration iter, the maximum number of iterations itermax,
! and the length of the bar steps.
!=======================================================================

  subroutine progress_bar(iter, itermax, step)
    integer(kind = IPRE), intent(in) :: iter, itermax
    integer(kind = IPRE), intent(in), optional :: step
    integer(kind = IPRE) :: i, perc, opt_step
    character(len = :), allocatable :: bar

    opt_step = 50
    if ( present(step) ) opt_step = step

    ! Initialize the bar
    bar = "  ["
    do i = 1, opt_step
      bar = bar // " "
    end do
    bar = bar // "]"

    ! Compute the percentage
    perc = real(iter) / real(itermax) * 100.

    ! Fill the bar
    do i = 1, floor( perc/(100./opt_step) )
      bar(3+i:3+i) = "="
    end do

    ! Place the percentage
    i = ceiling( (opt_step+2)/2. )
    write(bar(i+1:i+3), "(I3)") perc
    bar(i+4:i+4) = "%"

    ! Fill the space
    if (perc .lt. 100 .and. perc .gt. 50-100/opt_step) bar(i+1:i+1) = "="

    ! Return to the beginning of the line and display the bar
    write(*, "(A1, A)", advance = "no") char(13), bar
    return
  end subroutine progress_bar

!=======================================================================
! progress_perc
!-----------------------------------------------------------------------
! Display a progression percentage.
!
! Syntax
!-----------------------------------------------------------------------
! call progress_perc(iter, itermax, prefix)
!
! Description
!-----------------------------------------------------------------------
! call progress_perc(iter, itermax, prefix) displays a percentage given
! the current iteration iter, the maximum number of iterations itermax,
! and a prefix.
!=======================================================================

  subroutine progress_perc(iter, itermax, prefix)
    integer(kind = IPRE), intent(in) :: iter, itermax
    character(len = *), intent(in), optional :: prefix
    real(kind = RPRE) :: perc
    character(len = :), allocatable :: opt_prefix

    opt_prefix = ""
    if ( present(prefix) ) opt_prefix = prefix

    perc = real(iter) / real(itermax) * 100.
    write(*, "(A1, A, F6.2, A)", advance = "no") char(13), opt_prefix, perc, "%"
    return
  end subroutine progress_perc

!=======================================================================
! rng
!-----------------------------------------------------------------------
! rng controls random number generation.
!
! Syntax
!-----------------------------------------------------------------------
! call rng()
! call rng(seed)
!
! Description
!-----------------------------------------------------------------------
! call rng() uses the current date and time as seed for random number
! generation.
!
! call rng(seed) sets the input seed for random number generation.
!
! Notes
!-----------------------------------------------------------------------
! It is advised to call rng at the beginning of a program so that each
! run of the program produces different sequences of random numbers.
!=======================================================================

  subroutine rng(seed)
    integer(kind = IPRE), intent(in), optional :: seed
    integer(kind = 4) :: seed_size, values(8)
    integer(kind = 4), dimension(:), allocatable :: seed_put

    call random_seed(size = seed_size)
    allocate(seed_put(seed_size))
    if (present(seed)) then
      seed_put = seed
    else
      call date_and_time(values = values)
      seed_put = values(8) * values(7) * values(6)
    end if
    call random_seed(put = seed_put)
    return
  end subroutine rng

!=======================================================================
! randi
!-----------------------------------------------------------------------
! randi generates uniformly distributed random integers.
!
! Syntax
!-----------------------------------------------------------------------
! x = randi(imax)
! x = randi([imin, imax])
! x = randi(imax, dim1)
! x = randi([imin, imax], dim1)
! A = randi(imax, dim1, dim2)
! A = randi([imin, imax], dim1, dim2)
! X = randi(imax, dim1, dim2, dim3)
! X = randi([imin, imax], dim1, dim2, dim3)
!
! Description
!-----------------------------------------------------------------------
! x = randi(imax) returns a random scalar integer between 1 and imax.
!
! x = randi([imin, imax]) returns a random scalar integer between imin
! and imax.
!
! x = randi(imax, dim1) returns a dim1 vector of random scalar integers
! between 1 and imax.
!
! x = randi([imin, imax], dim1) returns a dim1 vector of random scalar
! integers between imin and imax.
!
! A = randi(imax, dim1, dim2) returns a dim1-by-dim2 matrix of random
! scalar integers between 1 and imax.
!
! A = randi([imin, imax], dim1, dim2) returns a dim1-by-dim2 matrix of
! random scalar integers between imin and imax.
!
! X = randi(imax, dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of random scalar integers between 1 and imax.
!
! X = randi([imin, imax], dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of random scalar integers between imin and imax.
!=======================================================================

  integer(kind = IPRE) function randi0_0(imax)
    integer(kind = IPRE), intent(in) :: imax

    randi0_0 = floor( randu0() * real(imax) ) + 1
    return
  end function randi0_0

  integer(kind = IPRE) function randi0_1(imax)
    integer(kind = IPRE), dimension(2), intent(in) :: imax

    randi0_1 = minval(imax) + nint( randu0() * real(maxval(imax) - minval(imax)) )
    return
  end function randi0_1

  function randi1_0(imax, dim1)
    integer(kind = IPRE), dimension(:), allocatable :: randi1_0
    integer(kind = IPRE), intent(in) :: imax, dim1

    randi1_0 = floor( randu1(dim1) * real(imax) ) + 1
    return
  end function randi1_0

  function randi1_1(imax, dim1)
    integer(kind = IPRE), dimension(:), allocatable :: randi1_1
    integer(kind = IPRE), dimension(2), intent(in) :: imax
    integer(kind = IPRE), intent(in) :: dim1

    randi1_1 = minval(imax) + nint( randu1(dim1) * real(maxval(imax) - minval(imax)) )
    return
  end function randi1_1

  function randi2_0(imax, dim1, dim2)
    integer(kind = IPRE), dimension(:,:), allocatable :: randi2_0
    integer(kind = IPRE), intent(in) :: imax, dim1, dim2

    randi2_0 = floor( randu2(dim1, dim2) * real(imax) ) + 1
    return
  end function randi2_0

  function randi2_1(imax, dim1, dim2)
    integer(kind = IPRE), dimension(:,:), allocatable :: randi2_1
    integer(kind = IPRE), dimension(2), intent(in) :: imax
    integer(kind = IPRE), intent(in) :: dim1, dim2

    randi2_1 = minval(imax) + nint( randu2(dim1, dim2) * real(maxval(imax) - minval(imax)) )
    return
  end function randi2_1

  function randi3_0(imax, dim1, dim2, dim3)
    integer(kind = IPRE), dimension(:,:,:), allocatable :: randi3_0
    integer(kind = IPRE), intent(in) :: imax, dim1, dim2, dim3

    randi3_0 = floor( randu3(dim1, dim2, dim3) * real(imax) ) + 1
    return
  end function randi3_0

  function randi3_1(imax, dim1, dim2, dim3)
    integer(kind = IPRE), dimension(:,:,:), allocatable :: randi3_1
    integer(kind = IPRE), dimension(2), intent(in) :: imax
    integer(kind = IPRE), intent(in) :: dim1, dim2, dim3

    randi3_1 = minval(imax) + nint( randu3(dim1, dim2, dim3) * real(maxval(imax) - minval(imax)) )
    return
  end function randi3_1

!=======================================================================
! randn
!-----------------------------------------------------------------------
! randn generates normally distributed random numbers using polar
! Box-Muller algorithm.
!
! Syntax
!-----------------------------------------------------------------------
! x = randn()
! x = randn(dim1)
!
! Description
!-----------------------------------------------------------------------
! x = randn() returns a single normally distributed random number with
! mean 0 and standard deviation 1.
!
! x = randn(dim1) returns a dim1 vector of normally distributed random
! numbers.
!
! A = randn(dim1, dim2) returns a dim1-by-dim2 matrix of normally
! distributed random numbers.
!
! X = randn(dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of normally distributed random numbers.
!
! Examples
!-----------------------------------------------------------------------
! x = randn(3)
!     -1.22003853  -0.211721316   0.522971511
!=======================================================================

  real(kind = RPRE) function randn0()
    real(kind = RPRE) :: u, v, s

    do
      u = 2.*randu() - 1.
      v = 2.*randu() - 1.
      s = u*u + v*v
      if ( (s .gt. 0.) .and. (s .lt. 1.) ) exit
    end do
    randn0 = u * sqrt( -2.0d0 * log(s) / s )
    return
  end function randn0

  function randn1(dim1)
    real(kind = RPRE), dimension(:), allocatable :: randn1
    integer(kind = IPRE), intent(in) :: dim1
    integer(kind = IPRE) :: i

    allocate(randn1(dim1))
    do i = 1, dim1
      randn1(i) = randn0()
    end do
    return
  end function randn1

  function randn2(dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: randn2
    integer(kind = IPRE), intent(in) :: dim1, dim2
    integer(kind = IPRE) :: i, j

    allocate(randn2(dim1, dim2))
    do i = 1, dim1
      do j = 1, dim2
        randn2(i,j) = randn()
      end do
    end do
    return
  end function randn2

  function randn3(dim1, dim2, dim3)
    real(kind = RPRE), dimension(:,:,:), allocatable :: randn3
    integer(kind = IPRE), intent(in) :: dim1, dim2, dim3
    integer(kind = IPRE) :: i, j, k

    allocate(randn3(dim1, dim2, dim3))
    do i = 1, dim1
      do j = 1, dim2
        do k = 1, dim3
          randn3(i,j,k) = randn()
        end do
      end do
    end do
    return
  end function randn3

!=======================================================================
! randperm
!-----------------------------------------------------------------------
! randperm draws unique random integers.
!
! Syntax
!-----------------------------------------------------------------------
! x = randperm(n)
! x = randperm(n, k)
!
! Description
!-----------------------------------------------------------------------
! x = randperm(n) returns a row vector containing a random permutation
! of the integers from 1 to n inclusive.
!
! x = randperm(n, k) returns a row vector containing k unique integers
! selected randomly from 1 to n inclusive.
!=======================================================================

  function randperm(n, k)
    integer(kind = IPRE), dimension(:), allocatable :: randperm
    integer(kind = IPRE), intent(in) :: n
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, j, tmp
    integer(kind = IPRE), dimension(:), allocatable :: a

    opt_k = n
    if (present(k)) opt_k = k

    a = linspace(1, n, n)
    do i = n, n-opt_k+1, -1
      j = randi(i)
      tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    end do
    randperm = a(n-opt_k+1:n)
    return
  end function randperm

!=======================================================================
! randu
!-----------------------------------------------------------------------
! randu generates uniformly distributed random numbers.
!
! Syntax
!-----------------------------------------------------------------------
! x = randu()
! x = randu(dim1)
! A = randu(dim1, dim2)
! X = randu(dim1, dim2, dim3)
!
! Description
!-----------------------------------------------------------------------
! x = randu() returns a single uniformly distributed random number in
! the interval [0,1].
!
! x = randu(dim1) returns a dim1 vector of uniformly distributed random
! numbers.
!
! A = randu(dim1, dim2) returns a dim1-by-dim2 matrix of uniformly
! distributed random numbers.
!
! X = randu(dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of uniformly distributed random numbers.
!
! Examples
!-----------------------------------------------------------------------
! x = randu()
!     0.383413825
!
! x = randu(5)*2 - 1
!     0.640258908  -0.873707294   0.787327528
!=======================================================================

  real(kind = RPRE) function randu0()
    call random_number(randu0)
    return
  end function randu0

  function randu1(dim1)
    real(kind = RPRE), dimension(:), allocatable :: randu1
    integer(kind = IPRE), intent(in) :: dim1

    allocate(randu1(dim1))
    call random_number(randu1)
    return
  end function randu1

  function randu2(dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: randu2
    integer(kind = IPRE), intent(in) :: dim1, dim2

    allocate(randu2(dim1, dim2))
    call random_number(randu2)
    return
  end function randu2

  function randu3(dim1, dim2, dim3)
    real(kind = RPRE), dimension(:,:,:), allocatable :: randu3
    integer(kind = IPRE), intent(in) :: dim1, dim2, dim3

    allocate(randu3(dim1, dim2, dim3))
    call random_number(randu3)
    return
  end function randu3

!=======================================================================
! repmat
!-----------------------------------------------------------------------
! repmat repeats copies of arrays.
!
! Syntax
!-----------------------------------------------------------------------
! A = repmat(x, n1)
! A = repmat(x, n1, dim)
! B = repmat(A, n1, n2)
!
! Description
!-----------------------------------------------------------------------
! A = repmat(x, n1) returns an array A with n1 copies of vector x. x is
! treated as a column vector, therefore shape(A) = ( size(x), n1 ).
!
! A = repmat(x, n1, dim) returns an array A with n1 copies of vector x
! along the direction specified by dim.
!
! B = repmat(A, n1, 2) returns an array B with n1 copies of A in first
! dimension and n2 copies of B in second dimension.
!=======================================================================

  function repmat1(x, n1, dim)
    real(kind = RPRE), dimension(:,:), allocatable :: repmat1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in) :: n1
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: i, m

    m = size(x)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      repmat1 = zeros(m, n1)
      do i = 1, n1
        repmat1(:,i) = x
      end do
    elseif (dim .eq. 2) then
      repmat1 = zeros(n1, m)
      do i = 1, n1
        repmat1(i,:) = x
      end do
    end if
    return
  end function repmat1

  function repmat2(A, n1, n2)
    real(kind = RPRE), dimension(:,:), allocatable :: repmat2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in) :: n1, n2
    integer(kind = IPRE) :: i, j, m, n

    m = size(A, 1)
    n = size(A, 2)
    repmat2 = zeros(m*n1, n*n2)
    do i = 1, n1
      do j = 1, n2
        repmat2((i-1)*m+1:i*m,(j-1)*n+1:j*n) = A
      end do
    end do
    return
  end function repmat2

!=======================================================================
! rms
!-----------------------------------------------------------------------
! rms computes the root-mean-square level of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = rms(x)
! x = rms(A)
! x = rms(A, 1)
! x = rms(A, 2)
!
! Description
!-----------------------------------------------------------------------
! y = rms(x) returns the root-mean-square level of the vector x.
!
! x = rms(A) returns a dim2 vector with the root-mean-square level of
! each column of matrix A.
!
! x = rms(A, 1) (see x = rms(A)).
!
! x = rms(A, 2) returns a dim1 vector with the root-mean-square level of
! each row of matrix A.
!=======================================================================

  real(kind = RPRE) function rms1(x)
    real(kind = RPRE), dimension(:), intent(in) :: x

    rms1 = sqrt( sum(x*x) / size(x) )
    return
  end function rms1

  function rms2(A, dim)
    real(kind = RPRE), dimension(:), allocatable :: rms2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: dim
    integer(kind = IPRE) :: i, m, n

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(rms2(n))
      do i = 1, n
        rms2(i) = rms(A(:,i))
      end do
    elseif (dim .eq. 2) then
      allocate(rms2(m))
      do i = 1, m
        rms2(i) = rms(A(i,:))
      end do
    end if
    return
  end function rms2

!=======================================================================
! savebin
!-----------------------------------------------------------------------
! savebin saves arrays to binary files.
!
! Syntax
!-----------------------------------------------------------------------
! call savebin(filename, x)
! call savebin(filename, A)
! call savebin(filename, X)
!
! Description
!-----------------------------------------------------------------------
! call savebin(filename, x) saves a vector x into the binary file
! filename.
!
! call savebin(filename, A) saves a 2-dimensional array into the binary
! file filename.
!
! call savebin(filename, X) saves a 3-dimensional array into the binary
! file filename.
!=======================================================================

  subroutine savebin1_r4(filename, x)
    character(len = *), intent(in) :: filename
    real(kind = 4), dimension(:), intent(in) :: x
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(x)*size(x))
    write(infile%unit, rec = 1) x
    call infile%close()
    return
  end subroutine savebin1_r4

  subroutine savebin1_r8(filename, x)
    character(len = *), intent(in) :: filename
    real(kind = 8), dimension(:), intent(in) :: x
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(x)*size(x))
    write(infile%unit, rec = 1) x
    call infile%close()
    return
  end subroutine savebin1_r8

  subroutine savebin2_r4(filename, A)
    character(len = *), intent(in) :: filename
    real(kind = 4), dimension(:,:), intent(in) :: A
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(A)*size(A))
    write(infile%unit, rec = 1) A
    call infile%close()
    return
  end subroutine savebin2_r4

  subroutine savebin2_r8(filename, A)
    character(len = *), intent(in) :: filename
    real(kind = 8), dimension(:,:), intent(in) :: A
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(A)*size(A))
    write(infile%unit, rec = 1) A
    call infile%close()
    return
  end subroutine savebin2_r8

  subroutine savebin3_r4(filename, X)
    character(len = *), intent(in) :: filename
    real(kind = 4), dimension(:,:,:), intent(in) :: X
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(X)*size(X))
    write(infile%unit, rec = 1) X
    call infile%close()
    return
  end subroutine savebin3_r4

  subroutine savebin3_r8(filename, X)
    character(len = *), intent(in) :: filename
    real(kind = 8), dimension(:,:,:), intent(in) :: X
    type(File) :: infile

    infile = File(999, trim(filename))
    call infile%open(kind(X)*size(X))
    write(infile%unit, rec = 1) X
    call infile%close()
    return
  end subroutine savebin3_r8

!=======================================================================
! savetxt
!-----------------------------------------------------------------------
! savetxt saves 1 and 2-dimensional arrays to txt files.
!
! Syntax
!-----------------------------------------------------------------------
! call savetxt(filename, x)
! call savetxt(filename, A)
!
! Description
!-----------------------------------------------------------------------
! call savetxt(filename, x) saves a vector array x into the txt file
! filename.
!
! call savetxt(filename, A) saves a 2-dimensional array A into the txt
! file filename.
!=======================================================================

  subroutine savetxt1_i4(filename, x)
    character(len = *), intent(in) :: filename
    integer(kind = 4), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(x)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) x(i)
    end do
    call infile%close()
    return
  end subroutine savetxt1_i4

  subroutine savetxt1_r4(filename, x)
    character(len = *), intent(in) :: filename
    real(kind = 4), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(x)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) x(i)
    end do
    call infile%close()
    return
  end subroutine savetxt1_r4

  subroutine savetxt1_i8(filename, x)
    character(len = *), intent(in) :: filename
    integer(kind = 8), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(x)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) x(i)
    end do
    call infile%close()
    return
  end subroutine savetxt1_i8

  subroutine savetxt1_r8(filename, x)
    character(len = *), intent(in) :: filename
    real(kind = 8), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(x)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) x(i)
    end do
    call infile%close()
    return
  end subroutine savetxt1_r8

  subroutine savetxt2_r4(filename, A)
    character(len = *), intent(in) :: filename
    real(kind = 4), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(A, 1)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) A(i,:)
    end do
    call infile%close()
    return
  end subroutine savetxt2_r4

  subroutine savetxt2_i4(filename, A)
    character(len = *), intent(in) :: filename
    integer(kind = 4), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(A, 1)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) A(i,:)
    end do
    call infile%close()
    return
  end subroutine savetxt2_i4

  subroutine savetxt2_r8(filename, A)
    character(len = *), intent(in) :: filename
    real(kind = 8), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(A, 1)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) A(i,:)
    end do
    call infile%close()
    return
  end subroutine savetxt2_r8

  subroutine savetxt2_i8(filename, A)
    character(len = *), intent(in) :: filename
    integer(kind = 8), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, m
    type(File) :: infile

    infile = File(999, trim(filename))
    m = size(A, 1)
    call infile%open()
    do i = 1, m
      write(infile%unit,*) A(i,:)
    end do
    call infile%close()
    return
  end subroutine savetxt2_i8

!=======================================================================
! signum
!-----------------------------------------------------------------------
! signum returns the sign of an array.
!
! Syntax
!-----------------------------------------------------------------------
! y = signum(x)
! Y = signum(X)
! B = signum(A)
!
! Description
!-----------------------------------------------------------------------
! y = signum(x) returns the sign of x (scalar).
!
! Y = signum(X) returns a vector Y with the signs of each element in the
! vector X.
!
! B = signum(A) returns a matrix B with the signs of each elements in
! the matrix A.
!=======================================================================

  real(kind = RPRE) function signum0(x)
    real(kind = RPRE), intent(in) :: x
    if (x .lt. 0.0d0) then
      signum0 = -1.0d0
    elseif (x .gt. 0.0d0) then
      signum0 = 1.0d0
    else
      signum0 = 0.0d0
    end if
    return
  end function signum0

  function signum1(x)
    real(kind = RPRE), dimension(:), allocatable :: signum1
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE) :: i, n

    n = size(x)
    signum1 = zeros(n)
    do i = 1, n
      signum1(i) = signum0(x(i))
    end do
    return
  end function signum1

  function signum2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: signum2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE) :: i, j, m, n

    m = size(A, 1)
    n = size(A, 2)
    signum2 = zeros(m, n)
    do i = 1, m
      do j = 1, n
        signum2(i,j) = signum0(A(i,j))
      end do
    end do
    return
  end function signum2

!=======================================================================
! silhouette
!-----------------------------------------------------------------------
! silhouette computes the silhouette values for every observations given
! the clustering indices.
!
! Syntax
!-----------------------------------------------------------------------
! s = silhouette(x, cluster)
! s = silhouette(X, cluster)
!
! Description
!-----------------------------------------------------------------------
! s = silhouette(x, cluster) returns the silhouette values for every
! elements in the vector x.
!
! s = silhouette(X, cluster) returns the silhouette values for every
! elements in the array X, each row being an observation and each
! column a parameter.
!
! Notes
!-----------------------------------------------------------------------
! After Rousseeuw P. J. (1986): "Silhouettes: a graphical aid to the
! interpretation and validation of cluster analysis".
!=======================================================================

  function silhouette1(x, cluster) result(s)
    real(kind = RPRE), dimension(:), allocatable :: s
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), dimension(:), intent(in) :: cluster
    integer(kind = IPRE) :: n
    real(kind = RPRE), dimension(:,:), allocatable :: A

    n = size(x)
    A = reshape( x, shape = [ n, 1 ], order = [ 1, 2 ] )
    s = silhouette2(A, cluster)
    return
  end function silhouette1

  function silhouette2(X, cluster) result(s)
    real(kind = RPRE), dimension(:), allocatable :: s
    real(kind = RPRE), dimension(:,:), intent(in) :: X
    integer(kind = IPRE), dimension(:), intent(in) :: cluster
    integer(kind = IPRE) :: i, j, K, l, n
    real(kind = RPRE) :: a, b
    integer(kind = IPRE), dimension(:), allocatable :: idx, cs
    real(kind = RPRE), dimension(:), allocatable :: d

    n = size(X, 1)
    K = maxval(cluster)
    if ( K .eq. 1 ) then
      print *, "Warning: in silhouette, the silhouette value cannot " &
        // "be defined for K = 1."
      s = zeros(n)
      return
    end if

    ! Size of each cluster
    !======================
    allocate(cs(K))
    do j = 1, K
      idx = find( cluster .eq. j )    ! All objects in cluster j
      cs(j) = size(idx)
    end do

    ! Loop over objects
    !===================
    s = zeros(n)
    do i = 1, n

      ! Compute the dissimilarity for each cluster to current object i
      !================================================================
      d = zeros(K)          ! Cluster dissimilarity to object i
      do j = 1, K
        idx = find( cluster .eq. j )
        d(j) = sum( ( X(idx,:) - repmat(X(i,:), cs(j), 2) )**2 ) / cs(j)
      end do

      ! Compute a(i)
      !==============
      j = cluster(i)
      if ( cs(j) .eq. 1 ) then
        s(i) = 0.0d0
        cycle               ! Skip next statements and begin next iteration
      else
        a = d(j) * cs(j) / ( cs(j) - 1 )
      end if

      ! Compute b(i)
      !==============
      b = minval(d, mask = d .ne. d(j) .and. d .ne. real(0., RPRE))

      ! Compute s(i)
      !==============
      s(i) = (b - a) / max(a, b)

    end do

    return
  end function silhouette2

!=======================================================================
! sinc
!-----------------------------------------------------------------------
! sinc computes sinc function defined as sinc(x) = sin(pi*x) / (pi*x),
! with the convention sinc(0) = 1.
!
! Syntax
!-----------------------------------------------------------------------
! y = sinc(x)
!
! Description
!-----------------------------------------------------------------------
! y = sinc(x) returns the sinc function of the elements in x.
!=======================================================================

  real(kind = RPRE) function sinc0(x)
    real(kind = RPRE), intent(in) :: x
    real(kind = RPRE) :: y

    if ( x .eq. 0.0d0 ) then
      sinc0 = 1.0d0
    else
      y = pi * x
      sinc0 = sin(y) / y
    end if
    return
  end function sinc0

  function sinc1(x)
    real(kind = RPRE), dimension(:), allocatable :: sinc1
    real(kind = RPRE), dimension(:), intent(in) :: x
    real(kind = RPRE), dimension(:), allocatable :: y

    allocate(y(size(x)))
    y = pi * merge(real(1.0e-20, RPRE), x, x .eq. 0.0d0)
    sinc1 = sin(y) / y
    return
  end function sinc1

!=======================================================================
! sind
!-----------------------------------------------------------------------
! sind computes the sine of argument in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = sind(x)
!
! Description
!-----------------------------------------------------------------------
! y = sind(x) returns the sine of the elements in x, which are expressed
! in degrees.
!
! Examples
!-----------------------------------------------------------------------
! y = sind(90.)
!     1.
!
! x = [ 0., 90., 180., 270. ]
! y = sind(x)
!     0.  1.  0.  -1.
!=======================================================================

  real(kind = RPRE) function sind0(x)
    real(kind = RPRE), intent(in) :: x

    sind0 = sin(x*pi/180.0d0)
    return
  end function sind0

  function sind1(x)
    real(kind = RPRE), dimension(:), allocatable :: sind1
    real(kind = RPRE), dimension(:), intent(in) :: x

    sind1 = sin(x*pi/180.0d0)
    return
  end function sind1

  function sind2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: sind2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    sind2 = sin(A*pi/180.0d0)
    return
  end function sind2

  function sind3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: sind3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    sind3 = sin(X*pi/180.0d0)
    return
  end function sind3

!=======================================================================
! skewness
!-----------------------------------------------------------------------
! skewness computes vector and matrix skewnesses.
!
! Syntax
!-----------------------------------------------------------------------
! y = skewness(x)
! y = skewness(x, flag)
! x = skewness(A)
! x = skewness(A, flag)
! x = skewness(A, 1)
! x = skewness(A, flag, 1)
! x = skewness(A, 2)
! x = skewness(A, flag, 2)
!
! Description
!-----------------------------------------------------------------------
! y = skewness(x) returns the skewness of the vector x.
!
! y = skewness(x, w) returns the skewness of the vector x given the
! flag. By default, flag is 1. If flag is 0, the function corrects for
! the systematic bias due to the size of the sample.
!
! x = skewness(A) returns a dim2 vector with the skewnesses of each
! column of matrix A.
!
! x = skewness(A, flag) returns a dim2 vector given the flag.
!
! x = skewness(A, 1) (see x = skewness(A)).
!
! x = skewness(A, flag, 1) (see x = skewness(A, flag))
!
! x = skewness(A, 2) returns a dim1 vector with the skewnesses of
! each row of matrix A.
!
! x = skewness(A, flag, 2) returns a dim1 vector given the flag.
!=======================================================================

  real(kind = RPRE) function skewness1(x, flag)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: flag
    integer(kind = IPRE) :: opt_flag, n

    opt_flag = 1
    if (present(flag)) opt_flag = flag

    n = size(x)
    skewness1 = ( sum( ( x - mean(x) )**3 ) / n ) / ( var(x, 1)**1.5 )
    if (opt_flag .eq. 0) then
      skewness1 = skewness1 * sqrt(real(n*(n-1), RPRE)) / real((n-2), RPRE)
    end if
    return
  end function skewness1

  function skewness2(A, flag, dim)
    real(kind = RPRE), dimension(:), allocatable :: skewness2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: flag, dim
    integer(kind = IPRE) :: opt_flag, i, m, n

    opt_flag = 1
    if (present(flag)) opt_flag = flag

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(skewness2(n))
      do i = 1, n
        skewness2(i) = skewness1(A(:,i), opt_flag)
      end do
    elseif (dim .eq. 2) then
      allocate(skewness2(m))
      do i = 1, m
        skewness2(i) = skewness1(A(i,:), opt_flag)
      end do
    end if
    return
  end function skewness2

!=======================================================================
! solve
!-----------------------------------------------------------------------
! solve solves a linear matrix equation.
!
! Syntax
!-----------------------------------------------------------------------
! x = solve(A, b)
!
! Description
!-----------------------------------------------------------------------
! x = solve(A, b) returns the "exact" solution of the well-determined
! linear matrix equation Ax = b.
!
! Examples
!-----------------------------------------------------------------------
! A = reshape([ 11., 7., 3., 2., 12., 6., 7., 9., 4. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! b = [ 34., 44., 37. ]
! x = solve(A, b)
!     1.  2.  3.
!=======================================================================

  function solve(A, b) result(x)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:), intent(in) :: b
    integer :: i, j, m, n
    real(kind = RPRE), dimension(:), allocatable :: w, x, y
    real(kind = RPRE), dimension(:,:), allocatable :: L, U, V

    m = size(A, 1)
    n = size(A, 2)
    if (issquare(A)) then
      x = zeros(m)
      y = zeros(m)
      y(1) = b(1)

      ! LU decomposition to solve LUx = b
      !===================================
      call lu(A, L, U)

      ! Forward substitution: Ly = b
      !==============================
      do i = 2, m
        y(i) = b(i)
        do j = 1, i-1
          y(i) = y(i) - y(j)*L(i,j)
        end do
      end do

      ! Back substitution: Ux = y
      !===========================
      x(m) = y(m)/U(m,m)
      do i = m-1, 1, -1
        x(i) = y(i)
        do j = m, i+1, -1
          x(i) = x(i) - x(j)*U(i,j)
        end do
        x(i) = x(i)/U(i,i)
      end do
    else
      x = svdsolve(A, b)
    end if
    return
  end function solve

!=======================================================================
! sort
!-----------------------------------------------------------------------
! sort sorts arrays elements.
!
! Syntax
!-----------------------------------------------------------------------
! y = sort(x)
! y = sort(x, 1)
! y = sort(x, 2)
!
! Description
!-----------------------------------------------------------------------
! y = sort(x) returns the sorted elements of the vector x in the
! ascending order.
!
! y = sort(x, 1) (see y = sort(x)).
!
! y = sort(x, 2) returns the sorted elements of the vector x in the
! descending order.
!=======================================================================

  function sort(x, order)
    real(kind = RPRE), dimension(:), allocatable :: sort
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: order
    integer(kind = IPRE) :: n

    n = size(x)
    sort = x
    if ((.not. present(order)) .or. (order .eq. 1)) then
      call quicksort(sort, n, 1)
    elseif (order .eq. 2) then
      call quicksort(sort, n, 2)
    end if
    return

  contains

    !-------------------------------------------------------------------
    ! quicksort
    !-------------------------------------------------------------------
    recursive subroutine quicksort(x, n, order)
      real(kind = RPRE), dimension(n), intent(inout) :: x
      integer(kind = IPRE), intent(in) :: n, order
      integer(kind = IPRE) :: left, right, marker
      real(kind = RPRE) :: pivot, tmp

      if (n .gt. 1) then
        left = 0
        right = n + 1
        pivot = x(randi(n))

        select case(order)
          case(1)
            do while ( left .lt. right )
              left = left + 1
              right = right - 1
              do while ( x(left) .lt. pivot )
                left = left + 1
              end do
              do while ( x(right) .gt. pivot )
                right = right - 1
              end do
              if ( left .lt. right ) then
                tmp = x(left)
                x(left) = x(right)
                x(right) = tmp
              end if
            end do
          case(2)
            do while ( left .lt. right )
              left = left + 1
              right = right - 1
              do while ( x(left) .gt. pivot )
                left = left + 1
              end do
              do while ( x(right) .lt. pivot )
                right = right - 1
              end do
              if ( left .lt. right ) then
                tmp = x(left)
                x(left) = x(right)
                x(right) = tmp
              end if
            end do
        end select

        if ( left .eq. right ) then
          marker = left + 1
        else
          marker = left
        end if

        call quicksort(x(:marker-1), marker-1, order)
        call quicksort(x(marker:), n-marker+1, order)
      end if
      return
    end subroutine quicksort

  end function sort

!=======================================================================
! spline1
!-----------------------------------------------------------------------
! spline1 performs a cubic spline interpolation with natural end
! condition.
!
! Syntax
!-----------------------------------------------------------------------
! yq = spline1(x, y, xq)
!
! Description
!-----------------------------------------------------------------------
! yq = spline1(x, y, xq) returns the evaluated vector yq at the query
! points in xq using a cubic spline interpolation.
!=======================================================================

  real(kind = RPRE) function spline1_0(x, y, xq) result(yq)
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), intent(in) :: xq
    real(kind = RPRE) :: tmp(1)

    tmp = spline1(x, y, [ xq ])
    yq = tmp(1)
    return
  end function spline1_0

  function spline1_1(x, y, xq) result(yq)
    real(kind = RPRE), dimension(:), allocatable :: yq
    real(kind = RPRE), dimension(:), intent(in) :: x, y, xq
    integer(kind = IPRE) :: i, n, nq, x1
    real(kind = RPRE), dimension(:), allocatable :: w, h, z, a, b, c, d

    n = size(x)
    nq = size(xq)
    allocate(w(n-1), h(n-1), z(n), a(n-1), b(n-1), c(n-1), d(n-1), yq(nq))

    ! Compute h and b
    !=================
    do i = 1, n-1
      w(i) = x(i+1) - x(i)
      h(i) = ( y(i+1) - y(i) ) / w(i)
    end do

    ! Compute z
    !===========
    z(1) = 0.0d0
    do i = 1, n-2
      z(i+1) = 3.0d0 * ( h(i+1) - h(i) ) / ( w(i+1) + w(i) )
    end do
    z(n) = 0.0d0

    ! Basis functions
    !=================
    do i = 1, n-1
      a(i) = ( z(i+1) - z(i) ) / ( 6.0d0 * w(i) )
      b(i) = 0.5d0 * z(i)
      c(i) = h(i) - w(i) * ( z(i+1) + 2.0d0*z(i) ) / 6.0d0
      d(i) = y(i)
    end do

    ! Evaluate
    !==========
    do i = 1, nq
      x1 = max(1, minloc(xq(i) - x, 1, mask = xq(i) .gt. x))
      yq(i) = d(x1) + ( xq(i) - x(x1) ) &
              * ( c(x1) + ( xq(i) - x(x1) ) &
              * ( b(x1) + ( xq(i) - x(x1) ) &
              * a(x1) ) )
    end do
    return
  end function spline1_1

!=======================================================================
! spline2
!-----------------------------------------------------------------------
! spline2 performs a bicubic spline interpolation with natural end
! condition.
!
! Syntax
!-----------------------------------------------------------------------
! zq = spline2(x, y, Z, xq, yq)
! ZQ = spline2(x, y, Z, XQ, YQ)
!
! Description
!-----------------------------------------------------------------------
! zq = spline2(x, y, Z, xq, yq) returns the evaluated vector zq at the
! query points in xq and yq using a bicubic interpolation.
!
! ZQ = spline2(x, y, Z, XQ, YQ) returns the evaluated matrix ZQ given
! mesh type grids XQ and YQ using a bicubic interpolation. ZQ is of the
! same shape as XQ and YQ.
!=======================================================================

  function spline2_1(x, y, z, xq, yq) result(zq)
    real(kind = RPRE), dimension(:), allocatable :: zq
    real(kind = RPRE), dimension(:), intent(in) :: x, y, xq, yq
    real(kind = RPRE), dimension(:,:), intent(in) :: z
    integer(kind = IPRE) :: i, iq, j, k, m, n, nq, x0, y0
    real(kind = RPRE) :: wt(16,16), zv(16), c(4,4), dx, dy, t, u
    real(kind = RPRE), dimension(:,:), allocatable :: zt, z1, z2, z12

    m = size(x)
    n = size(y)
    nq = size(xq)

    ! Work on the transpose so that x corresponds to the 2nd dimension
    ! and y to the 1st dimension
    !==================================================================
    zt = transpose(z)

    ! Inverted coefficient matrix
    !=============================
    wt = reshape( [ 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                    0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                   -3., 3., 0., 0.,-2.,-1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                    2.,-2., 0., 0., 1., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                    0., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., &
                    0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., &
                    0., 0., 0., 0., 0., 0., 0., 0.,-3., 3., 0., 0.,-2.,-1., 0., 0., &
                    0., 0., 0., 0., 0., 0., 0., 0., 2.,-2., 0., 0., 1., 1., 0., 0., &
                   -3., 0., 3., 0., 0., 0., 0., 0.,-2., 0.,-1., 0., 0., 0., 0., 0., &
                    0., 0., 0., 0.,-3., 0., 3., 0., 0., 0., 0., 0.,-2., 0.,-1., 0., &
                    9.,-9.,-9., 9., 6., 3.,-6.,-3., 6.,-6., 3.,-3., 4., 2., 2., 1., &
                   -6., 6., 6.,-6.,-3.,-3., 3., 3.,-4., 4.,-2., 2.,-2.,-2.,-1.,-1., &
                    2., 0.,-2., 0., 0., 0., 0., 0., 1., 0., 1., 0., 0., 0., 0., 0., &
                    0., 0., 0., 0., 2., 0.,-2., 0., 0., 0., 0., 0., 1., 0., 1., 0., &
                   -6., 6., 6.,-6.,-4.,-2., 4., 2.,-3., 3.,-3., 3.,-2.,-1.,-2.,-1., &
                    4.,-4.,-4., 4., 2., 2.,-2.,-2., 2.,-2., 2.,-2., 1., 1., 1., 1. ], &
                  shape = [ 16, 16 ], order = [ 2, 1 ] )

    ! Compute partial derivatives along x-axis and y-axis, and cross
    ! derivatives
    !================================================================
    call zdiff(x, y, zt, z1, z2, z12)

    ! Loop for each query point
    !===========================
    zq = zeros(nq)

    do iq = 1, nq

      ! Locate the query point
      !========================
      x0 = minloc(xq(iq) - x, 1, mask = xq(iq) .ge. x)
      y0 = minloc(yq(iq) - y, 1, mask = yq(iq) .ge. y)
      dx = x(x0+1) - x(x0)
      dy = y(y0+1) - y(y0)

      ! Build zv so that wt·zv = c, the cubic basis coefficients
      !==========================================================
      zv = [ zt(x0,y0), &
             zt(x0+1,y0), &
             zt(x0,y0+1), &
             zt(x0+1,y0+1), &
             z1(x0,y0)*dx, &
             z1(x0+1,y0)*dx, &
             z1(x0,y0+1)*dx, &
             z1(x0+1,y0+1)*dx, &
             z2(x0,y0)*dy, &
             z2(x0+1,y0)*dy, &
             z2(x0,y0+1)*dy, &
             z2(x0+1,y0+1)*dy, &
             z12(x0,y0)*dx*dy, &
             z12(x0+1,y0)*dx*dy, &
             z12(x0,y0+1)*dx*dy, &
             z12(x0+1,y0+1)*dx*dy ]

      ! Solve for c
      !=============
      c = reshape( matmul(wt, zv), shape = [ 4, 4 ], order = [ 1, 2 ] )

      ! Scaling coefficients so that 0 <= (t, u) <= 1
      !===============================================
      t = ( xq(iq) - x(x0) ) / dx
      u = ( yq(iq) - y(y0) ) / dy

      ! Evaluate at the query point
      !=============================
      do i = 1, 4
        do j = 1, 4
          zq(iq) = zq(iq) + c(i,j) * t**(i-1) * u**(j-1)
        end do
      end do

    end do
    return

  contains

    !-------------------------------------------------------------------
    ! zdiff
    !-------------------------------------------------------------------
    subroutine zdiff(x, y, zt, z1, z2, z12)
      real(kind = RPRE), dimension(:), intent(in) :: x, y
      real(kind = RPRE), dimension(:,:), intent(in) :: zt
      real(kind = RPRE), dimension(:,:), allocatable, intent(out) :: z1, z2, z12

      allocate(z1(m, n), z2(m, n), z12(m, n))

      ! Middle
      !========
      do j = 2, m-1
        do k = 2, n-1
          z1(j,k) = ( zt(j+1,k) - zt(j-1,k) ) / ( x(j+1) - x(j-1) )
          z2(j,k) = ( zt(j,k+1) - zt(j,k-1) ) / ( y(k+1) - y(k-1) )
          z12(j,k) = ( zt(j+1,k+1) - zt(j+1,k-1) - zt(j-1,k+1) + zt(j-1,k-1) ) &
                     / ( ( x(j+1) - x(j-1) ) * ( y(k+1) - y(k-1) ) )
        end do
      end do

      ! Left edge
      !===========
      do j = 2, m-1
        z1(j,1) = ( zt(j+1,1) - zt(j,1) ) / ( x(j+1) - x(j) )
        z2(j,1) = ( zt(j,2) - zt(j,1) ) / ( y(2) - y(1) )
        z12(j,1) = ( zt(j+1,2) - zt(j+1,1) - zt(j,2) + zt(j,1) ) &
                   / ( ( x(j+1) - x(j) ) * ( y(2) - y(1) ) )
      end do

      ! Upper edge
      !============
      do k = 2, n-1
        z1(1,k) = ( zt(2,k) - zt(1,k) ) / ( x(2) - x(1) )
        z2(1,k) = ( zt(1,k+1) - zt(1,k) ) / ( y(k+1) - y(k) )
        z12(1,k) = ( zt(2,k+1) - zt(2,k) - zt(1,k+1) + zt(1,k) ) &
                   / ( ( x(2) - x(1) ) * ( y(k+1) - y(k) ) )
      end do

      ! Right edge
      !============
      do j = 2, m-1
        z1(j,n) = ( zt(j+1,n) - zt(j,n) ) / ( x(j+1) - x(j) )
        z2(j,n) = ( zt(j,n) - zt(j,n-1) ) / ( y(n) - y(n-1) )
        z12(j,n) = ( zt(j+1,n) - zt(j+1,n-1) - zt(j,n) + zt(j,n-1) ) &
                   / ( ( x(j+1) - x(j) ) * ( y(n) - y(n-1) ) )
      end do

      ! Lower edge
      !============
      do k = 2, n-1
        z1(m,k) = ( zt(m,k) - zt(m-1,k) ) / ( x(m) - x(m-1) )
        z2(m,k) = ( zt(m,k+1) - zt(m,k) ) / ( y(k+1) - y(k) )
        z12(m,k) = ( zt(m,k+1) - zt(m,k) - zt(m-1,k+1) + zt(m-1,k) ) &
                   / ( ( x(m) - x(m-1) ) * ( y(k+1) - y(k) ) )
      end do

      ! Upper-left corner
      !===================
      z1(1,1) = ( zt(2,1) - zt(1,1) ) / ( x(2) - x(1) )
      z2(1,1) = ( zt(1,2) - zt(1,1) ) / ( y(2) - y(1) )
      z12(1,1) = ( zt(2,2) - zt(2,1) - zt(1,2) + zt(1,1) ) &
                 / ( ( x(2) - x(1) ) * ( y(2) - y(1) ) )

      ! Upper-right corner
      !====================
      z1(1,n) = ( zt(2,n) - zt(1,n) ) / ( x(2) - x(1) )
      z2(1,n) = ( zt(1,n) - zt(1,n-1) ) / ( y(n) - y(n-1) )
      z12(1,n) = ( zt(2,n) - zt(2,n-1) - zt(1,n) + zt(1,n-1) ) &
                 / ( ( x(2) - x(1) ) * ( y(n) - y(n-1) ) )

      ! Lower-left corner
      !===================
      z1(m,1) = ( zt(m,1) - zt(m-1,1) ) / ( x(m) - x(m-1) )
      z2(m,1) = ( zt(m,2) - zt(m,1) ) / ( y(2) - y(1) )
      z12(m,1) = ( zt(m,2) - zt(m,1) - zt(m-1,2) + zt(m-1,1) ) &
                 / ( ( x(m) - x(m-1) ) * ( y(2) - y(1) ) )

      ! Lower-right corner
      !====================
      z1(m,n) = ( zt(m,n) - zt(m-1,n) ) / ( x(m) - x(m-1) )
      z2(m,n) = ( zt(m,n) - zt(m,n-1) ) / ( y(n) - y(n-1) )
      z12(m,n) = ( zt(m,n) - zt(m,n-1) - zt(m-1,n) + zt(m-1,n-1) ) &
                 / ( ( x(m) - x(m-1) ) * ( y(m) - y(m-1) ) )

      return
    end subroutine zdiff

  end function spline2_1

  function spline2_2(x, y, z, xq, yq) result(zq)
    real(kind = RPRE), dimension(:,:), allocatable :: zq
    real(kind = RPRE), dimension(:), intent(in) :: x, y
    real(kind = RPRE), dimension(:,:), intent(in) :: z, xq, yq
    integer(kind = IPRE) :: m, n

    m = size(xq, 1)
    n = size(xq, 2)
    zq = reshape( spline2_1(x, y, z, [ xq ], [ yq ]), shape = [ m, n ] )
    return
  end function spline2_2

!=======================================================================
! split_argument
!-----------------------------------------------------------------------
! split_argument takes a command line argument of type 'argname=argval'
! (obtained by get_command_argument) and returns argname and argval.
!
! Syntax
!-----------------------------------------------------------------------
! call split_argument(argin, argname, argval)
!
! Description
!-----------------------------------------------------------------------
! call split_argument(argin, argname, argval) splits argin into argname
! and argval as strings.
!
! Examples
!-----------------------------------------------------------------------
! nargin = command_argument_count()
! do i = 1, nargin
!   call get_command_argument(i, argin)
!   call split_argument(argin, argname, argval)
!   print *, argname, argval
! end do
!=======================================================================

  subroutine split_argument(argin, argname, argval)
    character(len = *), intent(in) :: argin
    character(len = :), allocatable, intent(out) :: argname, argval
    integer(kind = IPRE) :: idx

    idx = index(argin, "=")
    if  (idx .ne. 0 ) then
      argname = trim(argin(:idx-1))
      argval = trim(argin(idx+1:))
    else
      print *, "Error: missing '=' in argument '" // trim(argin) // "'"
      stop
    end if
    return
  end subroutine split_argument

!=======================================================================
! std
!-----------------------------------------------------------------------
! std computes vector and matrix standard deviations.
!
! Syntax
!-----------------------------------------------------------------------
! y = std(x)
! y = std(x, w)
! x = std(A)
! x = std(A, w)
! x = std(A, 1)
! x = std(A, w, 1)
! x = std(A, 2)
! x = std(A, w, 2)
!
! Description
!-----------------------------------------------------------------------
! y = std(x) returns the standard deviation of the vector x.
!
! y = std(x, w) returns the standard deviation of the vector x with the
! normalization option w.
!   -   0 (default) normalize by N-1,
!   -   1 normalize by N.
!
! x = std(A) returns a dim2 vector with the standard deviations of each
! column of matrix A.
!
! x = std(A, w) returns a dim2 vector with the normalization option w.
!
! x = std(A, 1) (see x = std(A)).
!
! x = std(A, w, 1) (see x = std(A, w))
!
! x = std(A, 2) returns a dim1 vector with the standard deviations of
! each row of matrix A.
!
! x = std(A, w, 2) returns a dim1 vector with the normalization option
! w.
!=======================================================================

  real(kind = RPRE) function std1(x, w)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w

    opt_w = 0
    if (present(w)) opt_w = w

    std1 = sqrt(var(x, opt_w))
    return
  end function std1

  function std2(A, w, dim)
    real(kind = RPRE), dimension(:), allocatable :: std2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: w, dim
    integer(kind = IPRE) :: opt_w

    opt_w = 0
    if (present(w)) opt_w = w

    if (.not. present(dim)) then
      std2 = sqrt(var(A, opt_w))
    else
      std2 = sqrt(var(A, opt_w, dim))
    end if
    return
  end function std2

!=======================================================================
! svd
!-----------------------------------------------------------------------
! svd computes the singular value decomposition.
!
! Syntax
!-----------------------------------------------------------------------
! call svd(A, w)
! call svd(A, w, U, V)
!
! Description
!-----------------------------------------------------------------------
! call svd(A, w) returns the singular values of the matrix A sorted in
! descending order.
!
! call svd(A, w, U, V) returns the singular values of the m-by-n
! matrix A sorted in descending order. The output matrices are:
!   -   w is a n vector
!   -   U is a m-by-n unitary matrix
!   -   V is a n-by-n unitary matrix
!
! Examples
!-----------------------------------------------------------------------
! A = reshape([ 1., 2., 3., 4., 5., 6., 7., 8., 9. ], [ 3, 3 ], &
!             order = [ 2, 1 ])
! call svd(A, w)
! call disp(w)
!     16.8481007  1.06836963  2.10855418E-07
! call svd(A, w, U, V)
! call disp(U)
!     -0.214837193   -0.887230873   -0.408247918
!     -0.520587385   -0.249643773   -0.816496670
!     -0.826337695    0.387942642    0.408248365
! call disp(V)
!     -0.479671091    0.776690900   -0.408248752
!     -0.572367728    0.075686932    0.816496611
!     -0.665064454   -O.625318289   -0.408247977
!
! Notes
!-----------------------------------------------------------------------
! This code is adapted from Numerical Recipes in Fortran 90.
!=======================================================================

  subroutine svd(a, w, u, v, d, ierr)
    real(kind = RPRE), dimension(:,:), intent(in) :: a
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: w
    real(kind = RPRE), dimension(:,:), allocatable, intent(out), optional :: u, v
    logical, intent(in), optional :: d
    integer(kind = IPRE), intent(out), optional :: ierr
    integer(kind = IPRE) :: m, n, i, its, i1, j, k, kk, k1, l, ll, l1, mn
    integer(kind = IPRE), dimension(:), allocatable :: idx
    real(kind = RPRE) :: c, f, g, h, s, scale, tst1, tst2, x, y, z
    real(kind = RPRE), dimension(:), allocatable :: rv1
    real(kind = RPRE), dimension(:,:), allocatable :: opt_u, opt_v
    logical :: outu = .false., outv = .false., opt_d = .true., outierr = .false.

    m = size(a, 1)
    n = size(a, 2)

    if (.not. allocated(w)) allocate(w(n))
    allocate(rv1(n), opt_u(m, n), opt_v(n, n))

    opt_u = a

    if (present(d)) opt_d = d
    if (present(u)) outu = .true.
    if (present(v)) outv = .true.
    if (present(ierr)) outierr = .true.

    ! Householder reduction to bidiagonal form
    !==========================================
    g = 0.0d0
    scale = 0.0d0
    x = 0.0d0

    do i = 1, n
      l = i + 1
      rv1(i) = scale*g
      g = 0.0d0
      s = 0.0d0
      scale = 0.0d0
      if (i .le. m) then
        scale = sum(abs(opt_u(i:m,i)))
        if (scale .ne. 0.0d0) then
          opt_u(i:m,i) = opt_u(i:m,i)/scale
          s = sum(opt_u(i:m,i)**2)
          f = opt_u(i,i)
          g = -sign(sqrt(s), f)
          h = f*g - s
          opt_u(i,i) = f - g
          if (i .ne. n) then
            do j = l, n
              s = dot_product(opt_u(i:m,i), opt_u(i:m,j))
              opt_u(i:m,j) = opt_u(i:m,j) + s*opt_u(i:m,i)/h
            end do
          end if
          opt_u(i:m,i) = scale*opt_u(i:m,i)
        end if
      end if

      w(i) = scale*g
      g = 0.0d0
      s = 0.0d0
      scale = 0.0d0

      if ((i .le. m) .and. (i .ne. n)) then
        scale = sum(abs(opt_u(i,l:n)))
        if (scale .ne. 0.0d0) then
          opt_u(i,l:n) = opt_u(i,l:n)/scale
          s = sum(opt_u(i,l:n)**2)
          f = opt_u(i,l)
          g = -sign(sqrt(s), f)
          h = f*g - s
          opt_u(i,l) = f - g
          rv1(l:n) = opt_u(i,l:n)/h

          if (i .ne. m) then
            do j = l, m
              s = dot_product(opt_u(j,l:n), opt_u(i,l:n))
              opt_u(j,l:n) = opt_u(j,l:n) + s*rv1(l:n)
            end do
          end if

          opt_u(i,l:n) = scale*opt_u(i,l:n)
        end if
      end if

      x = max(x, abs(w(i)) + abs(rv1(i)))
    end do

    ! Accumulation of right-hand transformations
    !============================================
    if (outv) then
      do i = n, 1, -1
        if (i .ne. n) then
          if (g .ne. 0.0d0) then
            opt_v(l:n,i) = (opt_u(i,l:n)/opt_u(i,l))/g
            do j = l, n
              s = dot_product(opt_u(i,l:n), opt_v(l:n,j))
              opt_v(l:n,j) = opt_v(l:n,j) + s*opt_v(l:n,i)
            end do
          end if
          opt_v(i,l:n) = 0.0d0
          opt_v(l:n,i) = 0.0d0
        end if
        opt_v(i,i) = 1.0d0
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
        if (i .ne. n) opt_u(i,l:n) = 0.0d0
        if (g .ne. 0.0d0) then
          if (i .ne. mn) then
            do j = l, n
              s = dot_product(opt_u(l:m,i), opt_u(l:m,j))
              f = (s/opt_u(i,i))/g
              opt_u(i:m,j) = opt_u(i:m,j) + f*opt_u(i:m,i)
            end do
          end if
        opt_u(i:m,i) = opt_u(i:m,i)/g
        else
          opt_u(i:m,i) = 0.0d0
        end if
        opt_u(i,i) = opt_u(i,i) + 1.0d0
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
      520 continue
      do ll = 1, k
        l1 = k - ll
        l = l1 + 1
        tst2 = tst1 + abs(rv1(l))
        if (tst2 .eq. tst1) goto 565
        tst2 = tst1 + abs(w(l1))
        if (tst2 .eq. tst1) exit
      end do

      ! Cancellation of rv1(l) if L greater than 1
      !============================================
      c = 0.0d0
      s = 1.0d0
      do i = l, k
        f = s*rv1(i)
        rv1(i) = c*rv1(i)
        tst2 = tst1 + abs(f)
        if (tst2 .eq. tst1) goto 565
        g = w(i)
        h = pythag(f, g)
        w(i) = h
        c = g/h
        s = -f/h
        if (outu) then
          do j = 1, m
            y = opt_u(j,l1)
            z = opt_u(j,i)
            opt_u(j,l1) = y*c + z*s
            opt_u(j,i) = -y*s + z*c
          end do
        end if
      end do

      ! Test for convergence
      !======================
      565 continue
      z = w(k)
      if (l .eq. k) goto 650

      ! Shift from bottom 2 by 2 minor
      !================================
      if (its .ge. 30) then
        if (outierr) ierr = k
        return
      end if
      its = its + 1
      x = w(l)
      y = w(k1)
      g = rv1(k1)
      h = rv1(k)
      f = 0.5d0*(((g+z)/h)*((g-z)/y) + y/h - h/y)
      g = pythag(f, real(1., kind = RPRE))
      f = x - (z/x)*z + (h/x)*(y/(f + sign(g, f)) - h)

      ! Next QR transformation
      !========================
      c = 1.0d0
      s = 1.0d0
      do i1 = l, k1
        i = i1 + 1
        g = rv1(i)
        y = w(i)
        h = s*g
        g = c*g
        z = pythag(f, h)
        rv1(i1) = z
        c = f/z
        s = h/z
        f = x*c + g*s
        g = -x*s + g*c
        h = y*s
        y = y*c
        if (outv) then
          do j = 1, n
            x = opt_v(j,i1)
            z = opt_v(j,i)
            opt_v(j,i1) = x*c + z*s
            opt_v(j,i) = -x*s + z*c
          end do
        end if
        z = pythag(f, h)
        w(i1) = z

        ! Rotation can be arbitrary if Z is zero
        !========================================
        if (z .ne. 0.0d0) then
          c = f/z
          s = h/z
        end if
        f = c*g + s*y
        x = -s*g + c*y
        if (outu) then
          do j = 1, m
            y = opt_u(j,i1)
            z = opt_u(j,i)
            opt_u(j,i1) = y*c + z*s
            opt_u(j,i) = - y*s + z*c
          end do
        end if
      end do
      rv1(l) = 0.0d0
      rv1(k) = f
      w(k) = x
      go to 520

      ! Convergence
      !=============
      650 continue
      if (z .le. 0.0d0) then
        w(k) = -z
        if (outv) then
          opt_v(1:n,k) = -opt_v(1:n,k)
        end if
      end if
    end do

    ! Sort singular values
    !======================
    if ( opt_d ) then
      idx = argsort(w, 2)
      w = w(idx)
      if (present(u)) u = opt_u(:,idx)
      if (present(v)) v = opt_v(:,idx)
    else
      if (present(u)) u = opt_u
      if (present(v)) v = opt_v
    end if

    return

  contains

    !-------------------------------------------------------------------
    ! pythag
    !-------------------------------------------------------------------
    real(kind = RPRE) function pythag(x1, x2)
      real(kind = RPRE), intent(in) :: x1, x2
      real(kind = RPRE) :: r, s, t, u

      pythag = max(abs(x1), abs(x2))
      if (pythag .ne. 0.0d0) then
        r = (min(abs(x1), abs(x2))/pythag)**2
        do
          t = 4.0d0 + r
          if (t .eq. 4.0d0) exit
          s = r/t
          u = 1.0d0 + 2.0d0*s
          pythag = u*pythag
          r = (s/u)**2*r
        end do
      end if
      return
    end function pythag

  end subroutine svd

!=======================================================================
! svdsolve
!-----------------------------------------------------------------------
! svdsolve solves a linear matrix equation from the singular value
! decomposition of A.
!
! Syntax
!-----------------------------------------------------------------------
! x = svdsolve(A, b)
! x = svdsolve(A, b, k)
!
! Description
!-----------------------------------------------------------------------
! x = svdsolve(A, b) returns the full-rank solution of the linear matrix
! equation Ax = b.
!
! x = svdsolve(A, b, k) returns the reduced-rank solution of the linear
! matrix equation Ax = b, where A is a m-by-n matrix. Therefore, the
! rank of the solution is n-k.
!
! Notes
!-----------------------------------------------------------------------
! Sometimes, too small singular values produce very large solution. A
! proper way to determine the cut-off singular value is using a L-curve
! criterion. The cut-off singular value corresponds to the singular
! value from which the residuals norm is not improved while the solution
! norm increases substantially.
!=======================================================================

  function svdsolve(A, b, cutoff) result(x)
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    real(kind = RPRE), dimension(:), intent(in) :: b
    integer(kind = IPRE), intent(in), optional :: cutoff
    integer :: i, k, n
    real(kind = RPRE), dimension(:), allocatable :: w, x, xnorm, resnorm
    real(kind = RPRE), dimension(:,:), allocatable :: U, V

    n = size(A, 2)
    k = n
    if (present(cutoff)) k = k - cutoff
    xnorm = zeros(n)
    resnorm = zeros(n)
    call svd(A, w, U, V)
    do i = 1, n
      x = matmul(matmul(matmul(V(:,:i), diag(1/w(:i))), transpose(U(:,:i))), b)
      xnorm(i) = norm(x)
      resnorm(i) = norm(matmul(A, x) - b)
    end do
    x = matmul(matmul(matmul(V(:,:k), diag(1/w(:k))), transpose(U(:,:k))), b)
    return
  end function svdsolve

!=======================================================================
! tand
!-----------------------------------------------------------------------
! tand computes the tangent of argument in degrees.
!
! Syntax
!-----------------------------------------------------------------------
! y = tand(x)
!
! Description
!-----------------------------------------------------------------------
! y = tand(x) returns the tangent of the elements in x, which are
! expressed in degrees.
!
! Examples
!-----------------------------------------------------------------------
! y = tand(0.)
!     0.
!
! x = [ 0., 90., 180., 270. ]
! y = tand(x)
!     0.  Inf   0.  -Inf
!=======================================================================

  real(kind = RPRE) function tand0(x)
    real(kind = RPRE), intent(in) :: x

    tand0 = tan(x*pi/180.0d0)
    return
  end function tand0

  function tand1(x)
    real(kind = RPRE), dimension(:), allocatable :: tand1
    real(kind = RPRE), dimension(:), intent(in) :: x

    tand1 = tan(x*pi/180.0d0)
    return
  end function tand1

  function tand2(A)
    real(kind = RPRE), dimension(:,:), allocatable :: tand2
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    tand2 = tan(A*pi/180.0d0)
    return
  end function tand2

  function tand3(X)
    real(kind = RPRE), dimension(:,:,:), allocatable :: tand3
    real(kind = RPRE), dimension(:,:,:), intent(in) :: X

    tand3 = tan(X*pi/180.0d0)
    return
  end function tand3

!=======================================================================
! tic / toc
!-----------------------------------------------------------------------
! tic saves the elapsed CPU time in seconds.
! toc displays and returns the elapsed time since tic.
!
! Syntax
!-----------------------------------------------------------------------
! call tic()
! call toc()
! call toc(t)
!
! Description
!-----------------------------------------------------------------------
! call tic() saves the elapsed CPU time in seconds.
!
! call toc() displays the elapsed time since call tic().
!
! call toc(t) displays and saves the elapsed time since call tic().
!
! Examples
!-----------------------------------------------------------------------
! call tic()
! ! ... some codes ...
! call toc()
!     Elapsed time: 0.1 seconds
!=======================================================================

  subroutine tic()
    integer(kind = IPRE) :: values(8)
    call date_and_time(values = values)
    tic_time = datenum( values(1), values(2), values(3), values(5), &
                        values(6), values(7), values(8) * 1000) &
               * 24.0d0 * 60.0d0 * 60.0d0
    return
  end subroutine tic

  subroutine toc(t)
    real(kind = 8), intent(out), optional :: t
    integer(kind = IPRE) :: values(8)
    real(kind = 8) :: toc_time, elapsed_time

    call date_and_time(values = values)
    toc_time = datenum( values(1), values(2), values(3), values(5), &
                        values(6), values(7), values(8) * 1000) &
               * 24.0d0 * 60.0d0 * 60.0d0
    elapsed_time = toc_time - tic_time
    if (present(t)) then
      t = elapsed_time
    else
      print *, "Elapsed time: " &
        // num2str(real(elapsed_time, kind = RPRE), "(F12.3)") &
        // " seconds"
    end if
    return
  end subroutine toc

!=======================================================================
! trace
!-----------------------------------------------------------------------
! trace computes the sum of diagonal elements.
!
! Syntax
!-----------------------------------------------------------------------
! x = trace(A)
!
! Description
!-----------------------------------------------------------------------
! x = trace(A) returns the sum of the elements on the main diagonal of
! the matrix A.
!
! Examples
!-----------------------------------------------------------------------
! A = eye(3)
! x = trace(A)
!     3.
!=======================================================================

  real(kind = RPRE) function trace(A)
    real(kind = RPRE), dimension(:,:), intent(in) :: A

    trace = sum(diag(A))
    return
  end function trace

!=======================================================================
! tril
!-----------------------------------------------------------------------
! tril extracts the lower triangular part of a matrix.
!
! Syntax
!-----------------------------------------------------------------------
! B = tril(A)
! B = tril(A, k)
!
! Description
!-----------------------------------------------------------------------
! B = tril(A) returns the lower triangular part of matrix A.
!
! B = tril(A, k) returns the elements on and below the kth diagonal of
! matrix X:
!   -   k = 0 is the main diagonal,
!   -   k > 0 is above the main diagonal,
!   -   k < 0 is below the main diagonal.
!
! Examples
!-----------------------------------------------------------------------
! A = ones(4, 4)
! B = tril(A, -1)
!     0.  0.  0.  0.
!     1.  0.  0.  0.
!     1.  1.  0.  0.
!     1.  1.  1.  0.
!=======================================================================

  function tril_i(A, k)
    integer(kind = IPRE), dimension(:,:), allocatable :: tril_i
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    tril_i = A
    do i = 1, min(m, n)
      tril_i(:i-opt_k-1,i) = 0.0d0
    end do
    return
  end function tril_i

  function tril_r(A, k)
    real(kind = RPRE), dimension(:,:), allocatable :: tril_r
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    tril_r = A
    do i = 1, min(m, n)
      tril_r(:i-opt_k-1,i) = 0.0d0
    end do
    return
  end function tril_r

  function tril_c(A, k)
    complex(kind = RPRE), dimension(:,:), allocatable :: tril_c
    complex(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    tril_c = A
    do i = 1, min(m, n)
      tril_c(:i-opt_k-1,i) = 0.0d0
    end do
    return
  end function tril_c

!=======================================================================
! triu
!-----------------------------------------------------------------------
! triu extracts the upper triangular part of a matrix.
!
! Syntax
!-----------------------------------------------------------------------
! B = triu(A)
! B = triu(A, k)
!
! Description
!-----------------------------------------------------------------------
! B = triu(A) returns the upper triangular part of matrix A.
!
! B = triu(A, k) returns the elements on and above the kth diagonal of
! matrix X:
!   -   k = 0 is the main diagonal,
!   -   k > 0 is above the main diagonal,
!   -   k < 0 is below the main diagonal.
!
! Examples
!-----------------------------------------------------------------------
! A = ones(4, 4)
! B = triu(A, -1)
!     1.  1.  1.  1.
!     1.  1.  1.  1.
!     0.  1.  1.  1.
!     0.  0.  1.  1.
!=======================================================================

  function triu_i(A, k)
    integer(kind = IPRE), dimension(:,:), allocatable :: triu_i
    integer(kind = IPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    triu_i = A
    do i = 1, min(m, n)
      triu_i(i-opt_k+1:,i) = 0.0d0
    end do
    return
  end function triu_i

  function triu_r(A, k)
    real(kind = RPRE), dimension(:,:), allocatable :: triu_r
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    triu_r = A
    do i = 1, min(m, n)
      triu_r(i-opt_k+1:,i) = 0.0d0
    end do
    return
  end function triu_r

  function triu_c(A, k)
    complex(kind = RPRE), dimension(:,:), allocatable :: triu_c
    complex(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: k
    integer(kind = IPRE) :: opt_k, i, m, n

    opt_k = 0
    if (present(k)) opt_k = k

    m = size(A, 1)
    n = size(A, 2)
    triu_c = A
    do i = 1, min(m, n)
      triu_c(i-opt_k+1:,i) = 0.0d0
    end do
    return
  end function triu_c

!=======================================================================
! utm2deg
!-----------------------------------------------------------------------
! utm2deg converts UTM-WGS84 coordinates to latitude / longitude
! (in degrees) coordinates.
!
! Syntax
!-----------------------------------------------------------------------
! call utm2deg(east, north, zn, zl, lat, lon)
!
! Description
!-----------------------------------------------------------------------
! call utm2deg(east, north, zn, zl, lat, lon) converts the UTM-WGS84
! coordinates in east and north to latitude / longitude coordinates
! (in degrees). It outputs the latitudes lat and the longitudes lon.
!
! Notes
!-----------------------------------------------------------------------
! This function has been translated and adapted from the Python's
! module utm: https://pypi.python.org/pypi/utm
!=======================================================================

  subroutine utm2deg0(east, north, zn, zl, lat, lon)
    real(kind = RPRE), intent(in) :: east, north
    integer(kind = IPRE), intent(in) :: zn
    character(len = 1), intent(in) :: zl
    real(kind = RPRE), intent(out) :: lat, lon
    real(kind = 8), parameter :: K0 = 0.9996d0
    real(kind = 8), parameter :: E = 0.00669438d0
    real(kind = 8), parameter :: R = 6378137
    real(kind = 8) :: m, mu, n, s, c, c2, d, x, y
    real(kind = 8) :: E_P2, M1, F, P2, P3, P4, P5
    real(kind = 8) :: p_rad, p_sin, p_sin2, p_cos, p_tan, p_tan2, p_tan4, &
      ep_sin, ep_sin_sqrt

    x = east - 500000
    y = north
    if (verify(zl, "OXWVUTSRQPN") .ne. 0) y = y - 10000000

    E_P2 = E / (1.0d0 - E)
    M1 = (1 - E / 4 - 3 * E**2 / 64 - 5 * E**3 / 256)
    F = (1 - sqrt(1 - E)) / (1 + sqrt(1 - E))
    P2 = (3.0d0 / 2 * F - 27.0d0 / 32 * F**3 + 269.0d0 / 512 * F**5)
    P3 = (21.0d0 / 16 * F**2 - 55.0d0 / 32 * F**4)
    P4 = (151.0d0 / 96 * F**3 - 417.0d0 / 128 * F**5)
    P5 = (1097.0d0 / 512 * F**4)

    m = y / K0
    mu = m / (R * M1)

    p_rad = (mu + P2 * sin(2 * mu) &
                + P3 * sin(4 * mu) &
                + P4 * sin(6 * mu) &
                + P5 * sin(8 * mu))

    p_sin = sin(p_rad)
    p_sin2 = p_sin**2

    p_cos = cos(p_rad)

    p_tan = p_sin / p_cos
    p_tan2 = p_tan**2
    p_tan4 = p_tan2**2

    ep_sin = 1 - E * p_sin2
    ep_sin_sqrt = sqrt(1 - E * p_sin2)

    n = R/ ep_sin_sqrt
    s = (1 - E) / ep_sin
    c = F * p_cos**2
    c2 = c**2

    d = x / (n * K0)
    lat = (p_rad - (p_tan / s) &
          * (d**2 / 2 &
          -  d**4 / 24 * (5 + 3 * p_tan2 + 10 * c -4 * c2 - 9 * E_P2)) &
          +  d**6 / 720 * (61 + 90 * p_tan2 + 298 * c + 45 * p_tan4 - 252 * E_P2 - 3 * c2))
    lon = ( d &
          - d**3 / 6 * (1 + 2 * p_tan2 + c) &
          + d**5 / 120 * (5 - 2 * c + 28 * p_tan2 - 3 * c2 + 8 * E_P2 + 24 * p_tan4)) / p_cos

    lat = lat*180.0d0/pi
    lon = lon*180.0d0/pi + (zn - 1) * 6 - 180 + 3
    return
  end subroutine utm2deg0

  subroutine utm2deg1(east, north, zn, zl, lat, lon)
    real(kind = RPRE), dimension(:), intent(in) :: east, north
    integer(kind = IPRE), dimension(:), intent(in) :: zn
    character(len = 1), dimension(:), intent(in) :: zl
    real(kind = RPRE), dimension(:), allocatable, intent(out) :: lat, lon
    integer(kind = IPRE) :: i, n

    n = size(east)
    allocate(lat(n), lon(n))
    do i = 1, n
      call utm2deg(east(i), north(i), zn(i), zl(i), lat(i), lon(i))
    end do
    return
  end subroutine utm2deg1

!=======================================================================
! var
!-----------------------------------------------------------------------
! var computes vector and matrix variances.
!
! Syntax
!-----------------------------------------------------------------------
! y = var(x)
! y = var(x, w)
! x = var(A)
! x = var(A, w)
! x = var(A, 1)
! x = var(A, w, 1)
! x = var(A, 2)
! x = var(A, w, 2)
!
! Description
!-----------------------------------------------------------------------
! y = var(x) returns the variance of the vector x.
!
! y = var(x, w) returns the variance of the vector x with the
! normalization option w.
!   -   0 (default) normalize by N-1,
!   -   1 normalize by N.
!
! x = var(A) returns a dim2 vector with the variances of each column of
! matrix A.
!
! x = var(A, w) returns a dim2 vector with the normalization option w.
!
! x = var(A, 1) (see x = var(A)).
!
! w = var(A, w, 1) (see x = var(A, w))
!
! x = var(A, 2) returns a dim1 vector with the variances of each row of
! matrix A.
!
! x = var(A, w, 2) returns a dim1 vector with the normalization option
! w.
!=======================================================================

  real(kind = RPRE) function var1(x, w)
    real(kind = RPRE), dimension(:), intent(in) :: x
    integer(kind = IPRE), intent(in), optional :: w
    integer(kind = IPRE) :: opt_w

    opt_w = 0
    if (present(w)) opt_w = w

    select case(opt_w)
      case(0)
        var1 = sum( (x - mean(x))**2 ) / (size(x) - 1)
      case(1)
        var1 = sum( (x - mean(x))**2 ) / size(x)
    end select
    return
  end function var1

  function var2(A, w, dim)
    real(kind = RPRE), dimension(:), allocatable :: var2
    real(kind = RPRE), dimension(:,:), intent(in) :: A
    integer(kind = IPRE), intent(in), optional :: w, dim
    integer(kind = IPRE) :: opt_w, i, m, n

    opt_w = 0
    if (present(w)) opt_w = w

    m = size(A, 1)
    n = size(A, 2)
    if ((.not. present(dim)) .or. (dim .eq. 1)) then
      allocate(var2(n))
      do i = 1, n
        var2(i) = var1(A(:,i), opt_w)
      end do
    elseif (dim .eq. 2) then
      allocate(var2(m))
      do i = 1, m
        var2(i) = var1(A(i,:), opt_w)
      end do
    end if
  end function var2

!=======================================================================
! vertcat
!-----------------------------------------------------------------------
! vertcat concatenates arrays vertically.
!
! Syntax
!-----------------------------------------------------------------------
! A = vertcat(x1, x2)
! A = vertcat(A1, A2)
! B = vertcat(x1, A2)
! B = vertcat(A1, x2)
!
! Description
!-----------------------------------------------------------------------
! A = vertcat(x1, x2) concatenates the vectors x1 and x2 treated as line
! vectors along the dimension 2. If the length of x1 and x2 are not
! equal, empty elements will be filled with zeros.
!
! A = vertcat(A1, A2) concatenates the matrices A1 and A2 along the
! dimension 2. If the second dimension of A1 and A2 are not equal, empty
! elements will be filled with zeros.
!
! B = vertcat(x1, A2) concatenates the vector x treated as line vector
! and the matrix A along the dimension 2. If the length of x and the
! second dimension of A are not equal, empty elements will be filled
! with zeros.
!
! B = vertcat(A1, x2) concatenates the matrix A and the vector x treated
! as a line vector along the dimension 2. If the second dimension of A
! and the length of x are not equal, empty elements will be filled with
! zeros.
!
! Examples
!-----------------------------------------------------------------------
! A1 = reshape([ 1., 2., 3., 4. ], [ 2, 2 ], order = [ 2, 1 ])
! A2 = reshape([ 5., 6., 7., 8. ], [ 2, 2 ], order = [ 2, 1 ])
! A = vertcat(A1, A2, 2)
!     1.  2.
!     3.  4.
!     5.  6.
!     7.  8.
!=======================================================================

  function vertcat_r1(x1, x2)
    real(kind = RPRE), dimension(:,:), allocatable :: vertcat_r1
    real(kind = RPRE), dimension(:), intent(in) :: x1, x2
    integer(kind = IPRE) :: n1, n2

    n1 = size(x1)
    n2 = size(x2)

    vertcat_r1 = zeros(2, max(n1, n2))
    vertcat_r1(1,1:n1) = x1
    vertcat_r1(2,1:n2) = x2
    return
  end function vertcat_r1

  function vertcat_r2(A1, A2)
    real(kind = RPRE), dimension(:,:), allocatable :: vertcat_r2
    real(kind = RPRE), dimension(:,:), intent(in) :: A1, A2
    integer(kind = IPRE) :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    vertcat_r2 = zeros(m1+m2, max(n1, n2))
    vertcat_r2(1:m1,1:n1) = A1
    vertcat_r2(m1+1:,1:n2) = A2
    return
  end function vertcat_r2

  function vertcat_c2(A1, A2)
    complex(kind = RPRE), dimension(:,:), allocatable :: vertcat_c2
    complex(kind = RPRE), dimension(:,:), intent(in) :: A1, A2
    integer(kind = IPRE) :: m1, n1, m2, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    m2 = size(A2, 1)
    n2 = size(A2, 2)

    vertcat_c2 = zeros(m1+m2, max(n1, n2))
    vertcat_c2(1:m1,1:n1) = A1
    vertcat_c2(m1+1:,1:n2) = A2
    return
  end function vertcat_c2

  function vertcat_r12(x1, A2)
    real(kind = RPRE), dimension(:,:), allocatable :: vertcat_r12
    real(kind = RPRE), dimension(:), intent(in) :: x1
    real(kind = RPRE), dimension(:,:), intent(in) :: A2
    integer(kind = IPRE) :: n1, m1, n2

    n1 = size(x1)
    m1 = size(A2, 1)
    n2 = size(A2, 2)

    vertcat_r12 = zeros(m1+1, max(n1, n2))
    vertcat_r12(1,1:n1) = x1
    vertcat_r12(2:,1:n2) = A2
    return
  end function vertcat_r12

  function vertcat_r21(A1, x2)
    real(kind = RPRE), dimension(:,:), allocatable :: vertcat_r21
    real(kind = RPRE), dimension(:,:), intent(in) :: A1
    real(kind = RPRE), dimension(:), intent(in) :: x2
    integer(kind = IPRE) :: m1, n1, n2

    m1 = size(A1, 1)
    n1 = size(A1, 2)
    n2 = size(x2)

    vertcat_r21 = zeros(m1+1, max(n1, n2))
    vertcat_r21(1:m1,1:n1) = A1
    vertcat_r21(m1+1,1:n2) = x2
    return
  end function vertcat_r21

!=======================================================================
! zeros
!-----------------------------------------------------------------------
! zeros creates array all of zeros.
!
! Syntax
!-----------------------------------------------------------------------
! x = zeros(dim1)
! A = zeros(dim1, dim2)
! X = zeros(dim1, dim2, dim3)
!
! Description
!-----------------------------------------------------------------------
! x = zeros(dim1) returns a dim1 vector of zeros.
!
! A = zeros(dim1, dim2) returns a dim1-by-dim2 matrix of zeros.
!
! X = zeros(dim1, dim2, dim3) returns a dim1-by-dim2-by-dim3
! 3-dimensional matrix of zeros.
!
! Examples
!-----------------------------------------------------------------------
! x = zeros(3)
! x =
!     0.  0.  0.
!
! A = zeros(3, 3)
! A =
!     0.  0.  0.
!     0.  0.  0.
!     0.  0.  0.
!=======================================================================

  function zeros1(dim1)
    real(kind = RPRE), dimension(:), allocatable :: zeros1
    integer(kind = IPRE), intent(in) :: dim1
    integer(kind = IPRE) :: ierr

    allocate(zeros1(dim1), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in zeros, could not allocate array."
      stop
    else
      zeros1 = 0.0d0
    end if
    return
  end function zeros1

  function zeros2(dim1, dim2)
    real(kind = RPRE), dimension(:,:), allocatable :: zeros2
    integer(kind = IPRE), intent(in) :: dim1, dim2
    integer(kind = IPRE) :: ierr

    allocate(zeros2(dim1, dim2), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in zeros, could not allocate array."
      stop
    else
      zeros2 = 0.0d0
    end if
    return
  end function zeros2

  function zeros3(dim1, dim2, dim3)
    real(kind = RPRE), dimension(:,:,:), allocatable :: zeros3
    integer(kind = IPRE), intent(in) :: dim1, dim2, dim3
    integer(kind = IPRE) :: ierr

    allocate(zeros3(dim1, dim2, dim3), stat = ierr)
    if ( ierr .ne. 0 ) then
      print *, "Error: in zeros, could not allocate array."
      stop
    else
      zeros3 = 0.0d0
    end if
    return
  end function zeros3

end module forlab
