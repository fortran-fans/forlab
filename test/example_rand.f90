!=======================================================================
! Created by
!     Keurfon Luu <keurfon.luu@mines-paristech.fr>
!     MINES ParisTech - Centre de Géosciences
!     PSL - Research University
!=======================================================================

program example_rand

  use forlab, only: IPRE, RPRE, randi, randu, randn, randperm, rng, &
                    disp, num2str, mean, std, skewness, kurtosis, k2test, &
                    kde, chol, repmat, linspace, prctile, horzcat, vertcat, &
                    chi2rand, chi2inv, zeros, var, savebin

  implicit none

  integer(kind = IPRE) :: i, m, n, n1, n2, df
  integer(kind = IPRE), dimension(:), allocatable :: idx, idx2
  real(kind = RPRE) :: sig
  real(kind = RPRE), dimension(:), allocatable :: x, y, mu, f1, xi, yi
  real(kind = RPRE), dimension(:,:), allocatable :: A, B, Sigma, L, R, f2, gam
  character(len = :), allocatable :: outdir

  ! Output directory
  outdir = "examples/rand/"
  call system("rm -rf " // outdir)
  call system("mkdir -p " // outdir)

  ! Initialize random number generation
  call rng()                  ! The seed is set according to the current time

  ! Create random vector of integers (not unique/unique)
  print *, "Random integers (not unique/unique):"

  idx = randi([ 1, 10 ], 9)   ! Random integers between in [ 1, 15 ]
  idx2 = randperm(10, 9)      ! Random unique integers between in [ 1, 10 ]

  call disp(horzcat(idx, idx2))

  ! Create uniform 1D data
  print *; print *, "Uniform 1D data (Y being permutation of X):"

  n = 10
  x = randu(n) * 10. - 5.     ! Uniformly distributed in [ -5, 5 ]
  idx = randperm(n)           ! Random permutation
  y = x(idx)                  ! Shuffle vector x

  print *, "       X                Y"
  call disp(horzcat(x, y))

  ! Create normal 1D data
  print *; print *, "Statistics for 100000 normally distributed samples:"

  n = 100000
  x = randn(n)                ! Normally distributed with mu = 0 and std = 1

  print *, "Mean: " // num2str(mean(x))
  print *, "Standard deviation: " // num2str(std(x))
  print *, "Skewness: " // num2str(skewness(x))
  print *, "Kurtosis: " // num2str(kurtosis(x))
  print *, "P-value: " // num2str(k2test(x))
  print *, "5th percentile: " // num2str(prctile(x, 5))
  print *, "95th percentile: " // num2str(prctile(x, 95))
  print *, "Percentage of absolute deviations lower than 1:"
  print *, num2str(count(abs(x) .le. 1.)/real(n, RPRE)*100., "(F6.2)") // "%"
  print *, "Percentage of absolute deviations lower than 2:"
  print *, num2str(count(abs(x) .le. 2.)/real(n, RPRE)*100., "(F6.2)") // "%"

  ! Create chi-square 1D data
  print *; print *, "Statistics for 100000 chi-square distributed samples " &
    // "with 10 degrees of freedom:"

  n = 100000
  df = 10
  x = chi2rand(df, n)         ! Chi-square distributed with df = 10

  print *, "Mean: " // num2str(mean(x)) &
    // " (" // num2str(real(df, RPRE)) // " expected)"
  print *, "Variance: " // num2str(var(x)) &
    // " (" // num2str(2.*df) // " expected)"
  print *, "Skewness: " // num2str(skewness(x)) &
    // " (" // num2str(sqrt(8./df)) // " expected)"
  print *, "Kurtosis: " // num2str(kurtosis(x)) &
    // " (" // num2str(12./df + 3.) // " expected)"
  print *, "5th percentile: " // num2str(prctile(x, 5)) &
    // " (" // num2str(chi2inv(real(0.05, RPRE), df)) // " expected)"
  print *, "95th percentile: " // num2str(prctile(x, 95)) &
    // " (" // num2str(chi2inv(real(0.95, RPRE), df)) // " expected)"

  ! Create uniform 2D data
  print *; print *, "Uniform 2D data:"

  n = 5
  A = randu(n, n) * 10. - 5.  ! Uniformly distributed in [ -5, 5 ]

  call disp(A)

  ! The same can be done for 3D data with randu(n, n, n)

  ! Create normal 2D data
  print *; print *, "Normal 2D data:"

  A = randn(n, n)             ! Normally distributed with mu = 0 and std = 1

  call disp(A)

  ! The same can be done for 3D data with randn(n, n, n)

  ! 1D Kernel Density Estimation
  print *; print *, "1D Kernel Density Estimation:"

  ! Bell 1
  n1 = 500
  x = 2. + 1.5 * randn(n1)

  ! Bell 2
  n2 = 1000
  x = [ x, 7. + 1. * randn(n2) ]

  xi = linspace(-5, 15, 200)
  call kde(x, f1, xi)

  call savebin(outdir // "data1d.bin", x)
  call savebin(outdir // "data1d_kde.bin", f1)
  call savebin(outdir // "data1d_kde_xaxis.bin", xi)

  print *, "Results saved in " // outdir
  deallocate(xi)

  ! 2D Kernel Density Estimation
  print *; print *, "2D Kernel Density Estimation:"

  ! Bell 1
  n1 = 250
  Sigma = reshape( [ 3., 0., 0., 0.5 ], [ 2, 2 ] )
  L = chol(Sigma)
  R = randn(2, n1)
  mu = [ 3., 3. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)))
  A = B

  ! Bell 2
  n2 = 500
  Sigma = reshape( [ 2., 0.5, 0.5, 1. ], [ 2, 2 ])
  L = chol(Sigma)
  R = randn(2, n2)
  mu = [ 6., -3. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)))
  A = vertcat(A, B)

  xi = linspace(-5, 15, 200)
  yi = linspace(-10, 10, 200)
  call kde(A, f2, xi, yi)

  call savebin(outdir // "data2d.bin", A)
  call savebin(outdir // "data2d_kde.bin", f2)
  call savebin(outdir // "data2d_kde_xaxis.bin", xi)
  call savebin(outdir // "data2d_kde_yaxis.bin", yi)

  print *, "Results saved in " // outdir

  print *; print *, "Run script /utils/view_kde.py to check results."

  ! Checking the property: if X ~ N(mu, sigma²), then (n-1)S²/sigma²
  ! follows a chi-square distribution with (n-1) degrees of freedom
  print *; print *, "Checking the property:"
  print *, "If X ~ N(mu, sigma²), then (n-1)S²/sigma² follows " &
    // "a chi-square distribution with (n-1) degrees of freedom"

  n = 10000
  sig = 2.
  df = 10
  x = zeros(n)
  do i = 1, n
    x(i) = (df+1.) * var(randn(df+1) * sig) / sig**2
  end do
  y = chi2rand(df, n)

  call savebin(outdir // "randn_to_chi2.bin", x)
  call savebin(outdir // "randchi2.bin", y)

  print *, "Number of degrees of freedom: " // num2str(df)
  print *, "Results saved in " // outdir

  print *; print *, "Run script /utils/view_chi2.py to check results."

  print *

end program example_rand
