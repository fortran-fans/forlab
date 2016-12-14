program example_rand

  use forlab, only: IPRE, RPRE, randi, randu, randn, randperm, rng, &
                    disp, num2str, mean, std, skewness, kurtosis, k2test, &
                    prctile, horzcat

  implicit none

  integer(kind = IPRE) :: m, n
  integer(kind = IPRE), dimension(:), allocatable :: idx, idx2
  real(kind = RPRE), dimension(:), allocatable :: x, y
  real(kind = RPRE), dimension(:,:), allocatable :: A
!  character(len = :), allocatable :: outdir

  ! Output directory
  !==================
!  outdir = "examples/rand/"
!  call system("mkdir -p " // outdir)

  ! Initialize random number generation
  !=====================================
  call rng()                  ! The seed is set according to the current time

  ! Create random vector of integers (not unique/unique)
  !======================================================
  print *, "Random integers (not unique/unique):"

  idx = randi([ 1, 10 ], 9)   ! Random integers between in [ 1, 15 ]
  idx2 = randperm(10, 9)      ! Random unique integers between in [ 1, 10 ]

  call disp(horzcat(idx, idx2))

  ! Create uniform 1D data
  !========================
  print *; print *, "Uniform 1D data (Y being permutation of X):"

  n = 10
  x = randu(n) * 10. - 5.     ! Uniformly distributed in [ -5, 5 ]
  idx = randperm(n)           ! Random permutation
  y = x(idx)                  ! Shuffle vector x

  print *, "       X                Y"
  call disp(horzcat(x, y))

  ! Create normal 1D data
  !=======================
  print *; print *, "Statistics for 100000 normally distributed values:"

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

  ! Create uniform 2D data
  !========================
  print *; print *, "Uniform 2D data:"

  n = 5
  A = randu(n, n) * 10. - 5.  ! Uniformly distributed in [ -5, 5 ]

  call disp(A)

  ! The same can be done for 3D data with randu(n, n, n)

  ! Create normal 2D data
  !=======================
  print *; print *, "Normal 2D data:"

  A = randn(n, n)             ! Normally distributed with mu = 0 and std = 1

  call disp(A)

  ! The same can be done for 3D data with randn(n, n, n)

  print *
  stop

end program example_rand
