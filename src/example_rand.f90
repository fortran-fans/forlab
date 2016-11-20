program example_rand

  use forlab, only: IPRE, RPRE, randi, randu, randn, randperm, rng, &
                    disp, num2str, mean, std, skewness, kurtosis, k2test, &
                    prctile, horzcat

  implicit none

  integer(kind = IPRE) :: m, n
  integer(kind = IPRE), dimension(:), allocatable :: idx
  real(kind = RPRE), dimension(:), allocatable :: x, y
  real(kind = RPRE), dimension(:,:), allocatable :: A, B

  character(len = :), allocatable :: outdir
  
  ! Output directory
  !==================
  outdir = "examples/rand/"
  call system("mkdir -p " // outdir)
  
  ! Initialize random number generation
  !=====================================
  call rng()                  ! The seed is set according to the current time
  
  ! Create random vector of integers
  !==================================
  print *, "Random integers (not unique):"
  
  idx = randi([ 1, 10 ], 9)   ! Random integers between in [ 1, 15 ]
  
  call disp(idx)
  
  ! Create random vector of unique integers
  !=========================================
  print *; print *, "Random integers (unique):"
  
  idx = randperm(10, 9)       ! Random unique integers between in [ 1, 10 ]
  
  call disp(idx)
  
  ! Create uniform 1D data
  !========================
  print *; print *, "Uniform 1D data:"
  
  n = 10
  x = randu(n) * 10. - 5.     ! Uniformly distributed in [ -5, 5 ]
  idx = randperm(n)           ! Random permutation
  y = x(idx)                  ! Shuffle vector x
  
  print *, "       X                Y"
  call disp(horzcat(x, y))
  
  ! Create normal 1D data
  !=======================
  print *; print *, "Statistics for " // num2str(n) // " normally distributed values:"
  
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
  
  stop

end program example_rand