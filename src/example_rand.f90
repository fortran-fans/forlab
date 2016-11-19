program example_rand

  use forlab

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
  call rng()
  
  ! Create uniform 1D data and sort them
  !======================================
  print *, "Uniform 1D data:"
  
  n = 10
  x = randu(n) * 10. - 5.     ! Uniformly distributed in [ -5, 5 ]
  idx = randperm(n)           ! Random permutation
  y = x(idx)                  ! Shuffle vector x
  
  print *, "       X                Y"
  call disp(horzcat(x, y))
  
  ! Create normal 1D data
  !=======================
  print *; print *, "Nombres aléatoires normal:"
  
  x = randn(n)
  print *, "Mean: " // num2str(mean(x))
  print *, "Standard deviation: " // num2str(std(x))
  print *, "Skewness: " // num2str(skewness(x))
  print *, "Kurtosis: " // num2str(kurtosis(x))
  print *, "P-value: " // num2str(k2test(x))
  
  stop

end program example_rand