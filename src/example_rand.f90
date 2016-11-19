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
  
  ! Create uniform 1D data
  !========================
  print *, "Uniform 1D data:"
  
  n = 10
  x = randu(n) * 10. - 5.     ! Uniformly distributed in [ -5, 5 ]
  idx = randperm(n)           ! Random permutation
  y = x(idx)                  ! Shuffle vector x
  
  print *, "       X                Y"
  call disp(horzcat(x, y))

  print *
  print *, "Nombres aléatoires normal : "
  print *; print *, "Pourcentage de déviations inférieurs à 1 : "
  print *, num2str(count(abs(randn(1000)) .le. 1.)/10., "(F6.2)") // "%"
  print *; print *, "Pourcentage de déviations inférieurs à 2 : "
  print *, num2str(count(abs(randn(1000)) .le. 2.)/10., "(F6.2)") // "%"
  print *; print *, "P-value for a normal random distribution : "
  print *, "pval = " // num2str(k2test(randn(1000)))
  print *, "skewness = " // num2str(skewness(randn(1000)))
  print *, "kurtosis = " // num2str(kurtosis(randn(1000)))
  
  stop

end program example_rand