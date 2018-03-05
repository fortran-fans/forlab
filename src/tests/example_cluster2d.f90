!=======================================================================
! Created by
!     Keurfon Luu <keurfon.luu@mines-paristech.fr>
!     MINES ParisTech - Centre de Géosciences
!     PSL - Research University
!=======================================================================

program example_cluster2d

  use forlab, only: IPRE, RPRE, rng, chol, randn, repmat, vertcat, savetxt, &
                    mean, num2str, zeros, gmm, kmeans, mbkmeans, &
                    silhouette, dbindex

  implicit none

  integer(kind = IPRE) :: i, j, k, itermax, kmax
  integer(kind = IPRE), dimension(:), allocatable :: idx
  real(kind = RPRE) :: perc = 0.2
  real(kind = RPRE), dimension(:), allocatable :: x, y, mu, sil, db
  real(kind = RPRE), dimension(:,:), allocatable :: A, B, L, R, Sigma, means
  character(len = :), allocatable :: outdir, method

  ! Output directory
  outdir = "examples/cluster2d/"
  call system("mkdir -p " // outdir)

  ! Initialize random number generation
  call rng()

  ! Create 2D data
  print *, "Creating 2D data:"

  ! Cluster 1
  Sigma = reshape( [ 3., 0., 0., 0.5 ], [ 2, 2 ] )
  L = chol(Sigma)
  R = randn(2, 500)
  mu = [ 3., 3. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)) )
  A = B

  ! Cluster 2
  Sigma = reshape( [ 1., 0., 0., 1. ], [ 2, 2 ] )
  L = chol(Sigma)
  R = randn(2, 400)
  mu = [ -3., 3. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)) )
  A = vertcat(A, B)

  ! Cluster 3
  Sigma = reshape( [ 2., 0., 0., 1. ], [ 2, 2 ] )
  L = chol(Sigma)
  R = randn(2, 600)
  mu = [ 6., -3. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)) )
  A = vertcat(A, B)

  ! Cluster 4
  Sigma = reshape( [ 1., 0., 0., 1.5 ], [ 2, 2 ])
  L = chol(Sigma)
  R = randn(2, 500)
  mu = [ 0., 0. ]
  B = transpose( matmul(L, R) + repmat(mu, size(R, 2)) )
  A = vertcat(A, B)

  call savetxt(outdir // "data.txt", A)
  print *, "Data saved in " // outdir


  ! Clustering
  k = 4
  itermax = 500
  method = "gmm"

  print *; print *, "Clustering using " // method // ":"
  select case(method)
  case("kmeans")
    idx = kmeans(A, k, means = means, itermax = itermax)
  case("mbkmeans")
    idx = mbkmeans(A, k, perc = perc, means = means, itermax = itermax)
  case("gmm")
    idx = gmm(A, k, means = means, itermax = itermax)
  end select

  call savetxt(outdir // "index.txt", idx)
  call savetxt(outdir // "means.txt", means)
  print *, "Clustering results saved in " // outdir

  ! Cluster analysis: compute several indices for different K
  kmax = 20
  sil = zeros(kmax)
  db = zeros(kmax)

  print *; print *, "Performing cluster analysis:"
  !$omp parallel do default(shared) private(idx, means)
  do i = 1, kmax
    idx = kmeans(A, i, means = means, itermax = itermax)
    sil(i) = mean(silhouette(A, idx))
    db(i) = dbindex(A, idx, means)
  end do
  !$end omp parallel do

  call savetxt(outdir // "silhouette.txt", sil)
  call savetxt(outdir // "db.txt", db)
  print *, "Cluster analysis results saved in " // outdir

  print *; print *, "Run script /utils/view_cluster2d.py to check results."

  print *

end program example_cluster2d
