!!# Forlab
!! Forlab aims to provide a package of functions for scientific
!! computing in Fortran.
!!
!!## Created by
!! Keurfon Luu <keurfon.luu@mines-paristech.fr>
!! MINES ParisTech - Centre de Géosciences
!! PSL - Research University
!!## Updated by 
!! Forlab Contributors
!!
!!## Notes
!! forlab is still in the rapid development stage.

#:include "common.fypp"

module forlab

    use forlab_kinds
    use forlab_file
    use forlab_optval, only: optval
    use stdlib_error
    use forlab_color, only: setcolor
    implicit none

    !! Parameters
    integer, public, parameter :: IPRE = 4
    integer, public, parameter :: RPRE = 8
    integer, public, parameter :: CLEN = 512
    real(kind=8), public, parameter :: pi = 3.141592653589793238460d0
    #:for k1,t1 in REAL_KINDS_TYPES
    ${t1}$, public, parameter ::pi_${k1}$=acos(-1.0_${k1}$)
    #:endfor
    !! Functions
    private
    public :: File, acosd, asind, atand, argmax, argmin, argsort, &
              angle, bsplrep1, bsplrep2, bspline1, bspline2, chol, cosd, countlines, &
              cov, cumsum, chi2cdf, chi2pdf, chi2inv, chi2rand, check_directory, &
              det, diag, disp, deg2utm, datenum, datevec, datestr, deboor, diff, &
              eig, error_stop, eye, file_exist, &
              find, flip, fliplr, flipud, fminbnd, gammainc, horzcat, &
              hann, interp1, interp2, interp3, inv, ismember, isoutlier, issquare, &
              isleap, issymmetric, kurtosis, k2test, kde, loadbin, loadtxt, linspace, logspace, &
              mean, median, mad,matpow, meshgrid, nextpow2, norm, normpdf, num2str, &
              ones, outer, pascal, prctile, progress_bar, progress_perc,qr, rng, randn,randperm, randu, &
              repmat, rms, savetxt, savebin, sind, sort, solve, &
              svd, svdsolve, std, spline1, spline2, skewness, signum, sinc, &
              split_argument, tand, tic, toc, trace, tril, triu, utm2deg, vertcat, &
              var, dbindex, gmm, kmeans, mbkmeans, silhouette, seq, setcolor, zeros

    !! #ifdef do_mpi
    public :: mpi_rpre
    !! #endif

    !! Operators
    public :: operator(.i.), operator(.x.)

    !! Abstract function
    abstract interface
        real(kind=RPRE) function func1d(x)
            import :: RPRE
            real(kind=RPRE), intent(in) :: x
        end function func1d
    end interface

    !! Polymorphic Interfaces
    interface angle
        !! angle compute the phase angle.
        !!([Specification](../module/forlab_angle.html))
        #:for k1, t1 in REAL_KINDS_TYPES
        real(${k1}$) elemental module function angle_${k1}$(z)
            complex(${k1}$), intent(in) :: z
        end function
        #:endfor
    end interface

    !! Acos & Asin & Atan
    #:set CIR_NAME=["acos","asin","atan"]
    #:for l1 in CIR_NAME
    interface ${l1}$d
        !! degree circular functions
        !!([Specification](../module/forlab_degcir.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        pure elemental module function ${l1}$d_${k1}$(x)
        ${t1}$,intent(in)::x
        ${t1}$::${l1}$d_${k1}$
        end function
        #:endfor
    end interface ${l1}$d
    #:endfor
    !! Cos & Sin & Tan
    #:set CIR_NAME=["cos","sin","tan"]
    #:for l1 in CIR_NAME
    interface ${l1}$d
        #:for k1,t1 in REAL_KINDS_TYPES
        pure elemental module function ${l1}$d_${k1}$(x)
        ${t1}$,intent(in)::x
        ${t1}$::${l1}$d_${k1}$
        end function
        #:endfor
    end interface ${l1}$d
    #:endfor

    interface argmax
        module procedure argmax1, argmax2, argmax3
    end interface argmax

    interface argmin
        module procedure argmin1, argmin2, argmin3
    end interface argmin

    interface argsort
        !! argsort generates the indices that would sort an array.
        !!([Specification](../module/forlab_argsort.html))
        #:for k1, t1 in INT_KINDS_TYPES+REAL_KINDS_TYPES
        module function argsort_${k1}$(x,order)
            integer,allocatable::argsort_${k1}$(:)
            ${t1}$,intent(in)::x(:)
            integer,optional,intent(in)::order
        end function argsort_${k1}$
        #:endfor
    end interface argsort

    interface bspline1
        module procedure bspline1_1
    end interface bspline1

    interface bspline2
        module procedure bspline2_2
    end interface bspline2

    interface chi2cdf
        module procedure chi2cdf0, chi2cdf1_0, chi2cdf1_1
    end interface chi2cdf

    interface chi2inv
        module procedure chi2inv0, chi2inv1_0, chi2inv1_1
    end interface chi2inv

    interface chi2pdf
        module procedure chi2pdf0, chi2pdf1_0, chi2pdf1_1
    end interface chi2pdf

    interface chi2rand
        #:for k1, t1 in REAL_KINDS_TYPES 
        impure elemental module subroutine chi2rand_${k1}$(X, v)
            implicit none
            ${t1}$, intent(out) :: X
            integer, intent(in) :: v
        end subroutine
        #:endfor
    end interface chi2rand

    interface chol
        !! chol computes Cholesky's decomposition of a symmetric positive
        !! definite matrix.
        !!([Specification](../module/forlab_chol.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        module function chol_${k1}$ (A) result(L)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:, :), allocatable :: L
        end function
        #:endfor

        !#:for k1,t1 in CMPLX_KINDS_TYPES
        !    module function chol_c${k1}$ (A) result(L)
        !        ${t1}$, dimension(:, :), intent(in) :: A
        !        ${t1}$, dimension(:, :), allocatable :: L
        !    end function
        !#:endfor
    end interface

    interface cov
        module procedure cov1_1, cov1_2, cov2_1, cov2_2
    end interface cov

    interface cumsum
        module procedure cumsum1, cumsum2
    end interface cumsum

    interface datenum
        real(dp) module function datenum0(year, month, day, hour, minute, &
                                    second, microsecond)
            integer, intent(in) :: year, month, day
            integer, intent(in), optional :: hour, minute, second, microsecond
        end function datenum0
    end interface

    interface datestr
        module procedure datestr0_0
    end interface datestr

    interface datevec
        module procedure datevec0
    end interface datevec

    interface dbindex
        module procedure dbindex1, dbindex2
    end interface dbindex

    interface deg2utm
        module procedure deg2utm0, deg2utm1
    end interface deg2utm


    interface det
        !! det computes the matrix determinant.
        !!([Specification](../module/forlab_det.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        ${t1}$ module function det_${k1}$ (A, outL, outU)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:, :), allocatable, intent(inout), optional :: outL, outU
        end function
        #:endfor
    end interface

    interface diag
        !! diag creates diagonal matrix or get the diagonal of a matrix.
        !!([Specification](../module/forlab_diag.html))
        #:for k1,t1 in REAL_KINDS_TYPES
            module function diag1_${k1}$ (A)
                ${t1}$, dimension(:), allocatable :: diag1_${k1}$
                ${t1}$, dimension(:, :), intent(in) :: A
            end function
            module function diag2_${k1}$ (x)
                ${t1}$, dimension(:, :), allocatable :: diag2_${k1}$
                ${t1}$, dimension(:), intent(in) :: x
            end function
        #:endfor
    end interface diag

    interface diff
        module procedure diff1, diff2
    end interface diff

    interface disp
        #:for k1,t1 in REAL_KINDS_TYPES
        module subroutine disp_r${k1}$0(x, string)
            ${t1}$, intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_r${k1}$1(x, string)
            ${t1}$, dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_r${k1}$2(A, string)
            ${t1}$, dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_r${k1}$3(X, dim, string)
            ${t1}$, dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: string
        end subroutine
        #:endfor
        #:for k1, t1 in CMPLX_KINDS_TYPES
        module subroutine disp_c${k1}$0(x, string)
            ${t1}$, intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_c${k1}$1(x, string)
            ${t1}$, dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_c${k1}$2(A, string)
            ${t1}$, dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
        end subroutine
        #:endfor
        module subroutine disp_l0(x, string)
            logical, intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_l1(x, string)
            logical, dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine disp_l1
        module subroutine disp_l2(A, string)
            logical, dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
        end subroutine disp_l2
        #:for k1, t1 in INT_KINDS_TYPES
        module subroutine disp_0_${k1}$(x, string)
            ${t1}$, intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_1_${k1}$(x, string)
            ${t1}$, dimension(:), intent(in) :: x
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_2_${k1}$(A, string)
            ${t1}$, dimension(:, :), intent(in) :: A
            character(len=*), intent(in), optional :: string
        end subroutine
        module subroutine disp_3_${k1}$(X, dim, string)
            ${t1}$, dimension(:, :, :), intent(in) :: X
            integer, intent(in), optional :: dim
            character(len=*), intent(in), optional :: string
        end subroutine
        #:endfor
        module subroutine disp_str(string)
            character(len=*), intent(in), optional :: string
        end subroutine
    end interface

    interface eig
        !! eig computes eigenvalues and eigenvectors of symmetric matrix using Jacobi algorithm.
        !!([Specification](../module/forlab_eig.html))
        #:for k1, t1 in REAL_KINDS_TYPES
        module subroutine eig_${k1}$(A,V,d,itermax)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:, :), allocatable, intent(out) :: V
            ${t1}$, dimension(:), allocatable, intent(out) :: d
            integer, intent(in), optional :: itermax
        end subroutine eig_${k1}$
        #:endfor
    end interface eig

    !! Eye
    interface eye
        #:for k1, t1 in REAL_KINDS_TYPES
        module subroutine eye_${k1}$(X)
            ${t1}$, intent(out) :: X(:, :)
        end subroutine
        #:endfor
    end interface

    interface find
        module procedure find1, find2, find3
    end interface find

    interface flip
        module procedure flip_i1, flip_r1, flip_i2, flip_r2, flip_i3, flip_r3
    end interface flip

    interface flipud
        module procedure flipud_i1, flipud_r1, flipud_i2, flipud_r2
    end interface flipud

    interface fliplr
        module procedure fliplr_i1, fliplr_r1, fliplr_i2, fliplr_r2
    end interface fliplr

    interface gammainc
        module procedure gammainc0, gammainc1_0
    end interface gammainc

    interface gmm
        module procedure gmm1, gmm2
    end interface gmm

    !! Horzcat & Vertcat
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + CMPLX_KINDS_TYPES + INT_KINDS_TYPES
        #:set CATTYPE = ['horzcat', 'vertcat']
        #:for c1 in CATTYPE
    interface ${c1}$
        #:for k1, t1 in RCI_KINDS_TYPES
        module function ${c1}$_${t1[0]}$_1_${k1}$(x1, x2)
            ${t1}$, dimension(:, :), allocatable :: ${c1}$_${t1[0]}$_1_${k1}$
            ${t1}$, dimension(:), intent(in) :: x1, x2
        end function
        module function ${c1}$_${t1[0]}$_2_${k1}$(A1, A2)
            ${t1}$, dimension(:, :), allocatable :: ${c1}$_${t1[0]}$_2_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A1, A2
        end function
        module function ${c1}$_${t1[0]}$_21_${k1}$(A1, x2)
            ${t1}$, dimension(:, :), allocatable :: ${c1}$_${t1[0]}$_21_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A1
            ${t1}$, dimension(:), intent(in) :: x2
        end function
        module function ${c1}$_${t1[0]}$_12_${k1}$(x1, A2)
            ${t1}$, dimension(:, :), allocatable :: ${c1}$_${t1[0]}$_12_${k1}$
            ${t1}$, dimension(:), intent(in) :: x1
            integer, dimension(:, :), intent(in) :: A2
        end function
        #:endfor
    end interface
        #:endfor

    interface interp1
        module procedure interp1_0, interp1_1
    end interface interp1

    interface interp2
        module procedure interp2_0, interp2_1, interp2_2
    end interface interp2

    interface interp3
        module procedure interp3_0, interp3_1
    end interface interp3

    interface inv
        #:for k1,t1 in REAL_KINDS_TYPES
            module function inv_${t1[0]}$${k1}$ (A)
                ${t1}$, dimension(:, :), allocatable :: inv_${t1[0]}$${k1}$
                ${t1}$, dimension(:, :), intent(in) :: A
            end function
        #:endfor
        #:for k1,t1 in CMPLX_KINDS_TYPES
            module function inv_${t1[0]}$${k1}$ (A)
                ${t1}$, dimension(:, :), allocatable :: inv_${t1[0]}$${k1}$
                ${t1}$, dimension(:, :), intent(in) :: A
            end function
        #:endfor
    end interface inv

    interface operator(.i.)
        !! Calculate the inverse of a real matrix.
        !! Example
        !! ---
        !! inv_of_A = .i.A
        #:for k1,t1 in REAL_KINDS_TYPES
        procedure inv_${t1[0]}$${k1}$
        #:endfor
        #:for k1,t1 in CMPLX_KINDS_TYPES
        procedure inv_${t1[0]}$${k1}$
        #:endfor
    end interface operator(.i.)

    interface operator(.x.)
        #:for k1 in REAL_KINDS
        module function rmut_${k1}$(m1, m2) result(ret)
            real(${k1}$), intent(in) :: m1(:, :), m2(:, :)
            real(${k1}$) :: ret(size(m1, 1), size(m2, 2))
        end function
        module function cmut_${k1}$(m1, m2) result(ret)
            complex(${k1}$), intent(in) :: m1(:, :), m2(:, :)
            complex(${k1}$) :: ret(size(m1, 1), size(m2, 2))
        end function
        module function rcmut_${k1}$(m1, m2) result(ret)
            real(${k1}$), intent(in) :: m1(:, :)
            complex(${k1}$), intent(in) :: m2(:, :)
            complex(${k1}$) :: ret(size(m1, 1), size(m2, 2))
        end function
        module function crmut_${k1}$(m1, m2) result(ret)
            real(${k1}$), intent(in) :: m2(:, :)
            complex(${k1}$), intent(in) :: m1(:, :)
            complex(${k1}$) :: ret(size(m1, 1), size(m2, 2))
        end function
        #:endfor
    end interface

    interface ismember
        module procedure ismember_i0i1, ismember_i0r1, ismember_i0i2, &
            ismember_i0r2, ismember_i0i3, ismember_i0r3, ismember_r0i1, &
            ismember_r0r1, ismember_r0i2, ismember_r0r2, ismember_r0i3, &
            ismember_r0r3
    end interface ismember

    interface issquare
        #:set RC_KINDS_TYPES = REAL_KINDS_TYPES + CMPLX_KINDS_TYPES
        #:for k1, t1 in RC_KINDS_TYPES
        logical module function issquare_${t1[0]}$${k1}$ (A)
            ${t1}$, dimension(:, :), intent(in) :: A
        end function
        #:endfor
    end interface

    interface issymmetric
        #:for k1, t1 in REAL_KINDS_TYPES
            logical module function issymmetric_${k1}$(A)
                ${t1}$, dimension(:, :), intent(in) :: A
            end function

        #:endfor
    end interface issymmetric

    interface kde
        module procedure kde1, kde2
    end interface kde

    interface kmeans
        module procedure kmeans1, kmeans2
    end interface kmeans

    interface kurtosis
        module procedure kurtosis1, kurtosis2
    end interface kurtosis

    !! Linspace & Logspace
    #:set SPACE = ['linspace', 'logspace']
    #:for s1 in SPACE
    interface ${s1}$
        #:for k1, t1 in REAL_KINDS_TYPES
        module subroutine ${s1}$_${k1}$(X, from, to)
            ${t1}$, dimension(:) :: X
            ${t1}$, intent(in) :: from, to
        end subroutine
        #:endfor
    end interface
    #:endfor

    interface loadbin
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES + CMPLX_KINDS_TYPES
        #:set RANKS = range(1,4)
        #:for RANK in RANKS
        #:for k1,t1 in RCI_KINDS_TYPES
        module subroutine loadbin_${RANK}$_${t1[0]}$${k1}$(filename, X)
            character(len=*), intent(in) :: filename
            #:if RANK == 1
            ${t1}$, dimension(:), allocatable, intent(out) :: X
            #:elif RANK == 2
            ${t1}$, dimension(:, :), allocatable, intent(out) :: X
            #:elif RANK == 3
            ${t1}$, dimension(:, :, :), allocatable, intent(out) :: X
            #:endif
        end subroutine
        #:endfor
        #:endfor
    end interface

    interface loadtxt
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES + CMPLX_KINDS_TYPES
        #:set RANKS = range(1,3)
        #:for RANK in RANKS
        #:for k1,t1 in RCI_KINDS_TYPES
        module subroutine loadtxt_${RANK}$_${t1[0]}$${k1}$(filename, X)
            character(len=*), intent(in) :: filename
            #:if RANK == 1
            ${t1}$, dimension(:), allocatable, intent(out) :: X
            #:elif RANK == 2
            ${t1}$, dimension(:, :), allocatable, intent(out) :: X
            #:endif
        end subroutine
        #:endfor
        #:endfor
    end interface

    interface log2
        module procedure log2_i0, log2_r0, log2_i1, log2_r1
    end interface log2

    interface lu
        !! lu computes the LU matrix factorization.
        !!([Specification](../module/forlab_lu.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        module subroutine lu_${k1}$ (A, L, U)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:, :), allocatable, intent(out) :: L, U
        end subroutine
        #:endfor
    end interface

    interface mad
        module procedure mad1, mad2
    end interface mad

    interface matpow
        !! Calculat matrix power
        !!([Specification](../module/forlab_matpow.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        module function matpow_${k1}$(a,num)result(c)
            ${t1}$, dimension(:,:), intent(in) :: a
            ${t1}$,allocatable :: c(:,:)
            integer::num
        end function matpow_${k1}$
        #:endfor
    end interface

    interface mbkmeans
        module procedure mbkmeans1, mbkmeans2
    end interface mbkmeans

    interface median
        module procedure median1, median2
    end interface median

    interface mean
        !! mean computes the mean value of an array.
        !! ([Specification](../module/forlab_stats_mean.html))
        #:for k1, t1 in REAL_KINDS_TYPES
        ${t1}$ module function mean_1_${k1}$(x)
            ${t1}$, dimension(:), intent(in) :: x
        end function
        module function mean_2_${k1}$(A, dim)
            ${t1}$, dimension(:), allocatable :: mean_2_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: dim
        end function
        #:endfor
    end interface mean

    interface meshgrid
        module procedure meshgrid2
    end interface meshgrid

    interface nextpow2
        module procedure nextpow2_0, nextpow2_1
    end interface nextpow2

    interface norm
        !! norm computes vector and matrix norms.
        !!([Specification](../module/forlab_norm.html))
        #:for k1,t1 in REAL_KINDS_TYPES
        module function norm1_${k1}$(x,p)result(norm1)
            ${t1}$, dimension(:), intent(in) :: x
            ${t1}$, intent(in), optional :: p
            ${t1}$:: norm1
        end function norm1_${k1}$
        module function norm2_${k1}$(A,p)result(norm2)
            ${t1}$, dimension(:,:), intent(in) :: A
            ${t1}$, intent(in), optional :: p
            ${t1}$:: norm2
        end function norm2_${k1}$
        #:endfor
    end interface norm

    interface normpdf
        module procedure normpdf0, normpdf1, normpdf2
    end interface normpdf

    interface num2str
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + CMPLX_KINDS_TYPES + INT_KINDS_TYPES
        #:for k1, t1 in RCI_KINDS_TYPES
        module function num2str_${t1[0]}$${k1}$(x, fmt)
            character(len=:), allocatable :: num2str_${t1[0]}$${k1}$
            ${t1}$, intent(in) :: x
            character(len=*), intent(in), optional :: fmt
        end function
        #:endfor
    end interface

    interface ones
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + CMPLX_KINDS_TYPES + INT_KINDS_TYPES
        #:for k1, t1 in RCI_KINDS_TYPES
        impure elemental module subroutine ones_${t1[0]}$${k1}$(X)
            ${t1}$, intent(out) :: X
        end subroutine
        #:endfor
    end interface

    interface outer
        #:for k1,t1 in INT_KINDS_TYPES
        module function outer_${k1}$(x, y)
            ${t1}$, dimension(:,:), allocatable :: outer_${k1}$
            ${t1}$, dimension(:), intent(in) :: x, y
        end function
        #:endfor
        #:for k1,t1 in REAL_KINDS_TYPES
        module function outer_${k1}$(x, y)
            ${t1}$, dimension(:,:), allocatable :: outer_${k1}$
            ${t1}$, dimension(:), intent(in) :: x, y
        end function
        #:endfor
    end interface

    interface prctile
        module procedure prctile0, prctile1
    end interface prctile

    !! Progress_bar & Progress_perc
    interface
        module subroutine progress_bar(iter, itermax, step)
            integer, intent(in) :: iter, itermax
            integer, intent(in), optional :: step
        end subroutine progress_bar
        module subroutine progress_perc(iter, itermax, prefix)
            integer, intent(in) :: iter, itermax
            character(len=*), intent(in), optional :: prefix
        end subroutine progress_perc
    end interface



    interface repmat
        module procedure repmat1, repmat2
    end interface repmat

    interface rms
        module procedure rms1, rms2
    end interface rms

    interface savebin
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES + CMPLX_KINDS_TYPES
        #:set RANKS = range(1,4)
        #:for RANK in RANKS
        #:for k1,t1 in RCI_KINDS_TYPES
        module subroutine savebin_${RANK}$_${t1[0]}$${k1}$(filename, X)
            character(len=*), intent(in) :: filename
            #:if RANK == 1
            ${t1}$, dimension(:), intent(in) :: X
            #:elif RANK == 2
            ${t1}$, dimension(:, :), intent(in) :: X
            #:elif RANK == 3
            ${t1}$, dimension(:, :, :), intent(in) :: X
            #:endif
        end subroutine
        #:endfor
        #:endfor
    end interface

    interface savetxt
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES + CMPLX_KINDS_TYPES
        #:set RANKS = range(1,3)
        #:for RANK in RANKS
        #:for k1,t1 in RCI_KINDS_TYPES
        module subroutine savetxt_${RANK}$_${t1[0]}$${k1}$(filename, X)
            character(len=*), intent(in) :: filename
            #:if RANK == 1
            ${t1}$, dimension(:), intent(in) :: X
            #:elif RANK == 2
            ${t1}$, dimension(:, :), intent(in) :: X
            #:endif
        end subroutine
        #:endfor
        #:endfor
    end interface

    interface seq
        !! seq returns evenly spaced vector.
        !!([Specification](../module/forlab_seq.html))
        #:set RI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES 
        #:for k1, t1 in RI_KINDS_TYPES
        module subroutine seq_${k1}$ (X, from, to, by)
            ${t1}$, dimension(:), allocatable, intent(out) :: X
            ${t1}$, intent(in) :: from, to
            ${t1}$, optional, intent(in) :: by
        end subroutine
        #:endfor
    end interface

    interface signum
        module procedure signum0, signum1, signum2
    end interface signum

    interface sinc
        module procedure sinc0, sinc1
    end interface sinc

    interface silhouette
        module procedure silhouette1, silhouette2
    end interface silhouette

    interface skewness
        module procedure skewness1, skewness2
    end interface skewness

    interface solve
        #:for k1,t1 in REAL_KINDS_TYPES
        module function solve_${k1}$(A, b) result(x)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:), intent(in) :: b
            ${t1}$, dimension(:), allocatable :: x
        end function solve_${k1}$

        #:endfor
    end interface solve

    interface sort
    #:for k1, t1 in INT_KINDS_TYPES+REAL_KINDS_TYPES
        module function sort_${k1}$(x,order)
            ${t1}$,allocatable::sort_${k1}$(:)
            ${t1}$,intent(in)::x(:)
            integer,optional,intent(in)::order
        end function sort_${k1}$

    #:endfor
    end interface sort

    interface spline1
        module procedure spline1_0, spline1_1
    end interface spline1

    interface spline2
        module procedure spline2_1, spline2_2
    end interface spline2

    interface svd
    #:for k1, t1 in REAL_KINDS_TYPES
        module subroutine svd_${k1}$(a, w, u, v , d, ierr)
            ${t1}$, dimension(:, :), intent(in) :: a
            ${t1}$, dimension(:), allocatable, intent(out) :: w
            ${t1}$, dimension(:, :), allocatable, intent(out), optional :: u, v
            integer, intent(out), optional :: ierr
            logical,intent(in),optional ::d
        end subroutine svd_${k1}$

    #:endfor
    end interface svd

    interface svdsolve
        #:for k1, t1 in REAL_KINDS_TYPES
        module function svdsolve_${k1}$(A, b, cutoff) result(x)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$, dimension(:), intent(in) :: b
            ${t1}$, dimension(:), allocatable :: x
            integer, intent(in), optional :: cutoff
        end function svdsolve_${k1}$

        #:endfor
    end interface svdsolve
    interface toc
        module subroutine toc_default()
        end subroutine
        #:for k1, t1 in REAL_KINDS_TYPES
        module subroutine toc_${k1}$(t)
            ${t1}$, intent(out) :: t
        end subroutine
        #:endfor
    end interface

    interface trace
        #:for k1,t1 in REAL_KINDS_TYPES
        module function trace_${k1}$(A)result(trace)
            ${t1}$, dimension(:, :), intent(in) :: A
            ${t1}$::trace
        end function trace_${k1}$
        #:endfor
    end interface trace
    interface tril
        #:for k1,t1 in INT_KINDS_TYPES+REAL_KINDS_TYPES
        module function tril_${k1}$(A,k)
            ${t1}$, dimension(:, :), allocatable :: tril_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_${k1}$
        #:endfor
        #:for k1,t1 in CMPLX_KINDS_TYPES
        module function tril_c${k1}$(A,k)
            ${t1}$, dimension(:, :), allocatable :: tril_c${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function tril_c${k1}$
    #:endfor
    end interface tril

    interface triu
        #:for k1,t1 in INT_KINDS_TYPES+REAL_KINDS_TYPES
        module function triu_${k1}$(A,k)
            ${t1}$, dimension(:, :), allocatable :: triu_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_${k1}$
        #:endfor
        #:for k1,t1 in CMPLX_KINDS_TYPES
        module function triu_c${k1}$(A,k)
            ${t1}$, dimension(:, :), allocatable :: triu_c${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: k
        end function triu_c${k1}$
        #:endfor
    end interface triu

    interface utm2deg
        module procedure utm2deg0, utm2deg1
    end interface utm2deg

    !! Var & Std
    #:set VSNAME = ['var', 'std']
    #:for v1 in VSNAME
    interface ${v1}$
        #:if v1 == 'var'
        !! `var` computes vector and matrix variances.
        !!([Specification](../module/forlab_var.html))
        #:elif v1 == 'std'
        !! `std` computes vector and matrix standard deviations.
        !!([Specification](../module/forlab_var.html))
        #:endif
        #:for k1, t1 in REAL_KINDS_TYPES
        ${t1}$ module function ${v1}$_1_${k1}$(x, w)
            ${t1}$, dimension(:), intent(in) :: x
            integer, intent(in), optional :: w
        end function
        module function ${v1}$_2_${k1}$(A, w, dim)
            ${t1}$, dimension(:), allocatable :: ${v1}$_2_${k1}$
            ${t1}$, dimension(:, :), intent(in) :: A
            integer, intent(in), optional :: w, dim
        end function
        #:endfor
    end interface
    #:endfor

    !! Qr
    interface qr
        #:for k1,t1 in REAL_KINDS_TYPES
        module subroutine qr_${k1}$(a,q,r,l)
            ${t1}$,intent(in)::a(:,:)
            ${t1}$,allocatable,intent(out)::q(:,:),r(:,:)
            integer,optional::l
        end subroutine qr_${k1}$

        #:endfor
    end interface qr

    !! Rng
    interface
        module subroutine rng(seed)
            integer, intent(in), optional :: seed
        end subroutine rng
        module subroutine tic()
        end subroutine
    end interface

    !! Randn
    interface randn
        #:for k1, t1 in REAL_KINDS_TYPES
        impure elemental module subroutine randn_${k1}$(X, mean, std)
            ${t1}$, intent(out) :: X
            ${t1}$, optional, intent(in) :: mean, std
        end subroutine
        #:endfor
    end interface

    !! Randu
    interface randu
        #:set RI_KINDS_TYPES = REAL_KINDS_TYPES + INT_KINDS_TYPES
        #:for k1, t1 in RI_KINDS_TYPES
        impure elemental module subroutine randu_${t1[0]}$${k1}$(X, from, to)
            ${t1}$, intent(out) :: X
            ${t1}$, optional, intent(in) :: from, to 
        end subroutine
        #:endfor
    end interface

    interface zeros
        #:set RCI_KINDS_TYPES = REAL_KINDS_TYPES + CMPLX_KINDS_TYPES + INT_KINDS_TYPES
        #:for k1, t1 in RCI_KINDS_TYPES
        impure elemental module subroutine zeros_${t1[0]}$${k1}$(X)
            ${t1}$, intent(out) :: X
        end subroutine
        #:endfor
    end interface

contains

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

    integer(kind=IPRE) function argmax1(x)
        real(kind=RPRE), dimension(:), intent(in) :: x

        argmax1 = maxloc(x, 1,.not. isnan(x))
        return
    end function argmax1

    function argmax2(A)
        integer(kind=IPRE) :: argmax2(2)
        real(kind=IPRE), dimension(:, :), intent(in) :: A

        argmax2 = maxloc(A,.not. isnan(A))
        return
    end function argmax2

    function argmax3(X)
        integer(kind=IPRE) :: argmax3(3)
        real(kind=IPRE), dimension(:, :, :), intent(in) :: X

        argmax3 = maxloc(X,.not. isnan(X))
        return
    end function argmax3

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

    integer(kind=IPRE) function argmin1(x)
        real(kind=RPRE), dimension(:), intent(in) :: x

        argmin1 = minloc(x, 1,.not. isnan(x))
        return
    end function argmin1

    function argmin2(A)
        integer(kind=IPRE) :: argmin2(2)
        real(kind=IPRE), dimension(:, :), intent(in) :: A

        argmin2 = minloc(A,.not. isnan(A))
        return
    end function argmin2

    function argmin3(X)
        integer(kind=IPRE) :: argmin3(3)
        real(kind=IPRE), dimension(:, :, :), intent(in) :: X

        argmin3 = minloc(X,.not. isnan(X))
        return
    end function argmin3

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

    subroutine bsplrep1(x, y, xq, yq, order, n1)
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:), allocatable, intent(out) :: xq, yq
        integer(kind=IPRE), intent(in), optional :: order, n1
        integer(kind=IPRE) :: opt_n1, i, iq, j, k, n
        real(kind=RPRE) :: w
        integer(kind=IPRE), dimension(:), allocatable :: x0
        real(kind=RPRE), dimension(:), allocatable :: t, y1

        n = size(x)
        k = 4
        opt_n1 = 100
        if (present(order)) k = order
        if (present(n1)) opt_n1 = n1

        if (k > n) then
            print *, "Error: in bsplrep1, order k should be less than the " &
                //"number of control points ("//num2str(k)//" > " &
                //num2str(n)//")."
            stop
        end if

        allocate(xq(opt_n1), yq(opt_n1))
        call zeros(xq)
        call zeros(yq)

        block
            real(8), allocatable :: tmp1(:), tmp2(:), tmp3(:)
            allocate(tmp1(k-1),tmp2(n-k+2),tmp3(k-1))
            call zeros(tmp1)
            call linspace(tmp2, 0.d0, 1.d0)
            call ones(tmp3)
            t = [tmp1, tmp2, tmp3]
        endblock
        allocate(y1(opt_n1))
        call linspace(y1, 0.d0, 1.d0)

        do iq = 1, opt_n1
            x0 = find(y1(iq) >= t)
            j = min(n, x0(size(x0)))
            do i = j - k + 1, j
                w = deboor(i, k, y1(iq), t)
                xq(iq) = xq(iq) + x(i)*w
                yq(iq) = yq(iq) + y(i)*w
            end do
        end do
        return
    end subroutine bsplrep1

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

    subroutine bsplrep2(x, y, z, xq, yq, zq, order, n1, n2)
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: z
        real(kind=RPRE), dimension(:, :), allocatable, intent(out) :: xq, yq, zq
        integer(kind=IPRE), intent(in), optional :: order, n1, n2
        integer(kind=IPRE) :: opt_n1, opt_n2, i1, i2, iq1, iq2, j1, j2, k, m, n
        real(kind=RPRE) :: w1, w2
        integer(kind=IPRE), dimension(:), allocatable :: x0, y0
        real(kind=RPRE), dimension(:), allocatable :: t1, t2, y1, y2

        m = size(x)
        n = size(y)
        k = 4
        opt_n1 = 100
        opt_n2 = 100
        if (present(order)) k = order
        if (present(n1)) opt_n1 = n1
        if (present(n2)) opt_n2 = n2

        if (k > min(m, n)) then
            print *, "Error: in bsplrep2, order k should be less than the " &
                //"number of control points ("//num2str(k)//" > " &
                //num2str(min(m, n))//")."
            stop
        end if

        allocate(xq(opt_n1, opt_n2), yq(opt_n1, opt_n2), zq(opt_n1, opt_n2))
        call zeros(xq)
        call zeros(yq)
        call zeros(zq)

        block
            real(8), allocatable :: tmp1(:), tmp2(:), tmp3(:)
            allocate(tmp1(k-1),tmp2(m-k+2),tmp3(k-1))
            call zeros(tmp1)
            call linspace(tmp2, 0.d0, 1.d0)
            call ones(tmp3)
            t1 = [tmp1, tmp2, tmp3]
            allocate(tmp2(n-k+2))
            t2 = [tmp1, tmp2, tmp3]
        endblock

        allocate(y1(opt_n1), y2(opt_n2))
        call linspace(y1, 0.d0, 1.d0)
        call linspace(y2, 0.d0, 1.d0)

        do iq1 = 1, opt_n1
            x0 = find(y1(iq1) >= t1)
            j1 = min(m, x0(size(x0)))
            do iq2 = 1, opt_n2
                y0 = find(y2(iq2) >= t2)
                j2 = min(n, y0(size(y0)))
                do i1 = j1 - k + 1, j1
                    w1 = deboor(i1, k, y1(iq1), t1)
                    do i2 = j2 - k + 1, j2
                        w2 = deboor(i2, k, y2(iq2), t2)
                        xq(iq1, iq2) = xq(iq1, iq2) + x(i1)*w1*w2
                        yq(iq1, iq2) = yq(iq1, iq2) + y(i2)*w1*w2
                        zq(iq1, iq2) = zq(iq1, iq2) + z(i1, i2)*w1*w2
                    end do
                end do
            end do
        end do
        return
    end subroutine bsplrep2

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

    function bspline1_1(x, y, xq, order, n1) result(yq)
        real(kind=RPRE), dimension(:), allocatable :: yq
        real(kind=RPRE), dimension(:), intent(in) :: x, y, xq
        integer(kind=IPRE), intent(in), optional :: order, n1
        integer(kind=IPRE) :: k, n, opt_n1
        real(kind=RPRE), dimension(:), allocatable :: bspl_x, bspl_y

        n = size(x)
        k = 4
        opt_n1 = 100
        if (present(order)) k = order
        if (present(n1)) opt_n1 = n1
        if (k > n) then
            print *, "Error: in bspline1, order k should be less than the " &
                //"number of control points ("//num2str(k)//" > " &
                //num2str(n)//")."
            stop
        end if

        call bsplrep1(x, y, bspl_x, bspl_y, k, opt_n1)
        yq = spline1(bspl_x, bspl_y, xq)
        return
    end function bspline1_1

    ! bspline2
    !-----------------------------------------------------------------------
    ! bspline2 approximates a set of 2-dimensional control points with
    ! spline curves in B-spline form.
    !
    ! Syntax
    !-----------------------------------------------------------------------
    ! ZQ = bspline2(x, y, z, XQ, YQ)
    ! ZQ = bspline2(x, y, z, XQ, YQ, order)
    !
    ! Description
    !-----------------------------------------------------------------------
    ! ZQ = bspline2(x, y, Z, XQ, YQ) returns the evaluated matrix ZQ given
    ! mesh type grids XQ and YQ using a bicubic spline (degree 3). ZQ is of
    ! the same shape as XQ and YQ.
    !
    ! ZQ = bspline2(x, y, Z, XQ, YQ, order) returns the evaluated matrix ZQ
    ! given mesh type grids XQ and YQ with spline curves given the order. ZQ
    ! is of the same shape as XQ and YQ.

    function bspline2_2(x, y, z, xq, yq, order) result(zq)
        real(kind=RPRE), dimension(:, :), allocatable :: zq
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: z, xq, yq
        integer(kind=IPRE), intent(in), optional :: order
        integer(kind=IPRE) :: i, m, n, ny, k
        real(kind=RPRE), dimension(:, :), allocatable :: tmp

        m = size(xq, 1)
        n = size(xq, 2)
        ny = size(y)
        k = 4
        if (present(order)) k = order

        allocate(tmp(ny, n))
        call zeros(tmp)
        do i = 1, ny
            tmp(i, :) = bspline1(x, z(i, :), xq(i, :), k)
        end do

        allocate(tmp(m, n))
        call zeros(tmp)
        do i = 1, n
            zq(:, i) = bspline1(y, tmp(:, i), yq(:, i), k)
        end do

        return
    end function bspline2_2

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

    subroutine check_directory(dirname)
        character(len=:), allocatable, intent(inout) :: dirname
        integer(kind=IPRE) :: i

        i = len_trim(dirname)
        if (dirname(i:i) /= "/") dirname = trim(dirname)//"/"
        return
    end subroutine check_directory

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

    real(kind=RPRE) function chi2cdf0(x, v)
        real(kind=RPRE), intent(in) :: x
        integer(kind=IPRE), intent(in) :: v

        chi2cdf0 = gammainc(real(x/2., RPRE), real(v/2., RPRE))
        return
    end function chi2cdf0

    function chi2cdf1_0(X, v)
        real(kind=RPRE), dimension(:), allocatable :: chi2cdf1_0
        real(kind=RPRE), dimension(:), intent(in) :: X
        integer(kind=IPRE), intent(in) :: v
        integer(kind=IPRE) :: i, n

        n = size(X)
        allocate(chi2cdf1_0(n))
        call zeros(chi2cdf1_0)
        do i = 1, n
            chi2cdf1_0(i) = chi2cdf0(X(i), v)
        end do
        return
    end function chi2cdf1_0

    function chi2cdf1_1(X, V)
        real(kind=RPRE), dimension(:), allocatable :: chi2cdf1_1
        real(kind=RPRE), dimension(:), intent(in) :: X
        integer(kind=IPRE), dimension(:), intent(in) :: V
        integer(kind=IPRE) :: i, n

        n = size(X)
        allocate(chi2cdf1_1(n))
        call zeros(chi2cdf1_1)
        do i = 1, n
            chi2cdf1_1(i) = chi2cdf0(X(i), V(i))
        end do
        return
    end function chi2cdf1_1

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

    real(kind=RPRE) function chi2inv0(p, v)
        real(kind=RPRE), intent(in), target :: p
        integer(kind=IPRE), intent(in), target :: v
        real(kind=RPRE) :: a, b
        real(kind=RPRE), pointer, save :: p_ptr
        integer(kind=IPRE), pointer, save :: v_ptr

        if (p <= 0. .or. p >= 1.) then
            print *, "Error: in chi2inv0(p, v), p should be between 0 and 1"
            stop
        end if
        if (v <= 0) then
            print *, "Error: in chi2inv0(p, v), v should be greater than 0"
            stop
        end if

        p_ptr => p
        v_ptr => v
        a = 0.
        b = real(v, RPRE)
        do while (chi2cdf(b, v) < p)
            b = b*b
        end do
        chi2inv0 = fminbnd(chi2func, a, b)
        return
    contains

        real(kind=RPRE) function chi2func(x)
            real(kind=RPRE), intent(in) :: x
            chi2func = abs(chi2cdf0(x, v_ptr) - p_ptr)
            return
        end function chi2func

    end function chi2inv0

    function chi2inv1_0(P, v)
        real(kind=RPRE), dimension(:), allocatable :: chi2inv1_0
        real(kind=RPRE), dimension(:), intent(in) :: P
        integer(kind=IPRE), intent(in) :: v
        integer(kind=IPRE) :: i, n

        n = size(P)
        allocate(chi2inv1_0(n))
        call zeros(chi2inv1_0)
        do i = 1, n
            chi2inv1_0(i) = chi2inv0(P(i), v)
        end do
        return
    end function chi2inv1_0

    function chi2inv1_1(P, V)
        real(kind=RPRE), dimension(:), allocatable :: chi2inv1_1
        real(kind=RPRE), dimension(:), intent(in) :: P
        integer(kind=IPRE), dimension(:), intent(in) :: V
        integer(kind=IPRE) :: i, n

        n = size(P)
        allocate(chi2inv1_1(n))
        call zeros(chi2inv1_1)
        do i = 1, n
            chi2inv1_1(i) = chi2inv0(P(i), V(i))
        end do
        return
    end function chi2inv1_1

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

    real(kind=RPRE) function chi2pdf0(x, v)
        real(kind=RPRE), intent(in) :: x
        integer(kind=IPRE), intent(in) :: v
        real(kind=RPRE) :: v2

        if (x > 0.) then
            v2 = 0.5*real(v, RPRE)
            chi2pdf0 = 1./(2.*gamma(v2))*(x/2)**(v2 - 1.)*exp(-x/2.)
        else
            chi2pdf0 = 0.
        end if
        return
    end function chi2pdf0

    function chi2pdf1_0(X, v)
        real(kind=RPRE), dimension(:), allocatable :: chi2pdf1_0
        real(kind=RPRE), dimension(:), intent(in) :: X
        integer(kind=IPRE), intent(in) :: v
        integer(kind=IPRE) :: i, n

        n = size(X)
        allocate(chi2pdf1_0(n))
        call zeros(chi2pdf1_0)
        do i = 1, n
            chi2pdf1_0(i) = chi2pdf0(X(i), v)
        end do
        return
    end function chi2pdf1_0

    function chi2pdf1_1(X, V)
        real(kind=RPRE), dimension(:), allocatable :: chi2pdf1_1
        real(kind=RPRE), dimension(:), intent(in) :: X
        integer(kind=IPRE), dimension(:), intent(in) :: V
        integer(kind=IPRE) :: i, n

        n = size(X)
        allocate(chi2pdf1_1(n))
        call zeros(chi2pdf1_1)
        do i = 1, n
            chi2pdf1_1(i) = chi2pdf0(X(i), V(i))
        end do
        return
    end function chi2pdf1_1

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

    real(kind=RPRE) function cov1_1(x, w)
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in), optional :: w
        integer(kind=IPRE) :: opt_w
        real(kind=RPRE), dimension(:), allocatable :: tmp

        opt_w = 0
        if (present(w)) opt_w = w

        tmp = x - mean(x)
        select case (opt_w)
        case (0)
            cov1_1 = dot_product(tmp, tmp)/(size(x) - 1)
        case (1)
            cov1_1 = dot_product(tmp, tmp)/size(x)
        end select
        return
    end function cov1_1

    function cov1_2(X, w)
        real(kind=RPRE), dimension(:, :), allocatable :: cov1_2
        real(kind=RPRE), dimension(:, :), intent(in) :: X
        integer(kind=IPRE), intent(in), optional :: w
        integer(kind=IPRE) :: opt_w
        real(kind=RPRE), dimension(:, :), allocatable :: tmp

        opt_w = 0
        if (present(w)) opt_w = w

        tmp = X - repmat(mean(X, 1), size(X, 1), 2)
        select case (opt_w)
        case (0)
            cov1_2 = matmul(transpose(tmp), tmp)/(size(X, 1) - 1)
        case (1)
            cov1_2 = matmul(transpose(tmp), tmp)/size(X, 1)
        end select
        return
    end function cov1_2

    function cov2_1(x, y, w)
        real(kind=RPRE), dimension(:, :), allocatable :: cov2_1
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        integer(kind=IPRE), intent(in), optional :: w
        integer(kind=IPRE) :: opt_w
        real(kind=RPRE), dimension(:, :), allocatable :: tmp

        opt_w = 0
        if (present(w)) opt_w = w

        cov2_1 = cov1_2(horzcat(x, y), opt_w)
        return
    end function cov2_1

    function cov2_2(X, Y, w)
        real(kind=RPRE), dimension(:, :), allocatable :: cov2_2
        real(kind=RPRE), dimension(:, :), intent(in) :: X, Y
        integer(kind=IPRE), intent(in), optional :: w
        integer(kind=IPRE) :: opt_w

        opt_w = 0
        if (present(w)) opt_w = w

        if (all(shape(X) == shape(Y))) then
            cov2_2 = cov1_2(horzcat([X], [Y]), opt_w)
        else
            stop "Error: in cov(X, Y), X and Y should have the same shape."
        end if
        return
    end function cov2_2

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

    function cumsum1(x)
        real(kind=RPRE), dimension(:), allocatable :: cumsum1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: i, n
        real(kind=RPRE), dimension(:), allocatable :: xsort

        n = size(x)
        xsort = sort(x, 1)
        cumsum1 = [(sum(xsort(1:i)), i=1, n)]
        return
    end function cumsum1

    function cumsum2(A, dim)
        real(kind=RPRE), dimension(:, :), allocatable :: cumsum2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: i, m, n

        m = size(A, 1)
        n = size(A, 2)
        allocate(cumsum2(m,n))
        call zeros(cumsum2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            do i = 1, n
                cumsum2(:, i) = cumsum1(A(:, i))
            end do
        elseif (dim == 2) then
            do i = 1, m
                cumsum2(i, :) = cumsum1(A(i, :))
            end do
        end if
        return
    end function cumsum2

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

    function datestr0_0(t)
        character(len=:), allocatable :: datestr0_0
        real(kind=8), intent(in) :: t
        integer(kind=IPRE) :: d(7)
        character(len=CLEN) :: dstr
        character(len=3) :: months_in_letters(12)

        d = datevec(t)

        ! Day
        !=====
        if (d(3) < 10) then
            dstr = "0"//num2str(d(3))//"-"
        else
            dstr = num2str(d(3))//"-"
        end if

        ! Month
        !=======
        months_in_letters = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", &
                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
        dstr = trim(dstr)//months_in_letters(d(2))//"-"

        ! Year
        !======
        dstr = trim(dstr)//num2str(d(1))

        if ((d(4) == 0) .and. (d(5) == 0) &
            .and. (d(6) == 0) .and. (d(7) == 0)) then
            datestr0_0 = trim(dstr)
            return

        else

            ! Hour
            !======
            if (d(4) < 10) then
                dstr = trim(dstr)//" "//"0"//num2str(d(4))//":"
            else
                dstr = trim(dstr)//" "//num2str(d(4))//":"
            end if

            ! Minute
            !========
            if (d(5) < 10) then
                dstr = trim(dstr)//"0"//num2str(d(5))//":"
            else
                dstr = trim(dstr)//num2str(d(5))//":"
            end if

            ! Second
            !========
            if (d(6) < 10) then
                dstr = trim(dstr)//"0"//num2str(d(6))//"."
            else
                dstr = trim(dstr)//num2str(d(6))//"."
            end if

            ! Microsecond
            !=============
            if (d(7) < 10) then
                dstr = trim(dstr)//"00000"//num2str(d(7))
            elseif (d(7) < 100) then
                dstr = trim(dstr)//"0000"//num2str(d(7))
            elseif (d(7) < 1000) then
                dstr = trim(dstr)//"000"//num2str(d(7))
            elseif (d(7) < 10000) then
                dstr = trim(dstr)//"00"//num2str(d(7))
            elseif (d(7) < 100000) then
                dstr = trim(dstr)//"0"//num2str(d(7))
            else
                dstr = trim(dstr)//num2str(d(7))
            end if

            datestr0_0 = trim(dstr)
        end if
        return
    end function datestr0_0

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

    function datevec0(t)
        integer(kind=IPRE) :: datevec0(7)
        real(kind=8), intent(in) :: t
        integer(kind=IPRE) :: i, days_per_month(12)
        real(kind=8) :: tmp, dateres

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
            if (tmp < floor(t)) then
                datevec0(1) = datevec0(1) + 1
            else
                exit
            end if
        end do
        dateres = floor(t) - datenum(datevec0(1), 1, 1)

        ! Month
        !=======
        tmp = 0.0d0
        days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        do i = 1, 12
            tmp = tmp + days_per_month(i)
            if ((isleap(datevec0(1))) .and. (i == 2)) tmp = tmp + 1
            if (tmp >= dateres) then
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
        datevec0(4) = floor(dateres*24.0d0)
        dateres = dateres - datevec0(4)/24.0d0

        ! Minute
        !========
        datevec0(5) = floor(dateres*24.0d0*60.0d0)
        dateres = dateres - datevec0(5)/(24.0d0*60.0d0)

        ! Second
        !========
        datevec0(6) = floor(dateres*24.0d0*60.0d0*60.0d0)
        dateres = dateres - datevec0(6)/(24.0d0*60.0d0*60.0d0)

        ! Microsecond
        !=============
        datevec0(7) = floor(dateres*24.0d0*60.0d0*60.0d0*1.0d+6)

        return
    end function datevec0

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

    real(kind=RPRE) function dbindex1(x, cluster, means, p, q) result(db)
        real(kind=RPRE), dimension(:), intent(in) :: x, means
        integer(kind=IPRE), dimension(:), intent(in) :: cluster
        real(kind=RPRE), intent(in), optional :: p, q
        integer(kind=IPRE) :: K, n
        real(kind=RPRE) :: opt_p, opt_q
        real(kind=RPRE), dimension(:, :), allocatable :: A, mu

        K = size(means)
        if (K == 1) then
            print *, "Warning: in dbindex, the Davies-Bouldin index cannot " &
                //"be defined for K = 1."
            db = 1.0d0
            return
        end if

        opt_p = 2.
        opt_q = 2.
        if (present(p)) opt_p = p
        if (present(q)) opt_q = q

        n = size(x)
        A = reshape(x, shape=[n, 1], order=[1, 2])
        mu = reshape(x, shape=[K, 1], order=[1, 2])
        db = dbindex2(A, cluster, mu, opt_p, opt_q)
        return
    end function dbindex1

    real(kind=RPRE) function dbindex2(X, cluster, means, p, q) result(db)
        real(kind=RPRE), dimension(:, :), intent(in) :: X
        integer(kind=IPRE), dimension(:), intent(in) :: cluster
        real(kind=RPRE), dimension(:, :), intent(in) :: means
        real(kind=RPRE), intent(in), optional :: p, q
        integer(kind=IPRE) :: i, j, K
        real(kind=RPRE) :: opt_p, opt_q, Mij
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:), allocatable :: S
        real(kind=RPRE), dimension(:, :), allocatable :: R

        K = size(means, 1)
        if (K == 1) then
            print *, "Warning: in dbindex, the Davies-Bouldin index cannot " &
                //"be defined for K = 1."
            db = 1.0d0
            return
        end if

        opt_p = 2.
        opt_q = 2.
        if (present(p)) opt_p = p
        if (present(q)) opt_q = q

        ! Measure the scattering within each cluster
        !============================================
        allocate(S(K))
        call zeros(S)
        do i = 1, K
            idx = find(cluster == i)
            do j = 1, size(idx)
                S(i) = S(i) + norm(X(idx(j), :) - means(i, :), opt_q)**2
            end do
            S(i) = sqrt(S(i)/real(size(idx), RPRE))
        end do

        ! Measure the similarity function R between each cluster
        !========================================================
        allocate(R(K,K))
        call zeros(R)
        do i = 1, K - 1
            do j = i + 1, K
                Mij = norm(means(i, :) - means(j, :), opt_p)  ! Distance between clusters i and j
                R(i, j) = (S(i) + S(j))/Mij
                R(j, i) = R(i, j)
            end do
        end do

        ! Compute the Davies-Bouldin index
        !==================================
        db = mean([(maxval(R(i, :)), i=1, K)])

        return
    end function dbindex2

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

    recursive function deboor(i, k, x, t) result(db)
        real(kind=RPRE) :: db
        integer(kind=IPRE), intent(in) :: i, k
        real(kind=RPRE), intent(in) :: x
        real(kind=RPRE), dimension(:), intent(in) :: t
        real(kind=RPRE) :: A1, A2

        if (k == 1) then
            if (x /= t(size(t))) then
                if ((x >= t(i)) .and. (x < t(i + 1))) then
                    db = 1.0d0
                else
                    db = 0.0d0
                end if
            else
                if ((x >= t(i)) .and. (x <= t(i + 1))) then
                    db = 1.0d0
                else
                    db = 0.0d0
                end if
            end if
        else
            if (t(i + k - 1) - t(i) /= 0.0d0) then
                A1 = (x - t(i))/(t(i + k - 1) - t(i))
            else
                A1 = 0.0d0
            end if
            if (t(i + k) - t(i + 1) /= 0.0d0) then
                A2 = (t(i + k) - x)/(t(i + k) - t(i + 1))
            else
                A2 = 0.0d0
            end if
            db = A1*deboor(i, k - 1, x, t) + A2*deboor(i + 1, k - 1, x, t)
        end if
        return
    end function deboor

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

    subroutine deg2utm0(lat, lon, east, north, zn, zl)
        real(kind=RPRE), intent(in) :: lat, lon
        real(kind=RPRE), intent(out) :: east, north
        integer(kind=IPRE), intent(out) :: zn
        character(len=1), intent(out) :: zl

        real(kind=8), parameter :: K0 = 0.9996d0
        real(kind=8), parameter :: E = 0.00669438d0
        real(kind=8), parameter :: R = 6378137
        real(kind=8) :: E_P2, m, n, c, a
        real(kind=8) :: M1, M2, M3, M4
        real(kind=8) :: lat_rad, lat_sin, lat_cos, lat_tan, lat_tan2, &
                        lat_tan4, lon_rad, central_lon, central_lon_rad

        E_P2 = E/(1.0d0 - E)
        M1 = (1 - E/4 - 3*E**2/64 - 5*E**3/256)
        M2 = (3*E/8 + 3*E**2/32 + 45*E**3/1024)
        M3 = (15*E**2/256 + 45*E**3/1024)
        M4 = (35*E**3/3072)

        lat_rad = lat*pi/180.0d0
        lat_sin = sin(lat_rad)
        lat_cos = cos(lat_rad)
        lat_tan = lat_sin/lat_cos
        lat_tan2 = lat_tan**2
        lat_tan4 = lat_tan2**2

        zn = zone_number(lat, lon)
        zl = zone_letter(lat)

        lon_rad = lon*pi/180.0d0
        central_lon = central_longitude(zn)
        central_lon_rad = central_lon*pi/180.0d0

        n = R/sqrt(1 - E*lat_sin**2)
        c = E_P2*lat_cos**2
        a = lat_cos*(lon_rad - central_lon_rad)
        m = R*(M1*lat_rad &
               - M2*sin(2*lat_rad) &
               + M3*sin(4*lat_rad) &
               - M4*sin(6*lat_rad))
        east = K0*n*(a + &
                     a**3/6*(1 - lat_tan2 + c) + &
                     a**5/120*(5 - 18*lat_tan2 + lat_tan4 + 72*c - 58*E_P2)) + 500000
        north = K0*(m + n*lat_tan*(a**2/2 + &
                                   a**4/24*(5 - lat_tan2 + 9*c + 4*c**2) + &
                                   a**6/720*(61 - 58*lat_tan2 + lat_tan4 + 600*c - 330*E_P2)))
        if (lat < 0.0d0) north = north + 10000000
        return

    contains

        !-------------------------------------------------------------------
        ! zone_number
        !-------------------------------------------------------------------
        integer(kind=IPRE) function zone_number(lat, lon)
            real(kind=RPRE), intent(in) :: lat, lon
            if ((lat >= 56.0d0) .and. (lat <= 64.0d0) &
                .and. (lon >= 3.0d0) .and. (lon <= 12.0d0)) then
                zone_number = 32
                return
            end if

            if ((lat >= 72.0d0) .and. (lat <= 84.0d0) &
                .and. (lon >= 0.0d0)) then
                if (lon <= 9.0d0) then
                    zone_number = 31
                    return
                elseif (lon <= 21.0d0) then
                    zone_number = 33
                    return
                elseif (lon <= 42.0d0) then
                    zone_number = 37
                    return
                end if
            end if

            zone_number = int((lon + 180.0d0)/6.0d0) + 1
            return
        end function zone_number

        !-------------------------------------------------------------------
        ! zone_letter
        !-------------------------------------------------------------------
        character(len=1) function zone_letter(lat)
            real(kind=RPRE), intent(in) :: lat
            character(len=*), parameter :: ZONE_LETTERS = "OXWVUTSRQPNMLKJHGFEDC"
            integer(kind=IPRE) :: ZONE_LATS(21) = [84, 72, 64, 56, 48, 40, 32, 24, 16, 8, 0, &
                                                   -8, -16, -24, -32, -40, -48, -56, -64, -72, -80]
            integer(kind=IPRE) :: i
            do i = 1, 21
                if (lat >= ZONE_LATS(i)) then
                    zone_letter = ZONE_LETTERS(i:i)
                    exit
                end if
            end do
            return
        end function zone_letter

        !-------------------------------------------------------------------
        ! central_longitude
        !-------------------------------------------------------------------
        real(kind=RPRE) function central_longitude(zn)
            integer(kind=IPRE), intent(in) :: zn
            central_longitude = (zn - 1)*6 - 180 + 3
            return
        end function central_longitude

    end subroutine deg2utm0

    subroutine deg2utm1(lat, lon, east, north, zn, zl)
        real(kind=RPRE), dimension(:), intent(in) :: lat, lon
        real(kind=RPRE), dimension(:), allocatable, intent(out) :: east, north
        integer(kind=IPRE), dimension(:), allocatable, intent(out) :: zn
        character(len=1), dimension(:), allocatable, intent(out) :: zl
        integer(kind=IPRE) :: i, n

        n = size(lat)
        allocate (east(n), north(n), zn(n), zl(n))
        do i = 1, n
            call deg2utm(lat(i), lon(i), east(i), north(i), zn(i), zl(i))
        end do
        return
    end subroutine deg2utm1



    

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
    ! y = find(x >= 5.)
    !     3   4   6   7   9
    !
    ! A = diag([1., 2., 3.])
    ! y = find(A /= 0.)
    !     1   1
    !     2   2
    !     3   3

    function find1(bool)
        integer(kind=IPRE), dimension(:), allocatable :: find1
        logical, dimension(:), intent(in) :: bool
        integer(kind=IPRE) :: i, j, n

        n = count(bool)
        if (n /= 0) then
            allocate(find1(n))
            call zeros(find1)
            j = 1
            do i = 1, size(bool)
                if (bool(i)) then
                    find1(j) = i
                    j = j + 1
                end if
            end do
        else
            allocate(find1(0))
            call zeros(find1)
        end if
        return
    end function find1

    function find2(bool)
        integer(kind=IPRE), dimension(:, :), allocatable :: find2
        logical, dimension(:, :), intent(in) :: bool
        integer(kind=IPRE) :: i, j, k, n

        n = count(bool)
        if (n /= 0) then
            allocate(find2(n,2))
            call zeros(find2)
            k = 1
            do i = 1, size(bool, 1)
                do j = 1, size(bool, 2)
                    if (bool(i, j)) then
                        find2(k, 1) = i
                        find2(k, 2) = j
                        k = k + 1
                    end if
                end do
            end do
        else
            allocate(find2(0,2))
            call zeros(find2)
        end if
        return
    end function find2

    function find3(bool)
        integer(kind=IPRE), dimension(:, :), allocatable :: find3
        logical, dimension(:, :, :), intent(in) :: bool
        integer(kind=IPRE) :: i, j, k, l, n

        n = count(bool)
        if (n /= 0) then
            allocate(find3(n,3))
            call zeros(find3)
            l = 1
            do i = 1, size(bool, 1)
                do j = 1, size(bool, 2)
                    do k = 1, size(bool, 3)
                        if (bool(i, j, k)) then
                            find3(l, 1) = i
                            find3(l, 2) = j
                            find3(l, 3) = k
                            l = l + 1
                        end if
                    end do
                end do
            end do
        else
            allocate(find3(0,3))
            call zeros(find3)
        end if
        return
    end function find3

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

    real(kind=RPRE) function fminbnd(fitness, a, b, eps)
        procedure(func1d) :: fitness
        real(kind=RPRE), intent(in) :: a, b
        real(kind=RPRE), intent(in), optional :: eps
        real(kind=RPRE) :: opt_eps, x1, x2, x3, x4
        real(kind=RPRE), parameter :: gr = 0.6180339887498949d0

        opt_eps = 1.0d-4
        if (present(eps)) opt_eps = eps

        x1 = a
        x2 = b
        x3 = x2 - gr*(x2 - x1)
        x4 = x1 + gr*(x2 - x1)
        do while (abs(x3 - x4) > opt_eps)
            if (fitness(x3) < fitness(x4)) then
                x2 = x4
            else
                x1 = x3
            end if
            x3 = x2 - gr*(x2 - x1)
            x4 = x1 + gr*(x2 - x1)
        end do
        fminbnd = 0.5d0*(x1 + x2)
        return
    end function fminbnd

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

    function flip_i1(x)
        integer(kind=IPRE), dimension(:), allocatable :: flip_i1
        integer(kind=IPRE), dimension(:), intent(in) :: x

        flip_i1 = flipud(x)
        return
    end function flip_i1

    function flip_r1(x)
        real(kind=RPRE), dimension(:), allocatable :: flip_r1
        real(kind=RPRE), dimension(:), intent(in) :: x

        flip_r1 = flipud(x)
        return
    end function flip_r1

    function flip_i2(A, dim)
        integer(kind=IPRE), dimension(:, :), allocatable :: flip_i2
        integer(kind=IPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim

        if ((.not. present(dim)) .or. (dim == 1)) then
            flip_i2 = flipud(A)
        elseif (dim == 2) then
            flip_i2 = fliplr(A)
        end if
        return
    end function flip_i2

    function flip_r2(A, dim)
        real(kind=RPRE), dimension(:, :), allocatable :: flip_r2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim

        if ((.not. present(dim)) .or. (dim == 1)) then
            flip_r2 = flipud(A)
        elseif (dim == 2) then
            flip_r2 = fliplr(A)
        end if
        return
    end function flip_r2

    function flip_i3(X, dim)
        integer(kind=IPRE), dimension(:, :, :), allocatable :: flip_i3
        integer(kind=IPRE), dimension(:, :, :), intent(in) :: X
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: n

        if ((.not. present(dim)) .or. (dim == 1)) then
            n = size(X, 1)
            flip_i3 = X(n:1:-1, :, :)
        elseif (dim == 2) then
            n = size(X, 2)
            flip_i3 = X(:, n:1:-1, :)
        elseif (dim == 3) then
            n = size(X, 3)
            flip_i3 = X(:, :, n:1:-1)
        end if
        return
    end function flip_i3

    function flip_r3(X, dim)
        real(kind=IPRE), dimension(:, :, :), allocatable :: flip_r3
        real(kind=IPRE), dimension(:, :, :), intent(in) :: X
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: n

        if ((.not. present(dim)) .or. (dim == 1)) then
            n = size(X, 1)
            flip_r3 = X(n:1:-1, :, :)
        elseif (dim == 2) then
            n = size(X, 2)
            flip_r3 = X(:, n:1:-1, :)
        elseif (dim == 3) then
            n = size(X, 3)
            flip_r3 = X(:, :, n:1:-1)
        end if
        return
    end function flip_r3

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

    function fliplr_i1(x)
        integer(kind=IPRE), dimension(:), allocatable :: fliplr_i1
        integer(kind=IPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: n

        n = size(x)
        fliplr_i1 = x(n:1:-1)
        return
    end function fliplr_i1

    function fliplr_r1(x)
        real(kind=RPRE), dimension(:), allocatable :: fliplr_r1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: n

        n = size(x)
        fliplr_r1 = x(n:1:-1)
        return
    end function fliplr_r1

    function fliplr_i2(A)
        integer(kind=IPRE), dimension(:, :), allocatable :: fliplr_i2
        integer(kind=IPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: n

        n = size(A, 2)
        fliplr_i2 = A(:, n:1:-1)
        return
    end function fliplr_i2

    function fliplr_r2(A)
        real(kind=RPRE), dimension(:, :), allocatable :: fliplr_r2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: n

        n = size(A, 2)
        fliplr_r2 = A(:, n:1:-1)
        return
    end function fliplr_r2

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

    function flipud_i1(x)
        integer(kind=IPRE), dimension(:), allocatable :: flipud_i1
        integer(kind=IPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: n

        n = size(x)
        flipud_i1 = x(n:1:-1)
        return
    end function flipud_i1

    function flipud_r1(x)
        real(kind=RPRE), dimension(:), allocatable :: flipud_r1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: n

        n = size(x)
        flipud_r1 = x(n:1:-1)
        return
    end function flipud_r1

    function flipud_i2(A)
        integer(kind=IPRE), dimension(:, :), allocatable :: flipud_i2
        integer(kind=IPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: n

        n = size(A, 1)
        flipud_i2 = A(n:1:-1, :)
        return
    end function flipud_i2

    function flipud_r2(A)
        real(kind=RPRE), dimension(:, :), allocatable :: flipud_r2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: n

        n = size(A, 1)
        flipud_r2 = A(n:1:-1, :)
        return
    end function flipud_r2

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

    real(kind=RPRE) function gammainc0(x, a)
        real(kind=RPRE), intent(in) :: x, a

        if (x < 0. .or. a <= 0.) then
            print *, "Error: in gammainc, x < 0 and/or a <= 0"
            stop
        end if
        if (x < a + 1.) then
            gammainc0 = gser(x, a)
        else
            gammainc0 = 1.-gcf(x, a)
        end if

        return
    contains

        real(kind=RPRE) function gser(x, a)
            real(kind=RPRE), intent(in) :: x, a
            integer(kind=IPRE), parameter :: itermax = 100
            real(kind=RPRE), parameter :: eps = 3.e-7
            integer(kind=IPRE) :: n
            real(kind=RPRE) :: gln, ap, del, s

            gln = log(gamma(a))
            if (x <= 0.) then
                gser = 0.
            else
                ap = a
                s = 1./a
                del = s
                do n = 1, itermax
                    ap = ap + 1.
                    del = del*x/ap
                    s = s + del
                    if (abs(del) < abs(s)*eps) exit
                end do
                gser = s*exp(-x + a*log(x) - gln)
            end if
            return
        end function gser

        real(kind=RPRE) function gcf(x, a)
            real(kind=RPRE), intent(in) :: x, a
            integer(kind=IPRE), parameter :: itermax = 100
            real(kind=RPRE), parameter :: eps = 3.e-7, fpmin = 1.e-30
            integer(kind=IPRE) :: i
            real(kind=RPRE) :: an, b, c, d, del, h, gln

            gln = log(gamma(a))
            b = x + 1.-a
            c = 1./fpmin
            d = 1./b
            h = d
            do i = 1, itermax
                an = -i*(i - a)
                b = b + 2
                d = an*d + b
                if (abs(d) < fpmin) d = fpmin
                c = b + an/c
                if (abs(c) < fpmin) c = fpmin
                d = 1./d
                del = d*c
                h = h*del
                if (abs(del - 1.) < eps) exit
            end do
            gcf = h*exp(-x + a*log(x) - gln)
            return
        end function gcf

    end function gammainc0

    function gammainc1_0(X, a)
        real(kind=RPRE), dimension(:), allocatable :: gammainc1_0
        real(kind=RPRE), dimension(:), intent(in) :: X
        real(kind=RPRE), intent(in) :: a
        integer(kind=IPRE) :: i, n

        n = size(X)
        allocate(gammainc1_0(n))
        call zeros(gammainc1_0)
        do i = 1, n
            gammainc1_0(i) = gammainc0(X(i), a)
        end do
        return
    end function

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

    function gmm1(x, K, means, stdev, prob, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: prob, means, stdev
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter
        integer(kind=IPRE) :: opt_itermax, i, j, n, iter
        real(kind=RPRE), dimension(:), allocatable :: phi, mu, mu_prev, sigma
        real(kind=RPRE), dimension(:, :), allocatable :: w, pdf, pdf_w

        n = size(x)

        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax

        ! Initialization
        !================
        allocate(phi(K), sigma(K))
        call ones(phi)
        call ones(sigma)
        phi = phi/real(K)           ! Equal initial probabilities for each cluster
        block
            integer, allocatable :: tmp(:)
            allocate(tmp(n))
            call randperm(tmp,K)
            mu = x(tmp)             ! Random initial means
        endblock
        sigma = sigma*std(x)        ! Covariance matrices for each variable

        ! Loop until convergence
        !========================
        allocate(w(n, K))
        call zeros(w)
        iter = 0
        do while (iter < opt_itermax)
            iter = iter + 1

            ! Expectation
            !=============
            allocate(pdf(n, K))
            call zeros(pdf)

            do j = 1, K
                pdf(:, j) = normpdf(x, mu(j), sigma(j))
            end do

            pdf_w = pdf*repmat(phi, n, 2)
            w = pdf_w/repmat(sum(pdf_w, dim=2), K)

            ! Maximization
            !==============
            mu_prev = mu
            do j = 1, K
                phi(j) = mean(w(:, j))
                mu(j) = dot_product(w(:, j), x)/sum(w(:, j))
                sigma(j) = dot_product(w(:, j), (x - mu(j))**2)/sum(w(:, j))
                sigma(j) = sqrt(sigma(j))
            end do

            if (norm(mu - mu_prev) < 1.0d-10) exit
        end do

        allocate(idx(n))
        call zeros(idx)
        do i = 1, n
            idx(i:i) = maxloc(pdf(i, :))
        end do

        if (present(niter)) niter = iter
        if (present(means)) means = mu
        if (present(stdev)) stdev = sigma
        if (present(prob)) prob = phi

        return
    end function gmm1

    function gmm2(A, K, means, covar, prob, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: prob
        real(kind=RPRE), dimension(:, :), allocatable, intent(inout), optional :: means
        real(kind=RPRE), dimension(:, :, :), allocatable, intent(inout), optional :: covar
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter

        integer(kind=IPRE) :: opt_itermax, i, j, n, p, iter
        real(kind=RPRE), dimension(:), allocatable :: phi
        real(kind=RPRE), dimension(:, :), allocatable :: mu, mu_prev, w, pdf, &
                                                         pdf_w, tmp
        real(kind=RPRE), dimension(:, :, :), allocatable :: sigma

        n = size(A, 1)
        p = size(A, 2)

        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax

        ! Initialization
        !================
        allocate(phi(K), sigma(p,p,K))
        call ones(phi)
        phi = phi/real(K)     ! Equal initial probabilities for each cluster
        block
            integer, allocatable :: tmp(:)
            allocate(tmp(n))
            call randperm(tmp, K)
            mu = A(tmp, :)    ! Random initial means
        endblock
        call zeros(sigma)     ! Covariance matrices for each variable

        tmp = cov(A)
        do j = 1, K
            sigma(:, :, j) = tmp
        end do

        ! Loop until convergence
        !========================
        allocate(w(n,K))
        call zeros(w)
        iter = 0
        do while (iter < opt_itermax)
            iter = iter + 1

            ! Expectation
            !=============
            allocate(pdf(n, K))
            call zeros(pdf)
            do j = 1, K
                pdf(:, j) = normpdf(A, mu(j, :), sigma(:, :, j))
            end do

            pdf_w = pdf*repmat(phi, n, 2)
            w = pdf_w/repmat(sum(pdf_w, dim=2), K)

            ! Maximization
            !==============
            mu_prev = mu
            do j = 1, K
                phi(j) = mean(w(:, j))
                mu(j, :) = matmul(w(:, j), A)/sum(w(:, j))
                tmp = A - repmat(mu(j, :), n, 2)
                block
                    integer, allocatable :: tmp(:,:)
                    allocate(tmp(p,p))
                    call zeros(tmp)
                    sigma(:, :, j) = tmp
                endblock
                do i = 1, n
                    sigma(:, :, j) = sigma(:, :, j) &
                                     + w(i, j)*matmul(transpose(tmp(i:i, :)), tmp(i:i, :)) &
                                     /sum(w(:, j))
                end do
            end do

            if (means_residuals(mu, mu_prev) < 1.0d-10) exit
        end do

        allocate(idx(n))
        call zeros(idx)
        do i = 1, n
            idx(i:i) = maxloc(pdf(i, :))
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
            real(kind=RPRE) :: eps
            real(kind=RPRE), dimension(:, :), intent(in) :: means1, means2
            real(kind=RPRE), dimension(:, :), allocatable :: means
            integer(kind=IPRE) :: k

            eps = 0.0d0
            means = abs(means2 - means1)
            do k = 1, p
                eps = eps + sum(means(:, k))**2
            end do
            eps = sqrt(eps)
            return
        end function means_residuals

    end function gmm2

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

    function hann(n)
        real(kind=RPRE), dimension(:), allocatable :: hann
        integer(kind=IPRE), intent(in) :: n

        allocate(hann(n))
        call linspace(hann, 0.d0, real(n-1,8))
        hann = 0.5d0*(1 - cos(2.0d0*pi*hann/n))
        return
    end function hann

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

    function interp1_0(x, v, xq) result(vq)
        real(kind=RPRE) :: vq
        real(kind=RPRE), intent(in) :: xq
        real(kind=RPRE), dimension(:), intent(in) :: x, v
        integer(kind=IPRE) :: i, x1, x2, ix(2)
        real(kind=RPRE) :: vn, xr(2), vr(2)

        x1 = minloc(xq - x, 1, mask=xq >= x)
        x2 = maxloc(xq - x, 1, mask=xq < x)
        if (x2 /= 0) then
            vn = abs((x(x2) - x(x1)))
            xr = x([x1, x2])
            vr = v([x1, x2])
            vq = vr(1)*(xr(2) - xq) + vr(2)*(xq - xr(1))
            vq = vq/vn
        else
            vq = v(size(v))
        end if
        return
    end function interp1_0

    function interp1_1(x, v, xq) result(vq)
        real(kind=RPRE), dimension(:), allocatable :: vq
        real(kind=RPRE), dimension(:), intent(in) :: xq, x, v
        integer(kind=IPRE) :: i, n

        n = size(xq)
        allocate(vq(n))
        call zeros(vq)
        do i = 1, n
            vq(i) = interp1_0(x, v, xq(i))
        end do
        return
    end function interp1_1

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

    function interp2_0(x, y, v, xq, yq) result(vq)
        real(kind=RPRE) :: vq
        real(kind=RPRE), intent(in) :: xq, yq
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: v
        integer(kind=IPRE) :: i, x1, y1, x2, y2, ix(4), iy(4)
        real(kind=RPRE) :: vn, xr(2), yr(2), N(4), vr(4)

        x1 = minloc(xq - x, 1, mask=xq >= x)
        y1 = minloc(yq - y, 1, mask=yq >= y)
        x2 = maxloc(xq - x, 1, mask=xq < x)
        y2 = maxloc(yq - y, 1, mask=yq < y)
        vn = abs((x(x2) - x(x1)) &
                 *(y(y2) - y(y1)))
        xr = x([x1, x2])
        yr = y([y1, y2])
        ix = [2, 1, 2, 1]
        iy = [2, 2, 1, 1]
        do i = 1, 4
            N(i) = abs((xr(ix(i)) - xq)*(yr(iy(i)) - yq))
        end do
        vr = reshape(v([x1, x2], &
                       [y1, y2]), shape=[4])
        vq = dot_product(vr, N/vn)
        return
    end function interp2_0

    function interp2_1(x, y, v, xq, yq) result(vq)
        real(kind=RPRE), dimension(:), allocatable :: vq
        real(kind=RPRE), dimension(:), intent(in) :: xq, yq, x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: v
        integer(kind=IPRE) :: i, n

        n = size(xq)
        allocate(vq(n))
        call zeros(vq)
        do i = 1, n
            vq(i) = interp2_0(x, y, v, xq(i), yq(i))
        end do
        return
    end function interp2_1

    function interp2_2(x, y, v, xq, yq) result(vq)
        real(kind=RPRE), dimension(:, :), allocatable :: vq
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: v, xq, yq
        integer(kind=IPRE) :: m, n

        m = size(xq, 1)
        n = size(xq, 2)
        vq = reshape(interp2_1(y, x, v, [yq], [xq]), shape=[m, n])
        return
    end function interp2_2

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

    function interp3_0(x, y, z, v, xq, yq, zq) result(vq)
        real(kind=RPRE) :: vq
        real(kind=RPRE), intent(in) :: xq, yq, zq
        real(kind=RPRE), dimension(:), intent(in) :: x, y, z
        real(kind=RPRE), dimension(:, :, :), intent(in) :: v
        integer(kind=IPRE) :: i, x1, y1, z1, x2, y2, z2, &
                              ix(8), iy(8), iz(8)
        real(kind=RPRE) :: vn, xr(2), yr(2), zr(2), N(8), vr(8)

        x1 = minloc(xq - x, 1, mask=xq >= x)
        y1 = minloc(yq - y, 1, mask=yq >= y)
        z1 = minloc(zq - z, 1, mask=zq >= z)
        x2 = maxloc(xq - x, 1, mask=xq < x)
        y2 = maxloc(yq - y, 1, mask=yq < y)
        z2 = maxloc(zq - z, 1, mask=zq < z)
        vn = abs((x(x2) - x(x1)) &
                 *(y(y2) - y(y1)) &
                 *(z(z2) - z(z1)))
        xr = x([x1, x2])
        yr = y([y1, y2])
        zr = z([z1, z2])
        ix = [2, 1, 2, 1, 2, 1, 2, 1]
        iy = [2, 2, 1, 1, 2, 2, 1, 1]
        iz = [2, 2, 2, 2, 1, 1, 1, 1]
        do i = 1, 8
            N(i) = abs((xr(ix(i)) - xq)*(yr(iy(i)) - yq)*(zr(iz(i)) - zq))
        end do
        vr = reshape(v([x1, x2], &
                       [y1, y2], &
                       [z1, z2]), shape=[8])
        vq = dot_product(vr, N/vn)
        return
    end function interp3_0

    function interp3_1(x, y, z, v, xq, yq, zq) result(vq)
        real(kind=RPRE), dimension(:), allocatable :: vq
        real(kind=RPRE), dimension(:), intent(in) :: xq, yq, zq, x, y, z
        real(kind=RPRE), dimension(:, :, :), intent(in) :: v
        integer(kind=IPRE) :: i, n

        n = size(xq)
        allocate(vq(n))
        call zeros(vq)
        do i = 1, n
            vq(i) = interp3_0(x, y, z, v, xq(i), yq(i), zq(i))
        end do
        return
    end function interp3_1

    logical function isleap(year)
        integer(kind=IPRE), intent(in) :: year
        if ((mod(year, 400) == 0) .or. &
            ((mod(year, 4) == 0) .and. (mod(year, 100) /= 0))) then
            isleap = .true.
        else
            isleap = .false.
        end if
        return
    end function isleap

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

    logical function ismember_i0i1(x, y)
        integer(kind=IPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:), intent(in) :: y
        integer(kind=IPRE) :: i, dim1

        ismember_i0i1 = .false.
        dim1 = size(y)
        do i = 1, dim1
            if (x == y(i)) then
                ismember_i0i1 = .true.
                return
            end if
        end do
        return
    end function ismember_i0i1

    logical function ismember_i0r1(x, y)
        integer(kind=IPRE), intent(in) :: x
        real(kind=RPRE), dimension(:), intent(in) :: y
        integer(kind=IPRE) :: i, dim1

        ismember_i0r1 = .false.
        dim1 = size(y)
        do i = 1, dim1
            if (x == y(i)) then
                ismember_i0r1 = .true.
                return
            end if
        end do
        return
    end function ismember_i0r1

    logical function ismember_i0i2(x, A)
        integer(kind=IPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: i, j, dim1, dim2

        ismember_i0i2 = .false.
        dim1 = size(A, 1)
        dim2 = size(A, 2)
        do i = 1, dim1
            do j = 1, dim2
                if (x == A(i, j)) then
                    ismember_i0i2 = .true.
                    return
                end if
            end do
        end do
        return
    end function ismember_i0i2

    logical function ismember_i0r2(x, A)
        integer(kind=IPRE), intent(in) :: x
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: i, j, dim1, dim2

        ismember_i0r2 = .false.
        dim1 = size(A, 1)
        dim2 = size(A, 2)
        do i = 1, dim1
            do j = 1, dim2
                if (x == A(i, j)) then
                    ismember_i0r2 = .true.
                    return
                end if
            end do
        end do
        return
    end function ismember_i0r2

    logical function ismember_i0i3(x, Y)
        integer(kind=IPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:, :, :), intent(in) :: Y
        integer(kind=IPRE) :: i, j, k, dim1, dim2, dim3

        ismember_i0i3 = .false.
        dim1 = size(Y, 1)
        dim2 = size(Y, 2)
        dim3 = size(Y, 3)
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim2
                    if (x == Y(i, j, k)) then
                        ismember_i0i3 = .true.
                        return
                    end if
                end do
            end do
        end do
        return
    end function ismember_i0i3

    logical function ismember_i0r3(x, Y)
        integer(kind=IPRE), intent(in) :: x
        real(kind=RPRE), dimension(:, :, :), intent(in) :: Y
        integer(kind=IPRE) :: i, j, k, dim1, dim2, dim3

        ismember_i0r3 = .false.
        dim1 = size(Y, 1)
        dim2 = size(Y, 2)
        dim3 = size(Y, 3)
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim2
                    if (x == Y(i, j, k)) then
                        ismember_i0r3 = .true.
                        return
                    end if
                end do
            end do
        end do
        return
    end function ismember_i0r3

    logical function ismember_r0i1(x, y)
        real(kind=RPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:), intent(in) :: y
        integer(kind=IPRE) :: i, dim1

        ismember_r0i1 = .false.
        dim1 = size(y)
        do i = 1, dim1
            if (x == y(i)) then
                ismember_r0i1 = .true.
                return
            end if
        end do
        return
    end function ismember_r0i1

    logical function ismember_r0r1(x, y)
        real(kind=RPRE), intent(in) :: x
        real(kind=RPRE), dimension(:), intent(in) :: y
        integer(kind=IPRE) :: i, dim1

        ismember_r0r1 = .false.
        dim1 = size(y)
        do i = 1, dim1
            if (x == y(i)) then
                ismember_r0r1 = .true.
                return
            end if
        end do
        return
    end function ismember_r0r1

    logical function ismember_r0i2(x, A)
        real(kind=RPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: i, j, dim1, dim2

        ismember_r0i2 = .false.
        dim1 = size(A, 1)
        dim2 = size(A, 2)
        do i = 1, dim1
            do j = 1, dim2
                if (x == A(i, j)) then
                    ismember_r0i2 = .true.
                    return
                end if
            end do
        end do
        return
    end function ismember_r0i2

    logical function ismember_r0r2(x, A)
        real(kind=RPRE), intent(in) :: x
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: i, j, dim1, dim2

        ismember_r0r2 = .false.
        dim1 = size(A, 1)
        dim2 = size(A, 2)
        do i = 1, dim1
            do j = 1, dim2
                if (x == A(i, j)) then
                    ismember_r0r2 = .true.
                    return
                end if
            end do
        end do
        return
    end function ismember_r0r2

    logical function ismember_r0i3(x, Y)
        real(kind=RPRE), intent(in) :: x
        integer(kind=IPRE), dimension(:, :, :), intent(in) :: Y
        integer(kind=IPRE) :: i, j, k, dim1, dim2, dim3

        ismember_r0i3 = .false.
        dim1 = size(Y, 1)
        dim2 = size(Y, 2)
        dim3 = size(Y, 3)
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim2
                    if (x == Y(i, j, k)) then
                        ismember_r0i3 = .true.
                        return
                    end if
                end do
            end do
        end do
        return
    end function ismember_r0i3

    logical function ismember_r0r3(x, Y)
        real(kind=RPRE), intent(in) :: x
        real(kind=RPRE), dimension(:, :, :), intent(in) :: Y
        integer(kind=IPRE) :: i, j, k, dim1, dim2, dim3

        ismember_r0r3 = .false.
        dim1 = size(Y, 1)
        dim2 = size(Y, 2)
        dim3 = size(Y, 3)
        do i = 1, dim1
            do j = 1, dim2
                do k = 1, dim2
                    if (x == Y(i, j, k)) then
                        ismember_r0r3 = .true.
                        return
                    end if
                end do
            end do
        end do
        return
    end function ismember_r0r3

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

    function isoutlier(x, m)
        logical, dimension(:), allocatable :: isoutlier
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in), optional :: m
        integer(kind=IPRE) :: opt_m

        opt_m = 3
        if (present(m)) opt_m = m
        isoutlier = abs(x - median(x)) > opt_m*mad(x, 2)
        return
    end function isoutlier

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

    function k2test(x) result(p)
        real(kind=RPRE) :: p
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=8) :: n
        real(kind=8) :: b1, b2, K2
        real(kind=8) :: Y, beta2, W2, delta, alpha, Z1
        real(kind=8) :: E, v2, xx, beta1, A, Z2

        n = size(x)
        b1 = skewness(x)
        b2 = kurtosis(x)

        ! Skewness test
        !===============
        Y = b1*sqrt((n + 1.)*(n + 3.)/(6.*(n - 2.)))
        beta2 = 3.*(n*n + 27.*n - 70.)*(n + 1.)*(n + 3.)/((n - 2.)*(n + 5.)*(n + 7.)*(n + 9.))
        W2 = -1.+sqrt(2.*(beta2 - 1.))
        delta = 1./sqrt(0.5*log(W2))
        alpha = sqrt(2./(W2 - 1.))
        Z1 = delta*log(Y/alpha + sqrt((Y/alpha)**2 + 1.))

        ! Kurtosis test
        !===============
        E = 3.*(n - 1.)/(n + 1.)
        v2 = 24.*n*(n - 2.)*(n - 3.)/((n + 1.)**2*(n + 3.)*(n + 5.))
        xx = (b2 - E)/sqrt(v2)
        beta1 = 6.*(n**2 - 5.*n + 2.)/((n + 7.)*(n + 9.)) &
                *sqrt(6.*(n + 3.)*(n + 5.)/(n*(n - 2.)*(n - 3.)))
        A = 6.+8./beta1*(2./beta1 + sqrt(1.+4./(beta1*beta1)))
        Z2 = ((1.-2./(9.*A)) - ((1.-2./A)/(1.+xx*sqrt(2./(A - 4.))))**(1./3.)) &
             /sqrt(2./(9.*A))

        ! Omnibus test
        !==============
        K2 = Z1*Z1 + Z2*Z2
        p = exp(-0.5*K2)

        return
    end function k2test

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

    subroutine kde1(x, f, xi, bw)
        real(kind=RPRE), dimension(:), intent(in) :: x
        real(kind=RPRE), dimension(:), allocatable, intent(out) :: f
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: xi
        real(kind=RPRE), intent(in), optional :: bw
        integer(kind=IPRE) :: ix, j, n, nx
        real(kind=RPRE) :: opt_bw
        real(kind=RPRE), dimension(:), allocatable :: opt_xi

        n = size(x)
        opt_bw = (4.*std(x)**5/(3.*n))**0.2
        if (present(bw)) opt_bw = bw
        if (present(xi) .and. allocated(xi)) then
            nx = size(xi)
            opt_xi = xi
        else
            nx = 100
            allocate(opt_xi(nx))
            call linspace(opt_xi, minval(x) - 3*opt_bw, maxval(x) + 3*opt_bw)
        end if

        allocate(f(nx))
        call zeros(f)
        do ix = 1, nx
            do j = 1, n
                f(ix) = f(ix) + exp(-0.5*((opt_xi(ix) - x(j))/opt_bw)**2)
            end do
        end do
        f = 0.3989422804014327*f/(n*opt_bw)

        if (present(xi) .and. .not. allocated(xi)) xi = opt_xi
        return
    end subroutine kde1

    subroutine kde2(A, f, xi, yi, H)
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        real(kind=RPRE), dimension(:, :), allocatable, intent(out) :: f
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: xi, yi
        real(kind=RPRE), dimension(:, :), intent(in), optional :: H
        integer(kind=IPRE) :: ix, iy, j, n, nx, ny
        real(kind=RPRE) :: opt_H(2, 2), invH(2, 2), x(2)
        real(kind=RPRE), dimension(:), allocatable :: opt_xi, opt_yi

        n = size(A, 1)
        if (present(H)) then
            opt_H = H
        else
            opt_H = cov(A)*n**(-1./3.)    ! Squared
        end if
        if (present(xi) .and. allocated(xi)) then
            nx = size(xi)
            opt_xi = xi
        else
            nx = 100
            allocate(opt_yi(nx))
            call linspace(opt_xi, minval(A(:, 1)) - 3*opt_H(1, 1), maxval(A(:, 1)) + 3*opt_H(1, 1))
        end if
        if (present(yi) .and. allocated(yi)) then
            ny = size(yi)
            opt_yi = yi
        else
            ny = 100
            allocate(opt_yi(ny))
            call linspace(opt_yi, minval(A(:, 2)) - 3*opt_H(2, 2), maxval(A(:, 2)) + 3*opt_H(2, 2))
        end if

        invH = inv(opt_H)
        allocate(f(nx, ny))
        call zeros(f)
        do ix = 1, nx
            do iy = 1, ny
                do j = 1, n
                    x = [opt_xi(ix), opt_yi(iy)] - [A(j, :)]
                    f(ix, iy) = f(ix, iy) + exp(-0.5*dot_product(matmul(x, invH), x))
                end do
            end do
        end do
        f = f/(sqrt(det(real(2.*pi, RPRE)*opt_H))*real(n, RPRE))

        if (present(xi) .and. .not. allocated(xi)) xi = opt_xi
        if (present(yi) .and. .not. allocated(yi)) yi = opt_yi
        return
    end subroutine kde2

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

    real(kind=RPRE) function kurtosis1(x, flag)
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in), optional :: flag
        integer(kind=IPRE) :: opt_flag, n

        opt_flag = 1
        if (present(flag)) opt_flag = flag

        n = size(x)
        kurtosis1 = (sum((x - mean(x))**4)/n)/(var(x, 1)**2)
        if (opt_flag == 0) then
            kurtosis1 = (n - 1)/((n - 2)*(n - 3))*((n + 1)*kurtosis1 - 3*(n - 1)) + 3
        end if
        return
    end function kurtosis1

    function kurtosis2(A, flag, dim)
        real(kind=RPRE), dimension(:), allocatable :: kurtosis2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: flag, dim
        integer(kind=IPRE) :: opt_flag, i, m, n

        opt_flag = 1
        if (present(flag)) opt_flag = flag

        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (kurtosis2(n))
            do i = 1, n
                kurtosis2(i) = kurtosis1(A(:, i), opt_flag)
            end do
        elseif (dim == 2) then
            allocate (kurtosis2(m))
            do i = 1, m
                kurtosis2(i) = kurtosis1(A(i, :), opt_flag)
            end do
        end if
        return
    end function kurtosis2

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

    real(kind=RPRE) function log2_i0(x)
        integer(kind=IPRE), intent(in) :: x

        log2_i0 = log(real(x))/log(2.0d0)
        return
    end function log2_i0

    real(kind=RPRE) function log2_r0(x)
        real(kind=RPRE), intent(in) :: x

        log2_r0 = log(x)/log(2.0d0)
        return
    end function log2_r0

    function log2_i1(x)
        real(kind=RPRE), dimension(:), allocatable :: log2_i1
        integer(kind=IPRE), dimension(:), intent(in) :: x

        log2_i1 = log(real(x))/log(2.0d0)
        return
    end function log2_i1

    function log2_r1(x)
        real(kind=RPRE), dimension(:), allocatable :: log2_r1
        real(kind=RPRE), dimension(:), intent(in) :: x

        log2_r1 = log(x)/log(2.0d0)
        return
    end function log2_r1

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

    function lsweight(r, ntype)
        real(kind=RPRE), dimension(:, :), allocatable :: lsweight
        real(kind=RPRE), dimension(:), intent(in) :: r
        character(len=*), intent(in) :: ntype
        integer(kind=IPRE) :: i, n
        real(kind=RPRE) :: eps

        eps = 4.685d0*mad(r, 2)/0.6745d0
        n = size(r)
        if ((eps == 0.0d0) .or. (ntype == "none")) then
            allocate(lsweight(n,n))
            call eye(lsweight)
        elseif (ntype == "biweight") then
            allocate(lsweight(n,n))
            call zeros(lsweight)
            do i = 1, n
                if (abs(r(i)) <= eps) lsweight(i, i) = (1.0d0 - (r(i)/eps)**2)**2
            end do
        end if
        return
    end function lsweight

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

    function kmeans1(x, K, means, init, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), dimension(:), intent(in), optional :: init
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: means
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter
        integer(kind=IPRE) :: opt_itermax, n, iter
        real(kind=RPRE), dimension(:, :), allocatable :: m1
        real(kind=RPRE), dimension(:, :), allocatable :: opt_init, A

        n = size(x)

        opt_itermax = 100
        if (present(itermax)) opt_itermax = itermax
        if (present(init)) then
            opt_init = reshape(init, shape=[K, 1], order=[1, 2])
        else
            block
                integer, allocatable :: tmp(:)
                allocate(tmp(n))
                call randperm(tmp, K)
                opt_init = reshape(x(tmp), shape=[K, 1], order=[1, 2])
            endblock
        end if

        A = reshape(x, shape=[n, 1], order=[1, 2])
        idx = kmeans2(A, K, m1, opt_init, opt_itermax, iter)

        if (present(niter)) niter = iter
        if (present(means)) means = [m1]

        return
    end function kmeans1

    function kmeans2(A, K, means, init, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), dimension(:, :), intent(in), optional :: init
        real(kind=RPRE), dimension(:, :), allocatable, intent(inout), optional :: means
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter
        integer(kind=IPRE) :: opt_itermax, i, n, p, iter
        real(kind=RPRE), dimension(:, :), allocatable :: opt_init, m, m1

        n = size(A, 1)
        p = size(A, 2)

        opt_itermax = 1000
        if (present(itermax)) opt_itermax = itermax
        if (present(init)) then
            opt_init = init
        else
            block
                integer, allocatable :: tmp(:)
                allocate(tmp(n))
                call randperm(tmp, K)
                opt_init = A(tmp, :)
            endblock
        end if

        ! Initialization
        !================
        m = opt_init
        idx = update_index(A, m)
        m1 = update_means(A, idx)

        ! Loop until convergence
        !========================
        iter = 0
        do while ((means_residuals(m, m1) > 1.0d-10) &
                  .and. (iter < opt_itermax))
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
            integer(kind=IPRE), dimension(:), allocatable :: idx
            real(kind=RPRE), dimension(:, :), intent(in) :: A, means
            integer(kind=IPRE) :: i, j, b(1)
            real(kind=RPRE), dimension(:), allocatable :: dist

            allocate(idx(n), dist(K))
            call zeros(idx)
            do i = 1, n
                call zeros(dist)
                do j = 1, K
                    dist(j) = norm(A(i, :) - means(j, :))
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
            real(kind=RPRE), dimension(:, :), allocatable :: means
            real(kind=RPRE), dimension(:, :), intent(in) :: A
            integer(kind=IPRE), dimension(:), intent(in) :: idx
            integer(kind=IPRE) :: j

            allocate(means(K, p))
            call zeros(means)
            do j = 1, K
                means(j, :) = mean(A(find(idx == j), :))
            end do
            return
        end function update_means

        !-------------------------------------------------------------------
        ! means_residuals
        !-------------------------------------------------------------------
        function means_residuals(means1, means2) result(eps)
            real(kind=RPRE) :: eps
            real(kind=RPRE), dimension(:, :), intent(in) :: means1, means2
            real(kind=RPRE), dimension(:, :), allocatable :: means
            integer(kind=IPRE) :: k

            eps = 0.0d0
            means = abs(means2 - means1)
            do k = 1, p
                eps = eps + sum(means(:, k))**2
            end do
            eps = sqrt(eps)
            return
        end function means_residuals

    end function kmeans2

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

    real(kind=RPRE) function mad1(x, method)
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in), optional :: method

        if ((.not. present(method)) .or. (method == 1)) then
            mad1 = mean(abs(x - mean(x)))
        elseif (method == 2) then
            mad1 = median(abs(x - median(x)))
        end if
        return
    end function mad1

    function mad2(A, dim, method)
        real(kind=RPRE), dimension(:), allocatable :: mad2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim, method
        integer(kind=IPRE) :: i, m, n

        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (mad2(n))
            if ((.not. present(method)) .or. (method == 1)) then
                do i = 1, n
                    mad2(i) = mad(A(:, i), 1)
                end do
            elseif (method == 2) then
                do i = 1, n
                    mad2(i) = mad(A(:, i), 2)
                end do
            end if
        elseif (dim == 2) then
            allocate (mad2(m))
            if ((.not. present(method)) .or. (method == 1)) then
                do i = 1, m
                    mad2(i) = mad(A(i, :), 1)
                end do
            elseif (method == 2) then
                do i = 1, m
                    mad2(i) = mad(A(i, :), 2)
                end do
            end if
        end if
        return
    end function mad2

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

    function mbkmeans1(x, K, perc, means, init, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), intent(in), optional :: perc
        real(kind=RPRE), dimension(:), intent(in), optional :: init
        real(kind=RPRE), dimension(:), allocatable, intent(inout), optional :: means
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter
        integer(kind=IPRE) :: opt_itermax, n, iter
        real(kind=RPRE) :: opt_perc
        real(kind=RPRE), dimension(:, :), allocatable :: m1
        real(kind=RPRE), dimension(:, :), allocatable :: opt_init, A

        n = size(x)

        opt_itermax = 50
        opt_perc = 0.2d0
        if (present(itermax)) opt_itermax = itermax
        if (present(perc)) opt_perc = perc
        if (present(init)) then
            opt_init = reshape(init, shape=[K, 1], order=[1, 2])
        else
            block
                integer, allocatable :: tmp(:)
                allocate(tmp(n))
                call randperm(tmp,K)
                opt_init = reshape(x(tmp), shape=[K, 1], order=[1, 2])
            endblock
        end if

        A = reshape(x, shape=[n, 1], order=[1, 2])
        idx = mbkmeans2(A, K, perc, m1, opt_init, opt_itermax, iter)

        if (present(niter)) niter = iter
        if (present(means)) means = [m1]

        return
    end function mbkmeans1

    function mbkmeans2(A, K, perc, means, init, itermax, niter) result(idx)
        integer(kind=IPRE), dimension(:), allocatable :: idx
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in) :: K
        real(kind=RPRE), intent(in), optional :: perc
        real(kind=RPRE), dimension(:, :), intent(in), optional :: init
        real(kind=RPRE), dimension(:, :), allocatable, intent(inout), optional :: means
        integer(kind=IPRE), intent(in), optional :: itermax
        integer(kind=IPRE), intent(inout), optional :: niter
        integer(kind=IPRE) :: opt_itermax, n, p, bs, iter
        real(kind=RPRE) :: opt_perc
        integer(kind=IPRE), dimension(:), allocatable :: v
        real(kind=RPRE), dimension(:, :), allocatable :: opt_init, m, m1, B

        n = size(A, 1)
        p = size(A, 2)

        opt_itermax = 50
        opt_perc = 0.2d0
        if (present(itermax)) opt_itermax = itermax
        if (present(perc)) opt_perc = perc
        if (present(init)) then
            opt_init = init
        else
            block
                integer, allocatable :: tmp(:)
                allocate(tmp(n))
                call randperm(tmp,K)
                opt_init = A(tmp, :)
            endblock
        end if

        ! Initialization
        !================
        bs = nint(opt_perc*n)   ! Batch size
        m = opt_init            ! Initial centroids
        allocate(v(K))
        call zeros(v)            ! Per-center counter

        ! Iterate until convergence
        !===========================
        do iter = 1, opt_itermax
            block
                integer, allocatable :: tmp(:)
                allocate(tmp(n))
                call randperm(tmp, bs)
                B = A(tmp, :)        ! Batch
            endblock
            m1 = m                          ! Previous means
            idx = cache_means(B, m)         ! Cache means
            call update_means(m, v, B, idx) ! Update means with gradient
            if (means_residuals(m, m1) < 1.0d-2) exit
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
            integer(kind=IPRE), dimension(:), allocatable :: idx
            real(kind=RPRE), dimension(:, :), intent(in) :: A, means
            integer(kind=IPRE) :: i, j, n, b(1)
            real(kind=RPRE), dimension(:), allocatable :: dist

            n = size(A, 1)
            allocate(idx(n), dist(K))
            call zeros(idx)
            call zeros(dist)
            do i = 1, n
                dist = 0.0d0
                do j = 1, K
                    dist(j) = norm(A(i, :) - means(j, :))
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
            real(kind=RPRE), dimension(:, :), intent(inout) :: means
            integer(kind=IPRE), dimension(:), intent(inout) :: v
            real(kind=RPRE), dimension(:, :), intent(in) :: A
            integer(kind=IPRE), dimension(:), intent(in) :: idx
            integer(kind=IPRE) :: i, n, c
            real(kind=RPRE) :: eta

            n = size(A, 1)
            do i = 1, n
                c = idx(i)
                v(c) = v(c) + 1
                eta = 1.0d0/real(v(c), RPRE)
                means(c, :) = (1.0d0 - eta)*means(c, :) + eta*A(i, :)
            end do
            return
        end subroutine update_means

        !-------------------------------------------------------------------
        ! means_residuals
        !-------------------------------------------------------------------
        function means_residuals(means1, means2) result(eps)
            real(kind=RPRE) :: eps
            real(kind=RPRE), dimension(:, :), intent(in) :: means1, means2
            real(kind=RPRE), dimension(:, :), allocatable :: means
            integer(kind=IPRE) :: k

            eps = 0.0d0
            means = abs(means2 - means1)
            do k = 1, p
                eps = eps + sum(means(:, k))**2
            end do
            eps = sqrt(eps)
            return
        end function means_residuals

    end function mbkmeans2

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

    real(kind=RPRE) function median1(x)
        real(kind=RPRE), dimension(:), intent(in) :: x
        real(kind=RPRE), dimension(:), allocatable :: x_sort
        integer(kind=IPRE) :: i, n

        n = size(x)
        x_sort = sort(x)
        i = ceiling(real(n/2.0d0))
        if (mod(n, 2) == 0) then
            median1 = (x_sort(i) + x_sort(i + 1))/2
        else
            median1 = x_sort(i)
        end if
        return
    end function median1

    function median2(A, dim)
        real(kind=RPRE), dimension(:), allocatable :: median2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: i, m, n

        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (median2(n))
            do i = 1, n
                median2(i) = median(A(:, i))
            end do
        elseif (dim == 2) then
            allocate (median2(m))
            do i = 1, m
                median2(i) = median(A(i, :))
            end do
        end if
        return
    end function median2

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

    subroutine meshgrid2(ax, ay, x, y)
        real(kind=RPRE), dimension(:), intent(in) :: ax, ay
        real(kind=RPRE), dimension(:, :), allocatable, intent(out) :: x, y
        integer(kind=IPRE) :: i, m, n

        m = size(ax)
        n = size(ay)
        if (.not. allocated(x)) allocate (x(n, m))
        if (.not. allocated(y)) allocate (y(n, m))
        do i = 1, n
            x(i, :) = ax
        end do
        do i = 1, m
            y(:, i) = ay
        end do
        return
    end subroutine meshgrid2

    ! mpi_rpre
    !-----------------------------------------------------------------------
    ! mpi_rpre returns either MPI_REAL or MPI_DOUBLE depending on RPRE.
    !
    ! Notes
    !-----------------------------------------------------------------------
    ! When calling MPI functions, use mpi_rpre instead of MPI_REAL or
    ! MPI_DOUBLE.
    !\note: fpm has not support macro command analysis.
    ! #ifdef do_mpi
    integer(kind=4) function mpi_rpre()
        select case (RPRE)
        case (4)
            mpi_rpre = RPRE   ! mpi_real
        case (8)
            mpi_rpre = RPRE   ! mpi_double
        end select
        return
    end function mpi_rpre
    ! #endif

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
    ! that satisfy 2**p <= abs(x).
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

    function nextpow2_0(x) result(pow)
        integer(kind=IPRE) :: pow
        integer(kind=IPRE), intent(in) :: x

        pow = ceiling(log(real(abs(x)))/log(2.))
        return
    end function nextpow2_0

    function nextpow2_1(x) result(pow)
        integer(kind=IPRE), dimension(:), allocatable :: pow
        integer(kind=IPRE), dimension(:), intent(in) :: x

        pow = ceiling(log(real(abs(x)))/log(2.))
        return
    end function nextpow2_1

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

    real(kind=RPRE) function normpdf0(x, mu, sigma) result(pdf)
        real(kind=RPRE), intent(in) :: x, mu, sigma

        pdf = exp(-0.5d0*(x - mu)**2/sigma**2)
        pdf = pdf/(sigma*sqrt(2.0d0*pi))
        return
    end function normpdf0

    function normpdf1(x, mu, sigma) result(pdf)
        real(kind=RPRE), dimension(:), allocatable :: pdf
        real(kind=RPRE), dimension(:), intent(in) :: x
        real(kind=RPRE), intent(in) :: mu, sigma

        pdf = exp(-0.5d0*(x - mu)**2/sigma**2)
        pdf = pdf/(sigma*sqrt(2.0d0*pi))
        return
    end function normpdf1

    function normpdf2(X, mu, sigma) result(pdf)
        real(kind=RPRE), dimension(:), allocatable :: pdf
        real(kind=RPRE), dimension(:, :), intent(in) :: X, sigma
        real(kind=RPRE), dimension(:), intent(in) :: mu
        integer(kind=IPRE) :: n
        real(kind=RPRE), dimension(:, :), allocatable :: tmp

        n = size(X, 2)
        tmp = X - repmat(mu, size(X, 1), 2)
        pdf = exp(-0.5d0*sum(matmul(tmp, inv(sigma))*tmp, dim=2))
        pdf = pdf/sqrt((2.0d0*pi)**n*det(sigma))
        return
    end function normpdf2

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

    function pascal(n)
        integer(kind=IPRE), dimension(:, :), allocatable :: pascal
        integer(kind=IPRE), intent(in) :: n
        integer(kind=IPRE) :: i, j

        allocate(pascal(n,n))
        call ones(pascal)
        do i = 2, n
            do j = 2, n
                pascal(i, j) = pascal(i - 1, j) + pascal(i, j - 1)
            end do
        end do
        return
    end function pascal

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

    real(kind=RPRE) function prctile0(x, p)
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in) :: p
        real(kind=RPRE) :: tmp(1)

        tmp = prctile1(x, [p])
        prctile0 = tmp(1)
        return
    end function prctile0

    function prctile1(x, p)
        real(kind=RPRE), dimension(:), allocatable :: prctile1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), dimension(:), intent(in) :: p
        integer(kind=IPRE) :: i, nx, np, idx
        real(kind=RPRE), dimension(:), allocatable :: xsort, ap

        nx = size(x)
        np = size(p)
        allocate(prctile1(np))
        call zeros(prctile1)
        xsort = sort(x)
        do i = 1, np
            if (p(i) <= 50.0d0/real(nx, RPRE)) then
                prctile1(i) = xsort(1)
            elseif (p(i) >= 100.0d0*((nx - 0.5d0)/real(nx, RPRE))) then
                prctile1(i) = xsort(nx)
            else
                allocate(ap(nx))
                call linspace(ap, 1.d0,real(nx,RPRE))
                ap = 100.0d0*(ap-0.5d0)/real(nx,rpre)
                idx = maxval(find(p(i) > ap))
                prctile1(i) = xsort(idx) &
                              + (xsort(idx + 1) - xsort(idx))*(p(i) - ap(idx)) &
                              /(ap(idx + 1) - ap(idx))
            end if
        end do
        return
    end function prctile1

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

    function repmat1(x, n1, dim)
        real(kind=RPRE), dimension(:, :), allocatable :: repmat1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in) :: n1
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: i, m

        m = size(x)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate(repmat1(m, n1))
            call zeros(repmat1)
            do i = 1, n1
                repmat1(:, i) = x
            end do
        elseif (dim == 2) then
            allocate(repmat1(n1, m))
            call zeros(repmat1)
            do i = 1, n1
                repmat1(i, :) = x
            end do
        end if
        return
    end function repmat1

    function repmat2(A, n1, n2)
        real(kind=RPRE), dimension(:, :), allocatable :: repmat2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in) :: n1, n2
        integer(kind=IPRE) :: i, j, m, n

        m = size(A, 1)
        n = size(A, 2)
        allocate(repmat2(m*n1, n*n2))
        call zeros(repmat2)
        do i = 1, n1
            do j = 1, n2
                repmat2((i - 1)*m + 1:i*m, (j - 1)*n + 1:j*n) = A
            end do
        end do
        return
    end function repmat2

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

    real(kind=RPRE) function rms1(x)
        real(kind=RPRE), dimension(:), intent(in) :: x

        rms1 = sqrt(sum(x*x)/size(x))
        return
    end function rms1

    function rms2(A, dim)
        real(kind=RPRE), dimension(:), allocatable :: rms2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: dim
        integer(kind=IPRE) :: i, m, n

        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (rms2(n))
            do i = 1, n
                rms2(i) = rms(A(:, i))
            end do
        elseif (dim == 2) then
            allocate (rms2(m))
            do i = 1, m
                rms2(i) = rms(A(i, :))
            end do
        end if
        return
    end function rms2

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

    subroutine randperm(X, k)
        !!\FIXME:
        integer, dimension(:), intent(out) :: X
        integer :: n
        integer, intent(in), optional :: k
        integer :: k_, i, j, tmp
        integer, dimension(:), allocatable :: a

        n = size(X)
        k_ = optval(k,n)
        allocate(a(n))
        call seq(a, 1, n)
        do i = n, n - k_ + 1, -1
            call randu(j,1,i)
            tmp = a(i)
            a(i) = a(j)
            a(j) = tmp
        end do
        X = a(n - k_ + 1:n)
        return
    end subroutine randperm

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

    real(kind=RPRE) function signum0(x)
        real(kind=RPRE), intent(in) :: x
        if (x < 0.0d0) then
            signum0 = -1.0d0
        elseif (x > 0.0d0) then
            signum0 = 1.0d0
        else
            signum0 = 0.0d0
        end if
        return
    end function signum0

    function signum1(x)
        real(kind=RPRE), dimension(:), allocatable :: signum1
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE) :: i, n

        n = size(x)
        allocate(signum1(n))
        call zeros(signum1)
        do i = 1, n
            signum1(i) = signum0(x(i))
        end do
        return
    end function signum1

    function signum2(A)
        real(kind=RPRE), dimension(:, :), allocatable :: signum2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE) :: i, j, m, n

        m = size(A, 1)
        n = size(A, 2)
        allocate(signum2(m,n))
        call zeros(signum2)
        do i = 1, m
            do j = 1, n
                signum2(i, j) = signum0(A(i, j))
            end do
        end do
        return
    end function signum2

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

    function silhouette1(x, cluster) result(s)
        real(kind=RPRE), dimension(:), allocatable :: s
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), dimension(:), intent(in) :: cluster
        integer(kind=IPRE) :: n
        real(kind=RPRE), dimension(:, :), allocatable :: A

        n = size(x)
        A = reshape(x, shape=[n, 1], order=[1, 2])
        s = silhouette2(A, cluster)
        return
    end function silhouette1

    function silhouette2(X, cluster) result(s)
        real(kind=RPRE), dimension(:), allocatable :: s
        real(kind=RPRE), dimension(:, :), intent(in) :: X
        integer(kind=IPRE), dimension(:), intent(in) :: cluster
        integer(kind=IPRE) :: i, j, K, l, n
        real(kind=RPRE) :: a, b
        integer(kind=IPRE), dimension(:), allocatable :: idx, cs
        real(kind=RPRE), dimension(:), allocatable :: d

        n = size(X, 1)
        K = maxval(cluster)
        if (K == 1) then
            print *, "Warning: in silhouette, the silhouette value cannot " &
                //"be defined for K = 1."
            allocate(s(n))
            call zeros(s)
            return
        end if

        ! Size of each cluster
        !======================
        allocate (cs(K))
        do j = 1, K
            idx = find(cluster == j)    ! All objects in cluster j
            cs(j) = size(idx)
        end do

        ! Loop over objects
        !===================
        allocate(s(n))
        call zeros(s)
        do i = 1, n

            ! Compute the dissimilarity for each cluster to current object i
            !================================================================
            allocate(d(K))
            call zeros(d)          ! Cluster dissimilarity to object i
            do j = 1, K
                idx = find(cluster == j)
                d(j) = sum((X(idx, :) - repmat(X(i, :), cs(j), 2))**2)/cs(j)
            end do

            ! Compute a(i)
            !==============
            j = cluster(i)
            if (cs(j) == 1) then
                s(i) = 0.0d0
                cycle               ! Skip next statements and begin next iteration
            else
                a = d(j)*cs(j)/(cs(j) - 1)
            end if

            ! Compute b(i)
            !==============
            b = minval(d, mask=d /= d(j) .and. d /= real(0., RPRE))

            ! Compute s(i)
            !==============
            s(i) = (b - a)/max(a, b)

        end do

        return
    end function silhouette2

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

    real(kind=RPRE) function sinc0(x)
        real(kind=RPRE), intent(in) :: x
        real(kind=RPRE) :: y

        if (x == 0.0d0) then
            sinc0 = 1.0d0
        else
            y = pi*x
            sinc0 = sin(y)/y
        end if
        return
    end function sinc0

    function sinc1(x)
        real(kind=RPRE), dimension(:), allocatable :: sinc1
        real(kind=RPRE), dimension(:), intent(in) :: x
        real(kind=RPRE), dimension(:), allocatable :: y

        allocate (y(size(x)))
        y = pi*merge(real(1.0e-20, RPRE), x, x == 0.0d0)
        sinc1 = sin(y)/y
        return
    end function sinc1

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

    real(kind=RPRE) function skewness1(x, flag)
        real(kind=RPRE), dimension(:), intent(in) :: x
        integer(kind=IPRE), intent(in), optional :: flag
        integer(kind=IPRE) :: opt_flag, n

        opt_flag = 1
        if (present(flag)) opt_flag = flag

        n = size(x)
        skewness1 = (sum((x - mean(x))**3)/n)/(var(x, 1)**1.5)
        if (opt_flag == 0) then
            skewness1 = skewness1*sqrt(real(n*(n - 1), RPRE))/real((n - 2), RPRE)
        end if
        return
    end function skewness1

    function skewness2(A, flag, dim)
        real(kind=RPRE), dimension(:), allocatable :: skewness2
        real(kind=RPRE), dimension(:, :), intent(in) :: A
        integer(kind=IPRE), intent(in), optional :: flag, dim
        integer(kind=IPRE) :: opt_flag, i, m, n

        opt_flag = 1
        if (present(flag)) opt_flag = flag

        m = size(A, 1)
        n = size(A, 2)
        if ((.not. present(dim)) .or. (dim == 1)) then
            allocate (skewness2(n))
            do i = 1, n
                skewness2(i) = skewness1(A(:, i), opt_flag)
            end do
        elseif (dim == 2) then
            allocate (skewness2(m))
            do i = 1, m
                skewness2(i) = skewness1(A(i, :), opt_flag)
            end do
        end if
        return
    end function skewness2



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

    real(kind=RPRE) function spline1_0(x, y, xq) result(yq)
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), intent(in) :: xq
        real(kind=RPRE) :: tmp(1)

        tmp = spline1(x, y, [xq])
        yq = tmp(1)
        return
    end function spline1_0

    function spline1_1(x, y, xq) result(yq)
        real(kind=RPRE), dimension(:), allocatable :: yq
        real(kind=RPRE), dimension(:), intent(in) :: x, y, xq
        integer(kind=IPRE) :: i, n, nq, x1
        real(kind=RPRE), dimension(:), allocatable :: w, h, z, a, b, c, d

        n = size(x)
        nq = size(xq)
        allocate (w(n - 1), h(n - 1), z(n), a(n - 1), b(n - 1), c(n - 1), d(n - 1), yq(nq))

        ! Compute h and b
        !=================
        do i = 1, n - 1
            w(i) = x(i + 1) - x(i)
            h(i) = (y(i + 1) - y(i))/w(i)
        end do

        ! Compute z
        !===========
        z(1) = 0.0d0
        do i = 1, n - 2
            z(i + 1) = 3.0d0*(h(i + 1) - h(i))/(w(i + 1) + w(i))
        end do
        z(n) = 0.0d0

        ! Basis functions
        !=================
        do i = 1, n - 1
            a(i) = (z(i + 1) - z(i))/(6.0d0*w(i))
            b(i) = 0.5d0*z(i)
            c(i) = h(i) - w(i)*(z(i + 1) + 2.0d0*z(i))/6.0d0
            d(i) = y(i)
        end do

        ! Evaluate
        !==========
        do i = 1, nq
            x1 = max(1, minloc(xq(i) - x, 1, mask=xq(i) > x))
            yq(i) = d(x1) + (xq(i) - x(x1)) &
                    *(c(x1) + (xq(i) - x(x1)) &
                      *(b(x1) + (xq(i) - x(x1)) &
                        *a(x1)))
        end do
        return
    end function spline1_1

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

    function spline2_1(x, y, z, xq, yq) result(zq)
        real(kind=RPRE), dimension(:), allocatable :: zq
        real(kind=RPRE), dimension(:), intent(in) :: x, y, xq, yq
        real(kind=RPRE), dimension(:, :), intent(in) :: z
        integer(kind=IPRE) :: i, iq, j, k, m, n, nq, x0, y0
        real(kind=RPRE) :: wt(16, 16), zv(16), c(4, 4), dx, dy, t, u
        real(kind=RPRE), dimension(:, :), allocatable :: zt, z1, z2, z12

        m = size(x)
        n = size(y)
        nq = size(xq)

        ! Work on the transpose so that x corresponds to the 2nd dimension
        ! and y to the 1st dimension
        !==================================================================
        zt = transpose(z)

        ! Inverted coefficient matrix
        !=============================
        wt = reshape([1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                      0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                      -3., 3., 0., 0., -2., -1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                      2., -2., 0., 0., 1., 1., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., &
                      0., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., 0., 0., 0., 0., &
                      0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 1., 0., 0., 0., &
                      0., 0., 0., 0., 0., 0., 0., 0., -3., 3., 0., 0., -2., -1., 0., 0., &
                      0., 0., 0., 0., 0., 0., 0., 0., 2., -2., 0., 0., 1., 1., 0., 0., &
                      -3., 0., 3., 0., 0., 0., 0., 0., -2., 0., -1., 0., 0., 0., 0., 0., &
                      0., 0., 0., 0., -3., 0., 3., 0., 0., 0., 0., 0., -2., 0., -1., 0., &
                      9., -9., -9., 9., 6., 3., -6., -3., 6., -6., 3., -3., 4., 2., 2., 1., &
                      -6., 6., 6., -6., -3., -3., 3., 3., -4., 4., -2., 2., -2., -2., -1., -1., &
                      2., 0., -2., 0., 0., 0., 0., 0., 1., 0., 1., 0., 0., 0., 0., 0., &
                      0., 0., 0., 0., 2., 0., -2., 0., 0., 0., 0., 0., 1., 0., 1., 0., &
                      -6., 6., 6., -6., -4., -2., 4., 2., -3., 3., -3., 3., -2., -1., -2., -1., &
                      4., -4., -4., 4., 2., 2., -2., -2., 2., -2., 2., -2., 1., 1., 1., 1.], &
                     shape=[16, 16], order=[2, 1])

        ! Compute partial derivatives along x-axis and y-axis, and cross
        ! derivatives
        !================================================================
        call zdiff(x, y, zt, z1, z2, z12)

        ! Loop for each query point
        !===========================
        allocate(zq(nq))
        call zeros(zq)

        do iq = 1, nq

            ! Locate the query point
            !========================
            x0 = minloc(xq(iq) - x, 1, mask=xq(iq) >= x)
            y0 = minloc(yq(iq) - y, 1, mask=yq(iq) >= y)
            dx = x(x0 + 1) - x(x0)
            dy = y(y0 + 1) - y(y0)

            ! Build zv so that wt·zv = c, the cubic basis coefficients
            !==========================================================
            zv = [zt(x0, y0), &
                  zt(x0 + 1, y0), &
                  zt(x0, y0 + 1), &
                  zt(x0 + 1, y0 + 1), &
                  z1(x0, y0)*dx, &
                  z1(x0 + 1, y0)*dx, &
                  z1(x0, y0 + 1)*dx, &
                  z1(x0 + 1, y0 + 1)*dx, &
                  z2(x0, y0)*dy, &
                  z2(x0 + 1, y0)*dy, &
                  z2(x0, y0 + 1)*dy, &
                  z2(x0 + 1, y0 + 1)*dy, &
                  z12(x0, y0)*dx*dy, &
                  z12(x0 + 1, y0)*dx*dy, &
                  z12(x0, y0 + 1)*dx*dy, &
                  z12(x0 + 1, y0 + 1)*dx*dy]

            ! Solve for c
            !=============
            c = reshape(matmul(wt, zv), shape=[4, 4], order=[1, 2])

            ! Scaling coefficients so that 0 <= (t, u) <= 1
            !===============================================
            t = (xq(iq) - x(x0))/dx
            u = (yq(iq) - y(y0))/dy

            ! Evaluate at the query point
            !=============================
            do i = 1, 4
                do j = 1, 4
                    zq(iq) = zq(iq) + c(i, j)*t**(i - 1)*u**(j - 1)
                end do
            end do

        end do
        return

    contains

        !-------------------------------------------------------------------
        ! zdiff
        !-------------------------------------------------------------------
        subroutine zdiff(x, y, zt, z1, z2, z12)
            real(kind=RPRE), dimension(:), intent(in) :: x, y
            real(kind=RPRE), dimension(:, :), intent(in) :: zt
            real(kind=RPRE), dimension(:, :), allocatable, intent(out) :: z1, z2, z12

            allocate (z1(m, n), z2(m, n), z12(m, n))

            ! Middle
            !========
            do j = 2, m - 1
                do k = 2, n - 1
                    z1(j, k) = (zt(j + 1, k) - zt(j - 1, k))/(x(j + 1) - x(j - 1))
                    z2(j, k) = (zt(j, k + 1) - zt(j, k - 1))/(y(k + 1) - y(k - 1))
                    z12(j, k) = (zt(j + 1, k + 1) - zt(j + 1, k - 1) - zt(j - 1, k + 1) + zt(j - 1, k - 1)) &
                                /((x(j + 1) - x(j - 1))*(y(k + 1) - y(k - 1)))
                end do
            end do

            ! Left edge
            !===========
            do j = 2, m - 1
                z1(j, 1) = (zt(j + 1, 1) - zt(j, 1))/(x(j + 1) - x(j))
                z2(j, 1) = (zt(j, 2) - zt(j, 1))/(y(2) - y(1))
                z12(j, 1) = (zt(j + 1, 2) - zt(j + 1, 1) - zt(j, 2) + zt(j, 1)) &
                            /((x(j + 1) - x(j))*(y(2) - y(1)))
            end do

            ! Upper edge
            !============
            do k = 2, n - 1
                z1(1, k) = (zt(2, k) - zt(1, k))/(x(2) - x(1))
                z2(1, k) = (zt(1, k + 1) - zt(1, k))/(y(k + 1) - y(k))
                z12(1, k) = (zt(2, k + 1) - zt(2, k) - zt(1, k + 1) + zt(1, k)) &
                            /((x(2) - x(1))*(y(k + 1) - y(k)))
            end do

            ! Right edge
            !============
            do j = 2, m - 1
                z1(j, n) = (zt(j + 1, n) - zt(j, n))/(x(j + 1) - x(j))
                z2(j, n) = (zt(j, n) - zt(j, n - 1))/(y(n) - y(n - 1))
                z12(j, n) = (zt(j + 1, n) - zt(j + 1, n - 1) - zt(j, n) + zt(j, n - 1)) &
                            /((x(j + 1) - x(j))*(y(n) - y(n - 1)))
            end do

            ! Lower edge
            !============
            do k = 2, n - 1
                z1(m, k) = (zt(m, k) - zt(m - 1, k))/(x(m) - x(m - 1))
                z2(m, k) = (zt(m, k + 1) - zt(m, k))/(y(k + 1) - y(k))
                z12(m, k) = (zt(m, k + 1) - zt(m, k) - zt(m - 1, k + 1) + zt(m - 1, k)) &
                            /((x(m) - x(m - 1))*(y(k + 1) - y(k)))
            end do

            ! Upper-left corner
            !===================
            z1(1, 1) = (zt(2, 1) - zt(1, 1))/(x(2) - x(1))
            z2(1, 1) = (zt(1, 2) - zt(1, 1))/(y(2) - y(1))
            z12(1, 1) = (zt(2, 2) - zt(2, 1) - zt(1, 2) + zt(1, 1)) &
                        /((x(2) - x(1))*(y(2) - y(1)))

            ! Upper-right corner
            !====================
            z1(1, n) = (zt(2, n) - zt(1, n))/(x(2) - x(1))
            z2(1, n) = (zt(1, n) - zt(1, n - 1))/(y(n) - y(n - 1))
            z12(1, n) = (zt(2, n) - zt(2, n - 1) - zt(1, n) + zt(1, n - 1)) &
                        /((x(2) - x(1))*(y(n) - y(n - 1)))

            ! Lower-left corner
            !===================
            z1(m, 1) = (zt(m, 1) - zt(m - 1, 1))/(x(m) - x(m - 1))
            z2(m, 1) = (zt(m, 2) - zt(m, 1))/(y(2) - y(1))
            z12(m, 1) = (zt(m, 2) - zt(m, 1) - zt(m - 1, 2) + zt(m - 1, 1)) &
                        /((x(m) - x(m - 1))*(y(2) - y(1)))

            ! Lower-right corner
            !====================
            z1(m, n) = (zt(m, n) - zt(m - 1, n))/(x(m) - x(m - 1))
            z2(m, n) = (zt(m, n) - zt(m, n - 1))/(y(n) - y(n - 1))
            z12(m, n) = (zt(m, n) - zt(m, n - 1) - zt(m - 1, n) + zt(m - 1, n - 1)) &
                        /((x(m) - x(m - 1))*(y(m) - y(m - 1)))

            return
        end subroutine zdiff

    end function spline2_1

    function spline2_2(x, y, z, xq, yq) result(zq)
        real(kind=RPRE), dimension(:, :), allocatable :: zq
        real(kind=RPRE), dimension(:), intent(in) :: x, y
        real(kind=RPRE), dimension(:, :), intent(in) :: z, xq, yq
        integer(kind=IPRE) :: m, n

        m = size(xq, 1)
        n = size(xq, 2)
        zq = reshape(spline2_1(x, y, z, [xq], [yq]), shape=[m, n])
        return
    end function spline2_2

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

    subroutine split_argument(argin, argname, argval)
        character(len=*), intent(in) :: argin
        character(len=:), allocatable, intent(out) :: argname, argval
        integer(kind=IPRE) :: idx

        idx = index(argin, "=")
        if (idx /= 0) then
            argname = trim(argin(:idx - 1))
            argval = trim(argin(idx + 1:))
        else
            print *, "Error: missing '=' in argument '"//trim(argin)//"'"
            stop
        end if
        return
    end subroutine split_argument

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

    subroutine utm2deg0(east, north, zn, zl, lat, lon)
        real(kind=RPRE), intent(in) :: east, north
        integer(kind=IPRE), intent(in) :: zn
        character(len=1), intent(in) :: zl
        real(kind=RPRE), intent(out) :: lat, lon
        real(kind=8), parameter :: K0 = 0.9996d0
        real(kind=8), parameter :: E = 0.00669438d0
        real(kind=8), parameter :: R = 6378137
        real(kind=8) :: m, mu, n, s, c, c2, d, x, y
        real(kind=8) :: E_P2, M1, F, P2, P3, P4, P5
        real(kind=8) :: p_rad, p_sin, p_sin2, p_cos, p_tan, p_tan2, p_tan4, &
                        ep_sin, ep_sin_sqrt

        x = east - 500000
        y = north
        if (verify(zl, "OXWVUTSRQPN") /= 0) y = y - 10000000

        E_P2 = E/(1.0d0 - E)
        M1 = (1 - E/4 - 3*E**2/64 - 5*E**3/256)
        F = (1 - sqrt(1 - E))/(1 + sqrt(1 - E))
        P2 = (3.0d0/2*F - 27.0d0/32*F**3 + 269.0d0/512*F**5)
        P3 = (21.0d0/16*F**2 - 55.0d0/32*F**4)
        P4 = (151.0d0/96*F**3 - 417.0d0/128*F**5)
        P5 = (1097.0d0/512*F**4)

        m = y/K0
        mu = m/(R*M1)

        p_rad = (mu + P2*sin(2*mu) &
                 + P3*sin(4*mu) &
                 + P4*sin(6*mu) &
                 + P5*sin(8*mu))

        p_sin = sin(p_rad)
        p_sin2 = p_sin**2

        p_cos = cos(p_rad)

        p_tan = p_sin/p_cos
        p_tan2 = p_tan**2
        p_tan4 = p_tan2**2

        ep_sin = 1 - E*p_sin2
        ep_sin_sqrt = sqrt(1 - E*p_sin2)

        n = R/ep_sin_sqrt
        s = (1 - E)/ep_sin
        c = F*p_cos**2
        c2 = c**2

        d = x/(n*K0)
        lat = (p_rad - (p_tan/s) &
               *(d**2/2 &
                 - d**4/24*(5 + 3*p_tan2 + 10*c - 4*c2 - 9*E_P2)) &
               + d**6/720*(61 + 90*p_tan2 + 298*c + 45*p_tan4 - 252*E_P2 - 3*c2))
        lon = (d &
               - d**3/6*(1 + 2*p_tan2 + c) &
               + d**5/120*(5 - 2*c + 28*p_tan2 - 3*c2 + 8*E_P2 + 24*p_tan4))/p_cos

        lat = lat*180.0d0/pi
        lon = lon*180.0d0/pi + (zn - 1)*6 - 180 + 3
        return
    end subroutine utm2deg0

    subroutine utm2deg1(east, north, zn, zl, lat, lon)
        real(kind=RPRE), dimension(:), intent(in) :: east, north
        integer(kind=IPRE), dimension(:), intent(in) :: zn
        character(len=1), dimension(:), intent(in) :: zl
        real(kind=RPRE), dimension(:), allocatable, intent(out) :: lat, lon
        integer(kind=IPRE) :: i, n

        n = size(east)
        allocate (lat(n), lon(n))
        do i = 1, n
            call utm2deg(east(i), north(i), zn(i), zl(i), lat(i), lon(i))
        end do
        return
    end subroutine utm2deg1

end module forlab
