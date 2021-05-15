# Forlab
Forlab is a Fortran module that provides a lot of functions for scientific computing mostly inspired by Matlab and Python's module NumPy.
Forlab is developed by Keurfon Luu.

| | |  
|:-:|---|
| **Version:** | 1.0.1 |
| **Author:** | Keurfon Luu |
| **Web site:** | https://github.com/keurfonluu/forlab |
| **Copyright:** | This document has been placed in the public domain. |
| **License:** | Forlab is released under the MIT License. |

## Getting Started
### Download and Fpm Build
```bash
git clone https://github.com/zoziha/forlab.git
cd forlab
fpm build
fpm test
```
## Forlab Docs
```bash
ford README.md  # todo
```
## Precision Description
Forlab uses double precision by default.  
If you have special needs, you can change the variables `ipre` and `rpre` in the `forlab.f90` by yourself.

## Interfaces
```fortran
public :: File, acosd, asind, atand, argmax, argmin, argsort, arange, &
          angle, bsplrep1, bsplrep2, bspline1, bspline2, chol, cosd, countlines, &
          cov, cumsum, chi2cdf, chi2pdf, chi2inv, chi2rand, check_directory, &
          det, diag, disp, deg2utm, datenum, datevec, datestr, deboor, diff, &
          eig, empty, eye, &
          find, flip, fliplr, flipud, fminbnd, gammainc, horzcat, &
          hann, interp1, interp2, interp3, inv, ismember, isoutlier, issquare, &
          isleap, issymmetric, kurtosis, k2test, kde, loadtxt, loadbin, linspace, &
          mean, median, mad, meshgrid, nextpow2, norm, normpdf, num2str, ones, &
          outer, pascal, prctile, progress_bar, progress_perc, rng, randu, randn, &
          randi, randperm, repmat, rms, savetxt, savebin, sind, sort, solve, &
          svd, svdsolve, std, spline1, spline2, skewness, signum, sinc, &
          split_argument, tand, tic, toc, trace, tril, triu, utm2deg, vertcat, &
          var, zeros, dbindex, gmm, kmeans, mbkmeans, silhouette
public :: mpi_rpre
public :: operator(.i.)

```

## To Fix
1. When using the "gfortran -g" option and the `sort` function, an error "segment error" appears; when using the "gfortran -o3"  option and the `sort` function, it runs normally.

## Link
[keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)
