# Forlab
Forlab is a Fortran module that provides a lot of functions for scientific computing mostly inspired by Matlab and Python's module NumPy.
Forlab is mainly developed by Keurfon Luu.

| | |  
|:-:|---|
| **Version:** | 1.0.1 |
| **Author:** | Forlab Contributors |
| **Web site:** | https://github.com/zoziha/forlab |
| **Copyright:** | _This document_ has been placed in the public domain. |
| **License:** | _Forlab_ is released under the MIT License. |

## Getting Started
### Get the code
```bash
git clone https://github.com/zoziha/forlab.git
cd forlab
```
### Supported Compilers
The following combinations are tested on the default branch of `forlab`:  
|Name|Vesrion|Platform|Architecture|  
|---|---|---|---|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|
### Build with Make
You can build using provided Makefiles:
```bash
cd src && make
```
### Build with [fpm](https://github.com/fortran-lang/fpm)
You can build using provided `fpm.toml`:
```bash
fpm build
fpm test
```
To use `forlab` within your fpm project, add the following to fpm.toml file:
```toml
[dependencies]
forlab = { git = "https://github.com/zoziha/forlab.git" }
```
## Forlab Docs
```bash
ford API-doc-FORD-file.md  # todo
```
[forlab-API-doc](https://zoziha.github.io/forlab-API-doc/) is alse in here.
## Precision Description
Forlab uses double precision by default.  
If you have special needs, you can change the variables `ipre` and `rpre` in the `forlab.f90` by yourself.  
**In the near future, the `forlab` package will support multiple precision.**

## Interfaces
```fortran
    public :: File, acosd, asind, atand, argmax, argmin, argsort, arange, &
              angle, bsplrep1, bsplrep2, bspline1, bspline2, chol, cosd, countlines, &
              cov, cumsum, chi2cdf, chi2pdf, chi2inv, chi2rand, check_directory, &
              det, diag, disp, deg2utm, datenum, datevec, datestr, deboor, diff, &
              eig, &
              find, flip, fliplr, flipud, fminbnd, gammainc, horzcat, &
              hann, interp1, interp2, interp3, inv, ismember, isoutlier, issquare, &
              isleap, issymmetric, kurtosis, k2test, kde, &
              mean, median, mad,matpow, meshgrid, nextpow2, norm, normpdf, num2str, &
              outer, pascal, prctile, progress_bar, progress_perc,qr, rng, &
              randi, randperm, repmat, rms, savetxt, savebin, sind, sort, solve, &
              svd, svdsolve, std, spline1, spline2, skewness, signum, sinc, &
              split_argument, tand, tic, toc, trace, tril, triu, utm2deg, vertcat, &
              var, dbindex, gmm, kmeans, mbkmeans, silhouette

    public :: empty, sempty, dempty, qempty
    public :: eye, seye, deye, qeye
    public :: linspace, slinspace, dlinspace, qlinspace
    public :: logspace, slogspace, dlogspace, qlogspace
    public :: loadbin, sloadbin, dloadbin, qloadbin
    public :: ones, sones, dones, qones
    public :: randn, srandn, drandn, qrandn
    public :: randu, srandu, drandu, qrandu
    public :: loadtxt, sloadtxt, dloadtxt, qloadtxt
    public :: zeros, szeros, dzeros, qzeros
    !! #ifdef do_mpi
    public :: mpi_rpre
    !! #endif

    !! Operators
    public :: operator(.i.), operator(.x.)
```

## Link
[keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)
