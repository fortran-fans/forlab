# ForLab
Forlab is a Fortran module that provides a lot of functions for scientific computing.
It's more like a small **toolbox**.

| | |  
|:-:|---|
| **Version:** | 1.0.1 |
| **Author:** | Forlab Contributors |
| **Web site:** | https://github.com/zoziha/forlab |
| **API-Doc Web site:** | https://zoziha.github.io/forlab-API-doc/ |
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
cd src/fpm && make
```
### Build with [fpm](https://github.com/fortran-lang/fpm)
You can build using provided `fpm.toml`:
```bash
fpm build
fpm test
```
To use `forlab` within your fpm project, add the following to `fpm.toml` file:
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
    public :: File, acosd, asind, atand, argmax, argmin, argsort, &
              angle, bsplrep1, bsplrep2, bspline1, bspline2, chol, cosd, countlines, &
              cov, cumsum, chi2cdf, chi2pdf, chi2inv, chi2rand, check_directory, &
              det, diag, disp, deg2utm, datenum, datevec, datestr, deboor, diff, &
              eig, eye, file_exist, &
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
```
More helps are in here: ([Examples & Help Documents](./doc/Helps.md))

---
## More informations
### Origin
Forlab is mainly developed by Keurfon Luu originally.
#### Link
[keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)
### Fypp
The original intention of developing the multi-precision library(`forlab`) is 
to facilitate the user to switch the program accuracy requirements in a timely manner, 
which is challenging. We use `fypp` to build a multi-precision `forlab`. 
I have to say that `fypp` has helped us a lot. I learned that the use of code 
to generate code is called **meta-programming**. I also think that metaprogramming 
has great potential, especially for some low-level polymorphic functions and 
improving the dynamics of statically compiled languages, which is very helpful.  
I hope that `fypp` will get better and better, and that `fortran` will natively 
support `meta-programming` technology in the future.
### The problems we encountered
1. The adaptability of fortran metaprogramming ability is not strong;
2. Modular development `module` and setting `submodule` should best be **combined** effectively to improve development efficiency.
3. We don't want `forlab` to increase its **volume** unlimitedly. We hope that 
it can be used in areas where it can achieve value, such as rapid development 
of fortran automation applets. So we will keep the forlab lightweight, and 
update and repair it from time to time.
4. Fpm currently has some problems and pain points when compiling the program (But we are very optimistic about the potential of `fpm`):
   + **Slow** compilation speed
   + Does not support `fypp` and `make` tools
   + Cannot manage and distribute `fpm` packages well