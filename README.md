# Forlab
Forlab is a Fortran module that provides a lot of functions for scientific computing mostly inspired by Matlab and Python's module NumPy.
Forlab is developed by Keurfon Luu.

| | |  
|:-:|---|
| **Version:** | 1.0.4 |
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

## To Fix
1. When using the "gfortran -g" option and the `sort` function, an error "segment error" appears; when using the "gfortran -o3"  option and the `sort` function, it runs normally.

## Link
[keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)