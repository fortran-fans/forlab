# FORLAB

[![Actions Status](https://github.com/fortran-fans/forlab/workflows/fpm/badge.svg)](https://github.com/fortran-fans/forlab/actions)

FORLAB is a Fortran module that provides some functions for scientific computing.
It's more like a small **toolbox**.  
FORLAB uses [stdlib](https://github.com/fortran-lang/stdlib) as an upstream package. FORLAB hopes to be a small scaffolding tool. Compared with [stdlib](https://github.com/fortran-lang/stdlib), FORLAB is less formal.

| | |  
|:-:|---|
| **Version:** | 1.0.1 |
| **Author:** | FORLAB Contributors |
| **Web site:** | https://github.com/fortran-fans/forlab |
| **API-Doc Web site:** | https://zoziha.github.io/forlab-API-doc/ |
| **Copyright:** | _This document_ has been placed in the public domain. |
| **License:** | _FORLAB_ is released under the MIT License. |

## Getting Started ([中文文档](./README_CN.md))
### Get the code

```bash
git clone https://github.com/fortran-fans/forlab.git
cd forlab
```

### Supported Compilers

The following combinations are tested on the default branch of `forlab`:  
|Name|Vesrion|Platform|Architecture|  
|---|---|---|---|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|
|GCC Fortran|10|Ubuntu|x86_64|
|GCC Fortran|10|MacOS|x86_64|

### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
Fortran Package Manager (fpm) is a great package manager and build system for Fortran.
You can build using provided `fpm.toml`:
```bash
fpm build
fpm test --list
fpm test <test_name, see `fpm.toml` or list>
```

To use `forlab` within your `fpm` project, add the following to `fpm.toml` file:
```toml
[dependencies] # or [dev-dependencies] for tests.
forlab = { git="https://github.com/fortran-fans/forlab.git", branch="forlab-fpm" }
```

## API-Doc

```bash
ford API-doc-FORD-file.md  # todo
```
see [forlab-API-doc](https://fortran-fans.github.io/forlab/page/specs/index.html).

Some examples are prepared in the `./example` folder, and you can use `fpm` to run them.
```sh
fpm run --example --list
fpm run --example <demo_name, see `fpm.toml` or list>
```

## More informations

### Links
1. [keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)  
    Forlab is mainly developed by Keurfon Luu originally.
2. [stdlib](https://github.com/fortran-lang/stdlib)  
   Fortran standard library.
3. Fortran [Generics](https://github.com/j3-fortran/generics)

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
1. The adaptability of `fortran` metaprogramming ability is not strong;
2. Modular development `module` and setting `submodule` should best be **combined** effectively to improve development efficiency.
3. We don't want `forlab` to increase its **volume** unlimitedly. We hope that 
it can be used in areas where it can achieve value, such as rapid development 
of fortran automation applets. So we will keep the forlab lightweight, and 
update and repair it from time to time.
4. Fpm currently has some problems and pain points when compiling the program (But we are very optimistic about the potential of `fpm`):
   + Slow compilation speed. (Improvements in this PR: [optimize file listing](https://github.com/fortran-lang/fpm/pull/507))
   + Cannot manage and distribute `fpm` packages well now.
5. Fortran [Generics](https://github.com/j3-fortran/generics): Due to the lack of more complete generics, certain functions such as multiple precision and multiple array dimensions cannot be implemented now.