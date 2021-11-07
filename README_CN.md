# FORLAB

[![Actions Status](https://github.com/fortran-fans/forlab/workflows/fpm/badge.svg)](https://github.com/fortran-fans/forlab/actions)

FORLAB是一个为科学计算提高一些常用函数的Fortran代码库。它更像是一个小工具箱。

FORLAB使用stdlib作为上游包，相比于stdlib，FORLAB是非正式的，它希望成为一个小的脚手架工具。


| 项目 | 描述 |  
|:-:|---|
| **版本:** | 1.0.1 |
| **作者:** | FORLAB 贡献者 |
| **源码网页:** | https://github.com/fortran-fans/forlab |
| **API-Doc网页:** | https://fortran-fans.github.io/forlab/ |
| **许可证:** | _ORLAB在MIT开源许可证下发行. |

## 开始([English README](README.md))

### 获取代码

```bash
git clone https://github.com/fortran-fans/forlab.git
cd forlab
```

### 支持的编译器

以下编译器在FORLAB的分支上经过测试： 
|名字|版本|平台|CPU架构|  
|---|---|---|---|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|
|GCC Fortran|10|Ubuntu|x86_64|
|GCC Fortran|10|MacOS|x86_64|

### 使用[fortran-lang/fpm](https://github.com/fortran-lang/fpm)构建

Fortran包管理器(FPM)是一个为Fortran而生的包管理器和构建系统。  
你可以使用提供的`fpm.toml`来构建FORLAB：

```bash
fpm build
fpm test --list
fpm test <test_name, see `fpm.toml` or list>
```

可以在你的FPM工程的`fpm.toml`文件中添加以下的语句，以使用FORLAB：

```toml
[dependencies] # or [dev-dependencies] for tests.
forlab = { git="https://github.com/fortran-fans/forlab.git", branch="forlab-fpm" }
```

## API文档

```bash
ford API-doc-FORD-file.md  # todo
cd doc && mdbook build
```
see [forlab-API-doc](https://fortran-fans.github.io/forlab/).

有一些API使用的例子被放置在了`example`文件夹下，你可以使用FPM来运行它们：

```sh
fpm run --example --list
fpm run --example <demo_name, see `fpm.toml` or list>
```

## 其它信息

### Links
1. [keurfonluu/Forlab](https://github.com/keurfonluu/Forlab)  
    FORLAB原本是由Keurfon Luu主要开发！
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