---
title: IO
---

# IO

[TOC]

### `file` - File Constructor and File Derived Type

#### Status

Experimental

#### Class

Impure function.

#### Description

`file` function constructs a file entity of `file` type, including some file operation methods.

#### Syntax

`ofile = [[forlab_io(module):file(interface)]](filename [, mode])`

#### Arguments

`filename`: Shall be a `character(len=*)` scalar.
This argument is `intent(in)`. 
Contains the file name.

`mode`: Shall be a `character(len=*)` scalar.
This argument is `intent(in)` and `optional`.
Contains characters describing the way in which the file will be used (see [stdlib_io:open](https://stdlib.fortran-lang.org/page/specs/stdlib_io.html#description_1)). The available modes are:

| Character | Meaning |
| --------- | ------- |
| `'r'` | construct a file for reading (default) |
| `'w'` | construct a file for writing, truncating the file first |
| `'x'` | construct a file for exclusive creation, failing if the file already exists |
| `'a'` | construct a file for writing, appending to the end of the file if it exists |
| `'+'` | construct a file for updating (reading and writing) |
| `'b'` | binary mode |
| `'t'` | text mode (default) |


The default `mode` is `'rt'` (i.e. construct a file for reading a text file). The `mode` may include one of the four different methods for opening a file (i.e., `'r'`, `'w'`, `'x'`, and `'a'`). These four methods can be associated with the character `'+'` to open the file for updating. In addition, it can be specified if the file should be handled as a binary file (`'b'`) or a text file (`'t'`).

#### Return value

Returns a file entity of `file` type, in which the following are defined:

```fortran
type file
    character(:), allocatable :: filename
    character(3) :: mode
    integer :: unit
    integer :: lines
contains
    procedure :: exist
    procedure :: open
    procedure :: countlines
    procedure :: close
end type file
```

#### Examples

```fortran
program demo_io_file_1
    use forlab_io, only: file, disp
    use stdlib_error, only: check
    type(file) :: infile

    infile = file('DP.txt', 'r')
    call check(infile%exist(), msg="File not exist, "//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is: ')
    call infile%close()     !! `infile` is closed, `infile%filename` is deallocated.

end program demo_io_file_1
```

```fortran
program demo_io_file_2
    use forlab_io, only: file, disp
    use stdlib_error, only: check
    type(file), allocatable :: infile

    !! To create a `file` type scalar to read:
    !!
    !!```fortran
    !! infile = file("somefile.txt")        ! The default `mode` is "rt"
    !! infile = file("somefile.txt", "r")
    !!```
    !!
    !! To create a `file` type scalar to write:
    !!    infile = file("somefile.txt", "w")
    !!
    !! To append to the end of the file if it exists:
    !!
    !!    infile = file("somefile.txt", "a")

    infile = file('DP.txt', 'r')
    call check(infile%exist(), msg="File not exist, "//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is: ')
    call infile%close()     !! It is optional. `infile` is closed, `infile%filename` is deallocated.
    deallocate(infile)      !! `infile` is deallocated.

end program demo_io_file_2
```

### `file%exist` 

#### Description

Checks the `file` is exist or not.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`result = self % [[file(type):exist(bound)]]()`

#### Arguments

None.

#### Return value

Returns a `logical` scalar: the file exists as `.true.`, the file does not exist as `.false.`.

#### Example

```fortran
program demo_io_file_exist
    use forlab_io, only: file
    use stdlib_error, only: check
    type(file), allocatable :: infile

    infile = file("filename.txt", "r")
    call check(infile%exist(), msg="File not exist: "//infile%filename)
    deallocate(infile) 
    
end program demo_io_file_exist
```

### `file%open` 

#### Description

Open the `file` object.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`call self % [[file(type):open(bound)]]()`

#### Arguments

None.

#### Example

```fortran
program demo_io_file_open
    use forlab_io, only: file
    use stdlib_error, only: check
    type(file), allocatable :: infile

    infile = file("filename.txt", "r")
    call check(infile%exist(), msg="File not exist: "//infile%filename)
    call infile%open()      !! Open file operation
    deallocate(infile) 
    
end program demo_io_file_open
```

### `file%countlines` 

#### Description

Counts the number of lines in a txt file, the number of file lines is stored in `file%lines`.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`call self % [[file(type):countlines(bound)]]()`

#### Arguments

None.

#### Example

```fortran
program demo_io_file_countlines
    use forlab_io, only: file, disp
    type(file), allocatable :: infile

    infile = file("filename.txt", "r")
    call infile%countlines()    !! Counts the number of lines in a txt file, the number of file lines is stored in `infile%lines`.
    call disp(infile%lines, "The number of file lines : ")
    deallocate(infile) 
    
end program demo_io_file_countlines
```

### `file%close` 

#### Description

Closes the `file` object, deallocate `file%filename`.

#### Status

Experimental.

#### Class

Impure function.

#### Syntax

`call self % [[file(type):close(bound)]]()`

#### Arguments

None.

#### Example

```fortran
program demo_io_file_close
    use forlab_io, only: file, disp
    type(file) :: infile

    infile = file("filename.txt", "r")
    call infile%open()
    call infile%close()     !! Closes the `infile` object, deallocate `infile%filename`.

end program demo_io_file_close
```

### `color`

#### Description

Prints a PS code at default output_unit.

#### Status

Experimental.

#### Class

Impure subroutine.

#### Syntax

`call [[forlab_io(module):color(interface)]] ( [string=achar(27) // '[0m'] )`

#### Argument

`string`: Shall be a `character(len=*)` scalar.
This argument is `intent(in)` and `optional`.  
The default value is `achar(27) // '[0m'`.

#### Output 

Prints a PS code at default output_unit.

#### Example

```fortran
program demo_io_color
    use forlab_io, only: color
    use forlab_color, only: red, green
    character(len=*), parameter :: char = "It is Fortran Color: "
    call color(green)
    print *, char // 'green.'
    print *, red // char // 'red.'
    call color()
    print *, char // "default."
end program demo_io_color
```

### `disp` - display your data

#### Status

Experimental

#### Class

Impure subroutine.

#### Description

Outputs a `logical/integer/real/complex/character/string_type` scalar or `logical/integer/real/complex` and rank-1/rank-2 array to the screen or a file `unit`.

##### More details

```fortran
call disp( A(i, j, 2, :, 1:10) [, header, unit, brief] )    !! `i, j, ...` can be determined by `do` loop.
```

For `complex` type, the output format is `*(A25, 1X)`; 
For    other types, the output format is `*(A12, 1X)`.

To prevent users from accidentally passing large-length arrays to `disp`, causing unnecessary io blockage:
1. If the `brief` argument is not specified, `disp` will print **the brief array content with a length of `10*50` by default**.
2. Specify `brief=.true.`, `disp` will print **the brief array content with a length of `5*5`**;
3. Specify `brief=.false.`, `disp` will print **`all` the contents of the array**.

#### Syntax

`call [[forlab_io(module):disp(interface)]]([x, header, unit, brief])`  

#### Arguments

`x`: Shall be a `logical/integer/real/complex/string_type` scalar or `logical/integer/real/complex` and rank-1/rank-2 array.
This argument is `intent(in)` and `optional`.

`header`: Shall be a `character(len=*)` scalar. 
This argument is `intent(in)` and `optional`.

`unit`: Shall be an `integer` scalar linked to an IO stream.
This argument is `intent(in)` and `optional`.

`brief`: Shall be a `logical` scalar.
This argument is `intent(in)` and `optional`.  
Controls an abridged version of the `x` object is printed.

#### Output

The result is to print `header` and `x` on the screen (or another output `unit/file`) in this order.  
If `x` is a rank-1/rank-2 `array` type, the dimension length information of the `array` will also be outputted.

If `disp` is not passed any arguments, a blank line is printed.

If the `x` is present and of `real/complex` type, the data will retain four significant decimal places, like `(g0.4)`.

#### Example

```fortran
program demo_io_disp
    
    use forlab_io, only: disp
    real(8) :: r(2, 3)
    complex :: c(2, 3), c_3d(2, 100, 20)
    integer :: i(2, 3)
    logical :: l(10, 10)
    r = 1.; c = 1.; c_3d = 2.; i = 1; l = .true.
    r(1, 1) = -1.e-11
    r(1, 2) = -1.e10
    c(2, 2) = (-1.e10,-1.e10)
    c_3d(1,3,1) = (1000, 0.001)
    c_3d(1,3,2) = (1.e4, 100.)
    call disp('string', header='disp(string):')
    call disp('It is a note.')
    call disp()
    call disp(r, header='disp(r):')
    call disp(r(1,:), header='disp(r(1,:))')
    call disp(c, header='disp(c):')
    call disp(i, header='disp(i):')
    call disp(l, header='disp(l):', brief=.true.)
    call disp(c_3d(:,:,3), header='disp(c_3d(:,:,3)):', brief=.true.)
    call disp(c_3d(2,:,:), header='disp(c_3d(2,:,:)):', brief=.true.)
end program demo_io_disp
```
**Results:**
```fortran
 disp(string):
 string
 It is a note.
 disp(r):
 [matrix size: 2×3]
 -0.1000E-10  -0.1000E+11    1.000
   1.000        1.000        1.000
 disp(r(1,:))
 [vector size: 3]
 -0.1000E-10  -0.1000E+11    1.000
 disp(c):
 [matrix size: 2×3]
            (1.000,0.000)             (1.000,0.000)             (1.000,0.000)
            (1.000,0.000) (-0.1000E+11,-0.1000E+11)             (1.000,0.000)
 disp(i):
 [matrix size: 2×3]
           1            1            1
           1            1            1
 disp(l):
 [matrix size: 10×10]
           T            T            T          ...            T
           T            T            T          ...            T
           T            T            T          ...            T
           :            :            :            :            :
           T            T            T          ...            T
 disp(c_3d(:,:,3)):
 [matrix size: 2×100]
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
 disp(c_3d(2,:,:)):
 [matrix size: 100×20]
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
                        :                         :                         :                         :                         :  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000) 
```