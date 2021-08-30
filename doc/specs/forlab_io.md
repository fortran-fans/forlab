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