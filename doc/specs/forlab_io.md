---
title: IO
---

# IO

[TOC]

## `file` - File Constructor and File Derived Type

### Status

Experimental

### Description
`file` function constructs a file entity of `file` type, including some file operation methods.

### Syntax

`ofile = [[stdlib_io(module):file(interface)]](filename [, mode])`

### Arguments

`filename`: Shall be a character expression containing the file name.

`mode` (optional): Shall be a character expression containing characters describing the way in which the file will be used. The available modes are:

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

`ofile`: a file entity of `file` type

### Return value

Returns a file entity of `file` type, in which the following are defined:

```fortran
type file
    integer :: unit
    character(:), allocatable :: filename
    character(3) :: mode
    integer :: lines
contains
    procedure :: exist => file_exist1
    procedure :: open => open_file
    procedure :: countlines => countlines1
    procedure :: close
end type file
```

### Example

```fortran
program test_io_file
    use forlab_io, only: file, disp
    use stdlib_error, only: error_stop
    type(file) :: infile

    infile = file('DP.txt', 'r')
    if(.not.infile%exist()) call error_stop('Error: File not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is: ')
    call infile%close()

end program test_io_file
```