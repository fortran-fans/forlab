# Examples & Help Documents
## File interaction method
### Description
In addition to pre-compiled code, our program may also need to interact with files.
Forlab sets up **ASCII text** file, **binary file** read and write functions, which are implemented in `forlab_save.fypp`, `forlab_load.fypp` and `forlab_file.fypp`.
We can perform data interaction from regularized **ASCII text files** and **binary files**. 
Generally, in order to avoid eof errors, we need to obtain some basic information 
of the file when interacting with the file, such as **whether the file exists, file name, file size, and file storage format**.
### Query the number of file lines
`countlines` returns the number of lines in the file.
```fortran
filelines = countlines('ASCII.txt')
    !! Directly query the number of file('ASCII.txt') lines
---
infile = file(filename)
    !! Initialization file
filelines = infile%countlines()
```
> [file] \todo
### Check if the file exists
Similar to querying the number of file lines, `file_exist` queries whether the file exists.
```fortran
ok = file_exist('ASCII.txt')
    !! Directly query the number of file('ASCII.txt') lines
---
infile = file(filename)
    !! Initialization file
ok = infile%file_exist()
```
### ForLab file operations
We hide the direct specification of the unit number and leave it to the compiler to specify, we can access the file unit number through `infile%unit`.  
Type `file` contains two data: `file%unit`, `file%filename`.
```fortran
type(file) :: infile
infile = file(filename)
    !! Initialization file
call infile%open()
ok = infile%file_exist()
filelines = infile%file_exist()
call infile%close()
```
### Save/Load methods
#### ASCII files
It is difficult to realize that the fortran return value is polymorphic. We use `(s)loadtxt` to read and accept single-precision data (`dloadtxt-dp; qloadtxt-qp`)
```fortran
call savetxt(filename, x)
    !! Store the data of x in filename, the maximum dimension of x is 2 dimensions
x = (s/d/q)loadtxt(filename)
    !! Read data from file
```
#### Binary file
Similar to the ASCII file reading and writing method.
```fortran
call savebin(filename, x)
    !! Store the data of x in filename, the maximum dimension of x is 3 dimensions
x = (s/d/q)loadbin(filename)
    !! Read data from file
```
If you need more complex requirements, please use the open source `hdf5` and `netcdf` packages.