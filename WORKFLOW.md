# workflow tools
1. fypp
2. ford
3. fpm
4. git
5. vs code

# add f90 files
1. add a submodule f90 files
    + submodule name
    + use forlab_kinds
    + module porcedure
    + formatted
2. add forlab interface
3. update makefile
    + add f90
    + add dependencies
4. make test
    + make && test

# add fypp files

# fypp -> f90 -> obj
1. make: make fypp -> f90; mv f90 -> src
2. fpm build: f90 -> obj
bash command:
```bash
make dev
fpm test
```