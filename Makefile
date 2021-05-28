# Fortran stdlib Makefile

FC = gfortran
FFLAGS = -Wall -Wextra -Wimplicit-interface -fPIC -g -fcheck=all
FYPPFLAGS=

export FC
export FFLAGS
export FYPPFLAGS

.PHONY: all clean test

all:
	$(MAKE) -f Makefile --directory=src
#	$(MAKE) -f Makefile --directory=src/tests

test:
	$(MAKE) -f Makefile --directory=test test
	@echo
	@echo "All tests passed."

clean:
	$(MAKE) -f Makefile.manual clean --directory=src
	$(MAKE) -f Makefile.manual clean --directory=test