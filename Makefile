# Fortran forlab Makefile
FYPPFLAGS=

export FYPPFLAGS

.PHONY: all clean

all:
	$(MAKE) -f Makefile --directory=src/fypp

clean:
	$(MAKE) -f Makefile clean --directory=src/fypp