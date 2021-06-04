# Fortran forlab Makefile
FYPPFLAGS=

export FYPPFLAGS

.PHONY: dev clean

dev:
	$(MAKE) -f Makefile --directory=src/fypp

clean:
	$(MAKE) -f Makefile clean --directory=src/fypp