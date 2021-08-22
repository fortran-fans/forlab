# Fortran forlab Makefile, only for developement.
FYPPFLAGS=

export FYPPFLAGS

.PHONY: dev clean

dev:
	$(MAKE) -f Makefile --directory=meta-src

clean:
	$(MAKE) -f Makefile clean --directory=meta-src