
SRC = $(wildcard *.f90)						# Get all f90 files

LIB = libforlab.a

FC = gfortran
FFLAGS = # -O3
OBJS = $(SRC:.f90=.o)

.PHONY: all clean fypp

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

fypp:
	$(MAKE) -f Makefile --directory=fypp

clean:
	$(RM) $(LIB) *.o *.mod *.smod

$(OBJS): %.o: %.f90
	$(FC) $(FFLAGS) -c $<

# Fortran module dependencies
OBJS_EXT = $(filter-out forlab_file.o forlab_kinds.o forlab.o, $(OBJS))
forlab.o: \
	forlab_file.o \
	forlab_kinds.o

$(filter %.o, $(OBJS_EXT)): \
	forlab.o \
	forlab_kinds.o \
	forlab_file.o