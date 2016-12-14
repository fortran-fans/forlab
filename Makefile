#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!  Copyright (c) 2015 by
#!  Magnitude, France  and  MINES ParisTech, France
#!  All rights reserved.
#!
#!  This software is furnished under a license and may be used and copied
#!  only in  accordance with  the  terms  of  such  license  and with the
#!  inclusion of the above copyright notice. This software or  any  other
#!  copies thereof may not be provided or otherwise made available to any
#!  other person.  No title to and ownership of  the  software is  hereby
#!  transferred.
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#         Makefile for Fortran programs
#
# ======================================================================
# Declarations for compiler (comment or decomment as necessary)
# ======================================================================
#
# ---- Gnu complier (gcc)
FC      :=  gfortran
#
# ---- Gnu MPI compiler
# FC      :=  mpif90
#
# flags for debugging or for maximum performance or MPI, comment as necessary
#
# ---- Option for gfortran compiler
#FFLAGS  :=  -Ofast
#FFLAGS  :=  -O3 -ffree-line-length-none
#FFLAGS  :=  -O3 -ffree-line-length-none -Wall -Wextra -fbounds-check
#FFLAGS  :=  -O3 -ffree-line-length-none -cpp -Ddo_mpi
FFLAGS  :=  -O3 -ffast-math -march=native -funroll-loops -fno-protect-parens -flto -fcheck=all
#
# ======================================================================
# Declarations of executables to be compiled and various dependances
# ======================================================================
# Name of executable
TARGET1 :=  example_rand.exe
TARGET2 :=  example_fit.exe

# Directories
SRCDIR  :=  src
OBJDIR  :=  obj

MAIN    :=  $(SRCDIR)

# Link objects to create executable (tab required on second line)
OBJS1   :=  $(OBJDIR)/forlab.o \
            $(OBJDIR)/example_rand.o

OBJS2   :=  $(OBJDIR)/forlab.o \
						$(OBJDIR)/example_fit.o

# These routines depend on include file - recompile if include modified
ALL     :=  $(TARGET1) $(TARGET2)
all: $(ALL)
example_rand: $(TARGET1)
example_fit: $(TARGET2)

# ======================================================================
# General rules, these should not require modification
# General rules for building ".o" objects from fortran programs or subroutines
# ======================================================================

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(OBJDIR)/%.o: $(MAIN)/%.f90 | $(OBJDIR)
	$(FC) $(FFLAGS) -c $^ -o $@ -J$(OBJDIR)

$(TARGET1): $(OBJS1)
	$(FC) $(FFLAGS) -o $@ $(OBJS1)

$(TARGET2): $(OBJS2)
	$(FC) $(FFLAGS) -o $@ $(OBJS2)

# Utilities

.PHONY: all example_rand example_fit clean veryclean

clean:
	rm -rf $(ALL) $(OBJDIR)

veryclean:
	rm -rf $(ALL) $(OBJDIR) examples utils/*.txt
# ======================================================================
# That's all
# ======================================================================
