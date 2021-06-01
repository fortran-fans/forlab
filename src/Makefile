SRCFYPP =\
	forlab.fypp \
	forlab_issquare.fypp \
	forlab_inv.fypp \
	forlab_det.fypp \
	forlab_lu.fypp \
	forlab_diag.fypp \
	forlab_disp.fypp

SRC =	forlab_kinds.f90 \
		forlab_zeros.f90 \
		forlab_eye.f90 \
		forlab_empty.f90 \
		$(SRCGEN)

LIB = libforlab.a


SRCGEN = $(SRCFYPP:.fypp=.f90)
OBJS = $(SRC:.f90=.o)
MODS = $(OBJS:.o=.mod)
SMODS = $(OBJS:.o=*.smod)

.PHONY: all clean

all: $(LIB)

$(LIB): $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	$(RM) $(LIB) $(OBJS) $(MODS) $(SMODS) $(SRCGEN)

%.o: %.f90
	$(FC) $(FFLAGS) -c $<

$(SRCGEN): %.f90: %.fypp common.fypp
	fypp $(FYPPFLAGS) $< $@

# Fortran module dependencies
forlab.o: \
	forlab_kinds.o
forlab_issquare.o: \
	forlab.o \
	forlab_kinds.o
forlab_inv.o: \
	forlab.o \
	forlab_kinds.o
forlab_det.o: \
	forlab.o \
	forlab_kinds.o
forlab_lu.o: \
	forlab.o \
	forlab_kinds.o
forlab_diag.o: \
	forlab.o \
	forlab_kinds.o
forlab_eye.o: \
	forlab.o \
	forlab_kinds.o
forlab_zeros.o: \
	forlab.o \
	forlab_kinds.o
forlab_empty.o: \
	forlab.o \
	forlab_kinds.o
forlab_disp.o: \
	forlab.o \
	forlab_kinds.o