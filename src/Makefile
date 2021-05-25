SRCFYPP =\
	forlab.fypp \
	forlab_issquare.fypp

SRC = forlab_kinds.f90 \
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
	forlab.o