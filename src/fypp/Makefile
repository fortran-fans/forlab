SRCFYPP =\
	forlab.fypp \
	forlab_issquare.fypp \
	forlab_inv.fypp \
	forlab_det.fypp \
	forlab_lu.fypp \
	forlab_diag.fypp \
	forlab_disp.fypp

SRCGEN = $(SRCFYPP:.fypp=.f90)
SRCDIR = ../

.PHONY: all clean

all: $(SRCGEN)

clean:
	cd $(SRCDIR); $(RM) $(SRCGEN)

$(SRCGEN): %.f90: %.fypp
	fypp $(FYPPFLAGS) $< $(SRCDIR)$@