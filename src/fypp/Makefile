# SRCFYPP =\
# 	forlab.fypp \
# 	forlab_issquare.fypp \
# 	forlab_inv.fypp \
# 	forlab_det.fypp \
# 	forlab_lu.fypp \
# 	forlab_diag.fypp \
# 	forlab_disp.fypp \
# 	forlab_kinds.fypp \
# 	forlab_arange.fypp \
# 	forlab_randn.fypp \
# 	forlab_randu.fypp \
# 	forlab_zeros.fypp \
# 	forlab_rng.fypp
SRCFYPP := $(wildcard *.fypp)						# Get all fypp files
SRCFYPP := $(filter-out common.fypp, $(SRCFYPP))	# Filter some individual files

SRCGEN = $(SRCFYPP:.fypp=.f90)
SRCDIR = ../

.PHONY: all clean

all: $(SRCGEN)

clean:
	cd $(SRCDIR); $(RM) $(SRCGEN)

$(SRCGEN): %.f90: %.fypp common.fypp
	fypp $(FYPPFLAGS) $< $(SRCDIR)$@