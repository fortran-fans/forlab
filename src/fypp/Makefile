
SRCFYPP := $(wildcard *.fypp)						# Get all fypp files
SRCFYPP := $(filter-out common.fypp, $(SRCFYPP))	# Filter some individual files

SRCGEN = $(SRCFYPP:.fypp=.f90)
SRCDIR = ../

.PHONY: all clean

all: $(SRCGEN)

clean:
	cd $(SRCDIR); $(RM) $(SRCGEN)

$(SRCGEN): %.f90: %.fypp common.fypp
	fypp.exe $(FYPPFLAGS) $< $(SRCDIR)$@
