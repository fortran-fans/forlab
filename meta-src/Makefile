
SRCFYPP := $(wildcard *.fypp)						# Get all fypp files
SRCFYPP := $(filter-out common.fypp, $(SRCFYPP))	# Filter some individual files

# FPMSRCDIR: Output source files path 
FPMSRCDIR = ../src
VPATH = $(FPMSRCDIR)

SRCGEN := $(SRCFYPP:.fypp=.f90)

.PHONY: all clean

all: $(SRCGEN)

clean:
	cd $(FPMSRCDIR); $(RM) $(SRCGEN)

# GEN F90 files to `fpm/` from FYPP files
$(SRCGEN): %.f90: %.fypp common.fypp
	@mkdir -p $(FPMSRCDIR)
	fypp $(FYPPFLAGS) $< $(FPMSRCDIR)/$@
