
SRCFYPP := $(wildcard *.fypp)						# Get all fypp files
SRCFYPP := $(filter-out common.fypp, $(SRCFYPP))	# Filter some individual files

# SRCDIR: Output source files path 
SRCDIR = ../
VPATH = $(SRCDIR)

SRCGEN := $(SRCFYPP:.fypp=.f90)

.PHONY: all clean

all: $(SRCGEN)

clean:
	cd $(SRCDIR); $(RM) $(SRCGEN)

$(SRCGEN): %.f90: %.fypp
	fypp.exe $(FYPPFLAGS) $< $(SRCDIR)$@
