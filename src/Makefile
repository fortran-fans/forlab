
SRCFYPP := $(wildcard *.fypp)						# Get all fypp files
SRCFYPP := $(filter-out common.fypp, $(SRCFYPP))	# Filter some individual files

SRCF90 := $(wildcard *.f90 *.F90)					# Get all f90/F90  files
SRCF90 := $(filter-out f08estop.f90, $(SRCF90))		# Filter some individual files

SRCC := $(wildcard *.c *.cpp)						# Get all clang files

# FPMSRCDIR: Output source files path 
FPMSRCDIR = fpm
VPATH = $(FPMSRCDIR)

SRCGEN := $(SRCFYPP:.fypp=.f90)

.PHONY: all clean CPF90 CPC

all: $(SRCGEN) CPF90 CPC

clean:
	cd $(SRCDIR); $(RM) $(SRCGEN) $(SRCF90)

# GEN F90 files to `fpm/` from FYPP files
$(SRCGEN): %.f90: %.fypp
	@mkdir -p $(FPMSRCDIR)
	fypp $(FYPPFLAGS) $< $(FPMSRCDIR)/$@

# COPY F90 files to `fpm/`
CPF90: $(SRCF90)
	@mkdir -p $(FPMSRCDIR)
	cp -u $^ $(FPMSRCDIR)

# COPY canag files to `fpm/`
CPC: $(SRCC)
	@mkdir -p $(FPMSRCDIR)
	cp -u $^ $(FPMSRCDIR)
