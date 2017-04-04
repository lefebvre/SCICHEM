# Main program is liblanduse.${LSuf}

PROG =	liblanduse.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../landuse_mak.include

SRCS_f90 = dll/SCIPUFF/LandUse/landuse.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = dll/SCIPUFF/LandUse

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 


dll/SCIPUFF/LandUse/landuse.o:$(BD)/dll/SCIPUFF/LandUse/landuse.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  landuse.mod