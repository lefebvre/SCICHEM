# Main program is liblanduse.${LSuf}

PROG =	liblanduse.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../landuse_mak.include

SRCS_f90 = dll/SCIPUFF/LandUse/landuse.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


landuse.o:$(BD)/dll/SCIPUFF/LandUse/landuse.f90  charT_fd.o landuse_fd.o \
	  param_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  landuse.mod