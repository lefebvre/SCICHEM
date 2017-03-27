# Main program is srf2smp

PROG =	srf2smp

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../srf2smp_mak.include

SRCS_f90 = bin/SciDOSpost/Srf2Smp.f90 bin/PostProcess/Extract_fi.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


Extract_fi.o:$(BD)/bin/PostProcess/Extract_fi.f90  classdata_fd.o \
	  default_fd.o tooluser_fd.o

Srf2Smp.o:$(BD)/bin/SciDOSpost/Srf2Smp.f90  param_fd.o Extract_fi.o \
	  scipuff_fi.o $(INCMOD)


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  extract_fi.mod