# Main program is srf2smp

PROG =	srf2smp

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../srf2smp_mak.include

SRCS_f90 = bin/SciDOSpost/Srf2Smp.f90 bin/SciDOSpost/getAllFieldValues.f90 \
	  bin/SciDOSpost/mod_sciDOSpost_tools.f90 \
	  bin/PostProcess/Extract_fi.f90 bin/PostProcess/sppCallback.f90 \
	  bin/PostProcess/Errors.f90 bin/PostProcess/GetTimes_fi.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = bin/PostProcess bin/SciDOSpost

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 


bin/PostProcess/Extract_fi.o:$(BD)/bin/PostProcess/Extract_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/SciDOSpost/Srf2Smp.o:$(BD)/bin/SciDOSpost/Srf2Smp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

bin/PostProcess/GetTimes_fi.o:$(BD)/bin/PostProcess/GetTimes_fi.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/SciDOSpost/mod_sciDOSpost_tools.o:$(BD)/bin/SciDOSpost/mod_sciDOSpost_tools.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/SciDOSpost/getAllFieldValues.o:$(BD)/bin/SciDOSpost/getAllFieldValues.f90  \
	  bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  bin/SciDOSpost/mod_sciDOSpost_tools.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

bin/PostProcess/sppCallback.o:$(BD)/bin/PostProcess/sppCallback.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/Errors.o:$(BD)/bin/PostProcess/Errors.f90  \
	  bin/PostProcess/Extract_fi.o $(INCMOD)


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  scidosposttools.mod cmd_fi.mod extract_fi.mod \
	  gettimes_fi.mod