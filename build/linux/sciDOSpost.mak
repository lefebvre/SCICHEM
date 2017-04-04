# Main program is sciDOSpost

PROG =	sciDOSpost

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../sciDOSpost_mak.include

SRCS_f90 = bin/SciDOSpost/sciDOSpost.f90 bin/SciDOSpost/getAllFieldValues.f90 \
	  bin/SciDOSpost/mod_sciDOSpost_tools.f90 \
	  bin/PostProcess/ClassData.f90 bin/PostProcess/Errors.f90 \
	  bin/PostProcess/Memory.f90 bin/PostProcess/sppCallback.f90 \
	  bin/PostProcess/Extract_fi.f90 bin/PostProcess/GetTimes_fi.f90

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

bin/PostProcess/GetTimes_fi.o:$(BD)/bin/PostProcess/GetTimes_fi.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/SciDOSpost/mod_sciDOSpost_tools.o:$(BD)/bin/SciDOSpost/mod_sciDOSpost_tools.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/SciDOSpost/sciDOSpost.o:$(BD)/bin/SciDOSpost/sciDOSpost.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.o \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  bin/SciDOSpost/mod_sciDOSpost_tools.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/SciDOSpost/getAllFieldValues.o:$(BD)/bin/SciDOSpost/getAllFieldValues.f90  \
	  bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  bin/SciDOSpost/mod_sciDOSpost_tools.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

bin/PostProcess/ClassData.o:$(BD)/bin/PostProcess/ClassData.f90  \
	  bin/PostProcess/Extract_fi.o

bin/PostProcess/Errors.o:$(BD)/bin/PostProcess/Errors.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/Memory.o:$(BD)/bin/PostProcess/Memory.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/sppCallback.o:$(BD)/bin/PostProcess/sppCallback.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  scidosposttools.mod cmd_fi.mod extract_fi.mod \
	  gettimes_fi.mod