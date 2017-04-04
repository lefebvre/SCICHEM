# Main program is sciDOSpost

PROG =	sciDOSpost

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../sciDOSpost_mak.include

SRCS_f90 = bin/SciDOSpost/sciDOSpost.f90 bin/SciDOSpost/mod_sciDOSpost_tools.f90 \
	  bin/SciDOSpost/getAllFieldValues.f90 bin/PostProcess/ClassData.f90 \
	  bin/PostProcess/Errors.f90 bin/PostProcess/Memory.f90 \
	  bin/PostProcess/sppCallback.f90 bin/PostProcess/Extract_fi.f90 \
	  bin/PostProcess/GetTimes_fi.f90

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

GetTimes_fi.o:$(BD)/bin/PostProcess/GetTimes_fi.f90  tooluser_fd.o

mod_sciDOSpost_tools.o:$(BD)/bin/SciDOSpost/mod_sciDOSpost_tools.f90  \
	  basic_fd.o contour_fd.o Extract_fi.o GetTimes_fi.o MPIFunc_fi.o \
	  SCIPtool.o scipuff_fi.o tooluser_fd.o $(INCMOD)

sciDOSpost.o:$(BD)/bin/SciDOSpost/sciDOSpost.f90  param_fd.o \
	  domainCoord_fd.o Extract_fi.o GetTimes_fi.o met_fi.o MPIFunc_fi.o \
	  saglst_fd.o sagdef_fd.o sagstr_fd.o mod_sciDOSpost_tools.o \
	  SCIPtool.o scipuff_fi.o tooluser_fd.o $(INCMOD)

getAllFieldValues.o:$(BD)/bin/SciDOSpost/getAllFieldValues.f90  Extract_fi.o \
	  multcomp_fd.o plotlist_fi.o saglst_fd.o sagdef_fd.o sagerr_fd.o \
	  sagstr_fd.o mod_sciDOSpost_tools.o scipuff_fi.o $(INCMOD)

ClassData.o:$(BD)/bin/PostProcess/ClassData.f90  Extract_fi.o

Errors.o:$(BD)/bin/PostProcess/Errors.f90  Extract_fi.o SCIPtool.o

Memory.o:$(BD)/bin/PostProcess/Memory.f90  Extract_fi.o GetTimes_fi.o \
	  SCIPtool.o

sppCallback.o:$(BD)/bin/PostProcess/sppCallback.f90  basic_fd.o \
	  tooluser_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  scidosposttools.mod extract_fi.mod \
	  gettimes_fi.mod