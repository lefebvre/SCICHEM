# Main program is scipp

PROG =	scipp

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../scipp_mak.include

SRCS_f90 = bin/PostProcess/ClassData.f90 bin/PostProcess/CreateFields.f90 \
	  bin/PostProcess/ExtractHorzLines.f90 \
	  bin/PostProcess/LocalPuffs_fi.f90 \
	  bin/PostProcess/Initialization.f90 bin/PostProcess/GetSrcFunc.f90 \
	  bin/PostProcess/PufMom.f90 bin/PostProcess/ExtractContours.f90 \
	  bin/PostProcess/Readpuf.f90 bin/PostProcess/SelectField.f90 \
	  bin/PostProcess/ExtractProfiles.f90 bin/PostProcess/ExtractGrid.f90 \
	  bin/PostProcess/MainEntryPoint.f90 bin/PostProcess/Output3D.f90 \
	  bin/PostProcess/GetSrcFunc_fi.f90 bin/PostProcess/Tables.f90 \
	  bin/PostProcess/BubbleMom.f90 bin/PostProcess/FFT07.f90 \
	  bin/PostProcess/sppCallback.f90 bin/PostProcess/GetPlotLists.f90 \
	  bin/PostProcess/Extract_fi.f90 bin/PostProcess/ExtractOutput.f90 \
	  bin/PostProcess/TotalMass.f90 bin/PostProcess/TotalMass_fi.f90 \
	  bin/PostProcess/WriteFiles.f90 bin/PostProcess/GetPuffs.f90 \
	  bin/PostProcess/CommandLine.f90 bin/PostProcess/FFT07_fi.f90 \
	  bin/PostProcess/Memory.f90 bin/PostProcess/GetTimes_fi.f90 \
	  bin/PostProcess/RegularGrid.f90 bin/PostProcess/Contours.f90 \
	  bin/PostProcess/NativeGrid.f90 bin/PostProcess/Errors.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = bin/PostProcess

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

bin/PostProcess/ClassData.o:$(BD)/bin/PostProcess/ClassData.f90  \
	  bin/PostProcess/Extract_fi.o

bin/PostProcess/CreateFields.o:$(BD)/bin/PostProcess/CreateFields.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/ExtractHorzLines.o:$(BD)/bin/PostProcess/ExtractHorzLines.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/GetTimes_fi.o:$(BD)/bin/PostProcess/GetTimes_fi.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/LocalPuffs_fi.o:$(BD)/bin/PostProcess/LocalPuffs_fi.f90  \
	  bin/PostProcess/GetTimes_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/GetSrcFunc_fi.o:$(BD)/bin/PostProcess/GetSrcFunc_fi.f90 

bin/PostProcess/Initialization.o:$(BD)/bin/PostProcess/Initialization.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/type_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetSrcFunc_fi.o \
	  bin/PostProcess/GetTimes_fi.o bin/PostProcess/LocalPuffs_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/GetSrcFunc.o:$(BD)/bin/PostProcess/GetSrcFunc.f90 

bin/PostProcess/PufMom.o:$(BD)/bin/PostProcess/PufMom.f90  \
	  bin/PostProcess/LocalPuffs_fi.o dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

bin/PostProcess/ExtractContours.o:$(BD)/bin/PostProcess/ExtractContours.f90  \
	  bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fd.o

bin/PostProcess/Readpuf.o:$(BD)/bin/PostProcess/Readpuf.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  bin/PostProcess/LocalPuffs_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/PostProcess/SelectField.o:$(BD)/bin/PostProcess/SelectField.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o

bin/PostProcess/ExtractProfiles.o:$(BD)/bin/PostProcess/ExtractProfiles.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/ExtractGrid.o:$(BD)/bin/PostProcess/ExtractGrid.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/MainEntryPoint.o:$(BD)/bin/PostProcess/MainEntryPoint.f90  \
	  bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/PostProcess/Output3D.o:$(BD)/bin/PostProcess/Output3D.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o

bin/PostProcess/Tables.o:$(BD)/bin/PostProcess/Tables.f90 

bin/PostProcess/BubbleMom.o:$(BD)/bin/PostProcess/BubbleMom.f90  \
	  bin/PostProcess/LocalPuffs_fi.o dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

bin/PostProcess/FFT07.o:$(BD)/bin/PostProcess/FFT07.f90 

bin/PostProcess/sppCallback.o:$(BD)/bin/PostProcess/sppCallback.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/GetPlotLists.o:$(BD)/bin/PostProcess/GetPlotLists.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/ExtractOutput.o:$(BD)/bin/PostProcess/ExtractOutput.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/type_fd.o bin/PostProcess/Extract_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  bin/PostProcess/GetTimes_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/TotalMass_fi.o:$(BD)/bin/PostProcess/TotalMass_fi.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/PostProcess/TotalMass.o:$(BD)/bin/PostProcess/TotalMass.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  bin/PostProcess/LocalPuffs_fi.o dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  bin/PostProcess/TotalMass_fi.o

bin/PostProcess/WriteFiles.o:$(BD)/bin/PostProcess/WriteFiles.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/GetPuffs.o:$(BD)/bin/PostProcess/GetPuffs.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o bin/PostProcess/Extract_fi.o \
	  bin/PostProcess/GetTimes_fi.o bin/PostProcess/LocalPuffs_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

bin/PostProcess/CommandLine.o:$(BD)/bin/PostProcess/CommandLine.f90  \
	  bin/PostProcess/Extract_fi.o

bin/PostProcess/FFT07_fi.o:$(BD)/bin/PostProcess/FFT07_fi.f90 

bin/PostProcess/Memory.o:$(BD)/bin/PostProcess/Memory.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/RegularGrid.o:$(BD)/bin/PostProcess/RegularGrid.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/Contours.o:$(BD)/bin/PostProcess/Contours.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/NativeGrid.o:$(BD)/bin/PostProcess/NativeGrid.f90  \
	  bin/PostProcess/Extract_fi.o bin/PostProcess/GetTimes_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o

bin/PostProcess/Errors.o:$(BD)/bin/PostProcess/Errors.f90  \
	  bin/PostProcess/Extract_fi.o dll/scip/SCIPtool/inc/SCIPtool.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  localpuf.mod readpuf_inc.mod \
	  getsrcfunc_fi.mod cmd_fi.mod extract_fi.mod totalmass_fi.mod \
	  fft07_fi.mod gettimes_fi.mod