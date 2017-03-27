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

ClassData.o:$(BD)/bin/PostProcess/ClassData.f90  Extract_fi.o

CreateFields.o:$(BD)/bin/PostProcess/CreateFields.f90  Extract_fi.o \
	  SCIPtool.o

ExtractHorzLines.o:$(BD)/bin/PostProcess/ExtractHorzLines.f90  Extract_fi.o \
	  SCIPtool.o

GetTimes_fi.o:$(BD)/bin/PostProcess/GetTimes_fi.f90  tooluser_fd.o

LocalPuffs_fi.o:$(BD)/bin/PostProcess/LocalPuffs_fi.f90  GetTimes_fi.o \
	  struct_fd.o multcomp_fd.o tooluser_fd.o

GetSrcFunc_fi.o:$(BD)/bin/PostProcess/GetSrcFunc_fi.f90 

Initialization.o:$(BD)/bin/PostProcess/Initialization.f90  type_fd.o \
	  basic_fd.o contour_fd.o Extract_fi.o GetSrcFunc_fi.o GetTimes_fi.o \
	  LocalPuffs_fi.o SCIPtool.o scipuff_fi.o tooluser_fd.o

GetSrcFunc.o:$(BD)/bin/PostProcess/GetSrcFunc.f90 

PufMom.o:$(BD)/bin/PostProcess/PufMom.f90  LocalPuffs_fi.o SCIPtool.o \
	  scipuff_fi.o struct_fd.o

ExtractContours.o:$(BD)/bin/PostProcess/ExtractContours.f90  Extract_fi.o \
	  tooluser_fd.o write_fd.o

Readpuf.o:$(BD)/bin/PostProcess/Readpuf.f90  chem_fi.o class_fd.o param_fd.o \
	  errorParam_fd.o files_fi.o LocalPuffs_fi.o metparam_fd.o SCIPtool.o \
	  scipuff_fi.o struct_fd.o tooluser_fd.o $(INCMOD)

SelectField.o:$(BD)/bin/PostProcess/SelectField.f90  Extract_fi.o \
	  GetTimes_fi.o

ExtractProfiles.o:$(BD)/bin/PostProcess/ExtractProfiles.f90  Extract_fi.o \
	  SCIPtool.o

ExtractGrid.o:$(BD)/bin/PostProcess/ExtractGrid.f90  Extract_fi.o SCIPtool.o

MainEntryPoint.o:$(BD)/bin/PostProcess/MainEntryPoint.f90  Extract_fi.o \
	  MPIFunc_fi.o SCIPtool.o scipuff_fi.o tooluser_fd.o $(INCMOD)

Output3D.o:$(BD)/bin/PostProcess/Output3D.f90  Extract_fi.o GetTimes_fi.o

Tables.o:$(BD)/bin/PostProcess/Tables.f90 

BubbleMom.o:$(BD)/bin/PostProcess/BubbleMom.f90  LocalPuffs_fi.o SCIPtool.o \
	  scipuff_fi.o struct_fd.o

FFT07.o:$(BD)/bin/PostProcess/FFT07.f90 

sppCallback.o:$(BD)/bin/PostProcess/sppCallback.f90  basic_fd.o \
	  tooluser_fd.o

GetPlotLists.o:$(BD)/bin/PostProcess/GetPlotLists.f90  Extract_fi.o \
	  GetTimes_fi.o param_fd.o SCIPtool.o

ExtractOutput.o:$(BD)/bin/PostProcess/ExtractOutput.f90  type_fd.o \
	  contour_fd.o Extract_fi.o GetTimes_fi.o SCIPtool.o

TotalMass_fi.o:$(BD)/bin/PostProcess/TotalMass_fi.f90  tooluser_fd.o

TotalMass.o:$(BD)/bin/PostProcess/TotalMass.f90  Extract_fi.o GetTimes_fi.o \
	  LocalPuffs_fi.o SCIPtool.o scipuff_fi.o struct_fd.o TotalMass_fi.o

WriteFiles.o:$(BD)/bin/PostProcess/WriteFiles.f90  Extract_fi.o \
	  GetTimes_fi.o SCIPtool.o

GetPuffs.o:$(BD)/bin/PostProcess/GetPuffs.f90  chem_fi.o Extract_fi.o \
	  GetTimes_fi.o LocalPuffs_fi.o struct_fd.o multcomp_fd.o SCIPtool.o \
	  scipuff_fi.o $(INCMOD)

CommandLine.o:$(BD)/bin/PostProcess/CommandLine.f90  Extract_fi.o

FFT07_fi.o:$(BD)/bin/PostProcess/FFT07_fi.f90 

Memory.o:$(BD)/bin/PostProcess/Memory.f90  Extract_fi.o GetTimes_fi.o \
	  SCIPtool.o

RegularGrid.o:$(BD)/bin/PostProcess/RegularGrid.f90  contour_fd.o \
	  Extract_fi.o SCIPtool.o

Contours.o:$(BD)/bin/PostProcess/Contours.f90  Extract_fi.o SCIPtool.o

NativeGrid.o:$(BD)/bin/PostProcess/NativeGrid.f90  Extract_fi.o \
	  GetTimes_fi.o SCIPtool.o

Errors.o:$(BD)/bin/PostProcess/Errors.f90  Extract_fi.o SCIPtool.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  localpuf.mod readpuf_inc.mod \
	  getsrcfunc_fi.mod extract_fi.mod totalmass_fi.mod fft07_fi.mod \
	  gettimes_fi.mod