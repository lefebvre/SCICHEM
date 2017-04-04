# Main program is runsci

PROG =	runsci

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../runsci_mak.include

SRCS_f90 = bin/SCIPUFFdriver/GetTimeFromWeather.f90 \
	  bin/SCIPUFFdriver/AERMODrel_fd.f90 \
	  bin/SCIPUFFdriver/ParseDataME.f90 bin/SCIPUFFdriver/GetRelTime.f90 \
	  bin/SCIPUFFdriver/ReadAERMODInput.f90 \
	  bin/SCIPUFFdriver/ParseDataRE.f90 \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.f90 \
	  bin/SCIPUFFdriver/ParseDataMA.f90 bin/SCIPUFFdriver/FileInput.f90 \
	  bin/SCIPUFFdriver/ReadEmissionFile.f90 \
	  bin/SCIPUFFdriver/InitSCIPUFFProject.f90 \
	  bin/SCIPUFFdriver/ParseDataCO.f90 \
	  bin/SCIPUFFdriver/AERMODmet_fd.f90 \
	  bin/SCIPUFFdriver/ComputeDuration.f90 \
	  bin/SCIPUFFdriver/ParseDataEV.f90 \
	  bin/SCIPUFFdriver/GetZrufFromWeather.f90 \
	  bin/SCIPUFFdriver/SetTimeZone.f90 bin/SCIPUFFdriver/ParseDataTE.f90 \
	  bin/SCIPUFFdriver/ReadArrayLimits.f90 \
	  bin/SCIPUFFdriver/GenSCIPUFFSensor.f90 \
	  bin/SCIPUFFdriver/AERMODreceptor_fd.f90 \
	  bin/SCIPUFFdriver/RunAERMODInp.f90 \
	  bin/SCIPUFFdriver/UpdateStackEmission.f90 \
	  bin/SCIPUFFdriver/GenSCIPUFFWeather.f90 \
	  bin/SCIPUFFdriver/ParseDataOU.f90 \
	  bin/SCIPUFFdriver/GenSCIPUFFAreaSource.f90 \
	  bin/SCIPUFFdriver/SetSCIPUFFDomain.f90 \
	  bin/SCIPUFFdriver/InitAERMOD.f90 bin/SCIPUFFdriver/ParseDataSO.f90 \
	  bin/SCIPUFFdriver/GetDomRefFromWeather.f90 bin/runSCI/runSCI.f90 \
	  bin/runSCI/LoadSen.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


AERMODmet_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODmet_fd.f90 

AERMODreceptor_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODreceptor_fd.f90 

SCIPUFFdriver_fi.o:$(BD)/bin/SCIPUFFdriver/SCIPUFFdriver_fi.f90  \
	  AERMODmet_fd.o AERMODreceptor_fd.o default_fd.o param_fd.o \
	  tooluser_fd.o $(INCMOD)

GetTimeFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetTimeFromWeather.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

AERMODrel_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODrel_fd.f90 

ParseDataME.o:$(BD)/bin/SCIPUFFdriver/ParseDataME.f90  SCIPUFFdriver_fi.o $(INCMOD)

GetRelTime.o:$(BD)/bin/SCIPUFFdriver/GetRelTime.f90  SCIPUFFdriver_fi.o $(INCMOD)

ReadAERMODInput.o:$(BD)/bin/SCIPUFFdriver/ReadAERMODInput.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

ParseDataRE.o:$(BD)/bin/SCIPUFFdriver/ParseDataRE.f90  SCIPUFFdriver_fi.o $(INCMOD)

ParseDataMA.o:$(BD)/bin/SCIPUFFdriver/ParseDataMA.f90  SCIPUFFdriver_fi.o $(INCMOD)

FileInput.o:$(BD)/bin/SCIPUFFdriver/FileInput.f90  SCIPUFFdriver_fi.o $(INCMOD)

ReadEmissionFile.o:$(BD)/bin/SCIPUFFdriver/ReadEmissionFile.f90  \
	  constants_fd.o SCIPUFFdriver_fi.o $(INCMOD)

InitSCIPUFFProject.o:$(BD)/bin/SCIPUFFdriver/InitSCIPUFFProject.f90  \
	  SCIPtool.o SCIPUFFdriver_fi.o $(INCMOD)

ParseDataCO.o:$(BD)/bin/SCIPUFFdriver/ParseDataCO.f90  SCIPUFFdriver_fi.o $(INCMOD)

ComputeDuration.o:$(BD)/bin/SCIPUFFdriver/ComputeDuration.f90  default_fd.o \
	  param_fd.o tooluser_fd.o

ParseDataEV.o:$(BD)/bin/SCIPUFFdriver/ParseDataEV.f90  SCIPUFFdriver_fi.o $(INCMOD)

GetZrufFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetZrufFromWeather.f90  \
	  default_fd.o SCIPUFFdriver_fi.o $(INCMOD)

SetTimeZone.o:$(BD)/bin/SCIPUFFdriver/SetTimeZone.f90  datums_mod.o \
	  SCIPUFFdriver_fi.o $(INCMOD)

ParseDataTE.o:$(BD)/bin/SCIPUFFdriver/ParseDataTE.f90  SCIPUFFdriver_fi.o $(INCMOD)

ReadArrayLimits.o:$(BD)/bin/SCIPUFFdriver/ReadArrayLimits.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

GenSCIPUFFSensor.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFSensor.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

RunAERMODInp.o:$(BD)/bin/SCIPUFFdriver/RunAERMODInp.f90  sampler_fi.o \
	  SCIPtool.o SCIPUFFdriver_fi.o $(INCMOD)

UpdateStackEmission.o:$(BD)/bin/SCIPUFFdriver/UpdateStackEmission.f90  \
	  constants_fd.o SCIPUFFdriver_fi.o update_fd.o $(INCMOD)

GenSCIPUFFWeather.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFWeather.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

ParseDataOU.o:$(BD)/bin/SCIPUFFdriver/ParseDataOU.f90  SCIPUFFdriver_fi.o $(INCMOD)

GenSCIPUFFAreaSource.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFAreaSource.f90  \
	  constants_fd.o SCIPUFFdriver_fi.o $(INCMOD)

SetSCIPUFFDomain.o:$(BD)/bin/SCIPUFFdriver/SetSCIPUFFDomain.f90  \
	  constants_fd.o SCIPUFFdriver_fi.o $(INCMOD)

InitAERMOD.o:$(BD)/bin/SCIPUFFdriver/InitAERMOD.f90  SCIPUFFdriver_fi.o $(INCMOD)

ParseDataSO.o:$(BD)/bin/SCIPUFFdriver/ParseDataSO.f90  constants_fd.o \
	  SCIPUFFdriver_fi.o $(INCMOD)

GetDomRefFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetDomRefFromWeather.f90  \
	  SCIPUFFdriver_fi.o $(INCMOD)

LoadSen.o:$(BD)/bin/runSCI/LoadSen.f90  default_fd.o files_fi.o \
	  material_fd.o release_fd.o SCIMgr_fd.o

runSCI.o:$(BD)/bin/runSCI/runSCI.f90  basic_fd.o default_fd.o LoadSen.o \
	  localMPI.o MPIFunc_fi.o param_fd.o SCIPtool.o SCIPUFFdriver_fi.o \
	  tooluser_fd.o $(INCMOD)


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  aermodrel_fd.mod scipuffdriver_fi.mod \
	  aermodmet_fd.mod aermodreceptor_fd.mod prog_inc.mod loadsen_fi.mod