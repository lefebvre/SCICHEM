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
	  bin/runSCI/UpdateRelList_fd.f90 bin/runSCI/UpdateRelList_fi.f90 \
	  bin/runSCI/LoadSen.f90 bin/runSCI/UpdateContRel.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = bin/SCIPUFFdriver bin/runSCI

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


bin/SCIPUFFdriver/AERMODmet_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODmet_fd.f90 

bin/SCIPUFFdriver/AERMODreceptor_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODreceptor_fd.f90 

bin/SCIPUFFdriver/SCIPUFFdriver_fi.o:$(BD)/bin/SCIPUFFdriver/SCIPUFFdriver_fi.f90  \
	  bin/SCIPUFFdriver/AERMODmet_fd.o \
	  bin/SCIPUFFdriver/AERMODreceptor_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o $(INCMOD)

bin/SCIPUFFdriver/GetTimeFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetTimeFromWeather.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/AERMODrel_fd.o:$(BD)/bin/SCIPUFFdriver/AERMODrel_fd.f90 

bin/SCIPUFFdriver/ParseDataME.o:$(BD)/bin/SCIPUFFdriver/ParseDataME.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/GetRelTime.o:$(BD)/bin/SCIPUFFdriver/GetRelTime.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ReadAERMODInput.o:$(BD)/bin/SCIPUFFdriver/ReadAERMODInput.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataRE.o:$(BD)/bin/SCIPUFFdriver/ParseDataRE.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataMA.o:$(BD)/bin/SCIPUFFdriver/ParseDataMA.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/FileInput.o:$(BD)/bin/SCIPUFFdriver/FileInput.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ReadEmissionFile.o:$(BD)/bin/SCIPUFFdriver/ReadEmissionFile.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/InitSCIPUFFProject.o:$(BD)/bin/SCIPUFFdriver/InitSCIPUFFProject.f90  \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataCO.o:$(BD)/bin/SCIPUFFdriver/ParseDataCO.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ComputeDuration.o:$(BD)/bin/SCIPUFFdriver/ComputeDuration.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

bin/SCIPUFFdriver/ParseDataEV.o:$(BD)/bin/SCIPUFFdriver/ParseDataEV.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/GetZrufFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetZrufFromWeather.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/SetTimeZone.o:$(BD)/bin/SCIPUFFdriver/SetTimeZone.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataTE.o:$(BD)/bin/SCIPUFFdriver/ParseDataTE.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ReadArrayLimits.o:$(BD)/bin/SCIPUFFdriver/ReadArrayLimits.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/GenSCIPUFFSensor.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFSensor.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/RunAERMODInp.o:$(BD)/bin/SCIPUFFdriver/RunAERMODInp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/UpdateStackEmission.o:$(BD)/bin/SCIPUFFdriver/UpdateStackEmission.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.o $(INCMOD)

bin/SCIPUFFdriver/GenSCIPUFFWeather.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFWeather.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataOU.o:$(BD)/bin/SCIPUFFdriver/ParseDataOU.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/GenSCIPUFFAreaSource.o:$(BD)/bin/SCIPUFFdriver/GenSCIPUFFAreaSource.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/SetSCIPUFFDomain.o:$(BD)/bin/SCIPUFFdriver/SetSCIPUFFDomain.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/InitAERMOD.o:$(BD)/bin/SCIPUFFdriver/InitAERMOD.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/ParseDataSO.o:$(BD)/bin/SCIPUFFdriver/ParseDataSO.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/SCIPUFFdriver/GetDomRefFromWeather.o:$(BD)/bin/SCIPUFFdriver/GetDomRefFromWeather.f90  \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o $(INCMOD)

bin/runSCI/LoadSen.o:$(BD)/bin/runSCI/LoadSen.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

bin/runSCI/UpdateRelList_fd.o:$(BD)/bin/runSCI/UpdateRelList_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

bin/runSCI/UpdateRelList_fi.o:$(BD)/bin/runSCI/UpdateRelList_fi.f90  \
	  bin/runSCI/UpdateRelList_fd.o

bin/runSCI/runSCI.o:$(BD)/bin/runSCI/runSCI.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o bin/runSCI/LoadSen.o \
	  dll/stub/MPI/localMPI.o lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  dll/scip/SCIPtool/inc/SCIPtool.o \
	  bin/SCIPUFFdriver/SCIPUFFdriver_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.o bin/runSCI/UpdateRelList_fi.o $(INCMOD)

bin/runSCI/UpdateContRel.o:$(BD)/bin/runSCI/UpdateContRel.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.o bin/runSCI/UpdateRelList_fi.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  aermodrel_fd.mod scipuffdriver_fi.mod \
	  aermodmet_fd.mod aermodreceptor_fd.mod prog_inc.mod \
	  updaterellist_fd.mod updaterellist_fi.mod loadsen_fi.mod