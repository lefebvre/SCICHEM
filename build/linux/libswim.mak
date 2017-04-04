# Main program is libswim.${LSuf}

PROG =	libswim.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../swim_mak.include

SRCS_f90 = dll/SCIPUFF/SWIM/SWIMvertWt.f90 dll/SCIPUFF/SWIM/SWIMsmoothFilter.f90 \
	  dll/SCIPUFF/SWIM/SWIMexpandMet.f90 \
	  dll/SCIPUFF/SWIM/SWIMobsSrfLayer.f90 \
	  dll/SCIPUFF/SWIM/SWIMprofileBL.f90 \
	  dll/SCIPUFF/SWIM/SWIMcombObsField.f90 \
	  dll/SCIPUFF/SWIM/SWIMshearGrad.f90 \
	  dll/SCIPUFF/SWIM/SWIMbuildCliList.f90 \
	  dll/SCIPUFF/SWIM/SWIMlogMessage.f90 dll/SCIPUFF/SWIM/SWIMinitMG.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdateMet.f90 \
	  dll/SCIPUFF/SWIM/SWIMFFTMcWIF.f90 dll/SCIPUFF/SWIM/SWIMMcWIF.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdateBL.f90 \
	  dll/SCIPUFF/SWIM/SWIMFFTMcWIFsub.f90 \
	  dll/SCIPUFF/SWIM/SWIMsetObsWt.f90 \
	  dll/SCIPUFF/SWIM/SWIMstandardAtmos.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitASOS1min.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitObsAssim.f90 \
	  dll/SCIPUFF/SWIM/SWIMcurrentMet.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdateSmoothField.f90 \
	  dll/SCIPUFF/SWIM/SWIMreadWRF.f90 \
	  dll/SCIPUFF/SWIM/SWIMinterpGridded.f90 \
	  dll/SCIPUFF/SWIM/SWIMgetSunFac.f90 \
	  dll/SCIPUFF/SWIM/SWIMreadMEDOC.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitObsField.f90 \
	  dll/SCIPUFF/SWIM/SWIMsimpleBL.f90 dll/SCIPUFF/SWIM/SWIMoutput.f90 \
	  dll/SCIPUFF/SWIM/SWIMpuffTopog.f90 \
	  dll/SCIPUFF/SWIM/SWIMinterpObs.f90 dll/SCIPUFF/SWIM/SWIMinit.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitWRF.f90 \
	  dll/SCIPUFF/SWIM/SWIMread3DClimo.f90 \
	  dll/SCIPUFF/SWIM/SWIMreadTerrainNest.f90 \
	  dll/SCIPUFF/SWIM/SWIMutilMap.f90 \
	  dll/SCIPUFF/SWIM/SWIMinterpNest.f90 \
	  dll/SCIPUFF/SWIM/SWIMnextField.f90 \
	  dll/SCIPUFF/SWIM/SWIMgetTopog.f90 dll/SCIPUFF/SWIM/SWIMinitObs.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitSCIP.f90 dll/SCIPUFF/SWIM/SWIMnest.f90 \
	  dll/SCIPUFF/SWIM/SWIMmessage.f90 \
	  dll/SCIPUFF/SWIM/SWIMcheckTerrainHeader.f90 \
	  dll/SCIPUFF/SWIM/SWIMutilBL.f90 \
	  dll/SCIPUFF/SWIM/SWIMbuildObsList.f90 \
	  dll/SCIPUFF/SWIM/SWIMoutputMEDOC.f90 \
	  dll/SCIPUFF/SWIM/SWIMinterpTime.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitAERpfl.f90 \
	  dll/SCIPUFF/SWIM/SWIMobsVertScale.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitMEDOC.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdateObsField.f90 \
	  dll/SCIPUFF/SWIM/SWIMbuildFixedObs.f90 \
	  dll/SCIPUFF/SWIM/SWIMallocField.f90 \
	  dll/SCIPUFF/SWIM/SWIMgetGamma.f90 dll/SCIPUFF/SWIM/SWIMreadSCIP.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitAERsfc.f90 \
	  dll/SCIPUFF/SWIM/SWIMreadTerrain.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdatePolarField.f90 \
	  dll/SCIPUFF/SWIM/SWIMreportError.f90 dll/SCIPUFF/SWIM/SWIMhalt.f90 \
	  dll/SCIPUFF/SWIM/SWIMreallocFields.f90 \
	  dll/SCIPUFF/SWIM/SWIMutilInterface.f90 \
	  dll/SCIPUFF/SWIM/SWIMaddNest.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitMEDOClist.f90 \
	  dll/SCIPUFF/SWIM/SWIMCurrentSrfMet.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitGridded.f90 \
	  dll/SCIPUFF/SWIM/SWIMcalcBL.f90 \
	  dll/SCIPUFF/SWIM/SWIMturbProfile.f90 \
	  dll/SCIPUFF/SWIM/SWIMobsSort.f90 dll/SCIPUFF/SWIM/SWIMinitRun.f90 \
	  dll/SCIPUFF/SWIM/SWIMbuildObsGrid.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitLSV.f90 \
	  dll/SCIPUFF/SWIM/SWIMsetGridInterp.f90 \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.f90 \
	  dll/SCIPUFF/SWIM/SWIMupdateGriddedMet.f90 \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.f90 \
	  dll/SCIPUFF/SWIM/SWIMMcWIFmod.f90 dll/SCIPUFF/SWIM/SWIMversion.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitFixed.f90 \
	  dll/SCIPUFF/SWIM/SWIMrelaxMcWIF.f90 \
	  dll/SCIPUFF/SWIM/SWIMsetPrecipProb.f90 \
	  dll/SCIPUFF/SWIM/SWIMaddSmoothField.f90 \
	  dll/SCIPUFF/SWIM/SWIMexit.f90 dll/SCIPUFF/SWIM/SWIMutil.f90 \
	  dll/SCIPUFF/SWIM/SWIMinitMcWIF.f90 \
	  dll/SCIPUFF/SWIM/SWIMaddPolarField.f90 \
	  dll/SCIPUFF/SWIM/inc/logMessage_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/Zsort.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.f90 dll/SCIPUFF/SWIM/inc/SWIM_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.f90 \
	  dll/util/ClimoReader/netcdf_fd.f90 dll/SCIPUFF/uwm/SmoothFilter.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = dll/SCIPUFF/SWIM dll/SCIPUFF/SWIM/inc dll/util/ClimoReader dll/SCIPUFF/uwm

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< $(INCDIR) 


dll/SCIPUFF/SWIM/SWIMvertWt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMvertWt.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

dll/SCIPUFF/SWIM/SWIMsmoothFilter.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsmoothFilter.f90 

dll/SCIPUFF/SWIM/inc/logMessage_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/logMessage_fd.f90 

dll/util/ClimoReader/netcdf_fd.o:$(BD)/dll/util/ClimoReader/netcdf_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.f90 

dll/SCIPUFF/SWIM/inc/SWIM_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIM_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o

dll/SCIPUFF/SWIM/inc/SWIM_fi.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIM_fi.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/SWIM/inc/logMessage_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  dll/util/ClimoReader/netcdf_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMexpandMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMexpandMet.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMobsSrfLayer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsSrfLayer.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMInterpPointer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMInterpPointer.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

dll/SCIPUFF/SWIM/SWIMprofileBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMprofileBL.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o

dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilArrayPointer.f90 

dll/SCIPUFF/SWIM/SWIMcombObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcombObsField.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMshearGrad.o:$(BD)/dll/SCIPUFF/SWIM/SWIMshearGrad.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMbuildCliList.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildCliList.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMlogMessage.o:$(BD)/dll/SCIPUFF/SWIM/SWIMlogMessage.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/uwm/SmoothFilter.o:$(BD)/dll/SCIPUFF/uwm/SmoothFilter.f90 

dll/SCIPUFF/SWIM/SWIMinitMG.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMG.f90  \
	  dll/SCIPUFF/uwm/SmoothFilter.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdateMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateMet.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMFFTMcWIFsub.o:$(BD)/dll/SCIPUFF/SWIM/SWIMFFTMcWIFsub.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMMcWIFmod.o:$(BD)/dll/SCIPUFF/SWIM/SWIMMcWIFmod.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMFFTMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMFFTMcWIF.f90  \
	  dll/SCIPUFF/SWIM/SWIMFFTMcWIFsub.o dll/SCIPUFF/SWIM/SWIMMcWIFmod.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMMcWIF.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/SWIMMcWIFmod.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdateBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateBL.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMobsSort.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsSort.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMsetObsWt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetObsWt.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMobsSort.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMvertWt.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMstandardAtmos.o:$(BD)/dll/SCIPUFF/SWIM/SWIMstandardAtmos.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitASOS1min.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitASOS1min.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitObsAssim.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObsAssim.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o

dll/SCIPUFF/SWIM/SWIMgetTopog.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetTopog.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

dll/SCIPUFF/SWIM/SWIMcurrentMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcurrentMet.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/SWIMgetTopog.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdateSmoothField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateSmoothField.f90  \
	  dll/SCIPUFF/LandUse/landuse_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMreadWRF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadWRF.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinterpGridded.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpGridded.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMgetSunFac.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetSunFac.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.f90 

dll/SCIPUFF/SWIM/SWIMreadMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadMEDOC.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/util/ARAPlib/reallocate.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.f90 

dll/SCIPUFF/SWIM/SWIMinitObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObsField.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMsimpleBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsimpleBL.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMoutput.o:$(BD)/dll/SCIPUFF/SWIM/SWIMoutput.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMpuffTopog.o:$(BD)/dll/SCIPUFF/SWIM/SWIMpuffTopog.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/SWIMgetTopog.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinterpObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpObs.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMobsSort.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinit.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinit.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitWRF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitWRF.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  dll/util/ClimoReader/netcdf_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMread3DClimo.o:$(BD)/dll/SCIPUFF/SWIM/SWIMread3DClimo.f90 

dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.f90 

dll/SCIPUFF/SWIM/SWIMreadTerrain.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadTerrain.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMreadTerrainNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadTerrainNest.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMreadTerrain.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMutilMap.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilMap.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinterpNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpNest.f90  \
	  lib/util/ARAPlib/reallocate.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/SWIMinterpGridded.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMnextField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMnextField.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObs.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitSCIP.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitSCIP.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMnest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMnest.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMmessage.o:$(BD)/dll/SCIPUFF/SWIM/SWIMmessage.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMcheckTerrainHeader.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcheckTerrainHeader.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMutilBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilBL.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMbuildObsList.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildObsList.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/util/ARAPlib/reallocate.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMoutputMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMoutputMEDOC.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinterpTime.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpTime.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitAERpfl.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitAERpfl.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMobsVertScale.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsVertScale.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/SWIMobsSort.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMEDOC.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdateObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateObsField.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMbuildFixedObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildFixedObs.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMallocField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMallocField.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMgetGamma.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetGamma.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMreadSCIP.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadSCIP.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitAERsfc.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitAERsfc.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdatePolarField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdatePolarField.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMinterpGridded.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMreportError.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreportError.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMhalt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMhalt.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMreallocFields.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreallocFields.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMutilInterface.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilInterface.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMaddNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddNest.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitMEDOClist.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMEDOClist.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMCurrentSrfMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMCurrentSrfMet.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitGridded.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitGridded.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMcalcBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcalcBL.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.o \
	  dll/SCIPUFF/SWIM/SWIMInterpPointer.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMturbProfile.o:$(BD)/dll/SCIPUFF/SWIM/SWIMturbProfile.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitRun.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitRun.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/inc/Zsort.o:$(BD)/dll/SCIPUFF/SWIM/inc/Zsort.f90 

dll/SCIPUFF/SWIM/SWIMbuildObsGrid.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildObsGrid.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.o dll/SCIPUFF/SWIM/inc/Zsort.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitLSV.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitLSV.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMsetGridInterp.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetGridInterp.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMInterpPointer.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMupdateGriddedMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateGriddedMet.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/SWIMinterpGridded.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMversion.o:$(BD)/dll/SCIPUFF/SWIM/SWIMversion.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

dll/SCIPUFF/SWIM/SWIMinitFixed.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitFixed.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMrelaxMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMrelaxMcWIF.f90  \
	  dll/SCIPUFF/SWIM/SWIMMcWIFmod.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMsetPrecipProb.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetPrecipProb.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMaddSmoothField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddSmoothField.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/SWIMsmoothFilter.o dll/SCIPUFF/SWIM/inc/SWIM_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/SWIMutilArrayPointer.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMexit.o:$(BD)/dll/SCIPUFF/SWIM/SWIMexit.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMutil.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutil.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIM_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMinitMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMcWIF.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIM_fd.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

dll/SCIPUFF/SWIM/SWIMaddPolarField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddPolarField.f90  \
	  dll/SCIPUFF/SWIM/inc/SWIM_fi.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  swimvertwt.mod filterdefinition.mod \
	  fftmod.mod stndatmos_fi.mod swimintrpgrid_fi.mod \
	  swimgettopog_fi.mod molfunc_fi.mod terhead_fd.mod swimobssort.mod \
	  swiminterppointer.mod swimutilarrayptr.mod mcwifintrf.mod \
	  mcwifsubintrf.mod mgfield_fd.mod precipconstants_fd.mod \
	  logmessage_fd.mod vertgrid_fd.mod zsort.mod char8array_fd.mod \
	  gridcoord_fd.mod swimobsinterp_fd.mod swim_fi.mod swimerr_fd.mod \
	  swimmetfield_fd.mod swimobs_fd.mod swimrect_fd.mod \
	  swiminterp_fd.mod swimcurrentmet_fi.mod netcdf_fd.mod \
	  filterdefinitionmg.mod