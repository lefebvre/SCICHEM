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

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< $(INCDIR) 


SWIMvertWt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMvertWt.f90  SWIMparam_fd.o

SWIMsmoothFilter.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsmoothFilter.f90 

logMessage_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/logMessage_fd.f90 

netcdf_fd.o:$(BD)/dll/util/ClimoReader/netcdf_fd.f90  basic_fd.o param_fd.o $(INCMOD)

SWIMinterp_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMinterp_fd.f90 

SWIM_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIM_fd.f90  param_fd.o MapCoord_fd.o \
	  SWIMinterp_fd.o

SWIM_fi.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIM_fi.f90  basic_fd.o default_fd.o \
	  logMessage_fd.o MapCoord_fd.o message_fd.o netcdf_fd.o SWIM_fd.o \
	  SWIMinit_fd.o uniformGridT_fd.o $(INCMOD)

SWIMexpandMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMexpandMet.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMobsSrfLayer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsSrfLayer.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMInterpPointer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMInterpPointer.f90  \
	  SWIMinterp_fd.o SWIM_fd.o SWIMparam_fd.o

SWIMprofileBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMprofileBL.f90  constants_fd.o \
	  SWIM_fi.o SWIMInterpPointer.o SWIMparam_fd.o $(INCMOD)

SWIMobsInterp_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMobsInterp_fd.f90  \
	  SWIM_fd.o

SWIMutilArrayPointer.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilArrayPointer.f90 

SWIMcombObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcombObsField.f90  \
	  constants_fd.o SWIM_fi.o SWIMobsInterp_fd.o SWIMparam_fd.o \
	  SWIMutilArrayPointer.o $(INCMOD)

SWIMshearGrad.o:$(BD)/dll/SCIPUFF/SWIM/SWIMshearGrad.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMbuildCliList.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildCliList.f90  \
	  constants_fd.o SWIM_fi.o SWIMinit_fd.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMlogMessage.o:$(BD)/dll/SCIPUFF/SWIM/SWIMlogMessage.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SmoothFilter.o:$(BD)/dll/SCIPUFF/uwm/SmoothFilter.f90 

SWIMinitMG.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMG.f90  SmoothFilter.o SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMupdateMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateMet.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMFFTMcWIFsub.o:$(BD)/dll/SCIPUFF/SWIM/SWIMFFTMcWIFsub.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMMcWIFmod.o:$(BD)/dll/SCIPUFF/SWIM/SWIMMcWIFmod.f90  SWIM_fi.o SWIM_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMFFTMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMFFTMcWIF.f90  SWIMFFTMcWIFsub.o \
	  SWIMMcWIFmod.o SWIM_fi.o SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMMcWIF.f90  constants_fd.o \
	  SWIMMcWIFmod.o SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMupdateBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateBL.f90  constants_fd.o \
	  SWIM_fi.o SWIMinterp_fd.o SWIMInterpPointer.o SWIM_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMobsSort.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsSort.f90  SWIM_fi.o SWIM_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMsetObsWt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetObsWt.f90  constants_fd.o \
	  SWIM_fi.o SWIM_fd.o SWIMobsInterp_fd.o SWIMobsSort.o SWIMparam_fd.o \
	  SWIMvertWt.o $(INCMOD)

SWIMstandardAtmos.o:$(BD)/dll/SCIPUFF/SWIM/SWIMstandardAtmos.f90  \
	  constants_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinitASOS1min.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitASOS1min.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMinitObsAssim.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObsAssim.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMcurrentMet_fi.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMcurrentMet_fi.f90  \
	  SWIMinterp_fd.o

SWIMgetTopog.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetTopog.f90  SWIMinterp_fd.o \
	  SWIMInterpPointer.o SWIM_fd.o SWIMparam_fd.o

SWIMcurrentMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcurrentMet.f90  constants_fd.o \
	  coordinate_fd.o MapCoord_fd.o SWIM_fi.o SWIMcurrentMet_fi.o \
	  SWIMgetTopog.o SWIMinterp_fd.o SWIMInterpPointer.o SWIM_fd.o \
	  SWIMparam_fd.o SWIMpuff_fd.o $(INCMOD)

SWIMupdateSmoothField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateSmoothField.f90  \
	  landuse_fd.o SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMreadWRF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadWRF.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMinterpGridded.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpGridded.f90  \
	  constants_fd.o SWIM_fi.o SWIMinterp_fd.o SWIMInterpPointer.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMgetSunFac.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetSunFac.f90  SWIM_fi.o \
	  SWIMcurrentMet_fi.o SWIMinterp_fd.o SWIMInterpPointer.o \
	  SWIMparam_fd.o SWIMpuff_fd.o $(INCMOD)

SWIMchar8Array_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMchar8Array_fd.f90 

SWIMreadMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadMEDOC.f90  \
	  SWIMchar8Array_fd.o constants_fd.o reallocate.o SWIM_fi.o \
	  SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMvertGrid_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMvertGrid_fd.f90 

SWIMinitObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObsField.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIMparam_fd.o SWIMvertGrid_fd.o $(INCMOD)

SWIMsimpleBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsimpleBL.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMoutput.o:$(BD)/dll/SCIPUFF/SWIM/SWIMoutput.f90  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMpuffTopog.o:$(BD)/dll/SCIPUFF/SWIM/SWIMpuffTopog.f90  SWIM_fi.o \
	  SWIMcurrentMet_fi.o SWIMgetTopog.o SWIMparam_fd.o SWIMpuff_fd.o $(INCMOD)

SWIMinterpObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpObs.f90  constants_fd.o \
	  SWIM_fi.o SWIMInterpPointer.o SWIM_fd.o SWIMobsInterp_fd.o \
	  SWIMobsSort.o SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMinit.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinit.f90  charT_fd.o SCIMgrParam_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinitWRF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitWRF.f90  basic_fd.o \
	  constants_fd.o param_fd.o SWIM_fi.o SWIMparam_fd.o \
	  SWIMutilArrayPointer.o $(INCMOD)

SWIMread3DClimo.o:$(BD)/dll/SCIPUFF/SWIM/SWIMread3DClimo.f90 

SWIMgridCoord_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMgridCoord_fd.f90 

SWIMreadTerrain.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadTerrain.f90  datums_mod.o \
	  default_fd.o SWIMgridCoord_fd.o landuse_fd.o prjstruct_fd.o \
	  SWIM_fi.o SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMreadTerrainNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadTerrainNest.f90  \
	  SWIM_fi.o SWIMinit_fd.o SWIMparam_fd.o SWIMreadTerrain.o $(INCMOD)

SWIMutilMap.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilMap.f90  constants_fd.o \
	  datums_mod.o domain_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinterpNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpNest.f90  reallocate.o \
	  SWIM_fi.o SWIMInterpPointer.o SWIMinterpGridded.o SWIM_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMnextField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMnextField.f90  message_fd.o \
	  SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMinitObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitObs.f90  constants_fd.o \
	  default_fd.o SWIM_fi.o SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMinitSCIP.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitSCIP.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMnest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMnest.f90  SWIM_fi.o SWIMparam_fd.o \
	  uniformGridT_fd.o $(INCMOD)

SWIMmessage.o:$(BD)/dll/SCIPUFF/SWIM/SWIMmessage.f90  basic_fd.o \
	  SCIPresults_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMcheckTerrainHeader.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcheckTerrainHeader.f90  \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMutilBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilBL.f90  checkerr_fd.o \
	  constants_fd.o default_fd.o landuse_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMbuildObsList.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildObsList.f90  \
	  constants_fd.o default_fd.o reallocate.o SWIM_fi.o SWIMinit_fd.o \
	  SWIMInterpPointer.o SWIM_fd.o SWIMobsInterp_fd.o SWIMparam_fd.o \
	  SWIMpuff_fd.o $(INCMOD)

SWIMoutputMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMoutputMEDOC.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinterpTime.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinterpTime.f90  SWIM_fi.o \
	  SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMinitAERpfl.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitAERpfl.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMobsVertScale.o:$(BD)/dll/SCIPUFF/SWIM/SWIMobsVertScale.f90  SWIM_fd.o \
	  SWIMobsSort.o $(INCMOD)

SWIMinitMEDOC.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMEDOC.f90  \
	  SWIMchar8Array_fd.o constants_fd.o datums_mod.o SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMupdateObsField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateObsField.f90  \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMbuildFixedObs.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildFixedObs.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMallocField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMallocField.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMgetGamma.o:$(BD)/dll/SCIPUFF/SWIM/SWIMgetGamma.f90  SWIM_fi.o \
	  SWIMcurrentMet_fi.o SWIMinterp_fd.o SWIMInterpPointer.o SWIM_fd.o \
	  SWIMparam_fd.o SWIMpuff_fd.o $(INCMOD)

SWIMreadSCIP.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreadSCIP.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinitAERsfc.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitAERsfc.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMupdatePolarField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdatePolarField.f90  \
	  constants_fd.o SWIM_fi.o SWIMinterpGridded.o SWIMparam_fd.o $(INCMOD)

SWIMreportError.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreportError.f90  message_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMhalt.o:$(BD)/dll/SCIPUFF/SWIM/SWIMhalt.f90  basic_fd.o message_fd.o \
	  SCIPresults_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMreallocFields.o:$(BD)/dll/SCIPUFF/SWIM/SWIMreallocFields.f90  \
	  constants_fd.o SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMutilInterface.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutilInterface.f90  \
	  constants_fd.o default_fd.o SWIM_fi.o SWIMcurrentMet_fi.o \
	  SWIMgridStr_fd.o SWIMInterpPointer.o SWIMparam_fd.o SWIMpuff_fd.o $(INCMOD)

SWIMaddNest.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddNest.f90  constants_fd.o \
	  prjstruct_fd.o SWIM_fi.o SWIMInterpPointer.o SWIMparam_fd.o \
	  uniformGridT_fd.o $(INCMOD)

SWIMinitMEDOClist.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMEDOClist.f90  \
	  constants_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMCurrentSrfMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMCurrentSrfMet.f90  \
	  constants_fd.o SWIM_fi.o SWIMcurrentMet_fi.o SWIMparam_fd.o \
	  SWIMpuff_fd.o $(INCMOD)

SWIMinitGridded.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitGridded.f90  constants_fd.o \
	  SWIMgridCoord_fd.o landuse_fd.o SWIM_fi.o SWIMinit_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMcalcBL.o:$(BD)/dll/SCIPUFF/SWIM/SWIMcalcBL.f90  constants_fd.o SWIM_fi.o \
	  SWIMinterp_fd.o SWIMInterpPointer.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMturbProfile.o:$(BD)/dll/SCIPUFF/SWIM/SWIMturbProfile.f90  constants_fd.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMinitRun.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitRun.f90  constants_fd.o \
	  SWIM_fi.o SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

Zsort.o:$(BD)/dll/SCIPUFF/SWIM/inc/Zsort.f90 

SWIMbuildObsGrid.o:$(BD)/dll/SCIPUFF/SWIM/SWIMbuildObsGrid.f90  SWIM_fi.o \
	  SWIMparam_fd.o SWIMvertGrid_fd.o Zsort.o $(INCMOD)

SWIMinitLSV.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitLSV.f90  constants_fd.o \
	  SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMsetGridInterp.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetGridInterp.f90  SWIM_fi.o \
	  SWIMInterpPointer.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMupdateGriddedMet.o:$(BD)/dll/SCIPUFF/SWIM/SWIMupdateGriddedMet.f90  \
	  SWIM_fi.o SWIMinterpGridded.o SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMversion.o:$(BD)/dll/SCIPUFF/SWIM/SWIMversion.f90  param_fd.o \
	  SWIMparam_fd.o

SWIMinitFixed.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitFixed.f90  SWIM_fi.o \
	  SWIMinit_fd.o SWIMparam_fd.o $(INCMOD)

SWIMrelaxMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMrelaxMcWIF.f90  SWIMMcWIFmod.o \
	  SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMsetPrecipProb.o:$(BD)/dll/SCIPUFF/SWIM/SWIMsetPrecipProb.f90  \
	  constants_fd.o SWIM_fi.o SWIMparam_fd.o $(INCMOD)

SWIMaddSmoothField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddSmoothField.f90  \
	  constants_fd.o SWIMsmoothFilter.o SWIM_fi.o SWIM_fd.o \
	  SWIMparam_fd.o SWIMutilArrayPointer.o $(INCMOD)

SWIMexit.o:$(BD)/dll/SCIPUFF/SWIM/SWIMexit.f90  SWIM_fi.o SWIM_fd.o \
	  SWIMparam_fd.o $(INCMOD)

SWIMutil.o:$(BD)/dll/SCIPUFF/SWIM/SWIMutil.f90  constants_fd.o datums_mod.o \
	  default_fd.o SWIMgridCoord_fd.o SWIM_fi.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMinitMcWIF.o:$(BD)/dll/SCIPUFF/SWIM/SWIMinitMcWIF.f90  constants_fd.o \
	  SWIM_fi.o SWIMinit_fd.o SWIM_fd.o SWIMparam_fd.o $(INCMOD)

SWIMaddPolarField.o:$(BD)/dll/SCIPUFF/SWIM/SWIMaddPolarField.f90  SWIM_fi.o \
	  SWIMparam_fd.o $(INCMOD)


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