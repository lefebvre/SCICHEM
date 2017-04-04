# Main program is libsciptool.${LSuf}

PROG =	libsciptool.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../sciptool_mak.include

SRCS_f90 = dll/scip/SCIPtool/SCIPDelete.f90 \
	  dll/scip/SCIPtool/SCIPAdjointFilter.f90 \
	  dll/scip/SCIPtool/SCIPPlotList.f90 \
	  dll/scip/SCIPtool/SCIPTerrain.f90 dll/scip/SCIPtool/SCIPProject.f90 \
	  dll/scip/SCIPtool/SCIPVersion.f90 dll/scip/SCIPtool/SCIPCheck.f90 \
	  dll/scip/SCIPtool/SCIPWrite.f90 dll/scip/SCIPtool/SCIPExitTool.f90 \
	  dll/scip/SCIPtool/SCIPSubstrate.f90 \
	  dll/scip/SCIPtool/SCIPExcept.f90 \
	  dll/scip/SCIPtool/SCIPNewProject.f90 \
	  dll/scip/SCIPtool/SCIPCount.f90 dll/scip/SCIPtool/SCIPCurrent.f90 \
	  dll/scip/SCIPtool/SCIPRestart.f90 dll/scip/SCIPtool/SCIPError.f90 \
	  dll/scip/SCIPtool/SCIPField.f90 dll/scip/SCIPtool/SCIPInitTool.f90 \
	  dll/scip/SCIPtool/SCIPDraw.f90 dll/scip/SCIPtool/SCIPLoad.f90 \
	  dll/scip/SCIPtool/SCIPButton.f90 dll/scip/SCIPtool/SCIPDefault.f90 \
	  dll/scip/SCIPtool/SCIPRun.f90 dll/scip/SCIPtool/SCIPContour.f90 \
	  dll/scip/SCIPtool/SCIPTable.f90 dll/scip/SCIPtool/SCIPTransform.f90 \
	  dll/scip/SCIPtool/inc/SCIPtool.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


SCIPDelete.o:$(BD)/dll/scip/SCIPtool/SCIPDelete.f90  prjstruct_fd.o

SCIPAdjointFilter.o:$(BD)/dll/scip/SCIPtool/SCIPAdjointFilter.f90  \
	  adjoint_filter_fi.o material_fd.o release_fd.o

SCIPPlotList.o:$(BD)/dll/scip/SCIPtool/SCIPPlotList.f90  charT_fd.o \
	  field_fd.o plotlist_fd.o prjstruct_fd.o

SCIPTerrain.o:$(BD)/dll/scip/SCIPtool/SCIPTerrain.f90  field_fd.o \
	  prjstruct_fd.o spcstruct_fd.o

SCIPProject.o:$(BD)/dll/scip/SCIPtool/SCIPProject.f90  inpstruct_fd.o \
	  prjstruct_fd.o

SCIPVersion.o:$(BD)/dll/scip/SCIPtool/SCIPVersion.f90  charT_fd.o

SCIPCheck.o:$(BD)/dll/scip/SCIPtool/SCIPCheck.f90  domstruct_fd.o \
	  inpstruct_fd.o metstruct_fd.o mtlstruct_fd.o release_fd.o \
	  structure_fd.o timstruct_fd.o

SCIPWrite.o:$(BD)/dll/scip/SCIPtool/SCIPWrite.f90  charT_fd.o \
	  contourlist_fd.o domstruct_fd.o field_fd.o inpstruct_fd.o \
	  metstruct_fd.o mtlstruct_fd.o relstruct_fd.o SCIPresults_fd.o \
	  statstruct_fd.o structure_fd.o timstruct_fd.o tooluser_fd.o

SCIPExitTool.o:$(BD)/dll/scip/SCIPtool/SCIPExitTool.f90 

SCIPSubstrate.o:$(BD)/dll/scip/SCIPtool/SCIPSubstrate.f90  charT_fd.o

SCIPExcept.o:$(BD)/dll/scip/SCIPtool/SCIPExcept.f90  SCIMgr_fd.o

SCIPNewProject.o:$(BD)/dll/scip/SCIPtool/SCIPNewProject.f90  SCIMgr_fd.o

SCIPCount.o:$(BD)/dll/scip/SCIPtool/SCIPCount.f90  charT_fd.o

SCIPCurrent.o:$(BD)/dll/scip/SCIPtool/SCIPCurrent.f90  SCIPresults_fd.o

SCIPRestart.o:$(BD)/dll/scip/SCIPtool/SCIPRestart.f90  structure_fd.o \
	  tooluser_fd.o

SCIPError.o:$(BD)/dll/scip/SCIPtool/SCIPError.f90  message_fd.o

SCIPField.o:$(BD)/dll/scip/SCIPtool/SCIPField.f90  charT_fd.o field_fd.o

SCIPInitTool.o:$(BD)/dll/scip/SCIPtool/SCIPInitTool.f90  basic_fd.o \
	  charT_fd.o limitT_fd.o

SCIPDraw.o:$(BD)/dll/scip/SCIPtool/SCIPDraw.f90  contourlist_fd.o field_fd.o \
	  tooluser_fd.o

SCIPLoad.o:$(BD)/dll/scip/SCIPtool/SCIPLoad.f90  charT_fd.o domstruct_fd.o \
	  inpstruct_fd.o struct_fd.o metstruct_fd.o mtlstruct_fd.o \
	  multcomp_fd.o plotlist_fd.o plotmet_fd.o prjstruct_fd.o \
	  relstruct_fd.o SCIMgr_fd.o SCIPresults_fd.o spcstruct_fd.o \
	  statstruct_fd.o structure_fd.o timstruct_fd.o

SCIPButton.o:$(BD)/dll/scip/SCIPtool/SCIPButton.f90  SCIMgr_fd.o

SCIPDefault.o:$(BD)/dll/scip/SCIPtool/SCIPDefault.f90  domstruct_fd.o \
	  inpstruct_fd.o metstruct_fd.o mtlstruct_fd.o structure_fd.o \
	  timstruct_fd.o

SCIPRun.o:$(BD)/dll/scip/SCIPtool/SCIPRun.f90  timstruct_fd.o

SCIPContour.o:$(BD)/dll/scip/SCIPtool/SCIPContour.f90  contourlist_fd.o \
	  field_fd.o tooluser_fd.o

SCIPTable.o:$(BD)/dll/scip/SCIPtool/SCIPTable.f90  charT_fd.o field_fd.o

SCIPTransform.o:$(BD)/dll/scip/SCIPtool/SCIPTransform.f90  field_fd.o \
	  spcstruct_fd.o

SCIPtool.o:$(BD)/dll/scip/SCIPtool/inc/SCIPtool.f90  basic_fd.o charT_fd.o \
	  domstruct_fd.o field_fd.o inpstruct_fd.o struct_fd.o metstruct_fd.o \
	  mtlstruct_fd.o multcomp_fd.o plotlist_fd.o plotmet_fd.o \
	  prjstruct_fd.o relstruct_fd.o spcstruct_fd.o statstruct_fd.o \
	  structure_fd.o timstruct_fd.o tooluser_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  sciptool.mod