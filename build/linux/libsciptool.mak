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

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = dll/scip/SCIPtool dll/scip/SCIPtool/inc

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 


dll/scip/SCIPtool/SCIPDelete.o:$(BD)/dll/scip/SCIPtool/SCIPDelete.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

dll/scip/SCIPtool/SCIPAdjointFilter.o:$(BD)/dll/scip/SCIPtool/SCIPAdjointFilter.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

dll/scip/SCIPtool/SCIPPlotList.o:$(BD)/dll/scip/SCIPtool/SCIPPlotList.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

dll/scip/SCIPtool/SCIPTerrain.o:$(BD)/dll/scip/SCIPtool/SCIPTerrain.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o

dll/scip/SCIPtool/SCIPProject.o:$(BD)/dll/scip/SCIPtool/SCIPProject.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

dll/scip/SCIPtool/SCIPVersion.o:$(BD)/dll/scip/SCIPtool/SCIPVersion.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o

dll/scip/SCIPtool/SCIPCheck.o:$(BD)/dll/scip/SCIPtool/SCIPCheck.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o

dll/scip/SCIPtool/SCIPWrite.o:$(BD)/dll/scip/SCIPtool/SCIPWrite.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

dll/scip/SCIPtool/SCIPExitTool.o:$(BD)/dll/scip/SCIPtool/SCIPExitTool.f90 

dll/scip/SCIPtool/SCIPSubstrate.o:$(BD)/dll/scip/SCIPtool/SCIPSubstrate.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o

dll/scip/SCIPtool/SCIPExcept.o:$(BD)/dll/scip/SCIPtool/SCIPExcept.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

dll/scip/SCIPtool/SCIPNewProject.o:$(BD)/dll/scip/SCIPtool/SCIPNewProject.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

dll/scip/SCIPtool/SCIPCount.o:$(BD)/dll/scip/SCIPtool/SCIPCount.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o

dll/scip/SCIPtool/SCIPCurrent.o:$(BD)/dll/scip/SCIPtool/SCIPCurrent.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

dll/scip/SCIPtool/SCIPRestart.o:$(BD)/dll/scip/SCIPtool/SCIPRestart.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

dll/scip/SCIPtool/SCIPError.o:$(BD)/dll/scip/SCIPtool/SCIPError.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o

dll/scip/SCIPtool/SCIPField.o:$(BD)/dll/scip/SCIPtool/SCIPField.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o

dll/scip/SCIPtool/SCIPInitTool.o:$(BD)/dll/scip/SCIPtool/SCIPInitTool.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.o

dll/scip/SCIPtool/SCIPDraw.o:$(BD)/dll/scip/SCIPtool/SCIPDraw.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

dll/scip/SCIPtool/SCIPLoad.o:$(BD)/dll/scip/SCIPtool/SCIPLoad.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o

dll/scip/SCIPtool/SCIPButton.o:$(BD)/dll/scip/SCIPtool/SCIPButton.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

dll/scip/SCIPtool/SCIPDefault.o:$(BD)/dll/scip/SCIPtool/SCIPDefault.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o

dll/scip/SCIPtool/SCIPRun.o:$(BD)/dll/scip/SCIPtool/SCIPRun.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o

dll/scip/SCIPtool/SCIPContour.o:$(BD)/dll/scip/SCIPtool/SCIPContour.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

dll/scip/SCIPtool/SCIPTable.o:$(BD)/dll/scip/SCIPtool/SCIPTable.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o

dll/scip/SCIPtool/SCIPTransform.o:$(BD)/dll/scip/SCIPtool/SCIPTransform.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o

dll/scip/SCIPtool/inc/SCIPtool.o:$(BD)/dll/scip/SCIPtool/inc/SCIPtool.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  scipexception.mod sciptool.mod