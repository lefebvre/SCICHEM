# Main program is libscipuff.a

PROG =	libscipuff.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/SCIPUFFlib/FileMgr/load_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/read_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/load_msc.f90 \
	  lib/SCIPUFFlib/FileMgr/load_ctrl.f90 \
	  lib/SCIPUFFlib/FileMgr/load_end.f90 \
	  lib/SCIPUFFlib/FileMgr/util_mgr.f90 \
	  lib/SCIPUFFlib/FileMgr/read_mtl.f90 \
	  lib/SCIPUFFlib/FileMgr/read_ctrl.f90 \
	  lib/SCIPUFFlib/FileMgr/load_options.f90 \
	  lib/SCIPUFFlib/FileMgr/read_end.f90 \
	  lib/SCIPUFFlib/FileMgr/read_matdef.f90 \
	  lib/SCIPUFFlib/FileMgr/load_flags.f90 \
	  lib/SCIPUFFlib/FileMgr/util_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/read_met.f90 \
	  lib/SCIPUFFlib/FileMgr/read_options.f90 \
	  lib/SCIPUFFlib/FileMgr/read_msc.f90 \
	  lib/SCIPUFFlib/FileMgr/util_mtl.f90 \
	  lib/SCIPUFFlib/FileMgr/load_domain.f90 \
	  lib/SCIPUFFlib/FileMgr/load_start.f90 \
	  lib/SCIPUFFlib/FileMgr/load_mtl.f90 \
	  lib/SCIPUFFlib/FileMgr/read_scn.f90 \
	  lib/SCIPUFFlib/FileMgr/init_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/read_flags.f90 \
	  lib/SCIPUFFlib/FileMgr/read_domain.f90 \
	  lib/SCIPUFFlib/FileMgr/read_start.f90 \
	  lib/SCIPUFFlib/FileMgr/util_msc.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/options_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/matdef_fi.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/version_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/terrain_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/status_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/flags_fd.f90 \
	  lib/SCIPUFFlib/FileMgr/inc/weather_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/slicev.f90 \
	  lib/SCIPUFFlib/PlotMgr/adjoint.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeAVS.f90 \
	  lib/SCIPUFFlib/PlotMgr/slice.f90 lib/SCIPUFFlib/PlotMgr/sliceh.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeEIS.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeOIL.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeCTS.f90 \
	  lib/SCIPUFFlib/PlotMgr/plotcopy.f90 \
	  lib/SCIPUFFlib/PlotMgr/readProject.f90 \
	  lib/SCIPUFFlib/PlotMgr/poparea.f90 \
	  lib/SCIPUFFlib/PlotMgr/plotchem.f90 \
	  lib/SCIPUFFlib/PlotMgr/getpoint.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeOVL.f90 \
	  lib/SCIPUFFlib/PlotMgr/clipnorm.f90 \
	  lib/SCIPUFFlib/PlotMgr/slicevint.f90 \
	  lib/SCIPUFFlib/PlotMgr/metplot.f90 \
	  lib/SCIPUFFlib/PlotMgr/writeUSA.f90 \
	  lib/SCIPUFFlib/PlotMgr/plotlist.f90 \
	  lib/SCIPUFFlib/PlotMgr/plotread.f90 \
	  lib/SCIPUFFlib/PlotMgr/plotmet_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/adjoint_util.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/pointval_fi.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/type_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fd.f90 \
	  lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/newprj.f90 lib/SCIPUFFlib/SCIMgr/checkinp.f90 \
	  lib/SCIPUFFlib/SCIMgr/state.f90 lib/SCIPUFFlib/SCIMgr/relcheck.f90 \
	  lib/SCIPUFFlib/SCIMgr/toolversion.f90 \
	  lib/SCIPUFFlib/SCIMgr/exittool.f90 lib/SCIPUFFlib/SCIMgr/abort.f90 \
	  lib/SCIPUFFlib/SCIMgr/runprj.f90 lib/SCIPUFFlib/SCIMgr/loadter.f90 \
	  lib/SCIPUFFlib/SCIMgr/drawfield.f90 \
	  lib/SCIPUFFlib/SCIMgr/writeinp.f90 lib/SCIPUFFlib/SCIMgr/input.f90 \
	  lib/SCIPUFFlib/SCIMgr/toolerror.f90 \
	  lib/SCIPUFFlib/SCIMgr/exitsimple.f90 \
	  lib/SCIPUFFlib/SCIMgr/defaultinp.f90 \
	  lib/SCIPUFFlib/SCIMgr/toolstart.f90 \
	  lib/SCIPUFFlib/SCIMgr/loadprj.f90 \
	  lib/SCIPUFFlib/SCIMgr/toolutil.f90 \
	  lib/SCIPUFFlib/SCIMgr/loadmet.f90 lib/SCIPUFFlib/SCIMgr/release.f90 \
	  lib/SCIPUFFlib/SCIMgr/checkutil.f90 \
	  lib/SCIPUFFlib/SCIMgr/current.f90 \
	  lib/SCIPUFFlib/SCIMgr/substrate.f90 \
	  lib/SCIPUFFlib/SCIMgr/convert.f90 lib/SCIPUFFlib/SCIMgr/loadpuf.f90 \
	  lib/SCIPUFFlib/SCIMgr/domain.f90 \
	  lib/SCIPUFFlib/SCIMgr/runscipuff.f90 \
	  lib/SCIPUFFlib/SCIMgr/count.f90 lib/SCIPUFFlib/SCIMgr/flags.f90 \
	  lib/SCIPUFFlib/SCIMgr/callback.f90 lib/SCIPUFFlib/SCIMgr/except.f90 \
	  lib/SCIPUFFlib/SCIMgr/weather.f90 \
	  lib/SCIPUFFlib/SCIMgr/plotfield.f90 \
	  lib/SCIPUFFlib/SCIMgr/initsimple.f90 \
	  lib/SCIPUFFlib/SCIMgr/loadinp.f90 lib/SCIPUFFlib/SCIMgr/rstprj.f90 \
	  lib/SCIPUFFlib/SCIMgr/toolrun.f90 lib/SCIPUFFlib/SCIMgr/check.f90 \
	  lib/SCIPUFFlib/SCIMgr/plottable.f90 \
	  lib/SCIPUFFlib/SCIMgr/material.f90 \
	  lib/SCIPUFFlib/SCIMgr/transform.f90 \
	  lib/SCIPUFFlib/SCIMgr/delete.f90 lib/SCIPUFFlib/SCIMgr/end.f90 \
	  lib/SCIPUFFlib/SCIMgr/contour.f90 lib/SCIPUFFlib/SCIMgr/utm.f90 \
	  lib/SCIPUFFlib/SCIMgr/button.f90 lib/SCIPUFFlib/SCIMgr/time.f90 \
	  lib/SCIPUFFlib/SCIMgr/plotinit.f90 \
	  lib/SCIPUFFlib/SCIMgr/fileutil.f90 lib/SCIPUFFlib/SCIMgr/update.f90 \
	  lib/SCIPUFFlib/SCIMgr/writefield.f90 \
	  lib/SCIPUFFlib/SCIMgr/drawgrid.f90 \
	  lib/SCIPUFFlib/SCIMgr/sendmsg.f90 \
	  lib/SCIPUFFlib/SCIMgr/adjoint_filter.f90 \
	  lib/SCIPUFFlib/SCIMgr/time_util.f90 \
	  lib/SCIPUFFlib/SCIMgr/checkerr.f90 \
	  lib/SCIPUFFlib/SCIMgr/options.f90 \
	  lib/SCIPUFFlib/SCIMgr/inittool.f90 \
	  lib/SCIPUFFlib/SCIMgr/relcheck_fi.f90 \
	  lib/SCIPUFFlib/SCIMgr/ctrl.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/mss_error.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/environment_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/mss_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/state_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/udm_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/sensor_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fi.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.f90 \
	  lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_ip.f90 lib/SCIPUFFlib/SCIPUFF/split.f90 \
	  lib/SCIPUFFlib/SCIPUFF/dezone.f90 \
	  lib/SCIPUFFlib/SCIPUFF/puff_rel_dyn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/start.f90 lib/SCIPUFFlib/SCIPUFF/drydep.f90 \
	  lib/SCIPUFFlib/SCIPUFF/run.f90 \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_dyn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/chemReactions.f90 \
	  lib/SCIPUFFlib/SCIPUFF/set_tlev.f90 \
	  lib/SCIPUFFlib/SCIPUFF/srf_evap.f90 \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SamplerBinaryOut.f90 \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_static.f90 \
	  lib/SCIPUFFlib/SCIPUFF/get_topogSTUB.f90 \
	  lib/SCIPUFFlib/SCIPUFF/get_topSWIM.f90 \
	  lib/SCIPUFFlib/SCIPUFF/output.f90 \
	  lib/SCIPUFFlib/SCIPUFF/scipuff.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_met.f90 \
	  lib/SCIPUFFlib/SCIPUFF/step_pool.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SamplerInput.f90 \
	  lib/SCIPUFFlib/SCIPUFF/error.f90 lib/SCIPUFFlib/SCIPUFF/reflect.f90 \
	  lib/SCIPUFFlib/SCIPUFF/rebin.f90 \
	  lib/SCIPUFFlib/SCIPUFF/set_grid.f90 \
	  lib/SCIPUFFlib/SCIPUFF/ufall.f90 \
	  lib/SCIPUFFlib/SCIPUFF/time_cnv.f90 \
	  lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prise.f90 \
	  lib/SCIPUFFlib/SCIPUFF/MPIFunc.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_evap.f90 \
	  lib/SCIPUFFlib/SCIPUFF/get_metSWIM.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SetBotCells.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.f90 \
	  lib/SCIPUFFlib/SCIPUFF/vd_slinn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_matl.f90 \
	  lib/SCIPUFFlib/SCIPUFF/utilchem.f90 \
	  lib/SCIPUFFlib/SCIPUFF/init_srf.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SamplerInputUtil.f90 \
	  lib/SCIPUFFlib/SCIPUFF/wrt_prj.f90 \
	  lib/SCIPUFFlib/SCIPUFF/version.f90 \
	  lib/SCIPUFFlib/SCIPUFF/initial.f90 \
	  lib/SCIPUFFlib/SCIPUFF/read_prj.f90 \
	  lib/SCIPUFFlib/SCIPUFF/step_p_dyn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_inter.f90 \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.f90 \
	  lib/SCIPUFFlib/SCIPUFF/SamplerUtil.f90 \
	  lib/SCIPUFFlib/SCIPUFF/init_wash.f90 \
	  lib/SCIPUFFlib/SCIPUFF/ambchem.f90 \
	  lib/SCIPUFFlib/SCIPUFF/los_sensor.f90 \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.f90 \
	  lib/SCIPUFFlib/SCIPUFF/puff_file.f90 \
	  lib/SCIPUFFlib/SCIPUFF/step.f90 \
	  lib/SCIPUFFlib/SCIPUFF/step_drop.f90 \
	  lib/SCIPUFFlib/SCIPUFF/init_met.f90 \
	  lib/SCIPUFFlib/SCIPUFF/get_matl.f90 \
	  lib/SCIPUFFlib/SCIPUFF/surface.f90 \
	  lib/SCIPUFFlib/SCIPUFF/restart.f90 \
	  lib/SCIPUFFlib/SCIPUFF/mapfac.f90 \
	  lib/SCIPUFFlib/SCIPUFF/StepChemMPI.f90 \
	  lib/SCIPUFFlib/SCIPUFF/settle.f90 \
	  lib/SCIPUFFlib/SCIPUFF/aerosol.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inst_rel_dyn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prime.f90 \
	  lib/SCIPUFFlib/SCIPUFF/splitz.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_puff.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_is.f90 \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_init.f90 \
	  lib/SCIPUFFlib/SCIPUFF/sampler.f90 \
	  lib/SCIPUFFlib/SCIPUFF/stepchem.f90 \
	  lib/SCIPUFFlib/SCIPUFF/set_ip.f90 lib/SCIPUFFlib/SCIPUFF/util.f90 \
	  lib/SCIPUFFlib/SCIPUFF/deallocate.f90 \
	  lib/SCIPUFFlib/SCIPUFF/interchem.f90 \
	  lib/SCIPUFFlib/SCIPUFF/sat_humid.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inter_dyn.f90 \
	  lib/SCIPUFFlib/SCIPUFF/stepChemAmb.f90 \
	  lib/SCIPUFFlib/SCIPUFF/merge.f90 \
	  lib/SCIPUFFlib/SCIPUFF/initchem.f90 \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inp_puff.f90 lib/SCIPUFFlib/SCIPUFF/halt.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/structv13_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/structv04_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/dezone_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.f90 \
	  dll/SCIPUFF/LandUse/landuse_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.f90 \
	  dll/SCIPUFF/SWIM/inc/SWIMinit_fd.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_f = lib/PRIME/modules.f

OBJS_f := $(subst .f,.o,$(SRCS_f))

OBJS :=  $(OBJS_f90)  $(OBJS_f) 

DIRS = lib/SCIPUFFlib/FileMgr/inc lib/SCIPUFFlib/SCIPUFF/inc lib/SCIPUFFlib/SCIMgr/inc lib/SCIPUFFlib/FileMgr dll/SCIPUFF/SWIM/inc lib/SCIPUFFlib/SCIPUFF lib/SCIPUFFlib/PlotMgr/inc lib/SCIPUFFlib/PlotMgr lib/SCIPUFFlib/SCIMgr dll/SCIPUFF/LandUse lib/PRIME

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 

$(OBJS_f): %.o:$(filter /\%.f,$(SRCS_f))
	$(F77) -o $@ $(F77FLAGS) $< 


lib/SCIPUFFlib/FileMgr/inc/convert_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/convert_fd.f90 

lib/SCIPUFFlib/FileMgr/inc/release_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/release_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/default_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/error_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o

lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.f90 

lib/SCIPUFFlib/FileMgr/inc/domain_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/domain_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.f90 

lib/SCIPUFFlib/FileMgr/load_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_rel.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o

lib/SCIPUFFlib/FileMgr/inc/list_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/list_fd.f90 

lib/SCIPUFFlib/FileMgr/read_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_rel.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/files_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/FileMgr/inc/basic_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/basic_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.f90 

dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/met_fi.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o

lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.f90 

dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMparam_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o

lib/SCIPUFFlib/FileMgr/inc/time_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/time_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/charT_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/FileMgr/inc/terrain_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/terrain_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o

lib/SCIPUFFlib/FileMgr/inc/version_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/version_fd.f90 

lib/SCIPUFFlib/FileMgr/inc/weather_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/weather_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/terrain_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/version_fd.o

lib/SCIPUFFlib/FileMgr/load_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_msc.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/weather_fd.o

lib/SCIPUFFlib/FileMgr/load_ctrl.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_ctrl.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/FileMgr/load_end.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_end.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/FileMgr/util_mgr.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_mgr.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/class_fd.f90 

lib/SCIPUFFlib/FileMgr/inc/matdef_fi.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/matdef_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/FileMgr/read_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_mtl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/matdef_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_ctrl.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_ctrl.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/inc/options_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/options_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o

lib/SCIPUFFlib/FileMgr/load_options.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_options.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/options_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_end.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_end.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_matdef.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_matdef.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/matdef_fi.o

lib/SCIPUFFlib/FileMgr/inc/flags_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/flags_fd.f90 

lib/SCIPUFFlib/FileMgr/load_flags.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_flags.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/flags_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/utilmtlaux.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/FileMgr/util_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_rel.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/FileMgr/read_met.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_met.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/weather_fd.o

lib/SCIPUFFlib/FileMgr/read_options.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_options.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_msc.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/util_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_mtl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/matdef_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/ARAPlib/reallocate.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/FileMgr/load_domain.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_domain.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/load_start.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_start.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/FileMgr/inc/material_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/material_fd.f90 

lib/SCIPUFFlib/FileMgr/load_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_mtl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o \
	  lib/util/ARAPlib/reallocate.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/FileMgr/read_scn.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_scn.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o

lib/SCIPUFFlib/FileMgr/init_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/init_rel.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

lib/SCIPUFFlib/FileMgr/read_flags.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_flags.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_domain.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_domain.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/read_start.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_start.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/util_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_msc.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/FileMgr/inc/message_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/message_fd.f90 

lib/SCIPUFFlib/FileMgr/inc/status_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/status_fd.f90 

lib/SCIPUFFlib/SCIPUFF/accumsrf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/accumsrf.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/PlotMgr/inc/field_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/field_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o

lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/slice_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.f90  \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o

lib/SCIPUFFlib/SCIPUFF/util_srf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_srf.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/PlotMgr/slicev.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slicev.f90  \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o

lib/SCIPUFFlib/SCIMgr/abort.o:$(BD)/lib/SCIPUFFlib/SCIMgr/abort.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o

lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.f90 

lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o

lib/SCIPUFFlib/PlotMgr/adjoint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/adjoint.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/contour_fd.f90 

lib/SCIPUFFlib/PlotMgr/inc/write_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/write_fd.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o

lib/SCIPUFFlib/PlotMgr/inc/write_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/write_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fd.o

lib/SCIPUFFlib/PlotMgr/writeAVS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeAVS.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagnod_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o lib/AQAER/AqAerIface.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o $(INCMOD)

lib/SCIPUFFlib/PlotMgr/inc/slice_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/slice_fi.f90 

lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/PlotMgr/slice.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slice.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o $(INCMOD)

lib/SCIPUFFlib/PlotMgr/sliceh.o:$(BD)/lib/SCIPUFFlib/PlotMgr/sliceh.f90  \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o

lib/SCIPUFFlib/PlotMgr/writeEIS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeEIS.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.f90 

lib/SCIPUFFlib/PlotMgr/writeOIL.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeOIL.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/PlotMgr/writeCTS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeCTS.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/PlotMgr/plotcopy.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotcopy.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIMgr/inc/state_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/state_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

lib/SCIPUFFlib/SCIMgr/inc/sensor_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/sensor_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/flags_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/options_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/weather_fd.o

lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/SCIMgr/inc/environment_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/environment_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/update_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/update_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/environment_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o

lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/structure_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.o

lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.f90 

lib/SCIPUFFlib/PlotMgr/inc/type_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/type_fd.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.o

lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/sensor_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/structure_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/type_fd.o

lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/state_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.f90 

lib/SCIPUFFlib/PlotMgr/readProject.o:$(BD)/lib/SCIPUFFlib/PlotMgr/readProject.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.f90  \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.o

lib/SCIPUFFlib/PlotMgr/poparea.o:$(BD)/lib/SCIPUFFlib/PlotMgr/poparea.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.o \
	  lib/util/SAGlib/inc/sagcel_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

lib/SCIPUFFlib/PlotMgr/plotchem.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotchem.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagcel_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o $(INCMOD)

lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.f90 

lib/SCIPUFFlib/PlotMgr/inc/pointval_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/pointval_fi.f90 

lib/SCIPUFFlib/PlotMgr/getpoint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/getpoint.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/pointval_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/PlotMgr/writeOVL.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeOVL.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/PlotMgr/clipnorm.o:$(BD)/lib/SCIPUFFlib/PlotMgr/clipnorm.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o

lib/SCIPUFFlib/PlotMgr/slicevint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slicevint.f90  \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o

lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/PlotMgr/plotmet_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotmet_fi.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.o

lib/SCIPUFFlib/PlotMgr/metplot.o:$(BD)/lib/SCIPUFFlib/PlotMgr/metplot.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/PlotMgr/plotmet_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/PlotMgr/writeUSA.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeUSA.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/PlotMgr/plotlist.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotlist.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/PlotMgr/plotmet_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/PlotMgr/plotread.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotread.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/PlotMgr/plotmet_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/PlotMgr/adjoint_util.o:$(BD)/lib/SCIPUFFlib/PlotMgr/adjoint_util.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/newprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/newprj.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/checkinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkinp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/state.o:$(BD)/lib/SCIPUFFlib/SCIMgr/state.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/state_fd.o

lib/SCIPUFFlib/SCIMgr/relcheck_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/relcheck_fi.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o

lib/SCIPUFFlib/SCIMgr/relcheck.o:$(BD)/lib/SCIPUFFlib/SCIMgr/relcheck.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/relcheck_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

lib/SCIPUFFlib/SCIMgr/toolversion.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolversion.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/search_fd.f90 

lib/SCIPUFFlib/SCIMgr/exittool.o:$(BD)/lib/SCIPUFFlib/SCIMgr/exittool.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o

lib/SCIPUFFlib/SCIMgr/runprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/runprj.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/loadter.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadter.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/drawfield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/drawfield.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/PlotMgr/inc/type_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o

lib/SCIPUFFlib/SCIMgr/writeinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/writeinp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.o

lib/SCIPUFFlib/SCIMgr/input.o:$(BD)/lib/SCIPUFFlib/SCIMgr/input.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/toolerror.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolerror.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/exitsimple.o:$(BD)/lib/SCIPUFFlib/SCIMgr/exitsimple.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o

lib/SCIPUFFlib/SCIMgr/defaultinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/defaultinp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/toolstart.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolstart.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/loadprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadprj.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/toolutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolutil.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o

lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/datums_mod.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIMgr/loadmet.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadmet.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/PlotMgr/plotmet_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIMgr/release.o:$(BD)/lib/SCIPUFFlib/SCIMgr/release.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/list_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.f90 

lib/SCIPUFFlib/SCIMgr/checkutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkutil.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o

lib/SCIPUFFlib/SCIMgr/current.o:$(BD)/lib/SCIPUFFlib/SCIMgr/current.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

dll/SCIPUFF/LandUse/landuse_fd.o:$(BD)/dll/SCIPUFF/LandUse/landuse_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIMgr/substrate.o:$(BD)/lib/SCIPUFFlib/SCIMgr/substrate.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/convert.o:$(BD)/lib/SCIPUFFlib/SCIMgr/convert.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/loadpuf.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadpuf.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIMgr/domain.o:$(BD)/lib/SCIPUFFlib/SCIMgr/domain.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/runscipuff.o:$(BD)/lib/SCIPUFFlib/SCIMgr/runscipuff.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIMgr/count.o:$(BD)/lib/SCIPUFFlib/SCIMgr/count.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/flags.o:$(BD)/lib/SCIPUFFlib/SCIMgr/flags.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/callback.o:$(BD)/lib/SCIPUFFlib/SCIMgr/callback.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o

lib/SCIPUFFlib/SCIMgr/except.o:$(BD)/lib/SCIPUFFlib/SCIMgr/except.f90 

lib/SCIPUFFlib/SCIMgr/weather.o:$(BD)/lib/SCIPUFFlib/SCIMgr/weather.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/plotfield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plotfield.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/saggrd_fi.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagtri_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.f90 

lib/SCIPUFFlib/SCIMgr/initsimple.o:$(BD)/lib/SCIPUFFlib/SCIMgr/initsimple.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/loadinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadinp.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.o

lib/SCIPUFFlib/SCIMgr/rstprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/rstprj.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/toolrun.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolrun.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/check.o:$(BD)/lib/SCIPUFFlib/SCIMgr/check.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/domain_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.o

lib/SCIPUFFlib/SCIMgr/plottable.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plottable.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/SCIMgr/material.o:$(BD)/lib/SCIPUFFlib/SCIMgr/material.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/transform.o:$(BD)/lib/SCIPUFFlib/SCIMgr/transform.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

lib/SCIPUFFlib/SCIMgr/delete.o:$(BD)/lib/SCIPUFFlib/SCIMgr/delete.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

lib/SCIPUFFlib/SCIMgr/end.o:$(BD)/lib/SCIPUFFlib/SCIMgr/end.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/contour.o:$(BD)/lib/SCIPUFFlib/SCIMgr/contour.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/PlotMgr/inc/type_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contour_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcnt_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o

lib/SCIPUFFlib/SCIMgr/utm.o:$(BD)/lib/SCIPUFFlib/SCIMgr/utm.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o

lib/SCIPUFFlib/SCIMgr/button.o:$(BD)/lib/SCIPUFFlib/SCIMgr/button.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/time.o:$(BD)/lib/SCIPUFFlib/SCIMgr/time.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o

lib/SCIPUFFlib/SCIMgr/plotinit.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plotinit.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/util/SAGlib/inc/sagbck_fi.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/SCIMgr/fileutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/fileutil.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/update.o:$(BD)/lib/SCIPUFFlib/SCIMgr/update.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/writefield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/writefield.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/write_fi.o

lib/SCIPUFFlib/SCIMgr/drawgrid.o:$(BD)/lib/SCIPUFFlib/SCIMgr/drawgrid.f90  \
	  lib/SCIPUFFlib/SCIMgr/abort.o lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o

lib/SCIPUFFlib/SCIMgr/sendmsg.o:$(BD)/lib/SCIPUFFlib/SCIMgr/sendmsg.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIMgr/adjoint_filter.o:$(BD)/lib/SCIPUFFlib/SCIMgr/adjoint_filter.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/FileMgr/inc/material_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/state_fd.o

lib/SCIPUFFlib/SCIMgr/time_util.o:$(BD)/lib/SCIPUFFlib/SCIMgr/time_util.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/checkerr.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkerr.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o

lib/SCIPUFFlib/SCIMgr/options.o:$(BD)/lib/SCIPUFFlib/SCIMgr/options.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/inittool.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inittool.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIMgr/ctrl.o:$(BD)/lib/SCIPUFFlib/SCIMgr/ctrl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.o

lib/SCIPUFFlib/SCIMgr/inc/mss_error.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mss_error.f90 

lib/SCIPUFFlib/SCIMgr/inc/mss_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mss_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/udm_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/udm_fd.f90 

lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fi.f90 

lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.f90 

lib/SCIPUFFlib/SCIPUFF/util_ip.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_ip.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/split.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/split.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/dezone_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/dezone_fi.f90 

lib/SCIPUFFlib/SCIPUFF/dezone.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/dezone.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/dezone_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o

lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_util.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.f90 

lib/SCIPUFFlib/SCIPUFF/puff_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/puff_rel_dyn.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/start.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/start.f90  \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/drydep.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/drydep.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/run.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/run.f90  \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIPUFF/step_pool.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_pool.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/cont_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_dyn.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/step_pool.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o

lib/SCIPUFFlib/SCIPUFF/chemReactions.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/chemReactions.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/ARAPlib/reallocate.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/set_tlev.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_tlev.f90  \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.f90 

lib/SCIPUFFlib/SCIPUFF/srf_evap.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/srf_evap.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o

lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/SamplerBinaryOut.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerBinaryOut.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/cont_rel_static.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_static.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/SCIPUFF/get_topogSTUB.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_topogSTUB.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o

lib/SCIPUFFlib/SCIPUFF/get_topSWIM.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_topSWIM.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o

lib/SCIPUFFlib/SCIPUFF/output.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/output.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/start.o dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIPUFF/scipuff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/scipuff.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/util_met.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_met.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/SamplerUtil.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerUtil.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/SamplerInputUtil.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerInputUtil.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/ARAPlib/reallocate.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerUtil.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/SamplerInput.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerInput.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerInputUtil.o \
	  lib/util/ARAPlib/reallocate.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerUtil.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/error.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/error.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.f90 

lib/SCIPUFFlib/SCIPUFF/reflect.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/reflect.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/rebin.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/rebin.f90 

lib/SCIPUFFlib/SCIPUFF/set_grid.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_grid.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIPUFF/ufall.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/ufall.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o

lib/SCIPUFFlib/SCIPUFF/time_cnv.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/time_cnv.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prise.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prise.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/MPIFunc.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/MPIFunc.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o dll/stub/MPI/localMPI.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/util_evap.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_evap.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o

lib/SCIPUFFlib/SCIPUFF/get_metSWIM.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_metSWIM.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.o

lib/SCIPUFFlib/SCIPUFF/SetBotCells.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SetBotCells.f90  \
	  lib/util/SAGlib/inc/sagcel_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/SCIPUFF/vd_slinn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/vd_slinn.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/util_matl.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_matl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/utilchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/utilchem.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/chemReactions.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/util/ARAPlib/reallocate.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/init_srf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_srf.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/ARAPlib/reallocate.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/wrt_prj.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/wrt_prj.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o

lib/SCIPUFFlib/SCIPUFF/version.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/version.f90 

lib/SCIPUFFlib/SCIPUFF/initial.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/initial.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/inc/structv04_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/structv04_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/structv13_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/structv13_fd.f90 

lib/SCIPUFFlib/SCIPUFF/read_prj.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/read_prj.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/structv04_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/structv13_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/step_p_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_p_dyn.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/util_inter.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_inter.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/init_wash.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_wash.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/ambchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/ambchem.f90  \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/los_sensor.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/los_sensor.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/puff_file.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/puff_file.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/time_fd.o

lib/SCIPUFFlib/SCIPUFF/step.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/step_drop.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_drop.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMinit_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIPUFF/init_met.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_met.f90  \
	  lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/datums_mod.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMinit_fd.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIPUFF/get_matl.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_matl.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/surface.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/surface.f90  \
	  lib/SCIPUFFlib/SCIPUFF/accumsrf.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o \
	  lib/SCIPUFFlib/SCIPUFF/util_srf.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/restart.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/restart.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o lib/util/SAGlib/inc/saglst_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/SCIPUFF/mapfac.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/mapfac.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  dll/SCIPUFF/SWIM/inc/SWIMparam_fd.o

lib/SCIPUFFlib/SCIPUFF/StepChemMPI.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/StepChemMPI.f90  \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIMgr/inc/charT_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o dll/stub/MPI/localMPI.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/settle.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/settle.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o

lib/SCIPUFFlib/SCIPUFF/aerosol.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/aerosol.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inst_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inst_rel_dyn.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/PRIME/modules.o:$(BD)/lib/PRIME/modules.f 

lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prime.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prime.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o lib/PRIME/modules.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/splitz.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/splitz.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o

lib/SCIPUFFlib/SCIPUFF/util_puff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_puff.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/util_is.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_is.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/cont_rel_init.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_init.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/FileMgr/inc/convert_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/sampler.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/sampler.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.o lib/AQAER/AqAerIface.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/field_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcnt_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagtri_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.o \
	  lib/SCIPUFFlib/SCIPUFF/SamplerUtil.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/PlotMgr/inc/slice_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/stepchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/stepchem.f90  \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/set_ip.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_ip.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/util.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/deallocate.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/deallocate.f90  \
	  lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/cont_rel_util.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIMgr/relcheck_fi.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.o

lib/SCIPUFFlib/SCIPUFF/interchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/interchem.f90  \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/sat_humid.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/sat_humid.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.o

lib/SCIPUFFlib/SCIPUFF/inter_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inter_dyn.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/stepChemAmb.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/stepChemAmb.f90  \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/default_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/met_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/merge.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/merge.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/SCIPUFFlib/SCIPUFF/initchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/initchem.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/chemReactions.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/class_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/error_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  dll/SCIPUFF/LandUse/landuse_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.o \
	  lib/util/ARAPlib/reallocate.o $(INCMOD)

lib/SCIPUFFlib/SCIPUFF/inp_puff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inp_puff.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/files_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/utilmtlaux.o

lib/SCIPUFFlib/SCIPUFF/halt.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/halt.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.o

lib/SCIPUFFlib/SCIPUFF/inc/search_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/search_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/search_fd.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  options_fd.mod matdef_fi.mod version_fd.mod \
	  message_fd.mod release_fd.mod material_fd.mod terrain_fd.mod \
	  convert_fd.mod domain_fd.mod list_fd.mod basic_fd.mod status_fd.mod \
	  time_fd.mod flags_fd.mod weather_fd.mod eisfile_fi.mod \
	  oilfile_fi.mod ctsfile_fd.mod getcellval_itrf.mod \
	  setpoints_itrf.mod clipnorm.mod clipnorm_fi.mod usafile_fi.mod \
	  plotmet_fi.mod slice_fi.mod write_fi.mod write_nofile.mod \
	  contourlist_fd.mod field_fd.mod slice_fd.mod metoutput_fd.mod \
	  plotmet_fd.mod cellstr_fd.mod contour_fd.mod plotlist_fd.mod \
	  adjoint_fi.mod plotlist_fi.mod poparea_fi.mod poparea_fd.mod \
	  pointval_fi.mod areamode_fd.mod type_fd.mod write_fd.mod \
	  classdata_fd.mod scipversion_fd.mod abort.mod releasecheck_fi.mod \
	  scimgr_fd.mod scimgrstate.mod chart_fd.mod scipresults_fd.mod \
	  checkerr.mod checkerr_fd.mod domstruct_fd.mod inpstruct_fd.mod \
	  arrivaltimefunc_fi.mod plotaux_fi.mod plotfunc_fd.mod \
	  plotfunc_fi.mod mss_error.mod domaincoord_fd.mod inittool_fi.mod \
	  scimgr_fi.mod environment_fd.mod mss_fd.mod mssstruct_fd.mod \
	  state_fd.mod spcstruct_fd.mod adjointfilter_fi.mod update_fd.mod \
	  relstruct_fd.mod metstruct_fd.mod scimgrparam_fd.mod \
	  nestmodel_fd.mod datums.mod timstruct_fd.mod udm_fd.mod \
	  statstruct_fd.mod mtlstruct_fd.mod sensor_fd.mod prjstruct_fd.mod \
	  limitt_fd.mod nestmodel_fi.mod tooluser_fd.mod structure_fd.mod \
	  uniformgridt_fd.mod special_rst_fi.mod drydep_fd.mod \
	  chemreactions_fd.mod accumsrf.mod createstatics.mod \
	  stepscipuff_fi.mod pool_fi.mod botcells_fi.mod samplergridouput.mod \
	  parseline_fi.mod cont_rel_functions.mod samplerutil.mod \
	  utilmtlaux.mod oldpuff_fi.mod utilsrf.mod error_fi.mod \
	  metparam_fd.mod errorparam_fd.mod files_fi.mod mapcoord_fd.mod \
	  relparam_fd.mod met_fi.mod surface_fi.mod srfevap2d_fd.mod \
	  srfevap_fi.mod substrate_fi.mod cont_rel_fd.mod cont_rel_fi.mod \
	  surface_dose_fd.mod surface_fd.mod srfdos_fd.mod structv13_fd.mod \
	  chem_fi.mod diagnostics_fi.mod structv04_fd.mod srfparam_fd.mod \
	  class_fd.mod srfaux_fi.mod ipgrd_fi.mod inter_fi.mod search_fi.mod \
	  dezone_fi.mod basic_fi.mod flags_fi.mod grid_fi.mod matl_fi.mod \
	  nextrel_fi.mod project_fi.mod puff_fi.mod puffrelease_fi.mod \
	  release_fi.mod scipuff_fi.mod scnrel_fi.mod time_fi.mod tlev_fi.mod \
	  type_fi.mod search_fd.mod los_fd.mod sampler_fd.mod sciprime_fi.mod \
	  constants_fd.mod aerosol_fi.mod refl_fi.mod sampler_fi.mod \
	  default_fd.mod multcomp_fd.mod swimgridstr_fd.mod swimparam_fd.mod \
	  landuse_fd.mod swimpuff_fd.mod swiminit_fd.mod main1.mod \
	  prime_ambient.mod prime_dfsn.mod prime_numparm.mod prime_params.mod \
	  prime_plu.mod prime_wakedat.mod