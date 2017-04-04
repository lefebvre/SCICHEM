# Main program is libscipuff.a

PROG =	libscipuff.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/SCIPUFFlib/FileMgr/load_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/read_rel.f90 \
	  lib/SCIPUFFlib/FileMgr/load_msc.f90 \
	  lib/SCIPUFFlib/FileMgr/read_status.f90 \
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
	  lib/SCIPUFFlib/SCIMgr/toolrun.f90 lib/SCIPUFFlib/SCIMgr/status.f90 \
	  lib/SCIPUFFlib/SCIMgr/check.f90 lib/SCIPUFFlib/SCIMgr/plottable.f90 \
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
	  lib/SCIPUFFlib/SCIPUFF/start.f90 lib/SCIPUFFlib/SCIPUFF/run.f90 \
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

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

SRCS_f = lib/PRIME/modules.f

OBJS_f := $(notdir $(subst .f,.o,$(SRCS_f)))

OBJS :=  $(OBJS_f90)  $(OBJS_f) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 

$(OBJS_f): %.o:$(filter /\%.f,$(SRCS_f))
	$(F77) $(F77FLAGS) $< 


convert_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/convert_fd.f90 

release_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/release_fd.f90  param_fd.o

relparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/relparam_fd.f90 

constants_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/constants_fd.f90 

default_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/default_fd.f90 

errorParam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.f90 

error_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/error_fi.f90  errorParam_fd.o

domainCoord_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/domainCoord_fd.f90 

domain_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/domain_fd.f90  domainCoord_fd.o

MapCoord_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/MapCoord_fd.f90  domain_fd.o

multcomp_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/multcomp_fd.f90  param_fd.o \
	  MapCoord_fd.o

scipuff_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/scipuff_fi.f90  constants_fd.o \
	  default_fd.o param_fd.o error_fi.o struct_fd.o multcomp_fd.o \
	  release_fd.o

utilmtlaux.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/utilmtlaux.f90  default_fd.o \
	  error_fi.o scipuff_fi.o struct_fd.o

load_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_rel.f90  convert_fd.o \
	  release_fd.o relparam_fd.o scipuff_fi.o utilmtlaux.o

list_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/list_fd.f90 

read_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_rel.f90  list_fd.o release_fd.o \
	  scipuff_fi.o

files_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/files_fi.f90  param_fd.o

basic_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/basic_fd.f90 

metparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/metparam_fd.f90 

SWIMgridStr_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMgridStr_fd.f90  \
	  MapCoord_fd.o

met_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/met_fi.f90  basic_fd.o \
	  metparam_fd.o SWIMgridStr_fd.o

SCIPresults_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIPresults_fd.f90 

SWIMparam_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMparam_fd.f90  coordinate_fd.o \
	  errorParam_fd.o

time_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/time_fd.f90  param_fd.o

charT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/charT_fd.f90  param_fd.o

terrain_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/terrain_fd.f90  charT_fd.o

version_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/version_fd.f90 

weather_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/weather_fd.f90  param_fd.o \
	  terrain_fd.o version_fd.o

load_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_msc.f90  convert_fd.o \
	  files_fi.o met_fi.o SCIPresults_fd.o scipuff_fi.o SWIMparam_fd.o \
	  time_fd.o weather_fd.o

status_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/status_fd.f90 

read_status.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_status.f90  list_fd.o \
	  scipuff_fi.o status_fd.o

load_ctrl.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_ctrl.f90  convert_fd.o \
	  files_fi.o scipuff_fi.o time_fd.o

load_end.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_end.f90  convert_fd.o \
	  scipuff_fi.o time_fd.o

util_mgr.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_mgr.f90  default_fd.o \
	  error_fi.o list_fd.o

class_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/class_fd.f90 

matdef_fi.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/matdef_fi.f90  param_fd.o

read_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_mtl.f90  class_fd.o files_fi.o \
	  matdef_fi.o scipuff_fi.o

read_ctrl.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_ctrl.f90  basic_fd.o \
	  files_fi.o scipuff_fi.o

options_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/options_fd.f90  charT_fd.o

sampler_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sampler_fd.f90 

sampler_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sampler_fi.f90  param_fd.o \
	  sampler_fd.o

load_options.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_options.f90  met_fi.o \
	  options_fd.o sampler_fi.o scipuff_fi.o

read_end.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_end.f90  scipuff_fi.o

read_matdef.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_matdef.f90  class_fd.o \
	  default_fd.o error_fi.o matdef_fi.o

flags_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/flags_fd.f90 

load_flags.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_flags.f90  files_fi.o \
	  flags_fd.o scipuff_fi.o

util_rel.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_rel.f90  default_fd.o \
	  scipuff_fi.o

read_met.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_met.f90  param_fd.o error_fi.o \
	  files_fi.o met_fi.o metparam_fd.o scipuff_fi.o SWIMparam_fd.o \
	  weather_fd.o

read_options.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_options.f90  met_fi.o \
	  scipuff_fi.o

read_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_msc.f90  met_fi.o scipuff_fi.o

util_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_mtl.f90  class_fd.o \
	  default_fd.o error_fi.o matdef_fi.o scipuff_fi.o reallocate.o \
	  struct_fd.o utilmtlaux.o

load_domain.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_domain.f90  domain_fd.o \
	  scipuff_fi.o

load_start.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_start.f90  scipuff_fi.o \
	  time_fd.o

material_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/material_fd.f90 

load_mtl.o:$(BD)/lib/SCIPUFFlib/FileMgr/load_mtl.f90  class_fd.o list_fd.o \
	  material_fd.o reallocate.o SCIPresults_fd.o scipuff_fi.o \
	  utilmtlaux.o

read_scn.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_scn.f90  convert_fd.o \
	  relparam_fd.o scipuff_fi.o

read_flags.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_flags.f90  scipuff_fi.o

read_domain.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_domain.f90  scipuff_fi.o

read_start.o:$(BD)/lib/SCIPUFFlib/FileMgr/read_start.f90  scipuff_fi.o

util_msc.o:$(BD)/lib/SCIPUFFlib/FileMgr/util_msc.f90  param_fd.o error_fi.o \
	  files_fi.o met_fi.o scipuff_fi.o

message_fd.o:$(BD)/lib/SCIPUFFlib/FileMgr/inc/message_fd.f90 

accumsrf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/accumsrf.f90  error_fi.o sagstr_fd.o

field_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/field_fd.f90  param_fd.o \
	  domain_fd.o

slice_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/slice_fd.f90 

srfdos_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfdos_fd.f90 

srfparam_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfparam_fd.f90 

srfaux_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfaux_fi.f90  struct_fd.o

surface_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/surface_fd.f90  srfparam_fd.o \
	  struct_fd.o

surface_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/surface_fi.f90  sagstr_fd.o \
	  surface_fd.o

util_srf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_srf.f90  constants_fd.o \
	  error_fi.o scipuff_fi.o sagdef_fd.o srfaux_fi.o srfparam_fd.o \
	  surface_fi.o

slicev.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slicev.f90  accumsrf.o constants_fd.o \
	  field_fd.o met_fi.o sagdef_fd.o sagstr_fd.o scipuff_fi.o slice_fd.o \
	  srfdos_fd.o srfparam_fd.o util_srf.o

abort.o:$(BD)/lib/SCIPUFFlib/SCIMgr/abort.f90  error_fi.o files_fi.o

plotlist_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotlist_fd.f90  time_fd.o

adjoint_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/adjoint_fi.f90  plotlist_fd.o \
	  sagstr_fd.o

classdata_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/classdata_fd.f90 

plotlist_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotlist_fi.f90  \
	  plotlist_fd.o

adjoint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/adjoint.f90  abort.o adjoint_fi.o \
	  charT_fd.o classdata_fd.o error_fi.o field_fd.o files_fi.o \
	  plotlist_fi.o saglst_fd.o sagcel_fd.o sagdef_fd.o sagstr_fd.o \
	  SCIPresults_fd.o scipuff_fi.o slice_fd.o srfparam_fd.o surface_fi.o

contour_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/contour_fd.f90 

write_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/write_fd.f90  contour_fd.o

write_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/write_fi.f90  field_fd.o \
	  param_fd.o write_fd.o

writeAVS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeAVS.f90  abort.o charT_fd.o \
	  field_fd.o saglst_fd.o sagdef_fd.o sagnod_fd.o sagstr_fd.o \
	  sagtri_fd.o sagfun_usr.o write_fi.o

chem_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/chem_fi.f90  MPIFunc_fi.o \
	  AqAerIface.o multcomp_fd.o sagstr_fd.o $(INCMOD)

slice_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/slice_fi.f90 

srfevap_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/srfevap_fi.f90  struct_fd.o

slice.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slice.f90  chem_fi.o classdata_fd.o \
	  error_fi.o field_fd.o met_fi.o plotlist_fi.o saglst_fd.o \
	  struct_fd.o sagdef_fd.o sagstr_fd.o SCIPresults_fd.o scipuff_fi.o \
	  slice_fd.o slice_fi.o srfdos_fd.o srfevap_fi.o srfparam_fd.o \
	  surface_fd.o surface_fi.o util_srf.o $(INCMOD)

sliceh.o:$(BD)/lib/SCIPUFFlib/PlotMgr/sliceh.f90  accumsrf.o constants_fd.o \
	  default_fd.o field_fd.o sagdef_fd.o sagstr_fd.o scipuff_fi.o \
	  srfdos_fd.o srfparam_fd.o util_srf.o

writeEIS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeEIS.f90  error_fi.o field_fd.o \
	  files_fi.o scipuff_fi.o sagdef_fd.o sagfun_usr.o write_fi.o

contourlist_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/contourlist_fd.f90 

writeOIL.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeOIL.f90  charT_fd.o \
	  contourlist_fd.o error_fi.o field_fd.o sagdef_fd.o sagfun_usr.o \
	  write_fi.o

writeCTS.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeCTS.f90  error_fi.o sagdef_fd.o \
	  sagfun_usr.o write_fi.o

plotcopy.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotcopy.f90  error_fi.o field_fd.o \
	  plotlist_fi.o saglst_fd.o sagdef_fd.o sagstr_fd.o SCIPresults_fd.o \
	  scipuff_fi.o

prjstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/prjstruct_fd.f90  param_fd.o

state_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/state_fd.f90 

SCIMgrParam_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.f90  \
	  SCIPresults_fd.o

sensor_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/sensor_fd.f90 

domstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/domstruct_fd.f90  domain_fd.o \
	  prjstruct_fd.o

inpstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/inpstruct_fd.f90  flags_fd.o \
	  list_fd.o options_fd.o prjstruct_fd.o

limitT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/limitT_fd.f90 

metstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/metstruct_fd.f90  \
	  prjstruct_fd.o weather_fd.o

mtlstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mtlstruct_fd.f90  list_fd.o \
	  material_fd.o prjstruct_fd.o

relstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/relstruct_fd.f90  list_fd.o \
	  prjstruct_fd.o release_fd.o

spcstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/spcstruct_fd.f90  domain_fd.o \
	  prjstruct_fd.o

timstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/timstruct_fd.f90  \
	  prjstruct_fd.o time_fd.o

environment_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/environment_fd.f90 

update_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/update_fd.f90  basic_fd.o \
	  environment_fd.o release_fd.o

structure_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/structure_fd.f90  charT_fd.o \
	  domstruct_fd.o inpstruct_fd.o limitT_fd.o message_fd.o \
	  metstruct_fd.o mtlstruct_fd.o prjstruct_fd.o relstruct_fd.o \
	  spcstruct_fd.o timstruct_fd.o update_fd.o

poparea_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/poparea_fd.f90 

type_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/type_fd.f90  poparea_fd.o

tooluser_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/tooluser_fd.f90  \
	  contourlist_fd.o convert_fd.o field_fd.o plotlist_fd.o \
	  SCIMgrParam_fd.o sensor_fd.o structure_fd.o type_fd.o

SCIMgr_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fd.f90  basic_fd.o \
	  state_fd.o tooluser_fd.o

SCIMgr_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgr_fi.f90  basic_fd.o \
	  charT_fd.o message_fd.o prjstruct_fd.o

SCIMgrState.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/SCIMgrState.f90 

readProject.o:$(BD)/lib/SCIPUFFlib/PlotMgr/readProject.f90  abort.o met_fi.o \
	  prjstruct_fd.o SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o scipuff_fi.o

plotfunc_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/plotfunc_fi.f90  sagstr_fd.o

poparea_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/poparea_fi.f90  field_fd.o \
	  param_fd.o poparea_fd.o

poparea.o:$(BD)/lib/SCIPUFFlib/PlotMgr/poparea.f90  abort.o error_fi.o \
	  field_fd.o param_fd.o plotfunc_fi.o poparea_fi.o sagcel_fd.o \
	  sagdef_fd.o SCIMgr_fd.o SCIMgrState.o tooluser_fd.o

plotchem.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotchem.f90  chem_fi.o error_fi.o \
	  met_fi.o saglst_fd.o sagcel_fd.o sagdef_fd.o sagstr_fd.o \
	  scipuff_fi.o slice_fd.o surface_fi.o $(INCMOD)

cellstr_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/cellstr_fd.f90 

pointval_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/pointval_fi.f90 

getpoint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/getpoint.f90  cellstr_fd.o \
	  error_fi.o pointval_fi.o saglst_fd.o sagdef_fd.o sagstr_fd.o

writeOVL.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeOVL.f90  charT_fd.o \
	  contourlist_fd.o error_fi.o field_fd.o scipuff_fi.o saglst_fd.o \
	  sagdef_fd.o sagstr_fd.o sagfun_usr.o write_fi.o

clipnorm.o:$(BD)/lib/SCIPUFFlib/PlotMgr/clipnorm.f90  error_fi.o

slicevint.o:$(BD)/lib/SCIPUFFlib/PlotMgr/slicevint.f90  accumsrf.o \
	  sagstr_fd.o scipuff_fi.o slice_fd.o srfdos_fd.o srfparam_fd.o \
	  util_srf.o

plotmet_fd.o:$(BD)/lib/SCIPUFFlib/PlotMgr/inc/plotmet_fd.f90  charT_fd.o \
	  domain_fd.o plotlist_fd.o prjstruct_fd.o

plotmet_fi.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotmet_fi.f90  plotmet_fd.o

metplot.o:$(BD)/lib/SCIPUFFlib/PlotMgr/metplot.f90  default_fd.o error_fi.o \
	  field_fd.o files_fi.o met_fi.o plotlist_fi.o plotmet_fi.o \
	  saglst_fd.o sagdef_fd.o sagstr_fd.o SCIMgr_fd.o SWIMparam_fd.o

writeUSA.o:$(BD)/lib/SCIPUFFlib/PlotMgr/writeUSA.f90  contourlist_fd.o \
	  error_fi.o field_fd.o sagdef_fd.o sagfun_usr.o write_fi.o

plotlist.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotlist.f90  chem_fi.o error_fi.o \
	  field_fd.o files_fi.o met_fi.o plotlist_fi.o plotmet_fi.o \
	  saglst_fd.o sagdef_fd.o sagerr_fd.o sagstr_fd.o SCIPresults_fd.o \
	  scipuff_fi.o $(INCMOD)

plotread.o:$(BD)/lib/SCIPUFFlib/PlotMgr/plotread.f90  classdata_fd.o \
	  error_fi.o field_fd.o files_fi.o met_fi.o plotlist_fi.o \
	  plotmet_fi.o saglst_fd.o sagdef_fd.o sagerr_fd.o sagstr_fd.o \
	  SCIPresults_fd.o scipuff_fi.o srfparam_fd.o time_fd.o

adjoint_util.o:$(BD)/lib/SCIPUFFlib/PlotMgr/adjoint_util.f90  adjoint_fi.o \
	  constants_fd.o error_fi.o scipuff_fi.o

newprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/newprj.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o

checkinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkinp.f90  error_fi.o SCIMgr_fd.o \
	  SCIMgrState.o

state.o:$(BD)/lib/SCIPUFFlib/SCIMgr/state.f90  error_fi.o SCIMgr_fi.o \
	  state_fd.o

relcheck_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/relcheck_fi.f90  list_fd.o \
	  material_fd.o

relcheck.o:$(BD)/lib/SCIPUFFlib/SCIMgr/relcheck.f90  default_fd.o error_fi.o \
	  files_fi.o release_fd.o relcheck_fi.o SCIPresults_fd.o

toolversion.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolversion.f90  param_fd.o \
	  SCIMgr_fd.o SCIMgrState.o scipuff_fi.o

search_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/search_fd.f90 

exittool.o:$(BD)/lib/SCIPUFFlib/SCIMgr/exittool.f90  files_fi.o SCIMgr_fd.o \
	  SCIMgr_fi.o SCIMgrState.o search_fd.o

runprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/runprj.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o

loadter.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadter.f90  field_fd.o met_fi.o \
	  prjstruct_fd.o SCIMgr_fd.o SCIMgrState.o scipuff_fi.o

drawfield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/drawfield.f90  abort.o type_fd.o \
	  cellstr_fd.o constants_fd.o contourlist_fd.o error_fi.o field_fd.o \
	  plotfunc_fi.o plotlist_fi.o poparea_fi.o saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagstr_fd.o SCIMgr_fd.o SCIMgrState.o

statstruct_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/statstruct_fd.f90  list_fd.o \
	  prjstruct_fd.o

writeinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/writeinp.f90  error_fi.o SCIMgr_fd.o \
	  SCIMgrState.o statstruct_fd.o

input.o:$(BD)/lib/SCIPUFFlib/SCIMgr/input.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o

toolerror.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolerror.f90  error_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o

exitsimple.o:$(BD)/lib/SCIPUFFlib/SCIMgr/exitsimple.f90  files_fi.o met_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o scipuff_fi.o search_fd.o

defaultinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/defaultinp.f90  error_fi.o \
	  SCIMgr_fd.o SCIMgrState.o

toolstart.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolstart.f90  default_fd.o \
	  error_fi.o files_fi.o SCIMgr_fd.o

loadprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadprj.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o SCIPresults_fd.o scipuff_fi.o

toolutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolutil.f90  param_fd.o error_fi.o \
	  SCIMgr_fd.o scipuff_fi.o search_fd.o

datums_mod.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/datums_mod.f90  param_fd.o

loadmet.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadmet.f90  abort.o constants_fd.o \
	  datums_mod.o default_fd.o error_fi.o files_fi.o met_fi.o \
	  plotmet_fd.o plotlist_fi.o plotmet_fi.o SCIMgr_fd.o SCIMgr_fi.o \
	  SCIMgrState.o SWIMgridStr_fd.o SWIMparam_fd.o

release.o:$(BD)/lib/SCIPUFFlib/SCIMgr/release.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o

checkerr_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/checkerr_fd.f90 

checkutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkutil.f90  checkerr_fd.o \
	  datums_mod.o default_fd.o error_fi.o files_fi.o SCIMgr_fd.o \
	  SCIMgr_fi.o

current.o:$(BD)/lib/SCIPUFFlib/SCIMgr/current.f90  SCIPresults_fd.o

landuse_fd.o:$(BD)/dll/SCIPUFF/LandUse/landuse_fd.f90  param_fd.o

substrate.o:$(BD)/lib/SCIPUFFlib/SCIMgr/substrate.f90  files_fi.o \
	  landuse_fd.o SCIMgr_fd.o SCIMgrState.o

convert.o:$(BD)/lib/SCIPUFFlib/SCIMgr/convert.f90  error_fi.o SCIMgr_fd.o

loadpuf.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadpuf.f90  abort.o chem_fi.o \
	  error_fi.o multcomp_fd.o plotlist_fi.o SCIMgr_fd.o SCIMgrState.o \
	  SCIPresults_fd.o scipuff_fi.o spcstruct_fd.o time_fd.o struct_fd.o $(INCMOD)

domain.o:$(BD)/lib/SCIPUFFlib/SCIMgr/domain.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

runscipuff.o:$(BD)/lib/SCIPUFFlib/SCIMgr/runscipuff.f90  error_fi.o \
	  files_fi.o met_fi.o metparam_fd.o SCIMgr_fd.o SCIMgr_fi.o \
	  SCIMgrParam_fd.o SCIMgrState.o scipuff_fi.o surface_fi.o \
	  SWIMparam_fd.o

count.o:$(BD)/lib/SCIPUFFlib/SCIMgr/count.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o SCIMgrState.o

flags.o:$(BD)/lib/SCIPUFFlib/SCIMgr/flags.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

callback.o:$(BD)/lib/SCIPUFFlib/SCIMgr/callback.f90  basic_fd.o files_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o

except.o:$(BD)/lib/SCIPUFFlib/SCIMgr/except.f90 

weather.o:$(BD)/lib/SCIPUFFlib/SCIMgr/weather.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

plotfield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plotfield.f90  abort.o charT_fd.o \
	  constants_fd.o contourlist_fd.o error_fi.o field_fd.o files_fi.o \
	  plotfunc_fi.o plotlist_fi.o prjstruct_fd.o saglst_fd.o sagdef_fd.o \
	  saggrd_fi.o sagfun_usr.o sagstr_fd.o sagtri_fd.o SCIMgr_fd.o \
	  SCIMgrState.o scipuff_fi.o slice_fd.o srfaux_fi.o surface_fd.o \
	  surface_fi.o write_fd.o write_fi.o

adjoint_filter_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/adjoint_filter_fi.f90 

initsimple.o:$(BD)/lib/SCIPUFFlib/SCIMgr/initsimple.f90  adjoint_filter_fi.o \
	  error_fi.o files_fi.o SCIMgr_fi.o scipuff_fi.o param_fd.o \
	  SCIMgr_fd.o SCIMgrState.o

loadinp.o:$(BD)/lib/SCIPUFFlib/SCIMgr/loadinp.f90  error_fi.o SCIMgr_fd.o \
	  SCIMgrState.o statstruct_fd.o

rstprj.o:$(BD)/lib/SCIPUFFlib/SCIMgr/rstprj.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o

toolrun.o:$(BD)/lib/SCIPUFFlib/SCIMgr/toolrun.f90  SCIMgr_fd.o

status.o:$(BD)/lib/SCIPUFFlib/SCIMgr/status.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o statstruct_fd.o

check.o:$(BD)/lib/SCIPUFFlib/SCIMgr/check.f90  checkerr_fd.o domain_fd.o \
	  error_fi.o SCIMgr_fd.o SCIPresults_fd.o timstruct_fd.o

plottable.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plottable.f90  abort.o field_fd.o \
	  plotlist_fi.o prjstruct_fd.o SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o \
	  scipuff_fi.o srfaux_fi.o surface_fi.o

material.o:$(BD)/lib/SCIPUFFlib/SCIMgr/material.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

transform.o:$(BD)/lib/SCIPUFFlib/SCIMgr/transform.f90  abort.o \
	  constants_fd.o datums_mod.o default_fd.o error_fi.o field_fd.o \
	  param_fd.o SCIMgr_fd.o SCIMgrState.o tooluser_fd.o

delete.o:$(BD)/lib/SCIPUFFlib/SCIMgr/delete.f90  error_fi.o SCIMgr_fd.o \
	  SCIMgr_fi.o SCIMgrState.o SCIPresults_fd.o

end.o:$(BD)/lib/SCIPUFFlib/SCIMgr/end.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

contour.o:$(BD)/lib/SCIPUFFlib/SCIMgr/contour.f90  abort.o type_fd.o \
	  contour_fd.o contourlist_fd.o default_fd.o error_fi.o field_fd.o \
	  plotfunc_fi.o saglst_fd.o sagcnt_fd.o sagdef_fd.o sagstr_fd.o \
	  sagtri_fd.o SCIMgr_fd.o SCIMgrState.o tooluser_fd.o

utm.o:$(BD)/lib/SCIPUFFlib/SCIMgr/utm.f90  datums_mod.o param_fd.o \
	  error_fi.o

button.o:$(BD)/lib/SCIPUFFlib/SCIMgr/button.f90  error_fi.o files_fi.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o

time.o:$(BD)/lib/SCIPUFFlib/SCIMgr/time.f90  checkerr_fd.o default_fd.o \
	  error_fi.o files_fi.o SCIMgr_fd.o SCIMgr_fi.o

plotinit.o:$(BD)/lib/SCIPUFFlib/SCIMgr/plotinit.f90  abort.o adjoint_fi.o \
	  error_fi.o field_fd.o files_fi.o plotlist_fi.o prjstruct_fd.o \
	  sagbck_fi.o sagdef_fd.o sampler_fi.o SCIMgr_fd.o SCIMgr_fi.o \
	  SCIMgrState.o scipuff_fi.o surface_fi.o

fileutil.o:$(BD)/lib/SCIPUFFlib/SCIMgr/fileutil.f90  error_fi.o files_fi.o \
	  prjstruct_fd.o SCIMgr_fd.o

update.o:$(BD)/lib/SCIPUFFlib/SCIMgr/update.f90  error_fi.o files_fi.o \
	  release_fd.o SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o scipuff_fi.o

writefield.o:$(BD)/lib/SCIPUFFlib/SCIMgr/writefield.f90  abort.o charT_fd.o \
	  contourlist_fd.o error_fi.o field_fd.o files_fi.o plotfunc_fi.o \
	  saglst_fd.o sagdef_fd.o sagerr_fd.o sagstr_fd.o SCIMgr_fd.o \
	  SCIMgrState.o SCIPresults_fd.o tooluser_fd.o write_fi.o

drawgrid.o:$(BD)/lib/SCIPUFFlib/SCIMgr/drawgrid.f90  abort.o error_fi.o \
	  sagdef_fd.o SCIMgr_fd.o SCIMgrState.o

sendmsg.o:$(BD)/lib/SCIPUFFlib/SCIMgr/sendmsg.f90  error_fi.o message_fd.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o scipuff_fi.o

adjoint_filter.o:$(BD)/lib/SCIPUFFlib/SCIMgr/adjoint_filter.f90  \
	  adjoint_filter_fi.o error_fi.o material_fd.o release_fd.o \
	  SCIMgr_fd.o SCIMgr_fi.o SCIMgrState.o SCIPresults_fd.o state_fd.o

time_util.o:$(BD)/lib/SCIPUFFlib/SCIMgr/time_util.f90  default_fd.o \
	  SCIMgr_fd.o

checkerr.o:$(BD)/lib/SCIPUFFlib/SCIMgr/checkerr.f90  checkerr_fd.o \
	  default_fd.o error_fi.o SCIMgr_fi.o SCIMgrParam_fd.o

options.o:$(BD)/lib/SCIPUFFlib/SCIMgr/options.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

inittool.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inittool.f90  adjoint_filter_fi.o \
	  error_fi.o files_fi.o SCIMgr_fi.o scipuff_fi.o metparam_fd.o \
	  param_fd.o plotlist_fi.o SCIMgr_fd.o SCIMgrState.o search_fd.o \
	  surface_fi.o SWIMparam_fd.o

ctrl.o:$(BD)/lib/SCIPUFFlib/SCIMgr/ctrl.f90  default_fd.o error_fi.o \
	  files_fi.o SCIMgr_fd.o

mss_error.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mss_error.f90 

mss_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/mss_fd.f90 

nestmodel_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fd.f90 

udm_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/udm_fd.f90 

nestmodel_fi.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/nestmodel_fi.f90 

uniformGridT_fd.o:$(BD)/lib/SCIPUFFlib/SCIMgr/inc/uniformGridT_fd.f90 

ipgrd_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/ipgrd_fi.f90 

util_ip.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_ip.f90  error_fi.o ipgrd_fi.o \
	  scipuff_fi.o

split.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/split.f90  files_fi.o met_fi.o \
	  scipuff_fi.o step_p_fi.o struct_fd.o

dezone_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/dezone_fi.f90 

dezone.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/dezone.f90  dezone_fi.o error_fi.o \
	  srfparam_fd.o

cont_rel_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/cont_rel_fi.f90  param_fd.o \
	  struct_fd.o release_fd.o

cont_rel_util.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_util.f90  \
	  cont_rel_fi.o default_fd.o error_fi.o scipuff_fi.o

inter_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/inter_fi.f90 

puff_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/puff_rel_dyn.f90  adjoint_fi.o \
	  constants_fd.o cont_rel_fi.o cont_rel_util.o default_fd.o \
	  error_fi.o files_fi.o inter_fi.o met_fi.o scipuff_fi.o saglst_fd.o \
	  relparam_fd.o sagdef_fd.o surface_fi.o utilmtlaux.o

start.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/start.f90  accumsrf.o cont_rel_fi.o \
	  files_fi.o met_fi.o saglst_fd.o sagdef_fd.o sampler_fi.o \
	  scipuff_fi.o srfparam_fd.o surface_fi.o SWIMgridStr_fd.o \
	  SWIMparam_fd.o utilmtlaux.o

run.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/run.f90  cont_rel_util.o met_fi.o \
	  SCIPresults_fd.o scipuff_fi.o SWIMparam_fd.o

step_pool.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_pool.f90  scipuff_fi.o \
	  cont_rel_fi.o cont_rel_util.o struct_fd.o met_fi.o param_fd.o \
	  srfevap_fi.o utilmtlaux.o

cont_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_dyn.f90  constants_fd.o \
	  cont_rel_fi.o cont_rel_util.o default_fd.o error_fi.o files_fi.o \
	  inter_fi.o met_fi.o param_fd.o step_pool.o scipuff_fi.o \
	  srfevap_fi.o

chemReactions.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/chemReactions.f90  AqAerIface.o \
	  chem_fi.o constants_fd.o default_fd.o error_fi.o met_fi.o \
	  reallocate.o scipuff_fi.o $(INCMOD)

set_tlev.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_tlev.f90  cont_rel_util.o \
	  scipuff_fi.o

SWIMpuff_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMpuff_fd.f90 

srf_evap.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/srf_evap.f90  scipuff_fi.o \
	  constants_fd.o default_fd.o error_fi.o files_fi.o met_fi.o \
	  struct_fd.o sagstr_fd.o SCIPresults_fd.o srfevap_fi.o srfparam_fd.o \
	  step_p_fi.o surface_fi.o SWIMparam_fd.o SWIMpuff_fd.o

SamplerGridOutput.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerGridOutput.f90  \
	  error_fi.o files_fi.o sampler_fi.o scipuff_fi.o utilmtlaux.o

SamplerBinaryOut.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerBinaryOut.f90  \
	  error_fi.o files_fi.o met_fi.o sampler_fi.o SamplerGridOutput.o \
	  scipuff_fi.o

cont_rel_static.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_static.f90  \
	  cont_rel_fi.o cont_rel_util.o default_fd.o error_fi.o files_fi.o \
	  inter_fi.o met_fi.o struct_fd.o scipuff_fi.o step_p_fi.o \
	  surface_fi.o

get_topogSTUB.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_topogSTUB.f90  basic_fd.o \
	  constants_fd.o coordinate_fd.o met_fi.o metparam_fd.o \
	  SWIMgridStr_fd.o

get_topSWIM.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_topSWIM.f90  met_fi.o \
	  SCIPresults_fd.o

output.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/output.f90  cont_rel_fi.o \
	  cont_rel_util.o error_fi.o files_fi.o met_fi.o scipuff_fi.o start.o \
	  SWIMparam_fd.o

scipuff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/scipuff.f90  adjoint_fi.o chem_fi.o \
	  cont_rel_util.o files_fi.o met_fi.o MPIFunc_fi.o sagdef_fd.o \
	  sampler_fi.o SCIPresults_fd.o scipuff_fi.o surface_fi.o \
	  SWIMparam_fd.o $(INCMOD)

util_met.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_met.f90  coordinate_fd.o \
	  error_fi.o met_fi.o scipuff_fi.o

SamplerUtil.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerUtil.f90  chem_fi.o \
	  constants_fd.o coordinate_fd.o default_fd.o error_fi.o files_fi.o \
	  sampler_fd.o met_fi.o sampler_fi.o SamplerGridOutput.o scipuff_fi.o \
	  utilmtlaux.o $(INCMOD)

SamplerInputUtil.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerInputUtil.f90  \
	  files_fi.o sampler_fd.o met_fi.o reallocate.o sagdef_fd.o \
	  sampler_fi.o SamplerUtil.o scipuff_fi.o srfparam_fd.o surface_fi.o \
	  utilmtlaux.o $(INCMOD)

SamplerInput.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SamplerInput.f90  files_fi.o \
	  sampler_fd.o met_fi.o SamplerInputUtil.o reallocate.o sampler_fi.o \
	  SamplerGridOutput.o SamplerUtil.o scipuff_fi.o surface_fi.o \
	  utilmtlaux.o $(INCMOD)

error.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/error.f90  error_fi.o

refl_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/refl_fi.f90 

reflect.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/reflect.f90  error_fi.o files_fi.o \
	  refl_fi.o struct_fd.o

rebin.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/rebin.f90 

set_grid.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_grid.f90  files_fi.o met_fi.o \
	  scipuff_fi.o SWIMparam_fd.o

ufall.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/ufall.f90  constants_fd.o

time_cnv.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/time_cnv.f90  scipuff_fi.o

sciprime_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/sciprime_fi.f90  param_fd.o

set_stack_rel_prise.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prise.f90  \
	  files_fi.o met_fi.o sciprime_fi.o scipuff_fi.o

MPIFunc.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/MPIFunc.f90  charT_fd.o chem_fi.o \
	  MPIFunc_fi.o files_fi.o localMPI.o message_fd.o scipuff_fi.o $(INCMOD)

util_evap.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_evap.f90  files_fi.o \
	  scipuff_fi.o srfevap_fi.o

get_metSWIM.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_metSWIM.f90  constants_fd.o \
	  met_fi.o param_fd.o SCIPresults_fd.o scipuff_fi.o step_p_fi.o \
	  SWIMparam_fd.o SWIMpuff_fd.o

SetBotCells.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/SetBotCells.f90  sagcel_fd.o \
	  sagdef_fd.o sagstr_fd.o

vd_slinn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/vd_slinn.f90  scipuff_fi.o

util_matl.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_matl.f90  class_fd.o \
	  error_fi.o files_fi.o param_fd.o scipuff_fi.o struct_fd.o \
	  utilmtlaux.o

utilchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/utilchem.f90  chem_fi.o \
	  chemReactions.o default_fd.o error_fi.o files_fi.o scipuff_fi.o \
	  met_fi.o multcomp_fd.o reallocate.o struct_fd.o $(INCMOD)

init_srf.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_srf.f90  chem_fi.o error_fi.o \
	  files_fi.o saglst_fd.o reallocate.o sagdef_fd.o sagerr_fd.o \
	  sagstr_fd.o scipuff_fi.o srfaux_fi.o srfparam_fd.o struct_fd.o \
	  surface_fi.o utilmtlaux.o $(INCMOD)

wrt_prj.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/wrt_prj.f90  adjoint_fi.o files_fi.o \
	  met_fi.o scipuff_fi.o srfevap_fi.o

version.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/version.f90 

initial.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/initial.f90  adjoint_fi.o chem_fi.o \
	  cont_rel_fi.o cont_rel_util.o error_fi.o files_fi.o inter_fi.o \
	  landuse_fd.o scipuff_fi.o met_fi.o saglst_fd.o sagdef_fd.o \
	  sampler_fi.o SCIPresults_fd.o srfparam_fd.o step_p_fi.o \
	  srfevap_fi.o surface_fi.o SWIMparam_fd.o $(INCMOD)

structv04_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/structv04_fd.f90 

structv13_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/structv13_fd.f90 

read_prj.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/read_prj.f90  adjoint_fi.o \
	  scipuff_fi.o chem_fi.o error_fi.o files_fi.o met_fi.o MPIFunc_fi.o \
	  structv04_fd.o structv13_fd.o srfevap_fi.o SWIMparam_fd.o \
	  utilmtlaux.o $(INCMOD)

step_p_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_p_dyn.f90  chem_fi.o \
	  constants_fd.o files_fi.o met_fi.o MPIFunc_fi.o saglst_fd.o \
	  sagdef_fd.o scipuff_fi.o srfaux_fi.o step_p_fi.o struct_fd.o \
	  surface_fd.o surface_fi.o SWIMparam_fd.o utilmtlaux.o util_srf.o $(INCMOD)

util_inter.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_inter.f90  files_fi.o \
	  inter_fi.o refl_fi.o scipuff_fi.o struct_fd.o

init_wash.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_wash.f90  chem_fi.o \
	  struct_fd.o param_fd.o scipuff_fi.o $(INCMOD)

ambchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/ambchem.f90  AqAerIface.o chem_fi.o \
	  coordinate_fd.o default_fd.o error_fi.o files_fi.o met_fi.o \
	  scipuff_fi.o $(INCMOD)

los_sensor.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/los_sensor.f90  chem_fi.o \
	  constants_fd.o sampler_fd.o met_fi.o scipuff_fi.o struct_fd.o $(INCMOD)

puff_file.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/puff_file.f90  cont_rel_fi.o \
	  cont_rel_util.o error_fi.o files_fi.o plotlist_fd.o plotlist_fi.o \
	  scipuff_fi.o struct_fd.o SCIPresults_fd.o spcstruct_fd.o \
	  SWIMgridStr_fd.o time_fd.o

step.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step.f90  chem_fi.o cont_rel_fi.o \
	  cont_rel_util.o error_fi.o files_fi.o scipuff_fi.o met_fi.o \
	  MPIFunc_fi.o sampler_fi.o sciprime_fi.o srfparam_fd.o step_p_fi.o \
	  surface_fi.o $(INCMOD)

step_drop.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/step_drop.f90  constants_fd.o \
	  met_fi.o scipuff_fi.o step_p_fi.o utilmtlaux.o

SWIMinit_fd.o:$(BD)/dll/SCIPUFF/SWIM/inc/SWIMinit_fd.f90  param_fd.o

init_met.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/init_met.f90  charT_fd.o \
	  constants_fd.o datums_mod.o default_fd.o param_fd.o error_fi.o \
	  files_fi.o landuse_fd.o message_fd.o met_fi.o SCIPresults_fd.o \
	  scipuff_fi.o SWIMinit_fd.o SWIMparam_fd.o

get_matl.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/get_matl.f90  scipuff_fi.o \
	  utilmtlaux.o

surface.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/surface.f90  accumsrf.o chem_fi.o \
	  constants_fd.o error_fi.o files_fi.o scipuff_fi.o saglst_fd.o \
	  refl_fi.o sagdef_fd.o sagstr_fd.o srfaux_fi.o srfdos_fd.o \
	  srfparam_fd.o struct_fd.o surface_fd.o surface_fi.o utilmtlaux.o \
	  util_srf.o $(INCMOD)

restart.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/restart.f90  cont_rel_fi.o error_fi.o \
	  files_fi.o met_fi.o saglst_fd.o sagdef_fd.o scipuff_fi.o \
	  surface_fi.o

mapfac.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/mapfac.f90  scipuff_fi.o \
	  SWIMparam_fd.o

StepChemMPI.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/StepChemMPI.f90  AqAerIface.o \
	  charT_fd.o chem_fi.o error_fi.o files_fi.o localMPI.o message_fd.o \
	  met_fi.o MPIFunc_fi.o scipuff_fi.o step_p_fi.o surface_fi.o $(INCMOD)

settle.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/settle.f90  refl_fi.o scipuff_fi.o

aerosol_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/aerosol_fi.f90  constants_fd.o

aerosol.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/aerosol.f90  aerosol_fi.o \
	  constants_fd.o error_fi.o met_fi.o scipuff_fi.o

inst_rel_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inst_rel_dyn.f90  param_fd.o \
	  files_fi.o relparam_fd.o scipuff_fi.o

modules.o:$(BD)/lib/PRIME/modules.f 

set_stack_rel_prime.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_stack_rel_prime.f90  \
	  constants_fd.o error_fi.o files_fi.o modules.o met_fi.o \
	  sampler_fi.o sciprime_fi.o scipuff_fi.o

splitz.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/splitz.f90  constants_fd.o files_fi.o \
	  scipuff_fi.o step_p_fi.o

util_puff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_puff.f90  cont_rel_fi.o \
	  cont_rel_util.o error_fi.o files_fi.o struct_fd.o scipuff_fi.o \
	  utilmtlaux.o

util_is.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util_is.f90  class_fd.o scipuff_fi.o

cont_rel_init.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/cont_rel_init.f90  \
	  cont_rel_fi.o cont_rel_util.o default_fd.o error_fi.o files_fi.o \
	  met_fi.o param_fd.o relparam_fd.o scipuff_fi.o srfevap_fi.o \
	  utilmtlaux.o

sampler.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/sampler.f90  aerosol_fi.o \
	  AqAerIface.o chem_fi.o constants_fd.o error_fi.o field_fd.o \
	  files_fi.o sampler_fd.o met_fi.o plotlist_fi.o saglst_fd.o \
	  sagcnt_fd.o sagdef_fd.o sagerr_fd.o sagstr_fd.o sagtri_fd.o \
	  sampler_fi.o SamplerGridOutput.o SamplerUtil.o sciprime_fi.o \
	  scipuff_fi.o slice_fd.o srfparam_fd.o surface_fi.o utilmtlaux.o $(INCMOD)

stepchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/stepchem.f90  AqAerIface.o chem_fi.o \
	  error_fi.o files_fi.o met_fi.o MPIFunc_fi.o scipuff_fi.o \
	  srfdos_fd.o step_p_fi.o $(INCMOD)

set_ip.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/set_ip.f90  scipuff_fi.o

util.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/util.f90  default_fd.o error_fi.o \
	  scipuff_fi.o struct_fd.o

deallocate.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/deallocate.f90  adjoint_fi.o \
	  cont_rel_fi.o cont_rel_util.o ipgrd_fi.o scipuff_fi.o met_fi.o \
	  relcheck_fi.o sagdef_fd.o sampler_fd.o sampler_fi.o sciprime_fi.o \
	  srfaux_fi.o surface_fi.o

interchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/interchem.f90  AqAerIface.o \
	  chem_fi.o error_fi.o inter_fi.o sampler_fd.o scipuff_fi.o $(INCMOD)

sat_humid.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/sat_humid.f90  constants_fd.o

inter_dyn.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inter_dyn.f90  chem_fi.o error_fi.o \
	  inter_fi.o scipuff_fi.o $(INCMOD)

stepChemAmb.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/stepChemAmb.f90  AqAerIface.o \
	  chem_fi.o coordinate_fd.o default_fd.o error_fi.o files_fi.o \
	  met_fi.o scipuff_fi.o $(INCMOD)

merge.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/merge.f90  scipuff_fi.o inter_fi.o \
	  struct_fd.o

initchem.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/initchem.f90  chem_fi.o \
	  chemReactions.o class_fd.o error_fi.o files_fi.o landuse_fd.o \
	  scipuff_fi.o MPIFunc_fi.o multcomp_fd.o reallocate.o $(INCMOD)

inp_puff.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inp_puff.f90  files_fi.o \
	  scipuff_fi.o srfparam_fd.o struct_fd.o surface_fd.o utilmtlaux.o

halt.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/halt.f90  basic_fd.o scipuff_fi.o

search_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/search_fi.f90  search_fd.o


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
	  uniformgridt_fd.mod special_rst_fi.mod chemreactions_fd.mod \
	  accumsrf.mod createstatics.mod stepscipuff_fi.mod pool_fi.mod \
	  botcells_fi.mod samplergridouput.mod parseline_fi.mod \
	  cont_rel_functions.mod samplerutil.mod utilmtlaux.mod \
	  oldpuff_fi.mod utilsrf.mod error_fi.mod metparam_fd.mod \
	  errorparam_fd.mod files_fi.mod mapcoord_fd.mod relparam_fd.mod \
	  met_fi.mod surface_fi.mod srfevap2d_fd.mod srfevap_fi.mod \
	  substrate_fi.mod cont_rel_fd.mod cont_rel_fi.mod \
	  surface_dose_fd.mod surface_fd.mod srfdos_fd.mod structv13_fd.mod \
	  chem_fi.mod diagnostics_fi.mod structv04_fd.mod srfparam_fd.mod \
	  class_fd.mod srfaux_fi.mod ipgrd_fi.mod inter_fi.mod search_fi.mod \
	  dezone_fi.mod basic_fi.mod flags_fi.mod grid_fi.mod matl_fi.mod \
	  nextrel_fi.mod project_fi.mod puff_fi.mod scipuff_fi.mod \
	  scnrel_fi.mod time_fi.mod tlev_fi.mod type_fi.mod search_fd.mod \
	  los_fd.mod sampler_fd.mod sciprime_fi.mod constants_fd.mod \
	  aerosol_fi.mod refl_fi.mod sampler_fi.mod default_fd.mod \
	  multcomp_fd.mod swimgridstr_fd.mod swimparam_fd.mod landuse_fd.mod \
	  swimpuff_fd.mod swiminit_fd.mod main1.mod prime_ambient.mod \
	  prime_dfsn.mod prime_numparm.mod prime_params.mod prime_plu.mod \
	  prime_wakedat.mod