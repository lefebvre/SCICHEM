# Main program is libsag.a

PROG =	libsag.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/util/SAGlib/sag_skip.f90 lib/util/SAGlib/sag_fill.f90 \
	  lib/util/SAGlib/sag_find.f90 lib/util/SAGlib/sag_func.f90 \
	  lib/util/SAGlib/sag_cntr.f90 lib/util/SAGlib/sag_comb.f90 \
	  lib/util/SAGlib/sag_read.f90 lib/util/SAGlib/sag_util.f90 \
	  lib/util/SAGlib/sag_open.f90 lib/util/SAGlib/sag_err.f90 \
	  lib/util/SAGlib/sag_conn.f90 lib/util/SAGlib/sag_val.f90 \
	  lib/util/SAGlib/sag_draw.f90 lib/util/SAGlib/sag_node.f90 \
	  lib/util/SAGlib/sag_grad.f90 lib/util/SAGlib/sag_tri.f90 \
	  lib/util/SAGlib/sag_botm.f90 lib/util/SAGlib/sag_cell.f90 \
	  lib/util/SAGlib/sag_init.f90 lib/util/SAGlib/sag_dzon.f90 \
	  lib/util/SAGlib/sag_back.f90 lib/util/SAGlib/sag_wrt.f90 \
	  lib/util/SAGlib/inc/sagcnt_fd.f90 lib/util/SAGlib/inc/sagdrw_fd.f90 \
	  lib/util/SAGlib/inc/sagdef_fd.f90 lib/util/SAGlib/inc/sagwrt_fd.f90 \
	  lib/util/SAGlib/inc/sagerr_fi.f90 lib/util/SAGlib/inc/sagtri_fd.f90 \
	  lib/util/SAGlib/inc/saglst_fd.f90 lib/util/SAGlib/inc/sagcel_fd.f90 \
	  lib/util/SAGlib/inc/saggrd_fi.f90 \
	  lib/util/SAGlib/inc/sagfun_usr.f90 \
	  lib/util/SAGlib/inc/sagstr_fd.f90 lib/util/SAGlib/inc/sagbck_fi.f90 \
	  lib/util/SAGlib/inc/sagnod_fd.f90 lib/util/SAGlib/inc/sagfil_fd.f90 \
	  lib/util/SAGlib/inc/sagerr_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

OBJS :=  $(OBJS_f90) 

DIRS = lib/util/SAGlib/inc lib/SCIPUFFlib/SCIPUFF/inc lib/util/SAGlib

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


lib/util/SAGlib/inc/sagdef_fd.o:$(BD)/lib/util/SAGlib/inc/sagdef_fd.f90 

lib/util/SAGlib/inc/sagerr_fd.o:$(BD)/lib/util/SAGlib/inc/sagerr_fd.f90 

lib/util/SAGlib/inc/sagerr_fi.o:$(BD)/lib/util/SAGlib/inc/sagerr_fi.f90 

lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.f90 

lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/param_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.o

lib/util/SAGlib/inc/sagstr_fd.o:$(BD)/lib/util/SAGlib/inc/sagstr_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/util/SAGlib/sag_skip.o:$(BD)/lib/util/SAGlib/sag_skip.f90  \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/inc/sagdrw_fd.o:$(BD)/lib/util/SAGlib/inc/sagdrw_fd.f90 

lib/util/SAGlib/inc/sagcnt_fd.o:$(BD)/lib/util/SAGlib/inc/sagcnt_fd.f90 

lib/util/SAGlib/inc/sagnod_fd.o:$(BD)/lib/util/SAGlib/inc/sagnod_fd.f90 

lib/util/SAGlib/inc/sagwrt_fd.o:$(BD)/lib/util/SAGlib/inc/sagwrt_fd.f90 

lib/util/SAGlib/inc/sagfun_usr.o:$(BD)/lib/util/SAGlib/inc/sagfun_usr.f90  \
	  lib/util/SAGlib/inc/sagcnt_fd.o lib/util/SAGlib/inc/sagdrw_fd.o \
	  lib/util/SAGlib/inc/sagnod_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagwrt_fd.o

lib/util/SAGlib/inc/sagfil_fd.o:$(BD)/lib/util/SAGlib/inc/sagfil_fd.f90  \
	  lib/util/SAGlib/inc/sagnod_fd.o

lib/util/SAGlib/inc/saggrd_fi.o:$(BD)/lib/util/SAGlib/inc/saggrd_fi.f90 

lib/util/SAGlib/sag_fill.o:$(BD)/lib/util/SAGlib/sag_fill.f90  \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagdrw_fd.o \
	  lib/util/SAGlib/inc/sagfun_usr.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagfil_fd.o \
	  lib/util/SAGlib/inc/saggrd_fi.o lib/util/SAGlib/inc/sagnod_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/inc/sagtri_fd.o:$(BD)/lib/util/SAGlib/inc/sagtri_fd.f90  \
	  lib/util/SAGlib/inc/sagnod_fd.o

lib/util/SAGlib/inc/saglst_fd.o:$(BD)/lib/util/SAGlib/inc/saglst_fd.f90  \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_find.o:$(BD)/lib/util/SAGlib/sag_find.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/sag_skip.o lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/inc/sagcel_fd.o:$(BD)/lib/util/SAGlib/inc/sagcel_fd.f90 

lib/util/SAGlib/sag_func.o:$(BD)/lib/util/SAGlib/sag_func.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/util/SAGlib/inc/sagcel_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagfil_fd.o lib/util/SAGlib/inc/saggrd_fi.o \
	  lib/util/SAGlib/inc/sagnod_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_cntr.o:$(BD)/lib/util/SAGlib/sag_cntr.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcnt_fd.o \
	  lib/util/SAGlib/inc/sagfun_usr.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagdrw_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/saggrd_fi.o \
	  lib/util/SAGlib/inc/sagnod_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o lib/util/SAGlib/inc/sagwrt_fd.o

lib/util/SAGlib/sag_comb.o:$(BD)/lib/util/SAGlib/sag_comb.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_read.o:$(BD)/lib/util/SAGlib/sag_read.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_util.o:$(BD)/lib/util/SAGlib/sag_util.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/saggrd_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_open.o:$(BD)/lib/util/SAGlib/sag_open.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_err.o:$(BD)/lib/util/SAGlib/sag_err.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o

lib/util/SAGlib/sag_conn.o:$(BD)/lib/util/SAGlib/sag_conn.f90  \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_val.o:$(BD)/lib/util/SAGlib/sag_val.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_draw.o:$(BD)/lib/util/SAGlib/sag_draw.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcnt_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagdrw_fd.o \
	  lib/util/SAGlib/inc/sagfun_usr.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagfil_fd.o \
	  lib/util/SAGlib/inc/sagnod_fd.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_node.o:$(BD)/lib/util/SAGlib/sag_node.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagnod_fd.o \
	  lib/util/SAGlib/inc/sagfun_usr.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_grad.o:$(BD)/lib/util/SAGlib/sag_grad.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/saggrd_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_tri.o:$(BD)/lib/util/SAGlib/sag_tri.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagnod_fd.o \
	  lib/util/SAGlib/inc/sagstr_fd.o lib/util/SAGlib/inc/sagtri_fd.o

lib/util/SAGlib/sag_botm.o:$(BD)/lib/util/SAGlib/sag_botm.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcel_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/saggrd_fi.o \
	  lib/util/SAGlib/inc/sagfun_usr.o lib/util/SAGlib/inc/sagstr_fd.o $(INCMOD)

lib/util/SAGlib/sag_cell.o:$(BD)/lib/util/SAGlib/sag_cell.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagfun_usr.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_init.o:$(BD)/lib/util/SAGlib/sag_init.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/param_fd.o

lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.o

lib/util/SAGlib/sag_dzon.o:$(BD)/lib/util/SAGlib/sag_dzon.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.o \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagdef_fd.o \
	  lib/util/SAGlib/inc/sagerr_fd.o lib/util/SAGlib/inc/sagerr_fi.o \
	  lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/inc/sagbck_fi.o:$(BD)/lib/util/SAGlib/inc/sagbck_fi.f90 

lib/util/SAGlib/sag_back.o:$(BD)/lib/util/SAGlib/sag_back.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagbck_fi.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagstr_fd.o

lib/util/SAGlib/sag_wrt.o:$(BD)/lib/util/SAGlib/sag_wrt.f90  \
	  lib/util/SAGlib/inc/saglst_fd.o lib/util/SAGlib/inc/sagcnt_fd.o \
	  lib/util/SAGlib/inc/sagdef_fd.o lib/util/SAGlib/inc/sagerr_fd.o \
	  lib/util/SAGlib/inc/sagerr_fi.o lib/util/SAGlib/inc/sagstr_fd.o \
	  lib/util/SAGlib/inc/sagtri_fd.o lib/util/SAGlib/inc/sagfun_usr.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  sagskip.mod sagcnt_fd.mod sagdrw_fd.mod \
	  sagdef_fd.mod sagwrt_fd.mod sagerr_fi.mod sagtri_fd.mod \
	  ptrgrdstritf.mod saglst_fd.mod sagcel_fd.mod saggrd_fi.mod \
	  getpointstd_usr.mod sagadd_usr.mod sagcnt_usr.mod sagdrw_usr.mod \
	  sagmaxval_usr.mod sagmrg_usr.mod sagnod_usr.mod sagpop_fi.mod \
	  sagwrt_usr.mod sagstr_fd.mod sagbck_fi.mod sagnod_fd.mod \
	  sagfil_fd.mod sagerr_fd.mod defsize_fd.mod mode_fd.mod \
	  obsolete_fd.mod oprel_fd.mod param_fd.mod precip_fd.mod puff_fd.mod \
	  scipuff_fd.mod coordinate_fd.mod grdlock_fi.mod step_p_fi.mod \
	  matlstruct_fd.mod mauxstruct_fd.mod mcstruct_fd.mod \
	  pauxstruct_fd.mod puffstruct_fd.mod releaseid_fd.mod struct_fd.mod \
	  typestruct_fd.mod