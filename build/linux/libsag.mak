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

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


sagdef_fd.o:$(BD)/lib/util/SAGlib/inc/sagdef_fd.f90 

sagerr_fd.o:$(BD)/lib/util/SAGlib/inc/sagerr_fd.f90 

sagerr_fi.o:$(BD)/lib/util/SAGlib/inc/sagerr_fi.f90 

coordinate_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/coordinate_fd.f90 

param_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/param_fd.f90  coordinate_fd.o

sagstr_fd.o:$(BD)/lib/util/SAGlib/inc/sagstr_fd.f90  param_fd.o

sag_skip.o:$(BD)/lib/util/SAGlib/sag_skip.f90  sagdef_fd.o sagerr_fd.o \
	  sagerr_fi.o sagstr_fd.o

sagdrw_fd.o:$(BD)/lib/util/SAGlib/inc/sagdrw_fd.f90 

sagcnt_fd.o:$(BD)/lib/util/SAGlib/inc/sagcnt_fd.f90 

sagnod_fd.o:$(BD)/lib/util/SAGlib/inc/sagnod_fd.f90 

sagwrt_fd.o:$(BD)/lib/util/SAGlib/inc/sagwrt_fd.f90 

sagfun_usr.o:$(BD)/lib/util/SAGlib/inc/sagfun_usr.f90  sagcnt_fd.o \
	  sagdrw_fd.o sagnod_fd.o sagstr_fd.o sagwrt_fd.o

sagfil_fd.o:$(BD)/lib/util/SAGlib/inc/sagfil_fd.f90  sagnod_fd.o

saggrd_fi.o:$(BD)/lib/util/SAGlib/inc/saggrd_fi.f90 

sag_fill.o:$(BD)/lib/util/SAGlib/sag_fill.f90  sagdef_fd.o sagdrw_fd.o \
	  sagfun_usr.o sagerr_fd.o sagerr_fi.o sagfil_fd.o saggrd_fi.o \
	  sagnod_fd.o sagstr_fd.o

sagtri_fd.o:$(BD)/lib/util/SAGlib/inc/sagtri_fd.f90  sagnod_fd.o

saglst_fd.o:$(BD)/lib/util/SAGlib/inc/saglst_fd.f90  sagstr_fd.o sagtri_fd.o

sag_find.o:$(BD)/lib/util/SAGlib/sag_find.f90  saglst_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o sag_skip.o sagstr_fd.o

sagcel_fd.o:$(BD)/lib/util/SAGlib/inc/sagcel_fd.f90 

sag_func.o:$(BD)/lib/util/SAGlib/sag_func.f90  saglst_fd.o sagfun_usr.o \
	  sagcel_fd.o sagdef_fd.o sagerr_fd.o sagerr_fi.o sagfil_fd.o \
	  saggrd_fi.o sagnod_fd.o sagstr_fd.o sagtri_fd.o

sag_cntr.o:$(BD)/lib/util/SAGlib/sag_cntr.f90  saglst_fd.o sagcnt_fd.o \
	  sagfun_usr.o sagdef_fd.o sagdrw_fd.o sagerr_fd.o sagerr_fi.o \
	  saggrd_fi.o sagnod_fd.o sagstr_fd.o sagtri_fd.o sagwrt_fd.o

sag_comb.o:$(BD)/lib/util/SAGlib/sag_comb.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagfun_usr.o sagstr_fd.o

sag_read.o:$(BD)/lib/util/SAGlib/sag_read.f90  saglst_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o sagstr_fd.o

sag_util.o:$(BD)/lib/util/SAGlib/sag_util.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o saggrd_fi.o sagstr_fd.o \
	  sagtri_fd.o

sag_open.o:$(BD)/lib/util/SAGlib/sag_open.f90  saglst_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o sagstr_fd.o

sag_err.o:$(BD)/lib/util/SAGlib/sag_err.f90  param_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o

sag_conn.o:$(BD)/lib/util/SAGlib/sag_conn.f90  sagdef_fd.o sagerr_fd.o \
	  sagerr_fi.o sagtri_fd.o

sag_val.o:$(BD)/lib/util/SAGlib/sag_val.f90  saglst_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o sagstr_fd.o

sag_draw.o:$(BD)/lib/util/SAGlib/sag_draw.f90  saglst_fd.o sagcnt_fd.o \
	  sagdef_fd.o sagdrw_fd.o sagfun_usr.o sagerr_fd.o sagerr_fi.o \
	  sagfil_fd.o sagnod_fd.o sagstr_fd.o sagtri_fd.o

sag_node.o:$(BD)/lib/util/SAGlib/sag_node.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagnod_fd.o sagfun_usr.o \
	  sagstr_fd.o sagtri_fd.o

sag_grad.o:$(BD)/lib/util/SAGlib/sag_grad.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o saggrd_fi.o sagstr_fd.o

sag_tri.o:$(BD)/lib/util/SAGlib/sag_tri.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagnod_fd.o sagstr_fd.o \
	  sagtri_fd.o

sag_botm.o:$(BD)/lib/util/SAGlib/sag_botm.f90  saglst_fd.o sagcel_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o saggrd_fi.o sagfun_usr.o \
	  sagstr_fd.o $(INCMOD)

sag_cell.o:$(BD)/lib/util/SAGlib/sag_cell.f90  saglst_fd.o sagfun_usr.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagstr_fd.o

sag_init.o:$(BD)/lib/util/SAGlib/sag_init.f90  saglst_fd.o sagdef_fd.o \
	  sagerr_fd.o sagerr_fi.o sagstr_fd.o

struct_fd.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/struct_fd.f90  param_fd.o

step_p_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/step_p_fi.f90  struct_fd.o

sag_dzon.o:$(BD)/lib/util/SAGlib/sag_dzon.f90  step_p_fi.o saglst_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagstr_fd.o

sagbck_fi.o:$(BD)/lib/util/SAGlib/inc/sagbck_fi.f90 

sag_back.o:$(BD)/lib/util/SAGlib/sag_back.f90  saglst_fd.o sagbck_fi.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagstr_fd.o

sag_wrt.o:$(BD)/lib/util/SAGlib/sag_wrt.f90  saglst_fd.o sagcnt_fd.o \
	  sagdef_fd.o sagerr_fd.o sagerr_fi.o sagstr_fd.o sagtri_fd.o \
	  sagfun_usr.o


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