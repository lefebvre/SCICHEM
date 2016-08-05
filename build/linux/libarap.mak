# Main program is libarap.a

PROG =	libarap.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/util/ARAPlib/tqlrat.f90 lib/util/ARAPlib/get_next_data.f90 \
	  lib/util/ARAPlib/trbak3.f90 lib/util/ARAPlib/rfftb1.f90 \
	  lib/util/ARAPlib/vsort.f90 lib/util/ARAPlib/julian.f90 \
	  lib/util/ARAPlib/interp.f90 lib/util/ARAPlib/getfile.f90 \
	  lib/util/ARAPlib/reallocate.f90 lib/util/ARAPlib/rfftf.f90 \
	  lib/util/ARAPlib/radbg.f90 lib/util/ARAPlib/matinv.f90 \
	  lib/util/ARAPlib/rfftf1.f90 lib/util/ARAPlib/nblank.f90 \
	  lib/util/ARAPlib/radb2.f90 lib/util/ARAPlib/erfci.f90 \
	  lib/util/ARAPlib/radf4.f90 lib/util/ARAPlib/cupper.f90 \
	  lib/util/ARAPlib/rffti.f90 lib/util/ARAPlib/clower.f90 \
	  lib/util/ARAPlib/interp_2d.f90 lib/util/ARAPlib/erfc.f90 \
	  lib/util/ARAPlib/tred3.f90 lib/util/ARAPlib/fzero.f90 \
	  lib/util/ARAPlib/lognorm.f90 lib/util/ARAPlib/MapProjections.f90 \
	  lib/util/ARAPlib/matchstr.f90 lib/util/ARAPlib/heavi.f90 \
	  lib/util/ARAPlib/locate.f90 lib/util/ARAPlib/untabify.f90 \
	  lib/util/ARAPlib/radfg.f90 lib/util/ARAPlib/radb3.f90 \
	  lib/util/ARAPlib/tom624.f90 lib/util/ARAPlib/clnpar.f90 \
	  lib/util/ARAPlib/erfcinv.f90 lib/util/ARAPlib/pythag.f90 \
	  lib/util/ARAPlib/radf2.f90 lib/util/ARAPlib/sspev.f90 \
	  lib/util/ARAPlib/cfft.f90 lib/util/ARAPlib/rffti1.f90 \
	  lib/util/ARAPlib/rfftb.f90 lib/util/ARAPlib/imtql2.f90 \
	  lib/util/ARAPlib/getvalue.f90 lib/util/ARAPlib/radf3.f90 \
	  lib/util/ARAPlib/trig_degrees.f90 lib/util/ARAPlib/radb5.f90 \
	  lib/util/ARAPlib/radb4.f90 lib/util/ARAPlib/MatRot.f90 \
	  lib/util/ARAPlib/radf5.f90 lib/util/ARAPlib/deblank.f90 \
	  lib/util/ARAPlib/random.f90 lib/util/ARAPlib/cformat.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

SRCS_f = lib/util/ARAPlib/r1mpyq.f lib/util/ARAPlib/fdjac1.f \
	  lib/util/ARAPlib/dpmpar.f lib/util/ARAPlib/dogleg.f \
	  lib/util/ARAPlib/enorm.f lib/util/ARAPlib/hybrd.f \
	  lib/util/ARAPlib/qform.f lib/util/ARAPlib/r1updt.f \
	  lib/util/ARAPlib/qrfac.f

OBJS_f := $(notdir $(subst .f,.o,$(SRCS_f)))

SRCS_c = lib/util/ARAPlib/C2Fortran.c

OBJS_c := $(notdir $(subst .c,.o,$(SRCS_c)))

OBJS :=  $(OBJS_f90)  $(OBJS_f)  $(OBJS_c) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 

$(OBJS_f): %.o:$(filter /\%.f,$(SRCS_f))
	$(F77) $(F77FLAGS) $< 

$(OBJS_c): %.o:$(filter /\%.c,$(SRCS_c))
	$(CC) $(CCFLAGS) $< 

tqlrat.o:$(BD)/lib/util/ARAPlib/tqlrat.f90 

get_next_data.o:$(BD)/lib/util/ARAPlib/get_next_data.f90 

trbak3.o:$(BD)/lib/util/ARAPlib/trbak3.f90 

rfftb1.o:$(BD)/lib/util/ARAPlib/rfftb1.f90 

vsort.o:$(BD)/lib/util/ARAPlib/vsort.f90 

julian.o:$(BD)/lib/util/ARAPlib/julian.f90 

interp.o:$(BD)/lib/util/ARAPlib/interp.f90 

getfile.o:$(BD)/lib/util/ARAPlib/getfile.f90 

r1mpyq.o:$(BD)/lib/util/ARAPlib/r1mpyq.f 

reallocate.o:$(BD)/lib/util/ARAPlib/reallocate.f90 

rfftf.o:$(BD)/lib/util/ARAPlib/rfftf.f90 

radbg.o:$(BD)/lib/util/ARAPlib/radbg.f90 

matinv.o:$(BD)/lib/util/ARAPlib/matinv.f90 

rfftf1.o:$(BD)/lib/util/ARAPlib/rfftf1.f90 

nblank.o:$(BD)/lib/util/ARAPlib/nblank.f90 

radb2.o:$(BD)/lib/util/ARAPlib/radb2.f90 

erfci.o:$(BD)/lib/util/ARAPlib/erfci.f90 

radf4.o:$(BD)/lib/util/ARAPlib/radf4.f90 

cupper.o:$(BD)/lib/util/ARAPlib/cupper.f90 

rffti.o:$(BD)/lib/util/ARAPlib/rffti.f90 

clower.o:$(BD)/lib/util/ARAPlib/clower.f90 

interp_2d.o:$(BD)/lib/util/ARAPlib/interp_2d.f90 

fdjac1.o:$(BD)/lib/util/ARAPlib/fdjac1.f 

erfc.o:$(BD)/lib/util/ARAPlib/erfc.f90 

tred3.o:$(BD)/lib/util/ARAPlib/tred3.f90 

fzero.o:$(BD)/lib/util/ARAPlib/fzero.f90 

lognorm.o:$(BD)/lib/util/ARAPlib/lognorm.f90 

MapProjections.o:$(BD)/lib/util/ARAPlib/MapProjections.f90 

dpmpar.o:$(BD)/lib/util/ARAPlib/dpmpar.f 

matchstr.o:$(BD)/lib/util/ARAPlib/matchstr.f90 

dogleg.o:$(BD)/lib/util/ARAPlib/dogleg.f 

C2Fortran.o:$(BD)/lib/util/ARAPlib/C2Fortran.c 

heavi.o:$(BD)/lib/util/ARAPlib/heavi.f90 

locate.o:$(BD)/lib/util/ARAPlib/locate.f90 

untabify.o:$(BD)/lib/util/ARAPlib/untabify.f90 

radfg.o:$(BD)/lib/util/ARAPlib/radfg.f90 

radb3.o:$(BD)/lib/util/ARAPlib/radb3.f90 

enorm.o:$(BD)/lib/util/ARAPlib/enorm.f 

tom624.o:$(BD)/lib/util/ARAPlib/tom624.f90 

clnpar.o:$(BD)/lib/util/ARAPlib/clnpar.f90 

erfcinv.o:$(BD)/lib/util/ARAPlib/erfcinv.f90 

pythag.o:$(BD)/lib/util/ARAPlib/pythag.f90 

radf2.o:$(BD)/lib/util/ARAPlib/radf2.f90 

sspev.o:$(BD)/lib/util/ARAPlib/sspev.f90 

cfft.o:$(BD)/lib/util/ARAPlib/cfft.f90 

rffti1.o:$(BD)/lib/util/ARAPlib/rffti1.f90 

rfftb.o:$(BD)/lib/util/ARAPlib/rfftb.f90 

imtql2.o:$(BD)/lib/util/ARAPlib/imtql2.f90 

getvalue.o:$(BD)/lib/util/ARAPlib/getvalue.f90 

hybrd.o:$(BD)/lib/util/ARAPlib/hybrd.f 

qform.o:$(BD)/lib/util/ARAPlib/qform.f 

radf3.o:$(BD)/lib/util/ARAPlib/radf3.f90 

trig_degrees.o:$(BD)/lib/util/ARAPlib/trig_degrees.f90 

radb5.o:$(BD)/lib/util/ARAPlib/radb5.f90 

radb4.o:$(BD)/lib/util/ARAPlib/radb4.f90 

MatRot.o:$(BD)/lib/util/ARAPlib/MatRot.f90 

radf5.o:$(BD)/lib/util/ARAPlib/radf5.f90 

deblank.o:$(BD)/lib/util/ARAPlib/deblank.f90 

random.o:$(BD)/lib/util/ARAPlib/random.f90 

cformat.o:$(BD)/lib/util/ARAPlib/cformat.f90 

r1updt.o:$(BD)/lib/util/ARAPlib/r1updt.f 

qrfac.o:$(BD)/lib/util/ARAPlib/qrfac.f 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  reallocate.mod erfci_fi.mod interp2d_fi.mod \
	  mapconstants_fd.mod erfcinv_fi.mod trigconst.mod