# Main program is libodepack.a

PROG =	libodepack.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f = lib/util/odepack/sgesl.f lib/util/odepack/bnorm.f \
	  lib/util/odepack/nnfc.f lib/util/odepack/cdrv.f \
	  lib/util/odepack/sgbfa.f lib/util/odepack/cntnzu.f \
	  lib/util/odepack/vnorm.f lib/util/odepack/solsy.f \
	  lib/util/odepack/prjs.f lib/util/odepack/srcma.f \
	  lib/util/odepack/odrv.f lib/util/odepack/slss.f \
	  lib/util/odepack/aigbt.f lib/util/odepack/mdi.f \
	  lib/util/odepack/srcar.f lib/util/odepack/nnsc.f \
	  lib/util/odepack/sscal.f lib/util/odepack/lsode_bd.f \
	  lib/util/odepack/rchek.f lib/util/odepack/mdm.f \
	  lib/util/odepack/srcom.f lib/util/odepack/saxpy.f \
	  lib/util/odepack/ewset.f lib/util/odepack/jgroup.f \
	  lib/util/odepack/scopy.f lib/util/odepack/prepj.f \
	  lib/util/odepack/roots.f lib/util/odepack/decbt.f \
	  lib/util/odepack/stodi.f lib/util/odepack/prep.f \
	  lib/util/odepack/nntc.f lib/util/odepack/solbt.f \
	  lib/util/odepack/cfode.f lib/util/odepack/prepji.f \
	  lib/util/odepack/lsode.f lib/util/odepack/lsodi.f \
	  lib/util/odepack/r1mach.f lib/util/odepack/lsoibt.f \
	  lib/util/odepack/sdot.f lib/util/odepack/prja.f \
	  lib/util/odepack/vmnorm.f lib/util/odepack/sro.f \
	  lib/util/odepack/intdy.f lib/util/odepack/mdu.f \
	  lib/util/odepack/slsbt.f lib/util/odepack/lsodar.f \
	  lib/util/odepack/nroc.f lib/util/odepack/sgefa.f \
	  lib/util/odepack/xsetf.f lib/util/odepack/mdp.f \
	  lib/util/odepack/lsoda.f lib/util/odepack/isamax.f \
	  lib/util/odepack/pjibt.f lib/util/odepack/xsetun.f \
	  lib/util/odepack/srcms.f lib/util/odepack/lsodes.f \
	  lib/util/odepack/ainvg.f lib/util/odepack/stode.f \
	  lib/util/odepack/fnorm.f lib/util/odepack/stoda.f \
	  lib/util/odepack/adjlr.f lib/util/odepack/iprep.f \
	  lib/util/odepack/sgbsl.f lib/util/odepack/xerrwv.f \
	  lib/util/odepack/lsode_sl.f lib/util/odepack/nsfc.f \
	  lib/util/odepack/md.f

OBJS_f := $(notdir $(subst .f,.o,$(SRCS_f)))

OBJS :=  $(OBJS_f) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f): %.o:$(filter /\%.f,$(SRCS_f))
	$(F77) $(F77FLAGS) $< 


sgesl.o:$(BD)/lib/util/odepack/sgesl.f 

bnorm.o:$(BD)/lib/util/odepack/bnorm.f 

nnfc.o:$(BD)/lib/util/odepack/nnfc.f 

cdrv.o:$(BD)/lib/util/odepack/cdrv.f 

sgbfa.o:$(BD)/lib/util/odepack/sgbfa.f 

cntnzu.o:$(BD)/lib/util/odepack/cntnzu.f 

vnorm.o:$(BD)/lib/util/odepack/vnorm.f 

solsy.o:$(BD)/lib/util/odepack/solsy.f 

prjs.o:$(BD)/lib/util/odepack/prjs.f 

srcma.o:$(BD)/lib/util/odepack/srcma.f 

odrv.o:$(BD)/lib/util/odepack/odrv.f 

slss.o:$(BD)/lib/util/odepack/slss.f 

aigbt.o:$(BD)/lib/util/odepack/aigbt.f 

mdi.o:$(BD)/lib/util/odepack/mdi.f 

srcar.o:$(BD)/lib/util/odepack/srcar.f 

nnsc.o:$(BD)/lib/util/odepack/nnsc.f 

sscal.o:$(BD)/lib/util/odepack/sscal.f 

lsode_bd.o:$(BD)/lib/util/odepack/lsode_bd.f 

rchek.o:$(BD)/lib/util/odepack/rchek.f 

mdm.o:$(BD)/lib/util/odepack/mdm.f 

srcom.o:$(BD)/lib/util/odepack/srcom.f 

saxpy.o:$(BD)/lib/util/odepack/saxpy.f 

ewset.o:$(BD)/lib/util/odepack/ewset.f 

jgroup.o:$(BD)/lib/util/odepack/jgroup.f 

scopy.o:$(BD)/lib/util/odepack/scopy.f 

prepj.o:$(BD)/lib/util/odepack/prepj.f 

roots.o:$(BD)/lib/util/odepack/roots.f 

decbt.o:$(BD)/lib/util/odepack/decbt.f 

stodi.o:$(BD)/lib/util/odepack/stodi.f 

prep.o:$(BD)/lib/util/odepack/prep.f 

nntc.o:$(BD)/lib/util/odepack/nntc.f 

solbt.o:$(BD)/lib/util/odepack/solbt.f 

cfode.o:$(BD)/lib/util/odepack/cfode.f 

prepji.o:$(BD)/lib/util/odepack/prepji.f 

lsode.o:$(BD)/lib/util/odepack/lsode.f 

lsodi.o:$(BD)/lib/util/odepack/lsodi.f 

r1mach.o:$(BD)/lib/util/odepack/r1mach.f 

lsoibt.o:$(BD)/lib/util/odepack/lsoibt.f 

sdot.o:$(BD)/lib/util/odepack/sdot.f 

prja.o:$(BD)/lib/util/odepack/prja.f 

vmnorm.o:$(BD)/lib/util/odepack/vmnorm.f 

sro.o:$(BD)/lib/util/odepack/sro.f 

intdy.o:$(BD)/lib/util/odepack/intdy.f 

mdu.o:$(BD)/lib/util/odepack/mdu.f 

slsbt.o:$(BD)/lib/util/odepack/slsbt.f 

lsodar.o:$(BD)/lib/util/odepack/lsodar.f 

nroc.o:$(BD)/lib/util/odepack/nroc.f 

sgefa.o:$(BD)/lib/util/odepack/sgefa.f 

xsetf.o:$(BD)/lib/util/odepack/xsetf.f 

mdp.o:$(BD)/lib/util/odepack/mdp.f 

lsoda.o:$(BD)/lib/util/odepack/lsoda.f 

isamax.o:$(BD)/lib/util/odepack/isamax.f 

pjibt.o:$(BD)/lib/util/odepack/pjibt.f 

xsetun.o:$(BD)/lib/util/odepack/xsetun.f 

srcms.o:$(BD)/lib/util/odepack/srcms.f 

lsodes.o:$(BD)/lib/util/odepack/lsodes.f 

ainvg.o:$(BD)/lib/util/odepack/ainvg.f 

stode.o:$(BD)/lib/util/odepack/stode.f 

fnorm.o:$(BD)/lib/util/odepack/fnorm.f 

stoda.o:$(BD)/lib/util/odepack/stoda.f 

adjlr.o:$(BD)/lib/util/odepack/adjlr.f 

iprep.o:$(BD)/lib/util/odepack/iprep.f 

sgbsl.o:$(BD)/lib/util/odepack/sgbsl.f 

xerrwv.o:$(BD)/lib/util/odepack/xerrwv.f 

lsode_sl.o:$(BD)/lib/util/odepack/lsode_sl.f 

nsfc.o:$(BD)/lib/util/odepack/nsfc.f 

md.o:$(BD)/lib/util/odepack/md.f 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG) 
