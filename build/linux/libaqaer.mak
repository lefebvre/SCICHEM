# Main program is libaqaer.a

PROG =	libaqaer.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../aqaer_mak.include

SRCS_f90 = lib/AQAER/AqAerIface.f90 lib/AQAER/AE5/aero/inorg_aero.f90 \
	  lib/AQAER/AE5/aero/init_aero_map.f90 \
	  lib/AQAER/AE5/aero/init_aero.f90 lib/AQAER/AE5/aero/set_vdry.f90 \
	  lib/AQAER/AE5/aero/aerosol_chem.f90 \
	  lib/AQAER/AE5/aero/inc/aer_sections_inc.f90 \
	  lib/AQAER/AE5/aero/inc/aero_consts_inc.f90 \
	  lib/AQAER/AE5/aero/inc/aero_species_inc.f90 \
	  lib/AQAER/AE5/aqueous/init_aqueous.f90 \
	  lib/AQAER/AE5/aqueous/step_aqueous.f90 \
	  lib/AQAER/AE5/aqueous/inc/aqueous_consts_inc.f90 \
	  lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.f90 \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_f = lib/AQAER/AE5/aero/hetchem.f lib/AQAER/AE5/aero/orgaer5.f \
	  lib/AQAER/AE5/aero/aero_subs.f lib/AQAER/AE5/aero/getdep_v.f \
	  lib/AQAER/AE5/aero/isofwd.f lib/AQAER/AE5/aero/isocom.f \
	  lib/AQAER/AE5/aero/SOA_NEWT.f lib/AQAER/AE5/aero/AERO_INFO.f \
	  lib/AQAER/AE5/aero/isorev.f lib/AQAER/AE5/aqueous/indexn.f \
	  lib/AQAER/AE5/aqueous/hlconst.f lib/AQAER/AE5/aqueous/vode.f \
	  lib/AQAER/AE5/aqueous/getalpha.f lib/AQAER/AE5/aqueous/aqradm.f \
	  lib/AQAER/AE5/aqueous/inc/cmaq_species_inc.f

OBJS_f := $(subst .f,.o,$(SRCS_f))

OBJS :=  $(OBJS_f90)  $(OBJS_f) 

DIRS = lib/AQAER/AE5/aero/inc lib/SCIPUFFlib/SCIPUFF/inc lib/AQAER lib/AQAER/AE5/aero lib/AQAER/AE5/aqueous lib/AQAER/AE5/aqueous/inc

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LB) $(LBFLAGS) $(PROG) $(OBJS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< $(INCDIR) 

$(OBJS_f): %.o:$(filter /\%.f,$(SRCS_f))
	$(F77) -o $@ $(F77FLAGS) $< $(INCDIR) 


lib/AQAER/AE5/aero/inc/aer_sections_inc.o:$(BD)/lib/AQAER/AE5/aero/inc/aer_sections_inc.f90 

lib/AQAER/AE5/aero/inc/aero_consts_inc.o:$(BD)/lib/AQAER/AE5/aero/inc/aero_consts_inc.f90 

lib/AQAER/AE5/aero/inc/aero_species_inc.o:$(BD)/lib/AQAER/AE5/aero/inc/aero_species_inc.f90  \
	  lib/AQAER/AE5/aero/inc/aero_consts_inc.o

lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o:$(BD)/lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.f90  \
	  dll/stub/MPI/localMPI.o $(INCMOD)

lib/AQAER/AqAerIface.o:$(BD)/lib/AQAER/AqAerIface.f90  \
	  lib/AQAER/AE5/aero/inc/aer_sections_inc.o \
	  lib/AQAER/AE5/aero/inc/aero_species_inc.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o $(INCMOD)

lib/AQAER/AE5/aero/AERO_INFO.o:$(BD)/lib/AQAER/AE5/aero/AERO_INFO.f 

lib/AQAER/AE5/aero/hetchem.o:$(BD)/lib/AQAER/AE5/aero/hetchem.f  \
	  lib/AQAER/AE5/aero/AERO_INFO.o

lib/AQAER/AE5/aero/inorg_aero.o:$(BD)/lib/AQAER/AE5/aero/inorg_aero.f90  \
	  lib/AQAER/AE5/aero/AERO_INFO.o

lib/AQAER/AE5/aero/SOA_NEWT.o:$(BD)/lib/AQAER/AE5/aero/SOA_NEWT.f  \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o $(INCMOD)

lib/AQAER/AE5/aero/orgaer5.o:$(BD)/lib/AQAER/AE5/aero/orgaer5.f  \
	  lib/AQAER/AE5/aero/AERO_INFO.o lib/AQAER/AE5/aero/SOA_NEWT.o $(INCMOD)

lib/AQAER/AE5/aero/aero_subs.o:$(BD)/lib/AQAER/AE5/aero/aero_subs.f  \
	  lib/AQAER/AE5/aero/AERO_INFO.o

lib/AQAER/AE5/aero/getdep_v.o:$(BD)/lib/AQAER/AE5/aero/getdep_v.f  \
	  lib/AQAER/AE5/aero/inc/aero_consts_inc.o

lib/AQAER/AE5/aero/isofwd.o:$(BD)/lib/AQAER/AE5/aero/isofwd.f 

lib/AQAER/AE5/aero/init_aero_map.o:$(BD)/lib/AQAER/AE5/aero/init_aero_map.f90  \
	  lib/AQAER/AE5/aero/inc/aero_consts_inc.o \
	  lib/AQAER/AE5/aero/AERO_INFO.o lib/AQAER/AqAerIface.o $(INCMOD)

lib/AQAER/AE5/aero/isocom.o:$(BD)/lib/AQAER/AE5/aero/isocom.f 

lib/AQAER/AE5/aero/init_aero.o:$(BD)/lib/AQAER/AE5/aero/init_aero.f90  \
	  lib/AQAER/AqAerIface.o $(INCMOD)

lib/AQAER/AE5/aero/set_vdry.o:$(BD)/lib/AQAER/AE5/aero/set_vdry.f90  \
	  lib/AQAER/AE5/aero/AERO_INFO.o lib/AQAER/AqAerIface.o $(INCMOD)

lib/AQAER/AE5/aero/isorev.o:$(BD)/lib/AQAER/AE5/aero/isorev.f 

lib/AQAER/AE5/aero/aerosol_chem.o:$(BD)/lib/AQAER/AE5/aero/aerosol_chem.f90  \
	  lib/AQAER/AE5/aero/AERO_INFO.o lib/AQAER/AqAerIface.o \
	  lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o $(INCMOD)

lib/AQAER/AE5/aqueous/indexn.o:$(BD)/lib/AQAER/AE5/aqueous/indexn.f 

lib/AQAER/AE5/aqueous/hlconst.o:$(BD)/lib/AQAER/AE5/aqueous/hlconst.f  \
	  lib/AQAER/AqAerIface.o $(INCMOD)

lib/AQAER/AE5/aqueous/vode.o:$(BD)/lib/AQAER/AE5/aqueous/vode.f 

lib/AQAER/AE5/aqueous/getalpha.o:$(BD)/lib/AQAER/AE5/aqueous/getalpha.f 

lib/AQAER/AE5/aqueous/inc/cmaq_species_inc.o:$(BD)/lib/AQAER/AE5/aqueous/inc/cmaq_species_inc.f 

lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.o:$(BD)/lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.f90  \
	  lib/AQAER/AE5/aqueous/inc/cmaq_species_inc.o

lib/AQAER/AE5/aqueous/init_aqueous.o:$(BD)/lib/AQAER/AE5/aqueous/init_aqueous.f90  \
	  lib/AQAER/AqAerIface.o \
	  lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.o $(INCMOD)

lib/AQAER/AE5/aqueous/inc/aqueous_consts_inc.o:$(BD)/lib/AQAER/AE5/aqueous/inc/aqueous_consts_inc.f90  \
	  lib/AQAER/AE5/aero/inc/aero_consts_inc.o

lib/AQAER/AE5/aqueous/step_aqueous.o:$(BD)/lib/AQAER/AE5/aqueous/step_aqueous.f90  \
	  lib/AQAER/AE5/aero/AERO_INFO.o lib/AQAER/AqAerIface.o \
	  lib/AQAER/AE5/aqueous/inc/aqueous_consts_inc.o \
	  lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.o $(INCMOD)

lib/AQAER/AE5/aqueous/aqradm.o:$(BD)/lib/AQAER/AE5/aqueous/aqradm.f  \
	  lib/AQAER/AE5/aqueous/inc/aqueous_consts_inc.o \
	  lib/AQAER/AE5/aqueous/inc/aqueous_species_inc.o \
	  lib/AQAER/AqAerIface.o lib/SCIPUFFlib/SCIPUFF/inc/MPIFunc_fi.o $(INCMOD)


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  aqaer_fi.mod basic_aqaer_fd.mod \
	  basic_aqaer_fi.mod chem_aqaer_fi.mod default_aqaer_fd.mod \
	  error_aqaer_fi.mod files_aqaer_fi.mod message_aqaer_fd.mod \
	  multcomp_aqaer_fd.mod precip_aqaer_fd.mod soa_newt.mod \
	  aero_info.mod aer_sections_inc.mod aero_consts_inc.mod \
	  aero_species_inc.mod cmaq_species_inc.mod aqueous_consts_inc.mod \
	  aqueous_species_inc.mod chem_mpi_fi.mod mpi_fi.mod \
	  stepmc_mpi_fi.mod