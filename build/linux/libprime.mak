# Main program is libprime.a

PROG =	libprime.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/PRIME/isc_prime.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_f = lib/PRIME/prime_util.f lib/PRIME/prime.f

OBJS_f := $(subst .f,.o,$(SRCS_f))

OBJS :=  $(OBJS_f90)  $(OBJS_f) 

DIRS = lib/PRIME

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


lib/PRIME/prime_util.o:$(BD)/lib/PRIME/prime_util.f  lib/PRIME/modules.o

lib/PRIME/isc_prime.o:$(BD)/lib/PRIME/isc_prime.f90  $(INCMOD)

lib/PRIME/prime.o:$(BD)/lib/PRIME/prime.f  lib/PRIME/modules.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG) 