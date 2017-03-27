# Main program is libprime.a

PROG =	libprime.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = lib/PRIME/isc_prime.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

SRCS_f = lib/PRIME/prime_util.f lib/PRIME/prime.f

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


prime_util.o:$(BD)/lib/PRIME/prime_util.f  modules.o

isc_prime.o:$(BD)/lib/PRIME/isc_prime.f90  $(INCMOD)

prime.o:$(BD)/lib/PRIME/prime.f  modules.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG) 