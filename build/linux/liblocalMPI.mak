# Main program is liblocalMPI.a

PROG =	liblocalMPI.a

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = dll/stub/MPI/stubMPI.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_F90 = dll/stub/MPI/localMPI.F90

OBJS_F90 := $(subst .F90,.o,$(SRCS_F90))

SRCS_c = dll/stub/MPI/stubMPIc.c

OBJS_c := $(subst .c,.o,$(SRCS_c))

OBJS :=  $(OBJS_f90)  $(OBJS_F90)  $(OBJS_c) 

DIRS = dll/stub/MPI

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

$(OBJS_F90): %.o:$(filter /\%.F90,$(SRCS_F90))
	$(FC) -o $@ $(FCFLAGS) $< $(CPPFLAGS) 

$(OBJS_c): %.o:$(filter /\%.c,$(SRCS_c))
	$(CC) -o $@ $(CCFLAGS) $<


dll/stub/MPI/stubMPI.o:$(BD)/dll/stub/MPI/stubMPI.f90 

dll/stub/MPI/localMPI.o:$(BD)/dll/stub/MPI/localMPI.F90  \
	  dll/stub/MPI/stubMPI.o $(INCMOD)

dll/stub/MPI/stubMPIc.o:$(BD)/dll/stub/MPI/stubMPIc.c 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  stubmpi.mod localmpi.mod