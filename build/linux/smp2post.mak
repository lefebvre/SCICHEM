# Main program is smp2post
#
# Based on makefile for SMP2POSTFILE VERSION 0.1 2013-06-07
#
PROG = smp2post

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

SRCS_f90 = bin/smp2post/smp2postfile.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LNK) $(LNKFLAGS) $(PROG) $(OBJS) $(LIBS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 

smp2postfile.o:$(BD)/bin/smp2post/smp2postfile.f90

clean:
	rm -f $(OBJS) $(PROG)   

