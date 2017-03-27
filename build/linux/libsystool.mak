# Main program is libsystool.${LSuf}

PROG =	libsystool.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../systool_mak.include

SRCS_f90 = sys/unix/winAPI_mod.f90 sys/unix/SYStool/INI_mod.f90 \
	  sys/unix/SYStool/unixfunc.f90 sys/unix/SYStool/sysfunc.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

OBJS :=  $(OBJS_f90) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 


winAPI_mod.o:$(BD)/sys/unix/winAPI_mod.f90  basic_fd.o

INI_mod.o:$(BD)/sys/unix/SYStool/INI_mod.f90 

unixfunc.o:$(BD)/sys/unix/SYStool/unixfunc.f90  search_fd.o winAPI_mod.o

sysfunc.o:$(BD)/sys/unix/SYStool/sysfunc.f90  basic_fd.o default_fd.o \
	  param_fd.o INI_mod.o SCIPresults_fd.o winAPI_mod.o


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  winapi.mod winapi_fd.mod ini.mod \
	  maxlength_fd.mod