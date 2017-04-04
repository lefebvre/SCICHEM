# Main program is libmessages.${LSuf}

PROG =	libmessages.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../messages_mak.include

SRCS_f90 = dll/util/Messages/PostMessage.f90 dll/util/Messages/Messages.f90 \
	  dll/util/Messages/InitMessage.f90

OBJS_f90 := $(subst .f90,.o,$(SRCS_f90))

SRCS_c = dll/util/Messages/callbackhandler.c

OBJS_c := $(subst .c,.o,$(SRCS_c))

OBJS :=  $(OBJS_f90)  $(OBJS_c) 

DIRS = dll/util/Messages

all: $(DIRS) $(PROG) separator

separator:
	@echo ==========================================================================

$(DIRS): FORCE
	$(shell [ -d "$@" ] || mkdir -p "$@")

FORCE:

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) -o $@ $(FCFLAGS) $< 

$(OBJS_c): %.o:$(filter /\%.c,$(SRCS_c))
	$(CC) -o $@ $(CCFLAGS) $<


dll/util/Messages/Messages.o:$(BD)/dll/util/Messages/Messages.f90  \
	  lib/SCIPUFFlib/FileMgr/inc/basic_fd.o \
	  lib/SCIPUFFlib/FileMgr/inc/message_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/SCIMgrParam_fd.o

dll/util/Messages/PostMessage.o:$(BD)/dll/util/Messages/PostMessage.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o \
	  dll/util/Messages/Messages.o \
	  lib/SCIPUFFlib/FileMgr/inc/release_fd.o \
	  lib/SCIPUFFlib/SCIMgr/inc/update_fd.o

dll/util/Messages/InitMessage.o:$(BD)/dll/util/Messages/InitMessage.f90  \
	  lib/SCIPUFFlib/SCIPUFF/inc/errorParam_fd.o \
	  dll/util/Messages/Messages.o

dll/util/Messages/callbackhandler.o:$(BD)/dll/util/Messages/callbackhandler.c 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  messagehandler.mod