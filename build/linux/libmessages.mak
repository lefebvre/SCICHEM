# Main program is libmessages.${LSuf}

PROG =	libmessages.${LSuf}

include ../make.${Compiler_Version}

BD =../${PSrcDir}/src

include ../messages_mak.include

SRCS_f90 = dll/util/Messages/PostMessage.f90 dll/util/Messages/Messages.f90 \
	  dll/util/Messages/InitMessage.f90

OBJS_f90 := $(notdir $(subst .f90,.o,$(SRCS_f90)))

SRCS_c = dll/util/Messages/callbackhandler.c

OBJS_c := $(notdir $(subst .c,.o,$(SRCS_c)))

OBJS :=  $(OBJS_f90)  $(OBJS_c) 

all: $(PROG) separator

separator:
	@echo ==========================================================================

$(PROG): $(OBJS) $(LIBDEPENDS)
	$(LD) $(PROG) $(OBJS) $(LIBS) $(LDFLAGS)

$(OBJS_f90): %.o:$(filter /\%.f90,$(SRCS_f90))
	$(FC) $(FCFLAGS) $< 

$(OBJS_c): %.o:$(filter /\%.c,$(SRCS_c))
	$(CC) $(CCFLAGS) $< 

Messages.o:$(BD)/dll/util/Messages/Messages.f90  basic_fd.o message_fd.o \
	  SCIMgrParam_fd.o

PostMessage.o:$(BD)/dll/util/Messages/PostMessage.f90  errorParam_fd.o \
	  Messages.o release_fd.o update_fd.o

InitMessage.o:$(BD)/dll/util/Messages/InitMessage.f90  errorParam_fd.o \
	  Messages.o

callbackhandler.o:$(BD)/dll/util/Messages/callbackhandler.c 


# Entry for " make clean " to get rid of all object and module files 
clean:
	rm -f $(OBJS) $(PROG)  messagehandler.mod