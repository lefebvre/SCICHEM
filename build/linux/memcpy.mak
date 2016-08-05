# Main programs are

PROG = MEMCPY/memcpy.o

include make.$(Compiler_Version)

CLNKFLAGS += -fPIC

SRC = MEMCPY/memcpy.c

OBJ = $(PROG)

all: $(PROG) 
	
$(OBJ):$(SRC)
	$(CC) $(CLNKFLAGS) -c -o $(OBJ) $(SRC)

clean:
	rm -f $(OBJ)
