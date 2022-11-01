SHELL = /bin/sh

CC = gcc
CFLAGS  += -m64 -static
LDFLAGS += -m64 -static

TARGET = jft

INC = inc64

all: plain_asm

CSRC_MAIN    = main64.c
OBJ_MAIN     = $(CSRC_MAIN:.c=.o)

ASRC_MAIN    = jft.S
OBJ_ASM      = $(ASRC_MAIN:.S=.o)

CSRCS        = $(CSRC_MAIN)
OBJS 		 = $(CSRCS:.c=.o)

$(OBJ_ASM): $(ASRC_MAIN)
	$(CC) -c $(CFLAGS) -I$(INC) $^ -o $@

$(OBJ_MAIN): $(CSRC_MAIN)
	$(CC) -c $(CFLAGS) -I$(INC) $^ -o $@

with_c: $(OBJ_ASM) $(OBJ_MAIN)
	$(CC) $^ $(LDFLAGS) -I$(INC) -N -o $(TARGET)

clean:
	rm $(OBJ_ASM) $(OBJ_MAIN) $(TARGET)

plain_asm:
	gcc -nostdlib $(CFLAGS) -N -static-pie -Wl,--build-id=none -o jft jft.S
	# strip -s -R -g -x jft
