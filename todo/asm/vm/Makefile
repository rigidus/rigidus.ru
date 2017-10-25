SHELL = /bin/sh

CC = gcc
#CFLAGS  += -g
#LDFLAGS += -g

TARGET = oldgamevm

INC = inc

CSRC = main.c sdlwrap.c
ASMO = asm.o
TOASMS = $(CSRC:.c=.s)
OBJS = $(CSRC:.c=.o) $(ASMS:.s=.o)


.SUFFIXES:
.SUFFIXES: .c .o .s

.PHONY: all toasm clean

VPATH = src:inc

CFLAGS  += $(shell pkg-config --cflags  sdl2)
LDFLAGS += $(shell pkg-config --libs    sdl2)

all: sdlwrap.o asm.o main.o
	$(CC) $^ $(LDFLAGS) -I$(INC)  -o $(TARGET)

sdlwrap.o: sdlwrap.c
	$(CC) -c $(CFLAGS) -I$(INC) $^ -o $@

main.o: main.c
	$(CC) -c $(CFLAGS) -I$(INC) $^ -o $@

asm.o: asm.s
	$(CC) -c $(CFLAGS) $^ -o $@

toasm: $(CSRC)
	$(CC) -S $^ $(CFLAGS) `pkg-config --cflags --libs sdl2` -I$(INC)

clean:
	rm -Rf $(TARGET) $(OBJS) $(ASMO) $(TOASMS)
