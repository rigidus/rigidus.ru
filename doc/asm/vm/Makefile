SHELL = /bin/sh

CC = gcc
CDEBUG = -g
CFLAGS = $(CDEBUG)
LDFLAGS = -g

TARGET = oldgamevm

SRCS = main.c sdlwrap.c
ASMS = asm.o
TOASMS = $(SRCS:.c=.s)
OBJS = $(SRCS:.c=.o) $(ASMS:.s=.o)

INC = inc

.SUFFIXES:
.SUFFIXES: .c .o .s

.PHONY: all toasm clean

VPATH = src:inc

all: sdlwrap.o asm.o main.o
	$(CC) $^ $(CFLAGS) `pkg-config --cflags --libs sdl2` -I$(INC)  -o $(TARGET)

sdlwrap.o: sdlwrap.c
	$(CC) -c $(CFLAGS) `pkg-config --cflags --libs sdl2` -I$(INC) $^ -o $@

main.o: main.c
	$(CC) -c $(CFLAGS) `pkg-config --cflags --libs sdl2` -I$(INC) $^ -o $@

asm.o: asm.s
	$(CC) -c $(CFLAGS) `pkg-config --cflags --libs sdl2` $^ -o $@

toasm: $(SRCS)
	$(CC) -S $^ $(CFLAGS) `pkg-config --cflags --libs sdl2` -I$(INC)

clean:
	rm -Rf $(TARGET) $(OBJS) $(ASMS) $(TOASMS)
