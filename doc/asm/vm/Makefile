all: sdlwrap.o main.o
	gcc -g sdlwrap.o main.o `pkg-config --cflags --libs sdl2` -o result

main.o:
	gcc main.c -g -c `pkg-config --cflags --libs sdl2` -o main.o

sdlwrap.o:
	gcc sdlwrap.c -g -c `pkg-config --cflags --libs sdl2` -o sdlwrap.o

clean:
	rm *.o
	rm result
