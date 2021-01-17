all:
	gcc -c -Wall -Werror -fpic add.c
	gcc -c -Wall -Werror -fpic mul.c
	gcc -shared -o libcalc.so add.o mul.o
	gcc -L. -Wall -o main main.c -lcalc
