/* compile: gcc -L. -Wall -o main main.c -lcalc */
#include <stdio.h>
#include "calc.h"

int main(void) {
    printf("2*3=%d; 2+3=%d\n", mul(2,3), add(2,3));
    return 0;
}
