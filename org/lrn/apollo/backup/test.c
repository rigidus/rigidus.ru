#include <stdio.h>

typedef char byte;

void hex2disp (int param, byte result[4]) {
    int temp = param;
    int mask = 0xF;
    for (int x=0; x<4; x++) {
        printf ("temp: %X\n", param);
        printf ("mask: %X\n", mask);
        result[x] = (temp & mask);
        printf ("result: %X\n\n", result[x]);
        /* mask = mask << 4; */
        temp = temp >> 4;

    }
}

void main () {
    byte dd[] = { 0b00111111, 0b00111000, 0b01110111, 0b01110110 };
    hex2disp( 0xDEAD, dd );

    printf("all: %X %X %X %X\n", dd[0], dd[1], dd[2], dd[3]);
}
