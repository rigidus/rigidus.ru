#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <memory.h>
#include <string.h>

#include "asm.h"

int main(int argc, char * argv[])
{
    forth_asm_argc = argc;
    forth_asm_argv = (void*)argv;

    /* Disable buffering */
    setbuf(stdout, NULL);

    __asm("call forth_asm_start");

    return 0;
}
