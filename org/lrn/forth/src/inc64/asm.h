#include <asm-generic/unistd.h>

/* shared variables */
int forth_asm_argc;
char* forth_asm_argv;

/* prototype for asm function */
void asmo_init();
void forth_asm_start();
