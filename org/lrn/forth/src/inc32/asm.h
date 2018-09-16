#include <asm-generic/unistd.h>

extern char ** environ;

int    forth_asm_argc;
void  *forth_asm_argv;

void asmo_init();
void forth_asm_start();
