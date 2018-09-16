/* g++ sdltest.c `pkg-config --cflags --libs sdl2` -o sdltest   */
/* https://github.com/mahiuchun/Snake-SDL */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <SDL2/SDL.h>

#include "sdlwrap.h"
#include "asm.h"

SDL_Texture*  fruit_texture = NULL;
SDL_Texture*  shead_texture = NULL;
SDL_Texture*  snake_texture = NULL;
SDL_Texture*  field_texture = NULL;

int main(int argc, char * argv[])
{
    forth_asm_argc = argc;
    forth_asm_argv = (void*)argv;

    /* Disable buffering */
    setbuf(stdout, NULL);

    __asm("call forth_asm_start");

    /* gameover_flag = 0; */
    /* int delay = 16; */
    /* init(); */
    /* render(); */
    /* for (;;) { */
    /*     input(); */
    /*     __asm("call forth_asm_start"); */
    /*     __asm("call update2"); */
    /*     /\* update(); *\/ */
    /*     if (gameover_flag) { */
    /*         gameover(); */
    /*     } */
    /*     __asm("call render"); */
    /*     /\* render(); *\/ */
    /*     SDL_Delay(delay * 10); */
    /* } */
    return 0;
}
