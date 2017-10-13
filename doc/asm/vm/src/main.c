/* g++ sdltest.c `pkg-config --cflags --libs sdl2` -o sdltest   */
/* https://github.com/mahiuchun/Snake-SDL */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <SDL2/SDL.h>

#include "sdlwrap.h"
#include "asm.h"

int main(void)
{
    /* call the asm function */
    asm_mod_array();

    int delay = 16;
    init();
    render();
    for (;;) {
        input();
        if (update()) {
            gameover();
        }
        render();
        SDL_Delay(delay * 10);
    }
    return 0;
}
