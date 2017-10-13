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

int main(void)
{
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
