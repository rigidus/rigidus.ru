
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <SDL2/SDL.h>

#include "asm.h"
#include "sdlwrap.h"

#define QUEUE_SIZE 256

#define LEFT  1
#define UP    2
#define DOWN  3
#define RIGHT 4

#define MAX_X 24
#define MAX_Y 14
#define TILE_SIZE 32

char dir;
char old_dir;
char eaten;
char mat[MAX_X+1][MAX_Y+1];

typedef struct tag_node {
    char x;
    char y;
} node;

node body;
node head;
node tail;
node fruit;

typedef struct tag_queue {
    unsigned char first;
    unsigned char last;
    unsigned char len;
    node elems[QUEUE_SIZE];
} queue;

queue snake;

SDL_Renderer* renderer = NULL;
extern SDL_Texture*  fruit_texture;
extern SDL_Texture*  shead_texture;
extern SDL_Texture*  snake_texture;
extern SDL_Texture*  field_texture;

SDL_Rect rect;

SDL_Texture* load_sprite(char* pathname) {
    return SDL_CreateTextureFromSurface(renderer, SDL_LoadBMP(pathname));
}


void init(void)
{
    int i, j;
    SDL_Window *window = NULL;
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL_Init: %s\n", SDL_GetError());
    }
    atexit(SDL_Quit);
    SDL_CreateWindowAndRenderer(800, 480, 0, &window, &renderer);
    SDL_SetRenderDrawColor(renderer, 255, 255, 255, 255);
    SDL_RenderClear(renderer);
    SDL_RenderPresent(renderer);
    srand(time(NULL));
    asmo_init();
}

void gameover(void)
{
    printf("Snake Length: %d\n", snake.len);
    printf("dir: %d, old: %d\n", dir, old_dir);
    printf("Game Over\n");
    exit(0);
}
