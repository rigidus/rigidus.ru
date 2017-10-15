/* https://github.com/mahiuchun/Snake-SDL */

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

    asmo_init();

    /* push_head(); */
    next_fruit();
    eaten = 1;
    old_dir = 0;
    printf("Level 1\n");
}

void input(void)
{
    const Uint8 *state = SDL_GetKeyboardState(NULL);
    SDL_PumpEvents();
    if (state[SDL_SCANCODE_UP]) {
        dir = UP;
    } else if (state[SDL_SCANCODE_DOWN]) {
        dir = DOWN;
    } else if (state[SDL_SCANCODE_LEFT]) {
        dir = LEFT;
    } else if (state[SDL_SCANCODE_RIGHT]) {
        dir = RIGHT;
    } else if (state[SDL_SCANCODE_ESCAPE]) {
        exit(0);
    }
    /* Ignore opposite direction */
    if (dir + old_dir != 5 || snake.len == 1) {
        old_dir = dir;
    } else {
        dir = old_dir;
    }
}

int update(void)
{
    body = head;
    switch (dir) {
    case UP:
        head.y = head.y - 1;
        break;
    case DOWN:
        head.y = head.y + 1;
        break;
    case LEFT:
        head.x = head.x - 1;
        break;
    case RIGHT:
        head.x = head.x + 1;
        break;
    }
    if (head.x < 0 || head.x > MAX_X || head.y < 0 || head.y > MAX_Y) {
        return 1;
    }
    if (mat[head.x][head.y]) {
        return 1;
    }
    if (head.x == fruit.x && head.y == fruit.y) {
        next_fruit();
        eaten = 1;
        switch (snake.len) {
        case 10:
            delay -= 4;
            printf("Level 2\n");
            break;
        case 20:
            delay -= 4;
            printf("Level 3\n");
            break;
        case 30:
            delay /= 2;
            printf("Level 4\n");
            break;
        case 40:
            delay /= 2;
            printf("Level 5\n");
            break;
        }
    } else {
        /* pop_tail(); */
        __asm ("call dequeue");
        eaten = 0;
    }
    __asm ("call enqueue");
    /* push_head(); */
    return 0;
}

void render(void)
{
    if (snake.len > 1) {
        show_sprite(body.x, body.y, snake_texture);
    }
    if (eaten) {
        show_sprite(fruit.x, fruit.y, fruit_texture);
    } else {
        show_sprite(tail.x, tail.y, field_texture);
    }
    show_sprite(head.x, head.y, shead_texture);
    SDL_RenderPresent(renderer);
}

/*
void pop_tail(void)
{
    tail = snake.elems[snake.first];
    snake.first = (snake.first + 1) % QUEUE_SIZE;
    snake.len--;
    mat[tail.x][tail.y] = 0;
}
*/

void push_head(void)
{
    snake.elems[snake.last] = head;
    snake.last = (snake.last + 1) % QUEUE_SIZE;
    snake.len++;
    mat[head.x][head.y] = 1;
}

void gameover(void)
{
    printf("Snake Length: %d\n", snake.len);
    printf("Game Over\n");
    exit(0);
}


void show_sprite (int x, int y, SDL_Texture* texture) {
    SDL_Rect rect;
    rect.h = TILE_SIZE;
    rect.w = TILE_SIZE;
    rect.x = x * TILE_SIZE;
    rect.y = y * TILE_SIZE;
    SDL_RenderCopy(renderer, texture, NULL, &rect);
}

void next_fruit(void)
{
    do {
        fruit.x = (fruit.x * 6 + 1) % (MAX_X + 1);
        fruit.y = (fruit.y * 16 + 1) % (MAX_Y + 1);
    } while (mat[fruit.x][fruit.y]);
}
