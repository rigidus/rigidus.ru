
void init(void);
void input(void);
int  update(void);
void render(void);
void pop_tail(void);
void push_head(void);
void gameover(void);

void show_sprite (int x, int y, SDL_Texture* texture);
void next_fruit(void);

int delay;

extern SDL_Texture*  field_texture;
extern SDL_Texture*  fruit_texture;
extern SDL_Texture*  shead_texture;
extern SDL_Texture*  snake_texture;
