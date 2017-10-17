
void init(void);
void input(void);
void update(void);
void render(void);
void gameover(void);

void show_sprite (int x, int y, SDL_Texture* texture);
void part(void);

int delay;
int gameover_flag;

extern SDL_Texture*  field_texture;
extern SDL_Texture*  fruit_texture;
extern SDL_Texture*  shead_texture;
extern SDL_Texture*  snake_texture;
