/*This source code copyrighted by Lazy Foo' Productions (2004-2015)
and may not be redistributed without written permission.*/

//Using SDL and standard IO
#include<SDL2/SDL.h>
#include <stdio.h>

//параметры окна
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

// включить SDL и сделать окно
bool init();

//загрузка медиа (рисунок)
bool loadMedia();

// все закрыть
void close();

//окно, которое мы показываем
SDL_Window* gWindow = NULL;

// поверхность в окне
SDL_Surface* gScreenSurface = NULL;

//изображение, которое мы загрузим и покажем на экране
SDL_Surface* gHelloWorld = NULL;

bool init()
{
//инициализация флага (?) что это такое?
    bool success = true;
//инициализация SDL

    // если SDL_Init c SDL_INIT VIDEO в качестве параметра меньше нуля,
    if( SDL_Init( SDL_INIT_VIDEO ) < 0 ) {
        printf( "SDL could not initialize! SDL_Error: %s\n", SDL_GetError() );
        // все плохо, установить false в success
        success = false;
    } else {
        // в противном случае создаем окно
// gWindow (переменная, внутри которой указатель(?) на место, по которому лежит окно в памяти) приравнена к функции SDL_CreateWindow, которая должна создать окно (?)
        gWindow = SDL_CreateWindow( "SDL Tutorial", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN );
        // если указатель равен NULL,
        if( gWindow == NULL ) {
            printf( "Window could not be created! SDL_Error: %s\n", SDL_GetError() );
// все плохо, устанавливаем false
            success = false;
        } else {
            //в противном случае устанавливаем поверхность окна
            // переменная gScreenSurface (в которой указатель на поверхность окна в памяти (?)) приравнена к функции SDL_GetWindowSurface, у котоой в качестве параметра переменная gWindow, где теперь лежит адрес нашено окна (?)
            gScreenSurface = SDL_GetWindowSurface( gWindow );
        }
    }
    // возвращаем "успех" (возвращаем куда? куда попадает "уведомление", что все прошло успешно?)
    return success;
}

bool loadMedia()
{
    //устанавливаем флаг
    bool success = true;
//пробуем загрузить рисунок
    // переменная  gHelloWorld ( в которой лежит указатель на наш рисунок) приравнена к функции SDL_LoadBMP (которая и грузит изображение, как я понимаю). Чтоб все загрузилось, надо указать правильный путь.
    gHelloWorld = SDL_LoadBMP( "hello_world.bmp" );
// если указатель в  gHelloWorld равен нулю,
    if( gHelloWorld == NULL ) {
        printf( "Unable to load image %s! SDL Error: %s\n", "~/Desktop/make/hello_world.bmp", SDL_GetError() );
// все плохо, устанавливаем false
        success = false;
    }
    // в противном случае возвращаем "успех" (опять же, куда? )
    return success;
}

void close()
{
//зачистка изображение
    // берем функцию  SDL_FreeSurface с параметром gHelloWorld (зачем?) и говорим, что gHelloWorld снова равен NULL
    SDL_FreeSurface( gHelloWorld );
    gHelloWorld = NULL;
//уничтожаем окно
    //в общем, аналогично
    SDL_DestroyWindow( gWindow );
    gWindow = NULL;
//выход из библиотеки (?)
    SDL_Quit();
}

int main( int argc, char* args[] )
{

    //включаем SDL и создаем окно
    // не знаю, что означает ! перед функциями, подозреваю, что так мы говорим "если функция такая-то не сработала",
    if( !init() ) {
        // печатаем сообщение об ошибке
        printf( "Failed to initialize!\n" );
    } else {
//если окно создалось, то грузим изображение
        if( !loadMedia() ) {
            // если изображение загрузить не удалось, пишем соответствующее сообщение об ошибке
            printf( "Failed to load media!\n" );
        } else {
            //добавляем изображение. Что тут с аргументами пока что не понимаю.
            SDL_BlitSurface( gHelloWorld, NULL, gScreenSurface, NULL );
            //обновляем изображение
            SDL_UpdateWindowSurface( gWindow );

            //ждем пару секунд
            SDL_Delay( 2000 );

        }
    }
    // закрываем все
        close();
// возвращаем 0 операционной системе как знак успеха (?)
        return 0;
    }
