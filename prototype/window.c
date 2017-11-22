#include "window.h"

static SDL_Surface *window;

#define BLACK  0x00000000
#define WHITE  0xffffff00
#define RED    0x0000ff00
#define GREEN  0x00ff0000
#define BLUE   0xff000000
#define YELLOW 0x00eeff00
#define ORANGE 0x0055ff00
#define PURPLE 0xff00aa00

void draw_cursor(Game *game)
{
    SDL_Rect cursor_tile = { game->cursor->x * TILE_SIZE
                           , game->cursor->y * TILE_SIZE
                           , TILE_SIZE
                           , TILE_SIZE
                           };
    SDL_FillRect(window, &cursor_tile, WHITE);
}

void draw_field(Game *game)
{
    puts("begin drawing field");
    char title[TITLE_BUFFER];
    if (game->paused) {
        sprintf(title, "%s — paused", TITLE);
    } else {
        sprintf(title, "%s — score %d", TITLE, game->score);
    }

    SDL_WM_SetCaption(title, NULL);

    clear_screen();
    draw_cursor(game);

    for (int i = 0; i < FIELD_HEIGHT; ++i) {
        for (int j = 0; j < FIELD_WIDTH; ++j) {
            SDL_Rect tile = { 1 + j * TILE_SIZE
                            , 1 + i * TILE_SIZE
                            , TILE_SIZE - 2
                            , TILE_SIZE - 2
                            };
            Uint32 color;
            switch (game->field[(FIELD_WIDTH + 2) * (i + 1) + j + 1]) {
                case WHITE_TILE:
                    color = WHITE;
                    break;
                case RED_TILE:
                    color = RED;
                    break;
                case GREEN_TILE:
                    color = GREEN;
                    break;
                case BLUE_TILE:
                    color = BLUE;
                    break;
                case YELLOW_TILE:
                    color = YELLOW;
                    break;
                case ORANGE_TILE:
                    color = ORANGE;
                    break;
                case PURPLE_TILE:
                    color = PURPLE;
                    break;
                default:
                    color = BLACK;
                    break;
            }
            SDL_FillRect(window, &tile, color);
        }
    }

    SDL_Flip(window);
    puts("finished drawing field");
}

void clear_screen()
{
    SDL_FillRect(window, NULL, BLACK);
}

void window_quit()
{
    SDL_FreeSurface(window);
    SDL_Quit();
}

void window_init()
{
    if (SDL_Init(SDL_INIT_VIDEO < 0)) {
        fprintf(stderr, "Could not initialize SDL: %s\n", SDL_GetError());
        exit(1);
    }

    window = SDL_SetVideoMode(WINDOW_WIDTH, WINDOW_HEIGHT, 0,
                              SDL_HWPALETTE | SDL_DOUBLEBUF);
    if (window == NULL) {
        fprintf(stderr, "Failed to create window: %s\n", SDL_GetError());
        exit(1);
    }

    SDL_WM_SetCaption(TITLE, NULL);
    clear_screen();

    atexit(window_quit);
}

void delay(int n)
{
    SDL_Delay(n);
}
