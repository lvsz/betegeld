#include "window.h"

static SDL_Surface *window;

static Color colors[] = { RED
                        , GREEN
                        , BLUE
                        , YELLOW
                        };

Uint32 get_color(Color color)
{
    return SDL_MapRGB(window->format, color.r, color.g, color.b);
}

void draw_cursor(Game *game)
{
    SDL_Rect cursor_tile = { game->cursor->x * TILE_SIZE
                           , game->cursor->y * TILE_SIZE
                           , TILE_SIZE
                           , TILE_SIZE
                           };
    Color c = WHITE;
    SDL_FillRect(window, &cursor_tile, get_color(c));
}

void draw_field(Game *game)
{
    char title[TITLE_BUFFER];
    if (game->paused) {
        sprintf(title, "%s — paused", TITLE);
    } else {
        sprintf(title, "%s — score %d", TITLE, game->score);
    }

    SDL_WM_SetCaption(title, NULL);

    draw_cursor(game);

    for (int i = 0; i < FIELD_WIDTH; ++i) {
        for (int j = 0; j < FIELD_HEIGHT; ++j) {
            SDL_Rect tile = { 1 + i * TILE_SIZE
                            , 1 + j * TILE_SIZE
                            , TILE_SIZE - 2
                            , TILE_SIZE - 2
                            };
            SDL_FillRect(window, &tile, get_color(colors[game->field[i][j]]));
        }
    }

SDL_Flip(window);
}

void clear_screen()
{
    Color c = BLACK;
    SDL_FillRect(window, NULL, get_color(c));
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
