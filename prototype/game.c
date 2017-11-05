#include "game.h"

Game *game_init()
{
    Game *game = malloc(sizeof(Game));

    game->field = malloc(FIELD_WIDTH * sizeof(char *));

    srand(time(NULL));

    for (size_t i = 0; i < FIELD_WIDTH; ++i) {
        game->field[i] = calloc(FIELD_HEIGHT, sizeof(char));
        for (size_t j = 0; j < FIELD_HEIGHT; ++j) {
            game->field[i][j] = rand() % 4;
        }
    }

    game->cursor = malloc(sizeof(Point));
    game->cursor->x = FIELD_WIDTH / 2;
    game->cursor->y = FIELD_HEIGHT / 2;
    game->score = 0;
    game->paused = 0;

    return game;
}

void free_game(Game *game)
{
    for (size_t i = 0; i < FIELD_WIDTH; ++i) {
        free(game->field[i]);
    }

    free(game->field);
    free(game->cursor);
    free(game);
}

int run_game(Game *game)
{
    clear_screen();

    int playing = 1;

    Input input;
    while (playing) {
        draw_field(game);
        switch(input = read_input()) {
            case NOTHING:
                break;
            case UP:
                game->cursor->y = (game->cursor->y - 1) % FIELD_HEIGHT;
                break;
            case DOWN:
                game->cursor->y = (game->cursor->y + 1) % FIELD_HEIGHT;
                break;
            case LEFT:
                game->cursor->x = (game->cursor->x - 1) % FIELD_WIDTH;
                break;
            case RIGHT:
                game->cursor->x = (game->cursor->x + 1) % FIELD_WIDTH;
                break;
            case SPACE:
                break;
            case QUIT:
                return 0;
        }

        delay(20);
    }

    draw_field(game);
    return 0;
}
