#include "game.h"

Game *game_init()
{
    Game *game = malloc(sizeof(Game));

    game->field = new_field();
    game->cursor = malloc(sizeof(Point));
    game->cursor->x = FIELD_WIDTH / 2;
    game->cursor->y = FIELD_HEIGHT / 2;
    game->score = 0;
    game->paused = 0;

    return game;
}

void free_game(Game *game)
{
    free(game->field);
    free(game->cursor);
    free(game);
}

int detect_row(char *field)
{
    return 0;
}

int search_match(Game *game)
{
    char *field = game->field;
    for (int row = 0; row < FIELD_WIDTH * FIELD_HEIGHT; row += FIELD_WIDTH + 2) {
        for (int col = 0; col < FIELD_WIDTH; ++col) {
            char tile = field[row + col + 1];
            int i = 1;
            while (tile &= field[row + col + 1 + i]) {
                ++i;
            }
            if (i >= 3) {
                tile = field[row + col + 1];
                i = 0;
                while(tile &= field[row + col + 1 + i]) {
                    field[row + col + 1 + i] |= 128; /* set most significant bit */
                }
            }
        }
    }
    return 0;
}

int run_game(Game *game)
{
    clear_screen();

    int playing = 1;

    Point *cursor = game->cursor;
    Input input;
    while (playing) {
        draw_field(game);
        switch(input = read_input()) {
            case NOTHING:
                break;
            case UP:
                cursor->y = (cursor->y ? cursor->y : FIELD_HEIGHT) - 1;
                break;
            case DOWN:
                cursor->y = (cursor->y + 1) % FIELD_HEIGHT;
                break;
            case LEFT:
                cursor->x = (cursor->x ? cursor->x : FIELD_WIDTH) - 1;
                break;
            case RIGHT:
                cursor->x = (cursor->x + 1) % FIELD_WIDTH;
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
