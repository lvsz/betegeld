#include "window.h"
#include "game.h"

int main(int argc, char *argv[])
{
    Game *game = game_init();
    window_init();

    while(run_game(game));

    free_game(game);
    return 0;
}

