#ifndef _GAME_H_
#define _GAME_H_

#include <stdlib.h>
#include <time.h>

#include "field.h"
#include "window.h"
#include "io.h"
#include "types.h"
#include "defaults.h"

int run_game(Game *);
Game *game_init();
void free_game(Game *);

#endif /* _GAME_H_ */

