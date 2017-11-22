#ifndef _WINDOW_H_
#define _WINDOW_H_

#include "SDL/SDL.h"

#include "io.h"
#include "types.h"
#include "defaults.h"

#define TITLE_BUFFER 40

void window_init();
void window_pause();

void clear_screen();
void draw_field(Game *);

void delay(int);

#endif /* _WINDOW_H_ */

