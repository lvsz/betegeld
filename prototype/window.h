#ifndef _WINDOW_H_
#define _WINDOW_H_

#include "SDL/SDL.h"

#include "io.h"
#include "types.h"
#include "defaults.h"

#define TITLE_BUFFER 40

typedef struct Color {
    int r;
    int g;
    int b;
} Color;

#define BLACK  { 0x00, 0x00, 0x00 }
#define WHITE  { 0xFF, 0xFF, 0xFF }
#define RED    { 0xFF, 0x00, 0x00 }
#define GREEN  { 0x00, 0xBB, 0x00 }
#define BLUE   { 0x00, 0xAA, 0xDD }
#define YELLOW { 0xFF, 0xCC, 0x00 }

void window_init();
void window_pause();

void clear_screen();
void draw_field(Game *);

void delay(int);

#endif /* _WINDOW_H_ */

