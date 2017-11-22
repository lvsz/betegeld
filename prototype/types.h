#ifndef _TYPES_H_
#define _TYPES_H_

#include "defaults.h"

#define WHITE_TILE   1 /* 0000001 */
#define RED_TILE     2 /* 0000010 */
#define GREEN_TILE   4 /* 0000100 */
#define BLUE_TILE    8 /* 0001000 */
#define YELLOW_TILE 16 /* 0010000 */
#define ORANGE_TILE 32 /* 0100000 */
#define PURPLE_TILE 64 /* 1000000 */

typedef struct Point {
    size_t x;
    size_t y;
} Point;

typedef struct Game {
    char *field;
    Point *cursor;
    int score;
    int paused;
} Game;

typedef enum Input {
    NOTHING,
    UP,
    DOWN,
    LEFT,
    RIGHT,
    SPACE,
    QUIT
} Input;

#endif /* _TYPES_H_ */

