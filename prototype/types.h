#ifndef _TYPES_H_
#define _TYPES_H_

#include "defaults.h"

typedef struct Point {
    size_t x;
    size_t y;
} Point;

typedef struct Game {
    char **field;
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

