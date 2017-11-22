#include "field.h"

char *new_field()
{
    puts("making new field");
    char tile_types[] = {RED_TILE, GREEN_TILE, BLUE_TILE, YELLOW_TILE, ORANGE_TILE, PURPLE_TILE};
    char *field = calloc((FIELD_WIDTH + 2) * (FIELD_HEIGHT + 2), sizeof(char));

    srand(time(NULL));

    for (int i = FIELD_WIDTH + 2; i <= (FIELD_WIDTH + 2) * FIELD_HEIGHT; i += FIELD_WIDTH + 2) {
        for (int j = 0; j < FIELD_WIDTH; ++j) {
            field[i + j + 1] = tile_types[rand() % sizeof(tile_types)];
        }
    }
    puts("finished new field");
    /* prints field for debugging */
    /*
    for (int i = 0; i < (FIELD_HEIGHT + 2); ++i) {
            for (int j = 0; j < (FIELD_WIDTH + 2); ++j) {
                printf("%3d ", field[i * (FIELD_WIDTH + 2) + j]);
            }
            putchar(10);
    }
    */
    return field;
}

char field_ref(char *field, int x, int y)
{
    puts("referencing field");
    return field[(FIELD_WIDTH + 2) * (y + 1) + x + 1];
}
