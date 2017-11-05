#include "io.h"

Input read_input()
{
    SDL_Event event;
    while(SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                return QUIT;
            case SDL_KEYDOWN:
                switch(event.key.keysym.sym) {
                    case SDLK_UP:
                    case SDLK_w:
                        return UP;
                    case SDLK_DOWN:
                    case SDLK_s:
                        return DOWN;
                    case SDLK_LEFT:
                    case SDLK_a:
                        return LEFT;
                    case SDLK_RIGHT:
                    case SDLK_d:
                        return RIGHT;
                    case SDLK_ESCAPE:
                        return QUIT;
                    default:
                        return NOTHING;
                }
            default:
                break;
        }
    }

    return NOTHING;
}

