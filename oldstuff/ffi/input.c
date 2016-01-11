#include "SDL2/SDL.h"

char getEvent() {
    SDL_Event e;
    if(SDL_PollEvent( &e ) != 0) {
        if(e.type == SDL_KEYDOWN) {
            if (!e.key.repeat) {
                return ((char)e.key.keysym.sym);
            } else
                return getEvent();
        } else
            return ' ';
    } else
        return ' ';
}
