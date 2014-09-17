#include "SDL2/SDL.h"

char getEvent() {
    SDL_Event e;
    if(SDL_PollEvent( &e ) != 0) {
        if(e.type == SDL_KEYDOWN) {
            return ((char)e.key.keysym.sym);
        } else
            return ' ';
    } else
        return ' ';
}
