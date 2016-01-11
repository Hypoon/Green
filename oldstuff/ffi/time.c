#include "SDL2/SDL.h"

#define FPS 50
#define STEP (1000/FPS)

void sleepUntilNextFrame() {
    SDL_Delay(STEP-(SDL_GetTicks() % STEP));
}
