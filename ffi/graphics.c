#include "SDL2/SDL.h"
#include "stdio.h"
#include "SDL2/SDL_ttf.h"
#include "SDL2/SDL2_gfxPrimitives.h"

const int HEX_WIDTH  = 200;
const int HEX_HEIGHT = 100;

SDL_Window* gWindow = NULL;
SDL_Renderer* gRenderer = NULL;
TTF_Font *gfont = NULL;

void myinitialize(int width, int height) {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY,"1");
    gWindow = SDL_CreateWindow( "test",SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
    gRenderer = SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED);
    SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0xFF,0xFF);
    TTF_Init();
    gfont = TTF_OpenFont( "/usr/share/fonts/corefonts/arial.ttf", 12 );
}

void myclose() {
    TTF_CloseFont(gfont);
    TTF_Quit();
    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    SDL_Quit();
}

void writeText(char *str, int x, int y, int red, int green, int blue) {
    SDL_Color textColor = { red, green, blue };
    SDL_Surface* textSurface = TTF_RenderText_Solid(gfont , str, textColor );
    SDL_Texture* gTexture = SDL_CreateTextureFromSurface( gRenderer, textSurface );
    int width=(textSurface->w);
    int height=(textSurface->h);
    SDL_Rect dest = {x-width,y-height,width,height};
    SDL_RenderCopy( gRenderer, gTexture, NULL, &dest );
    SDL_FreeSurface(textSurface);
}

void drawDot(int centerx, int centery, int r, int red, int green, int blue) {
    SDL_SetRenderDrawColor(gRenderer,red,green,blue,0xFF);
    for (int i=-r;i<=r;i++) {
        for (int j=-r;j<=r;j++) {
            if(i*i+j*j<r*r) {
                SDL_RenderDrawPoint(gRenderer,i+centerx,j+centery-50);
            }
        }
    }
    SDL_SetRenderDrawColor(gRenderer,0x80+red/2,0x80+green/2,0x80+blue/2,0x80);
    for (int i=-r;i<=r;i++) {
        for (int j=-r;j<=r;j+=2) {
            if(i*i+j*j<r*r) {
                SDL_RenderDrawPoint(gRenderer,i+centerx,((j+1)/2)+centery);
            }
        }
    }
}

void drawHexagon(int centerx,int centery,double angle,int red,int green,int blue) {
    int radius = HEX_WIDTH/2;
    const Sint16 vx[6] = {centerx + radius*SDL_cos(angle               ),
                          centerx + radius*SDL_cos(angle+(    M_PI/3.0)),
                          centerx + radius*SDL_cos(angle+(2.0*M_PI/3.0)),
                          centerx + radius*SDL_cos(angle+(    M_PI    )),
                          centerx + radius*SDL_cos(angle+(4.0*M_PI/3.0)),
                          centerx + radius*SDL_cos(angle+(5.0*M_PI/3.0))};
    const Sint16 vy[6] = {centery + -0.57735026919*(radius*SDL_sin(angle               )),
                          centery + -0.57735026919*(radius*SDL_sin(angle+(    M_PI/3.0))),
                          centery + -0.57735026919*(radius*SDL_sin(angle+(2.0*M_PI/3.0))),
                          centery + -0.57735026919*(radius*SDL_sin(angle+(    M_PI    ))),
                          centery + -0.57735026919*(radius*SDL_sin(angle+(4.0*M_PI/3.0))),
                          centery + -0.57735026919*(radius*SDL_sin(angle+(5.0*M_PI/3.0)))};
    filledPolygonRGBA(gRenderer,vx,vy,6,red,green,blue,0xFF);
}

void updateDrawing() {
    SDL_RenderPresent(gRenderer);
}

void clearDrawing() {
    SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0xFF,0xFF);
    SDL_RenderClear(gRenderer);
}
