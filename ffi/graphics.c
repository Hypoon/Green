#include "SDL2/SDL.h"
#include "stdio.h"
#include "SDL2/SDL_ttf.h"

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
    //TTF_Init();
    //TTF_Font *gfont = TTF_OpenFont( "/usr/share/fonts/corefonts/arial.ttf", 12 );
    SDL_Color textColor = { red, green, blue };
    SDL_Surface* textSurface = TTF_RenderText_Solid(gfont , str, textColor );
    SDL_Texture* gTexture = SDL_CreateTextureFromSurface( gRenderer, textSurface );
    int width=(textSurface->w);
    int height=(textSurface->h);
    SDL_Rect dest = {x-width,y-height,width,height};
    SDL_RenderCopy( gRenderer, gTexture, NULL, &dest );
    SDL_FreeSurface(textSurface);
    //TTF_CloseFont(gfont);

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

void drawHexagon(int centerx,int centery,int red,int green,int blue) {
    int top=centery-HEX_HEIGHT/2;
    int bottom=centery+HEX_HEIGHT/2-1;
    int x1=centerx-HEX_WIDTH/2;
    int x2=centerx-HEX_WIDTH/4-1;
    int x3=centerx+HEX_WIDTH/4;
    int x4=centerx+HEX_WIDTH/2-1;
    SDL_SetRenderDrawBlendMode(gRenderer,SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(gRenderer,red,green,blue,0xFF);
    for (int i=0;i<HEX_HEIGHT/2;i++) {
        SDL_RenderDrawLine(gRenderer,x2-i+1,top+i,x3+i-1,top+i);
    }
    for (int i=0;i<HEX_HEIGHT/2;i++) {
        SDL_RenderDrawLine(gRenderer,x1+i,centery+i,x4-i,centery+i);
    }
    /*SDL_SetRenderDrawColor(gRenderer,0x00,0x00,0x00,0xFF);
    SDL_RenderDrawLine(gRenderer,x1+1,centery-1,x2+1,top);
    SDL_RenderDrawLine(gRenderer,x2+1,top,x3-1,top);
    SDL_RenderDrawLine(gRenderer,x3-1,top,x4-1,centery-1);
    SDL_RenderDrawLine(gRenderer,x4,centery,x3,bottom);
    SDL_RenderDrawLine(gRenderer,x3,bottom,x2,bottom);
    SDL_RenderDrawLine(gRenderer,x2,bottom,x1-1,centery-1);
    SDL_RenderDrawLine(gRenderer,x4-1,centery,x3-1,bottom);
    SDL_RenderDrawLine(gRenderer,x2+1,bottom,x1+1-1,centery-1);
    */
}

void updateDrawing() {
    SDL_RenderPresent(gRenderer);
}

void clearDrawing() {
    SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0xFF,0xFF);
    SDL_RenderClear(gRenderer);
}
