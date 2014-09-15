#include "SDL2/SDL.h"
#include "stdio.h"


const int HEX_WIDTH  = 200;
const int HEX_HEIGHT = 100;

SDL_Window* gWindow = NULL;
SDL_Renderer* gRenderer = NULL;


void myinitialize(int width, int height) {
    SDL_Init(SDL_INIT_VIDEO);
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY,"1");
    gWindow = SDL_CreateWindow( "test",SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_SHOWN);
    gRenderer = SDL_CreateRenderer(gWindow, -1, SDL_RENDERER_ACCELERATED);
    SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0xFF,0xFF);
    //IMG_Init(IMG_INIT_PNG);
}

void myclose() {
    SDL_DestroyRenderer(gRenderer);
    SDL_DestroyWindow(gWindow);
    //IMG_Quit();
    SDL_Quit();
}

void drawHexagon(int centerx,int centery,int red,int green,int blue) {
    int top=centery-HEX_HEIGHT/2;
    int bottom=centery+HEX_HEIGHT/2-1;
    int x1=centerx-HEX_WIDTH/2;
    int x2=centerx-HEX_WIDTH/4-1;
    int x3=centerx+HEX_WIDTH/4;
    int x4=centerx+HEX_WIDTH/2-1;
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

char getEvent() {
    SDL_Event e;
    if(SDL_PollEvent( &e ) != 0) {
        if(e.type == SDL_KEYDOWN) {
            switch( e.key.keysym.sym ) {
                case SDLK_w:
                    return 'w';
                case SDLK_q:
                    return 'q';
                case SDLK_e:
                    return 'e';
                case SDLK_s:
                    return 's';
                case SDLK_a:
                    return 'a';
                case SDLK_d:
                    return 'd';
                case SDLK_ESCAPE:
                    return '\x1B';
                default:
                    return ' ';
            }
        } else
            return ' ';
    } else
        return ' ';
}

/*int main(int argc, char* args[] ) {
    myinitialize();
    while(1) {
        SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0xFF,0xFF);
        SDL_RenderClear(gRenderer);
/        SDL_SetRenderDrawColor(gRenderer,0xFF,0x00,0x00,0xFF);
        SDL_Rect fillRect = {200,150,400,300};
        SDL_RenderFillRect(gRenderer,&fillRect);
        SDL_SetRenderDrawColor(gRenderer,0x00,0xFF,0x00,0xFF);
        SDL_Rect outlineRect = {133,100,533,400};
        SDL_RenderDrawRect(gRenderer,&outlineRect);
        SDL_SetRenderDrawColor(gRenderer,0x00,0x00,0xFF,0xFF);
        SDL_RenderDrawLine(gRenderer,0,300,800,300);
        SDL_SetRenderDrawColor(gRenderer,0xFF,0xFF,0x00,0xFF);
        for( int i=0;i<SCREEN_HEIGHT;i+=4) {
            SDL_RenderDrawPoint(gRenderer,400,i);
        }/
        drawHexagon(400,300,0,0xFF,0xFF);
        SDL_RenderPresent(gRenderer);
    }
    myclose();
    return 0;
}*/
/*int main(int argc, char* args[] ) {
    myinitialize(800,600);
    clearDrawing();
    SDL_SetRenderDrawColor(gRenderer,0xFF,0,0,0xFF);
    SDL_RenderDrawLine(gRenderer,1,1,799,1);
    SDL_SetRenderDrawColor(gRenderer,0,0xFF,0,0xFF);
    SDL_RenderDrawLine(gRenderer,799,1,799,599);
    SDL_SetRenderDrawColor(gRenderer,0,0,0xFF,0xFF);
    SDL_RenderDrawLine(gRenderer,799,599,1,599);
    SDL_SetRenderDrawColor(gRenderer,0,0,0,0xFF);
    SDL_RenderDrawLine(gRenderer,1,599,1,1);
    updateDrawing();
    while(1){
    }
    myclose();
    return 0;
}*/
