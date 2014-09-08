module SDL_HitTestResult
( SDL_HitTestResult(..)
, SDL_HITTEST_NORMAL
, SDL_HITTEST_DRAGGABLE
, SDL_HITTEST_RESIZE_TOPLEFT
, SDL_HITTEST_RESIZE_TOP
, SDL_HITTEST_RESIZE_TOPRIGHT
, SDL_HITTEST_RESIZE_RIGHT
, SDL_HITTEST_RESIZE_BOTTOMRIGHT
, SDL_HITTEST_RESIZE_BOTTOM
, SDL_HITTEST_RESIZE_BOTTOMLEFT
, SDL_HITTEST_RESIZE_LEFT
)

data SDL_HitTestResult
    = SDL_HITTEST_NORMAL
    | SDL_HITTEST_DRAGGABLE
    | SDL_HITTEST_RESIZE_TOPLEFT
    | SDL_HITTEST_RESIZE_TOP
    | SDL_HITTEST_RESIZE_TOPRIGHT
    | SDL_HITTEST_RESIZE_RIGHT
    | SDL_HITTEST_RESIZE_BOTTOMRIGHT
    | SDL_HITTEST_RESIZE_BOTTOM
    | SDL_HITTEST_RESIZE_BOTTOMLEFT
    | SDL_HITTEST_RESIZE_LEFT

SDL_HITTEST_NORMAL = SDL_HITTEST_NORMAL
SDL_HITTEST_DRAGGABLE
SDL_HITTEST_RESIZE_TOPLEFT
SDL_HITTEST_RESIZE_TOP
SDL_HITTEST_RESIZE_TOPRIGHT
SDL_HITTEST_RESIZE_RIGHT
SDL_HITTEST_RESIZE_BOTTOMRIGHT
SDL_HITTEST_RESIZE_BOTTOM
SDL_HITTEST_RESIZE_BOTTOMLEFT
SDL_HITTEST_RESIZE_LEFT
