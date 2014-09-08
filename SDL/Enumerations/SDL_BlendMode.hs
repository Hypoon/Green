module SDL_BlendMode
( SDL_BlendMode(..)
, SDL_BLENDMODE_NONE
, SDL_BLENDMODE_BLEND
, SDL_BLENDMODE_ADD
, SDL_BLENDMODE_MOD
) where

type SDL_BlendMode = Int

SDL_BLENDMODE_NONE  = 0x00000000
SDL_BLENDMODE_BLEND = 0x00000001
SDL_BLENDMODE_ADD   = 0x00000002
SDL_BLENDMODE_MOD   = 0x00000004