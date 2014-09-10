{-# LANGUAGE ForeignFunctionInterface #-}

module SDL
( sdl_init_timer         
, sdl_init_audio         
, sdl_init_video         
, sdl_init_joystick      
, sdl_init_haptic        
, sdl_init_gamecontroller
, sdl_init_events        
, sdl_init_noparachute   
, sdl_init_everything
, sdl_init
, sdl_initsubsystem
, sdl_quitsubsystem
, sdl_wasinit
, sdl_quit
) where

import Data.Bits
import Foreign.C
{-
import SDL_main
import SDL_stdinc
import SDL_assert
import SDL_atomic
import SDL_audio
import SDL_clipboard
import SDL_cpuinfo
import SDL_endian
import SDL_error
import SDL_events
import SDL_filesystem
import SDL_joystick
import SDL_gamecontroller
import SDL_haptic
import SDL_hints
import SDL_loadso
import SDL_log
import SDL_messagebox
import SDL_mutex
import SDL_power
import SDL_render
import SDL_rwops
import SDL_system
import SDL_thread
import SDL_timer
import SDL_version
import SDL_video
-}
sdl_init_timer          = 0x00000001 :: Int
sdl_init_audio          = 0x00000010 :: Int
sdl_init_video          = 0x00000020 :: Int
sdl_init_joystick       = 0x00000200 :: Int
sdl_init_haptic         = 0x00001000 :: Int
sdl_init_gamecontroller = 0x00002000 :: Int
sdl_init_events         = 0x00004000 :: Int
sdl_init_noparachute    = 0x00100000 :: Int
sdl_init_everything     =   sdl_init_timer 
                        .|. sdl_init_audio 
                        .|. sdl_init_video 
                        .|. sdl_init_events 
                        .|. sdl_init_joystick 
                        .|. sdl_init_haptic 
                        .|. sdl_init_gamecontroller

foreign import ccall unsafe "SDL2/SDL.h SDL_Init" c_SDL_Init :: CUInt -> IO(CInt)
sdl_init :: Int -> IO(Int)
sdl_init flags = fmap fromIntegral (c_SDL_Init $ fromIntegral flags)
--sdl_init flags = do
--    a <- ( c_SDL_Init (fromIntegral flags) )
--    return (fromIntegral a)

foreign import ccall unsafe "SDL2/SDL.h SDL_InitSubSystem" c_SDL_InitSubSystem :: CUInt -> IO(CInt)
sdl_initsubsystem :: Int -> IO(Int)
sdl_initsubsystem flags = fmap fromIntegral (c_SDL_InitSubSystem $ fromIntegral flags)

foreign import ccall unsafe "SDL2/SDL.h SDL_QuitSubSystem" c_SDL_QuitSubSystem :: CUInt -> IO()
sdl_quitsubsystem :: Int -> IO()
sdl_quitsubsystem flags = c_SDL_QuitSubSystem $ fromIntegral flags

foreign import ccall unsafe "SDL2/SDL.h SDL_WasInit" c_SDL_WasInit :: CUInt -> IO(CUInt)
sdl_wasinit :: Int -> IO(Int)
sdl_wasinit flags = fmap fromIntegral (c_SDL_WasInit $ fromIntegral flags)

foreign import ccall unsafe "SDL2/SDL.h SDL_Quit" c_SDL_Quit :: IO()
sdl_quit :: IO()
sdl_quit = c_SDL_Quit
