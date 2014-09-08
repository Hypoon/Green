module SDL
(
) where

import Data.Bits
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

sdl_init_timer          = 0x00000001
sdl_init_audio          = 0x00000010
sdl_init_video          = 0x00000020
sdl_init_joystick       = 0x00000200
sdl_init_haptic         = 0x00001000
sdl_init_gamecontroller = 0x00002000
sdl_init_events         = 0x00004000
sdl_init_noparachute    = 0x00100000
sdl_init_everything     =   sdl_init_timer 
                        .|. sdl_init_audio 
                        .|. sdl_init_video 
                        .|. sdl_init_events 
                        .|. sdl_init_joystick 
                        .|. sdl_init_haptic 
                        .|. sdl_init_gamecontroller
