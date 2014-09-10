import SDL
main = do
    a <- sdl_init sdl_init_everything
    putStrLn (show a)
    b <- sdl_wasinit(0)
    putStrLn (show b)
    sdl_quit
