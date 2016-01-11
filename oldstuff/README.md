Green
=====

Adventures in Haskell
ghc --make -outputdir=bin/ -o bin/main main.hs -optc -std=c99 ffi/* -lSDL2 -lSDL2_ttf
ghc --make -outputdir=bin/ -o bin/main main.hs -optc -std=c99 ffi/* -lSDL2 -lSDL2_ttf -lSDL2_gfx && ./bin/main
