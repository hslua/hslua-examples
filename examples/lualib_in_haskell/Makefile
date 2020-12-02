LUA_LIBDIR = /usr/include/lua5.3

run: export LD_LIBRARY_PATH := .
run: libarith.so lualibhelper.so
	lua main.lua

# Assumes GHC 8.8.4, as used in stackage lts-16.24
libarith.so: LibArith.hs
	stack ghc -- $^ -o $@ -shared -fPIC -dynamic -lHSrts-ghc8.8.4

lualibhelper.so: libarith.so libarithhelper.c
	stack ghc -- libarithhelper.c \
	  -no-hs-main -o $@ \
	  -shared -fPIC -dynamic \
		-L. -larith -I$(LUA_LIBDIR)

clean:
	rm -f *.hi
	rm -f *.o
	rm -f *.so
	rm -f *_stub.h
