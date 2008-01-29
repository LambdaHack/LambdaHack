default :
	ghc -O -o HHack2 --make Main.hs

%.hs : %.hsc
	hsc2hs -Icbits/ Curses.hsc

Curses.o : Curses.hs
	ghc -ffi -c -Icbits/ Curses.hs

clean :
	rm -f *.o *.hi Curses.hs 
