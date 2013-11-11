test:
	timeout 10m dist/build/LambdaHack/LambdaHack --noMore --noDelay --maxFps 100000 >> /tmp/log; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

test-interactive:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45


# The rest of the makefile is unmaintained at the moment.

default : dist/setup-config
	runghc Setup build

dist/setup-config : LambdaHack.cabal
	runghc Setup configure -fvty --user

vty :
	runghc Setup configure -fvty --user

gtk :
	runghc Setup configure --user

curses :
	runghc Setup configure -fcurses --user

clean :
	runghc Setup clean

ghci :
	ghci -XCPP -idist/build/autogen:Game/LambdaHack
