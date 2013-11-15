test:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix screensaver --gameMode screensaver --frontendStd --stopAfter 500 > /tmp/stdtest.log

test-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix screensaver --gameMode screensaver

test-travis:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix screensaver --gameMode screensaver --frontendStd --stopAfter 60 > /dev/null

testCoop:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCoop-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix testCoop --gameMode testCoop

testCoop-travis:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 60 > /dev/null

testDefense:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 500 > /tmp/stdtest.log

testDefense-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix testDefense --gameMode testDefense

testDefense-travis:
	dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 60 > /dev/null


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
