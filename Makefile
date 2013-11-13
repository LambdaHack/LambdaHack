test:
	timeout 10m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix screensaver --gameMode screensaver --frontendStd > /tmp/stdtest.log; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

test-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix screensaver --gameMode screensaver

test-travis:
	timeout 1m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix screensaver --gameMode screensaver --frontendStd > /dev/null ; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

testCoop:
	timeout 10m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCoop --gameMode testCoop --frontendStd > /tmp/stdtest.log; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

testCoop-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix testCoop --gameMode testCoop

testCoop-travis:
	timeout 1m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCoop --gameMode testCoop --frontendStd > /dev/null ; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

testDefense:
	timeout 10m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd > /tmp/stdtest.log; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)

testDefense-frontend:
	dist/build/LambdaHack/LambdaHack --noMore --maxFps 45 --savePrefix testDefense --gameMode testDefense

testDefense-travis:
	timeout 1m dist/build/LambdaHack/LambdaHack --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd > /dev/null ; EXIT=$$? ; (if [ $$EXIT -eq 124 ] ; then echo "test OK" ; else (echo "test failed with $$EXIT" ; exit 1) ; fi)


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
