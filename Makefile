test: test-shortest test-short test-long

test-long: testCampaign testCoop testDefense

test-short: testCampaign-short testCoop-short testDefense-short

testCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCampaign --gameMode screensaver --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCampaign-short:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --noDelay --noAnim --maxFps 100000 --savePrefix testCampaign --gameMode screensaver --frontendStd --stopAfter 60 > /tmp/stdtest.log

testCampaign-frontend:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --maxFps 45 --savePrefix screensaver --gameMode testCampaign

testCoop:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCoop-short:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 60 > /tmp/stdtest.log

testCoop-frontend:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --maxFps 180 --fovMode Permissive --savePrefix testCoop --gameMode testCoop

testDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --noAnim --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 500 > /tmp/stdtest.log

testDefense-short:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 60 > /tmp/stdtest.log

testDefense-frontend:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --maxFps 45 --savePrefix testDefense --gameMode testDefense

test-shortest:
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peek --gameMode peek --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-travis: test-shortest test-short

testPeek-play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peek --gameMode peek


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
