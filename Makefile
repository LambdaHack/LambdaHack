test: test-short test-medium test-long

test-long: testCampaign-long testCoop-long testDefense-long

test-medium: testCampaign-medium testCoop-medium testDefense-medium

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode screensaver --frontendStd --stopAfter 1000 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode screensaver --frontendStd --dumpConfig --stopAfter 200 > /tmp/stdtest.log

frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode screensaver

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testCoop --frontendStd --stopAfter 1000 > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix test --gameMode testCoop --frontendStd --dumpConfig --stopAfter 200 > /tmp/stdtest.log

frontendCoop:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testCoop

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noAnim --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --stopAfter 1000 > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --dumpConfig --stopAfter 200 > /tmp/stdtest.log

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testDefense

test-short: test-short-new test-short-load

test-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-travis: test-short test-medium

peekCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

peekSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish


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
