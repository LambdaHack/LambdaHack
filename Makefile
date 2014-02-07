# All xc* tests assume a profiling build (for stack traces).
# See the install-debug target below or .travis.yml.prof.

install-debug:
	cabal install --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization

configure-debug:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer

xcpeekCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

xcpeekSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish

xcfrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testCampaign --difficulty 1

xcfrontendSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testSkirmish

xcfrontendPvP:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testPvP

xcfrontendCoop:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testCoop --difficulty 1

xcfrontendDefense:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testDefense --difficulty 9


xctest-travis: xctest-short xctest-medium

xctest-travis-long: xctest-short xctest-long

xctest: xctest-short xctest-medium xctest-long

xctest-short: xctest-short-new xctest-short-load

xctest-medium: xctestCampaign-medium xctestSkirmish-medium xctestPvP-medium xctestCoop-medium xctestDefense-medium

xctest-long: xctestCampaign-long xctestCoop-long xctestDefense-long

xctestCampaign-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testCampaign --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

xctestCampaign-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testCampaign --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

xctestSkirmish-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testSkirmish --frontendStd --dumpInitRngs --stopAfter 500 > /tmp/stdtest.log

xctestSkirmish-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testSkirmish --frontendStd --dumpInitRngs --stopAfter 120 > /tmp/stdtest.log

xctestPvP-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testPvP --frontendStd --dumpInitRngs --stopAfter 500 > /tmp/stdtest.log

xctestPvP-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testPvP --frontendStd --dumpInitRngs --stopAfter 120 > /tmp/stdtest.log

xctestCoop-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testCoop --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

xctestCoop-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix test --gameMode testCoop --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

xctestDefense-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noAnim --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

xctestDefense-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

xctest-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log

xctest-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log


play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer

peekCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

peekSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish

frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testCampaign --difficulty 1

frontendSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testSkirmish

frontendPvP:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testPvP

frontendCoop:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testCoop --difficulty 1

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testDefense --difficulty 9


test-travis: test-short test-medium

test-travis-long: test-short test-long

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testCoop-long testDefense-long

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testCampaign --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testCampaign --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testSkirmish --frontendStd --dumpInitRngs --stopAfter 500 > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode testSkirmish --frontendStd --dumpInitRngs --stopAfter 120 > /tmp/stdtest.log

testPvP-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testPvP --frontendStd --dumpInitRngs --stopAfter 500 > /tmp/stdtest.log

testPvP-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testPvP --frontendStd --dumpInitRngs --stopAfter 120 > /tmp/stdtest.log

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix test --gameMode testCoop --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix test --gameMode testCoop --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noAnim --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --dumpInitRngs --stopAfter 500 --difficulty 1 > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --dumpInitRngs --stopAfter 120 --difficulty 1 > /tmp/stdtest.log

test-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log

test-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log


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
	ghci -XCPP -idist/build/autogen:.
