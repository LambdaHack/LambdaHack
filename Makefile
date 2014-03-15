# All xc* tests assume a profiling build (for stack traces).
# See the install-debug target below or .travis.yml.prof.

install-debug:
	cabal install --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization

configure-debug:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer

xcpeekCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign

xcpeekSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish

xcfrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode campaign --difficulty 1

xcfrontendSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode skirmish

xcfrontendBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode battle --difficulty 1

xcfrontendPvP:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode PvP --fovMode Shadow

xcfrontendCoop:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 --fovMode Permissive

xcfrontendDefense:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode defense --difficulty 9

xcbenchCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchFrontendBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchNull: xcbenchCampaign xcbenchBattle

xcbench: xcbenchCampaign xcbenchFrontendCampaign xcbenchBattle xcbenchFrontendBattle

xctest-travis: xctest-short xctest-medium xcbenchNull

xctest-travis-long: xctest-short xctest-long xcbenchNull

xctest: xctest-short xctest-medium xctest-long

xctest-short: xctest-short-new xctest-short-load

xctest-medium: xctestCampaign-medium xctestSkirmish-medium xctestBattle-medium xctestPvP-medium xctestCoop-medium xctestDefense-medium

xctest-long: xctestCampaign-long xctestCoop-long xctestDefense-long

xctestCampaign-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

xctestCampaign-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

xctestSkirmish-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 58 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

xctestSkirmish-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 28 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

xctestBattle-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

xctestBattle-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

xctestPvP-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 58 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

xctestPvP-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 28 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

xctestCoop-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

xctestCoop-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

xctestDefense-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

xctestDefense-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

xctest-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix campaign --dumpInitRngs --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix skirmish --dumpInitRngs --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix battle --dumpInitRngs --gameMode battle --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix PvP --dumpInitRngs --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix Coop --dumpInitRngs --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix defense --dumpInitRngs --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

xctest-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix campaign --dumpInitRngs --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix skirmish --dumpInitRngs --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix battle --dumpInitRngs --gameMode battle --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix PvP --dumpInitRngs --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix Coop --dumpInitRngs --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix defense --dumpInitRngs --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log


play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer

peekCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign

peekSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish

frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode campaign --difficulty 1

frontendSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode skirmish

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode battle --difficulty 1

frontendPvP:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode PvP --fovMode Shadow

frontendCoop:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 --fovMode Permissive

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 180 --savePrefix test --dumpInitRngs --automateAll --gameMode defense --difficulty 9

benchCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchNull: benchCampaign benchBattle

bench: benchCampaign benchFrontendCampaign benchBattle benchFrontendBattle


test-travis: test-short test-medium benchNull

test-travis-long: test-short test-long benchNull

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testBattle-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testCoop-long testDefense-long

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 58 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 28 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testBattle-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

testPvP-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 58 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 28 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 580 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --benchmark --stopAfter 280 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

test-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix campaign --dumpInitRngs --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix skirmish --dumpInitRngs --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix battle --dumpInitRngs --gameMode battle --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix PvP --dumpInitRngs --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix Coop --dumpInitRngs --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix defense --dumpInitRngs --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --dumpInitRngs --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --dumpInitRngs --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battle --dumpInitRngs --gameMode battle --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --dumpInitRngs --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --dumpInitRngs --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --dumpInitRngs --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekCampaign --dumpInitRngs --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix peekSkirmish --dumpInitRngs --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log



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
