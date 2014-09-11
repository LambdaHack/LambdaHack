# All xc* tests assume a profiling build (for stack traces).
# See the install-debug target below or .travis.yml.prof.

install-debug:
	cabal install --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization

configure-debug:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --dumpInitRngs

xcfrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1

xcfrontendSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

xcfrontendAmbush:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

xcfrontendBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 1

xcfrontendSafari:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 1

xcfrontendDefense:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 9

xcbenchCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchFrontendBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

xcbenchNull: xcbenchCampaign xcbenchBattle

xcbench: xcbenchCampaign xcbenchFrontendCampaign xcbenchBattle xcbenchFrontendBattle


xctest-travis-short: xctest-short xcbenchNull

xctest-travis: xctest-short xctest-medium xcbenchNull

xctest-travis-long: xctest-short xctest-long xcbenchNull

xctest: xctest-short xctest-medium xctest-long

xctest-short: xctest-short-new xctest-short-load

xctest-medium: xctestCampaign-medium xctestSkirmish-medium xctestAmbush-medium xctestBattle-medium xctestSafari-medium xctestPvP-medium xctestCoop-medium xctestDefense-medium

xctest-long: xctestCampaign-long xctestSkirmish-long xctestAmbush-long xctestBattle-long xctestSafari-long xctestPvP-long xctestCoop-long xctestDefense-long

xctestCampaign-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

xctestCampaign-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

xctestSkirmish-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

xctestSkirmish-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

xctestAmbush-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

xctestAmbush-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

xctestBattle-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

xctestBattle-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 150 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

xctestSafari-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode safari --difficulty 1 > /tmp/stdtest.log

xctestSafari-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode safari --difficulty 1 > /tmp/stdtest.log

xctestPvP-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

xctestPvP-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

xctestCoop-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

xctestCoop-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

xctestDefense-long:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

xctestDefense-medium:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

xctest-short-new:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log

xctest-short-load:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --noMore --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log


play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --dumpInitRngs

frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1

frontendSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 1

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 1

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 9

benchCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 1 --setDungeonRng 42 --setMainRng 42

benchNull: benchCampaign benchBattle

bench: benchCampaign benchFrontendCampaign benchBattle benchFrontendBattle


test-travis-short: test-short benchNull

test-travis: test-short test-medium benchNull

test-travis-long: test-short test-long benchNull

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testAmbush-medium testBattle-medium testSafari-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testSkirmish-long testAmbush-long testBattle-long testSafari-long testPvP-long testCoop-long testDefense-long

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode campaign --difficulty 1 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testAmbush-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

testBattle-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 150 --dumpInitRngs --automateAll --gameMode battle --difficulty 1 > /tmp/stdtest.log

testSafari-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode safari --difficulty 1 > /tmp/stdtest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode safari --difficulty 1 > /tmp/stdtest.log

testPvP-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode Coop --difficulty 1 > /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noMore --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode defense --difficulty 9 > /tmp/stdtest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --noMore --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log

test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --noMore --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log


build-binary:
	cabal configure -frelease --prefix=/usr/local
	cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHack
	cabal copy --destdir=/tmp/LambdaHack
	tar -czf /tmp/LambdaHack_x_ubuntu-12.04-amd64.tar.gz -C /tmp --exclude=LambdaHack/usr/local/lib --exclude=LambdaHack/usr/local/share/doc LambdaHack

build-binary-i386:
	cabal configure -frelease --prefix=/usr/local --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386"
	cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHack
	cabal copy --destdir=/tmp/LambdaHack
	tar -czf /tmp/LambdaHack_x_ubuntu-12.04-i386.tar.gz -C /tmp --exclude=LambdaHack/usr/local/lib --exclude=LambdaHack/usr/local/share/doc LambdaHack

# TODO: figure out, whey this must be so different from Linux
build-binary-windows-i386:
	wine cabal -frelease configure
	wine cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHackInstall
	rm -rf /tmp/LambdaHack
	rm -rf /tmp/LambdaHack_x_windows-i386.zip
	wine cabal copy --destdir=Z:/tmp/LambdaHackInstall
	mkdir -p /tmp/LambdaHack/GameDefinition
	cp /tmp/LambdaHackInstall/users/mikolaj/Application\ Data/cabal/bin/LambdaHack.exe /tmp/LambdaHack
	cp /home/mikolaj/.wine/drive_c/users/mikolaj/gtk/bin/zlib1.dll /tmp/LambdaHack
	cp GameDefinition/PLAYING.md /tmp/LambdaHack/GameDefinition
	cp GameDefinition/scores /tmp/LambdaHack/GameDefinition
	cp GameDefinition/config.ui.default /tmp/LambdaHack/GameDefinition
	cp CHANGELOG.md /tmp/LambdaHack
	cp CREDITS /tmp/LambdaHack
	cp LICENSE /tmp/LambdaHack
	cp README.md /tmp/LambdaHack
	wine Z:/home/mikolaj/.local/share/wineprefixes/7zip/drive_c/Program\ Files/7-Zip/7z.exe a -ssc -sfx Z:/tmp/LambdaHack_x_windows-i386.exe Z:/tmp/LambdaHack


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
