play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --dumpInitRngs

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions -fwith_expensive_assertions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions -frelease

ghcjs-configure:
	cabal configure --disable-library-profiling --disable-profiling --ghcjs --ghcjs-option=-dedupe -f-release

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" ../lambdahack.github.io/index.html

minific:
	ccjs dist/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=node > ../lambdahack.github.io/lambdahack.all.js

frontendRaid:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode raid

frontendBrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode brawl

frontendShootout:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode shootout

frontendEscape:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 3 --dumpInitRngs --automateAll --gameMode escape

frontendZoo:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode zoo

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode ambush

frontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode safari

frontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "safari survival"

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode battle

frontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "battle survival"

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defense


benchMemoryAnim:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 1 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 120 --setMainRng 47 --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchAnimBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

benchFrontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 1 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

benchNull: benchBattle benchAnimBattle benchCrawl

bench: benchBattle benchAnimBattle benchFrontendBattle benchCrawl benchFrontendCrawl

nativeBenchCrawl:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

nativeBenchBattle:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nativeBench: nativeBenchBattle nativeBenchCrawl

nodeBenchCrawl:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

nodeBenchBattle:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nodeBench: nodeBenchBattle nodeBenchCrawl


test-travis-short: test-short

test-travis: test-short test-medium benchNull

test: test-short test-medium benchNull

test-short: test-short-new test-short-load

test-medium: testRaid-medium testBrawl-medium testShootout-medium testEscape-medium testZoo-medium testAmbush-medium testCrawl-medium testSafari-medium testSafariSurvival-medium testBattle-medium testBattleSurvival-medium

testRaid-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/teletypetest.log

testBrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/teletypetest.log

testShootout-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode shootout 2> /tmp/teletypetest.log

testEscape-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 3 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode escape 2> /tmp/teletypetest.log

testZoo-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 2 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode zoo 2> /tmp/teletypetest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/teletypetest.log

testCrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl 2> /tmp/teletypetest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 2 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/teletypetest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 8 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/teletypetest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/teletypetest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/teletypetest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/teletypetest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log

# "--setDungeonRng 0 --setMainRng 0" is needed for determinism relative to seed
# generated before game save
test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootouti --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --boostRandomItem --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log


build-binary-common:
	cabal install --disable-library-profiling --disable-profiling --disable-documentation -f-release --only-dependencies
	cabal configure --disable-library-profiling --disable-profiling -f-release --prefix=/ --datadir=. --datasubdir=.
	cabal build exe:LambdaHack
	mkdir -p LambdaHackTheGame/GameDefinition/fonts
	cabal copy --destdir=LambdaHackTheGameInstall
	cp GameDefinition/config.ui.default LambdaHackTheGame/GameDefinition
	cp GameDefinition/fonts/16x16x.fon LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/8x8xb.fon LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/8x8x.fon LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/LICENSE.16x16x LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/Fix15Mono-Bold.woff LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/LICENSE.Fix15Mono-Bold LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/PLAYING.md LambdaHackTheGame/GameDefinition
	cp GameDefinition/InGameHelp.txt LambdaHackTheGame/GameDefinition
	cp README.md LambdaHackTheGame
	cp CHANGELOG.md LambdaHackTheGame
	cp LICENSE LambdaHackTheGame
	cp CREDITS LambdaHackTheGame

build-binary: build-binary-common
	cp LambdaHackTheGameInstall/bin/LambdaHack LambdaHackTheGame
	tar -czf LambdaHack_x_ubuntu-16.04-amd64.tar.gz LambdaHackTheGame

new-build-dev:
	cabal new-build --datadir=. --disable-optimization -j1
