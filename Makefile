play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs

shot:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs --printEachScreen

expose-lore:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --dumpInitRngs --savePrefix know --newGame 2 --gameMode crawl --knowItems --benchmark --noAnim --maxFps 1000

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions -fwith_expensive_assertions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions -frelease

ghcjs-configure:
	cabal configure --disable-library-profiling --disable-profiling --ghcjs --ghcjs-option=-dedupe -f-release

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" ../lambdahack.github.io/index.html

minific:
	ccjs dist/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=node --externs=dist/build/LambdaHack/LambdaHack.jsexe/all.js.externs > ../lambdahack.github.io/lambdahack.all.js

# Low delay to display animations swiftly and not bore the public too much.
# Delay can't be lower than 2, because browsers sometimes treat delay 1
# specially and add their extra delay.
create-gif :
	find ~/.LambdaHack/screenshots/ -name 'prtscn*.bmp' -print0 | xargs -0 -r mogrify -format gif
	gifsicle -O3 --careful -d2 -l ~/.LambdaHack/screenshots/prtscn*.gif -o ~/.LambdaHack/screenshots/screenshot.gif

frontendRaid:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode raid

frontendBrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode brawl

frontendShootout:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode shootout

frontendEscape:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 3 --dumpInitRngs --automateAll --gameMode escape

frontendZoo:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode zoo

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode ambush

frontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl

frontendCrawlEmpty:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode "crawl empty"

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode safari

frontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "safari survival"

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode battle

frontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "battle survival"

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defense

frontendDefenseEmpty:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode "defense empty"


benchMemoryAnim:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 120 --setMainRng 47 --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchAnimBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

benchFrontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

benchNull: benchBattle benchAnimBattle benchCrawl

bench: benchBattle benchAnimBattle benchFrontendBattle benchCrawl benchFrontendCrawl

nativeBenchCrawl:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --logPriority 4 --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

nativeBenchBattle:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nativeBench: nativeBenchBattle nativeBenchCrawl

nodeBenchCrawl:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl --setDungeonRng 0 --setMainRng 0

nodeBenchBattle:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nodeBench: nodeBenchBattle nodeBenchCrawl


test-travis: test-short test-medium benchNull

test: test-short test-medium benchNull

test-short: test-short-new test-short-load

test-medium: testRaid-medium testBrawl-medium testShootout-medium testEscape-medium testZoo-medium testAmbush-medium testCrawl-medium testCrawlEmpty-medium testCrawl-medium-know testSafari-medium testSafariSurvival-medium testBattle-medium testBattleSurvival-medium testDefenseEmpty-medium

testRaid-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/teletypetest.log

testBrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/teletypetest.log

testShootout-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode shootout 2> /tmp/teletypetest.log

testEscape-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 3 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode escape 2> /tmp/teletypetest.log

testZoo-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 2 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode zoo 2> /tmp/teletypetest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/teletypetest.log

testCrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl 2> /tmp/teletypetest.log

testCrawlEmpty-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "crawl empty" 2> /tmp/teletypetest.log

testCrawl-medium-know:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix know --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --knowItems 2> /tmp/teletypetest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 2 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/teletypetest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 8 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/teletypetest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/teletypetest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/teletypetest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/teletypetest.log

testDefenseEmpty-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "defense empty" 2> /tmp/teletypetest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log

# "--setDungeonRng 0 --setMainRng 0" is needed for determinism relative to seed
# generated before game save
test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootouti --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 --setDungeonRng 0 --setMainRng 0 2> /tmp/teletypetest.log


version:
	dist/build/LambdaHack/LambdaHack --version

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
	cp COPYLEFT LambdaHackTheGame
	cp CREDITS LambdaHackTheGame

build-binary-ubuntu: build-binary-common
	cp LambdaHackTheGameInstall/bin/LambdaHack LambdaHackTheGame
	dist/build/LambdaHack/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	tar -czf LambdaHack_$${LH_VERSION}_ubuntu-16.04-amd64.tar.gz LambdaHackTheGame

build-binary-macosx: build-binary-common
	cp LambdaHackTheGameInstall/bin/LambdaHack LambdaHackTheGame
	dist/build/LambdaHack/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	OS_VERSION=$$(sw_vers -productVersion); \
	tar -czf LambdaHack_$${LH_VERSION}_macosx-$${OS_VERSION}-amd64.tar.gz LambdaHackTheGame

new-build-dev:
	cabal new-build --datadir=. --disable-optimization -j1
