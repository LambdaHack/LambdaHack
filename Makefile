play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --dumpInitRngs


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --dumpInitRngs

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions -frelease

ghcjs-configure:
	cabal configure --disable-library-profiling --disable-profiling --ghcjs --ghcjs-option=-dedupe -f-release

prof-ghcjs:
	cabal configure --enable-profiling --ghc-option=-fprof-auto-exported --ghcjs --ghcjs-option=-dedupe -frelease

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" dist/build/LambdaHack/LambdaHack.jsexe/index.html

minific:
	java -jar ~/Downloads/closure-compiler.jar dist/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > ~/Downloads/all.js


frontendExploration:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 2 --maxFps 60 --dumpInitRngs --automateAll --gameMode exploration

frontendRaid:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 5 --maxFps 60 --dumpInitRngs --automateAll --gameMode raid

frontendBrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 5 --maxFps 60 --dumpInitRngs --automateAll --gameMode brawl

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 5 --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 3 --maxFps 60 --dumpInitRngs --automateAll --gameMode battle

frontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 7 --maxFps 60 --dumpInitRngs --automateAll --gameMode "battle survival"

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 2 --maxFps 60 --dumpInitRngs --automateAll --gameMode safari

frontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 8 --maxFps 60 --dumpInitRngs --automateAll --gameMode "safari survival"

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 8 --maxFps 60 --dumpInitRngs --automateAll --gameMode defense

benchMemoryAnim:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 120 --setMainRng 47 --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchAnimBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 4500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchExploration:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

benchFrontendExploration:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

benchNull: benchBattle benchAnimBattle benchExploration

bench:  benchBattle benchAnimBattle benchFrontendBattle benchExploration benchFrontendExploration

nativeBenchExploration:
	dist/build/LambdaHack/LambdaHack                   --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

nativeBenchBattle:
	dist/build/LambdaHack/LambdaHack                   --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nativeBench: nativeBenchBattle nativeBenchExploration

nodeBenchExploration:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

nodeBenchBattle:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nodeBench: nodeBenchBattle nodeBenchExploration


test-travis-short: test-short

test-travis-medium: test-short test-medium benchNull

test-travis-medium-no-safari: test-short test-medium-no-safari benchNull

test-travis-long: test-short test-long benchNull

test-travis-long-no-safari: test-short test-long-no-safari benchNull

test: test-short test-medium test-long benchNull

test-short: test-short-new test-short-load

test-medium: testExploration-medium testRaid-medium testBrawl-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testSafari-medium testSafariSurvival-medium testDefense-medium

test-medium-no-safari: testExploration-medium testRaid-medium testBrawl-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testDefense-medium

test-long: testExploration-long testRaid-medium testBrawl-medium testAmbush-medium testBattle-long testBattleSurvival-long testSafari-long testSafariSurvival-long testDefense-long

test-long-no-safari: testExploration-long testRaid-medium testBrawl-medium testAmbush-medium testBattle-long testBattleSurvival-long testDefense-long

testExploration-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode exploration 2> /tmp/stdtest.log

testExploration-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode exploration 2> /tmp/stdtest.log

testRaid-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/stdtest.log

testRaid-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/stdtest.log

testBrawl-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/stdtest.log

testBrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/stdtest.log

testAmbush-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/stdtest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/stdtest.log

testBattle-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/stdtest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/stdtest.log

testBattleSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 7 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/stdtest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 7 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/stdtest.log

testSafari-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/stdtest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/stdtest.log

testSafariSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/stdtest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/stdtest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix exploration --dumpInitRngs --automateAll --keepAutomated --gameMode exploration --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log

test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix exploration --dumpInitRngs --automateAll --keepAutomated --gameMode exploration --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfterSeconds 2 2> /tmp/stdtest.log


build-binary:
	cabal configure -frelease --prefix=/
	cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHack_x_ubuntu-12.04-amd64.tar.gz
	rm -rf /tmp/LambdaHackTheGameInstall
	rm -rf /tmp/LambdaHackTheGame
	mkdir -p /tmp/LambdaHackTheGame/GameDefinition
	cabal copy --destdir=/tmp/LambdaHackTheGameInstall
	cp /tmp/LambdaHackTheGameInstall/bin/LambdaHack /tmp/LambdaHackTheGame
	cp GameDefinition/PLAYING.md /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/scores /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/config.ui.default /tmp/LambdaHackTheGame/GameDefinition
	cp CHANGELOG.md /tmp/LambdaHackTheGame
	cp CREDITS /tmp/LambdaHackTheGame
	cp LICENSE /tmp/LambdaHackTheGame
	cp README.md /tmp/LambdaHackTheGame
	tar -czf /tmp/LambdaHack_x_ubuntu-12.04-amd64.tar.gz -C /tmp LambdaHackTheGame

build-binary-i386:
	cabal configure -frelease --prefix=/ --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386"
	cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHack_x_ubuntu-12.04-i386.tar.gz
	rm -rf /tmp/LambdaHackTheGameInstall
	rm -rf /tmp/LambdaHackTheGame
	mkdir -p /tmp/LambdaHackTheGame/GameDefinition
	cabal copy --destdir=/tmp/LambdaHackTheGameInstall
	cp /tmp/LambdaHackTheGameInstall/bin/LambdaHack /tmp/LambdaHackTheGame
	cp GameDefinition/PLAYING.md /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/scores /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/config.ui.default /tmp/LambdaHackTheGame/GameDefinition
	cp CHANGELOG.md /tmp/LambdaHackTheGame
	cp CREDITS /tmp/LambdaHackTheGame
	cp LICENSE /tmp/LambdaHackTheGame
	cp README.md /tmp/LambdaHackTheGame
	tar -czf /tmp/LambdaHack_x_ubuntu-12.04-i386.tar.gz -C /tmp LambdaHackTheGame

# It's a pity this is so different from Linux
build-binary-windows-i386:
	wine cabal configure -frelease
	wine cabal build exe:LambdaHack
	rm -rf /tmp/LambdaHack_x_windows-i386.zip
	rm -rf /tmp/LambdaHackTheGameInstall
	rm -rf /tmp/LambdaHackTheGame
	mkdir -p /tmp/LambdaHackTheGame/GameDefinition
	wine cabal copy --destdir=Z:/tmp/LambdaHackTheGameInstall
	cp /tmp/LambdaHackTheGameInstall/users/mikolaj/Application\ Data/cabal/bin/LambdaHack.exe /tmp/LambdaHackTheGame
	cp GameDefinition/PLAYING.md /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/scores /tmp/LambdaHackTheGame/GameDefinition
	cp GameDefinition/config.ui.default /tmp/LambdaHackTheGame/GameDefinition
	cp CHANGELOG.md /tmp/LambdaHackTheGame
	cp CREDITS /tmp/LambdaHackTheGame
	cp LICENSE /tmp/LambdaHackTheGame
	cp README.md /tmp/LambdaHackTheGame
	cp /home/mikolaj/.wine/drive_c/users/mikolaj/gtk/bin/zlib1.dll /tmp/LambdaHackTheGame
	wine Z:/home/mikolaj/.local/share/wineprefixes/7zip/drive_c/Program\ Files/7-Zip/7z.exe a -ssc -sfx Z:/tmp/LambdaHack_x_windows-i386.exe Z:/tmp/LambdaHackTheGame
