play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --dumpInitRngs


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --dumpInitRngs

configure-debug:
	cabal configure --enable-profiling --ghc-options="-fprof-auto-calls"

configure-prof:
	cabal configure --enable-profiling --ghc-option=-fprof-auto-exported -frelease

ghcjs-configure:
	cabal configure --disable-library-profiling --disable-profiling --ghcjs --ghcjs-option=-dedupe -f-release

prof-ghcjs:
	cabal configure --enable-profiling --ghc-option=-fprof-auto-exported --ghcjs --ghcjs-option=-dedupe -frelease

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" --password-store=default dist/build/LambdaHack/LambdaHack.jsexe/index.html

minific:
	java -jar ~/Downloads/closure-compiler.jar dist/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > ~/Downloads/all.js


frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 2 --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign

frontendRaid:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 5 --maxFps 60 --dumpInitRngs --automateAll --gameMode raid

frontendSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame 5 --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

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
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode campaign --setDungeonRng 120 --setMainRng 47 --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchAnimBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 100 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

benchCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode campaign --setDungeonRng 0 --setMainRng 0

benchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode campaign --setDungeonRng 0 --setMainRng 0

benchNull: benchBattle benchAnimBattle benchCampaign

bench:  benchBattle benchAnimBattle benchFrontendBattle benchCampaign benchFrontendCampaign

nativeBenchCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode campaign --setDungeonRng 0 --setMainRng 0

nativeBenchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 250 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nativeBench: nativeBenchBattle nativeBenchCampaign

nodeBenchCampaign:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode campaign --setDungeonRng 0 --setMainRng 0

nodeBenchBattle:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 250 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nodeBench: nodeBenchBattle nodeBenchCampaign


test-travis-short: test-short

test-travis-medium: test-short test-medium benchNull

test-travis-medium-no-safari: test-short test-medium-no-safari benchNull

test-travis-long: test-short test-long benchNull

test-travis-long-no-safari: test-short test-long-no-safari benchNull

test: test-short test-medium test-long benchNull

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testRaid-medium testSkirmish-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testSafari-medium testSafariSurvival-medium testCoop-medium testDefense-medium

test-medium-no-safari: testCampaign-medium testRaid-medium testSkirmish-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testRaid-medium testSkirmish-medium testAmbush-medium testBattle-long testBattleSurvival-long testSafari-long testSafariSurvival-long testDefense-long

test-long-no-safari: testCampaign-long testRaid-medium testSkirmish-medium testAmbush-medium testBattle-long testBattleSurvival-long testDefense-long

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode campaign > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 400 --dumpInitRngs --automateAll --keepAutomated --gameMode campaign > /tmp/stdtest.log

testRaid-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode raid > /tmp/stdtest.log

testRaid-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode raid > /tmp/stdtest.log

testSkirmish-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish > /tmp/stdtest.log

testAmbush-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush > /tmp/stdtest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush > /tmp/stdtest.log

testBattle-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode battle > /tmp/stdtest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 50 --dumpInitRngs --automateAll --keepAutomated --gameMode battle > /tmp/stdtest.log

testBattleSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 7 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" > /tmp/stdtest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 7 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 50 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" > /tmp/stdtest.log

testSafari-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 250 --dumpInitRngs --automateAll --keepAutomated --gameMode safari > /tmp/stdtest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode safari > /tmp/stdtest.log

testSafariSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 250 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" > /tmp/stdtest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" > /tmp/stdtest.log


testPvP-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode PvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 30 --dumpInitRngs --automateAll --keepAutomated --gameMode PvP > /tmp/stdtest.log

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode Coop > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode Coop > /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode defense > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 8 --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode defense > /tmp/stdtest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix campaign --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix skirmish --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix PvP --dumpInitRngs --automateAll --keepAutomated --gameMode PvP --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix Coop --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame 5 --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log

test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --dumpInitRngs --automateAll --keepAutomated --gameMode PvP --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfterSeconds 2 > /tmp/stdtest.log


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

# TODO: figure out, whey this must be so different from Linux
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
