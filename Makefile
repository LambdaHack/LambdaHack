# All xc* tests assume a profiling build (for stack traces).
# See the install-debug target below or .travis.yml.prof.

install-debug:
	cabal install --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization

configure-debug:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization


xcplay:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --dumpInitRngs

xcfrontendCampaign:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2

xcfrontendSkirmish:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

xcfrontendAmbush:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

xcfrontendBattle:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 2

xcfrontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode "battle survival" --difficulty 8

xcfrontendSafari:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 2

xcfrontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode "safari survival" --difficulty 8

xcfrontendDefense:
	dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 8


play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --dumpInitRngs

frontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2

frontendSkirmish:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 2

frontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode "battle survival" --difficulty 8

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 2

frontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode "safari survival" --difficulty 8

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 8

benchCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --keepAutomated --gameMode campaign --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --keepAutomated --gameMode battle --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchFrontendCampaign:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --benchmark --stopAfter 60 --automateAll --keepAutomated --gameMode campaign --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --benchmark --stopAfter 60 --automateAll --keepAutomated --gameMode battle --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchNull: benchCampaign benchBattle

bench: benchCampaign benchFrontendCampaign benchBattle benchFrontendBattle


test-travis-short: test-short

test-travis-medium: test-short test-medium

test-travis-medium-no-safari: test-short test-medium-no-safari

test-travis-long: test-short test-long

test-travis-long-no-safari: test-short test-long-no-safari

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testSafari-medium testSafariSurvival-medium testPvP-medium testCoop-medium testDefense-medium

test-medium-no-safari: testCampaign-medium testSkirmish-medium testAmbush-medium testBattle-medium testBattleSurvival-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testSkirmish-medium testAmbush-medium testBattle-long testBattleSurvival-long testSafari-long testSafariSurvival-long testPvP-medium testDefense-long

test-long-no-safari: testCampaign-long testSkirmish-medium testAmbush-medium testBattle-long testBattleSurvival-long testPvP-medium testDefense-long

testCampaign-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --difficulty 2 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --difficulty 2 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish > /tmp/stdtest.log

testAmbush-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush > /tmp/stdtest.log

testAmbush-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush > /tmp/stdtest.log

testBattle-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 100 --dumpInitRngs --automateAll --keepAutomated --gameMode battle --difficulty 2 > /tmp/stdtest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 50 --dumpInitRngs --automateAll --keepAutomated --gameMode battle --difficulty 2 > /tmp/stdtest.log

testBattleSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 100 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --difficulty 8 > /tmp/stdtest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 50 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --difficulty 8 > /tmp/stdtest.log

testSafari-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 250 --dumpInitRngs --automateAll --keepAutomated --gameMode safari --difficulty 2 > /tmp/stdtest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --keepAutomated --gameMode safari --difficulty 2 > /tmp/stdtest.log

testSafariSurvival-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 250 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --difficulty 8 > /tmp/stdtest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --difficulty 8 > /tmp/stdtest.log


testPvP-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --keepAutomated --gameMode PvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --keepAutomated --gameMode PvP > /tmp/stdtest.log

testCoop-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --difficulty 2 > /tmp/stdtest.log

testCoop-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 300 --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --difficulty 2 > /tmp/stdtest.log

testDefense-long:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --keepAutomated --gameMode defense --difficulty 8 > /tmp/stdtest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 300 --dumpInitRngs --automateAll --keepAutomated --gameMode defense --difficulty 8 > /tmp/stdtest.log

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix campaign --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix skirmish --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix PvP --dumpInitRngs --automateAll --keepAutomated --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix Coop --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --newGame --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log

test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix campaign --dumpInitRngs --automateAll --keepAutomated --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix skirmish --dumpInitRngs --automateAll --keepAutomated --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix PvP --dumpInitRngs --automateAll --keepAutomated --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix Coop --dumpInitRngs --automateAll --keepAutomated --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log


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
