play:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs

shot:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs --printEachScreen

expose-lore:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode crawl --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

dig-lore:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode dig --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

see-caves:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode see --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions -fwith_expensive_assertions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions

ghcjs-new-build:
	cabal new-build -j1 --ghcjs --disable-library-profiling --disable-profiling .

chrome-log:
	google-chrome --enable-logging --v=1 file:///home/mikolaj/r/lambdahack.github.io/index.html &

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" ../lambdahack.github.io/index.html

minific:
	npx google-closure-compiler dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/LambdaHack-0.10.0.0/x/LambdaHack/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --externs=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/LambdaHack-0.10.0.0/x/LambdaHack/build/LambdaHack/LambdaHack.jsexe/all.js.externs --jscomp_off="*" > ../lambdahack.github.io/lambdahack.all.js

minificForNode:
	npx google-closure-compiler dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/LambdaHack-0.10.0.0/x/LambdaHack/build/LambdaHack/LambdaHack.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --externs=dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/LambdaHack-0.10.0.0/x/LambdaHack/build/LambdaHack/LambdaHack.jsexe/all.js.externs --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/assert.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/child_process.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/crypto.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/dns.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/events.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/globals.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/https.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/os.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/punycode.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/readline.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/stream.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/tls.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/url.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/vm.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/buffer.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/cluster.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/dgram.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/domain.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/fs.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/http.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/net.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/path.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/querystring.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/repl.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/string_decoder.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/tty.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/util.js --externs=/home/mikolaj/r/closure-compiler/contrib/nodejs/zlib.js --jscomp_off="*" > ../lambdahack.github.io/lambdahack.all.js

# Low delay to display animations swiftly and not bore the public too much.
# Delay can't be lower than 2, because browsers sometimes treat delay 1
# specially and add their extra delay.
create-gif :
	find ~/.LambdaHack/screenshots/ -name 'prtscn*.bmp' -print0 | xargs -0 -r mogrify -format gif
	gifsicle -O3 --careful -d2 -l ~/.LambdaHack/screenshots/prtscn*.gif -o ~/.LambdaHack/screenshots/screenshot.gif

frontendRaid:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode raid --exposeActors

frontendBrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode brawl

frontendShootout:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode shootout

frontendHunt:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode hunt

frontendEscape:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 3 --dumpInitRngs --automateAll --gameMode escape

frontendZoo:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode zoo --exposeActors

frontendAmbush:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode ambush

frontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl --exposeItems --exposeActors

frontendCrawlEmpty:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode "crawl empty"

frontendSafari:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode safari --exposeActors

frontendSafariSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "safari survival" --exposeActors

frontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 4 --dumpInitRngs --automateAll --gameMode battle --exposeActors

frontendBattleDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 6 --dumpInitRngs --automateAll --gameMode "battle defense" --exposeActors

frontendBattleSurvival:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 6 --dumpInitRngs --automateAll --gameMode "battle survival" --exposeActors

frontendDefense:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defense --exposeItems --exposeActors

frontendDefenseEmpty:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode "defense empty"

fastCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl --exposeItems --exposeActors --showItemSamples --noAnim --maxFps 100000

# different benchmarks use(d) different arguments
RNGOPTS=--setDungeonRng "SMGen 123 123" --setMainRng "SMGen 125 125"
RNGOPTS1=--setDungeonRng "SMGen 127 123" --setMainRng "SMGen 125 125"
RNGOPTS2=--setDungeonRng "SMGen 129 123" --setMainRng "SMGen 125 125"

benchMemoryAnim:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS2)  --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchAnimBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchFrontendBattle:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

benchCrawl-new-build:
	$$(cabal-plan list-bin LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

benchFrontendCrawl:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

benchDig:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1 --automateAll --keepAutomated --gameMode dig $(RNGOPTS)

benchNull: benchBattle benchAnimBattle benchCrawl

bench: benchBattle benchAnimBattle benchFrontendBattle benchCrawl benchFrontendCrawl

nativeBenchCrawl:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

nativeBenchBattle:
	dist/build/LambdaHack/LambdaHack		   --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)

nativeBench: nativeBenchBattle nativeBenchCrawl

nodeBenchCrawl:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

nodeBenchBattle:
	node dist/build/LambdaHack/LambdaHack.jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)

# needs installed cabal-plan
nodeBenchCrawl-new-build:
	node $$(cabal-plan list-bin LambdaHack).jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

nodeBenchBattle-new-build:
	node $$(cabal-plan list-bin LambdaHack).jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)

nodeBench: nodeBenchBattle nodeBenchCrawl


test-travis: test-sniff test-short test-medium benchNull

test: test-sniff test-short test-medium benchNull

test-short: test-short-new test-short-load

test-medium: testRaid-medium testBrawl-medium testShootout-medium testHunt-medium testEscape-medium testZoo-medium testAmbush-medium testCrawl-medium testCrawlEmpty-medium testCrawl-medium-know testSafari-medium testSafariSurvival-medium testBattle-medium testBattleDefense-medium testBattleSurvival-medium testDig-medium testDefenseEmpty-medium

test-sniff:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterFrames 1  --dumpInitRngs --automateAll --keepAutomated --gameMode raid --sniff 2> /tmp/teletypetest.log

testRaid-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/teletypetest.log

testBrawl-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/teletypetest.log

testShootout-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode shootout 2> /tmp/teletypetest.log

testHunt-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode hunt 2> /tmp/teletypetest.log

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
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix know --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples 2> /tmp/teletypetest.log

testSafari-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 2 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/teletypetest.log

testSafariSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 8 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/teletypetest.log

testBattle-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/teletypetest.log

testBattleDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle defense" 2> /tmp/teletypetest.log

testBattleSurvival-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/teletypetest.log

testDefense-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 9 --noAnim --maxFps 100000 --frontendLazy --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/teletypetest.log

testDig-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterFrames 100 --dumpInitRngs --automateAll --keepAutomated --gameMode dig 2> /tmp/teletypetest.log

testDefenseEmpty-medium:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode "defense empty" 2> /tmp/teletypetest.log

testCrawl-appveyor:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterSeconds 800 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl

testDefense-appveyor:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --newGame 9 --noAnim --maxFps 100000 --frontendLazy --benchmark --stopAfterSeconds 800 --dumpInitRngs --automateAll --keepAutomated --gameMode defense

test-short-new:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix hunt --dumpInitRngs --automateAll --keepAutomated --gameMode hunt --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battle --showItemSamples --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battleDefense --dumpInitRngs --automateAll --keepAutomated --gameMode "battle defense" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log

# $(RNGOPTS) is needed for determinism relative to seed
# generated before game save
test-short-load:
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix hunt --dumpInitRngs --automateAll --keepAutomated --gameMode hunt --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battleDefense --dumpInitRngs --automateAll --keepAutomated --gameMode "battle defense" --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	dist/build/LambdaHack/LambdaHack --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log


build-binary-common:
	mkdir -p LambdaHackTheGame/GameDefinition/fonts
	cabal v1-install --force-reinstalls --disable-library-profiling --disable-profiling --disable-documentation --enable-optimization --only-dependencies
	cabal v1-configure --disable-library-profiling --disable-profiling --enable-optimization --prefix=/ --datadir=. --datasubdir=.
	cabal v1-build exe:LambdaHack
	cabal v1-copy --destdir=LambdaHackTheGameInstall
	([ -f "LambdaHackTheGameInstall/bin/LambdaHack" ] && mv LambdaHackTheGameInstall/bin/LambdaHack LambdaHackTheGame) || exit 0
	([ -f "LambdaHackTheGameInstall/msys64/bin/LambdaHack.exe" ] && mv LambdaHackTheGameInstall/msys64/bin/LambdaHack.exe LambdaHackTheGame) || exit 0
	([ -f "LambdaHackTheGameInstall/msys32/bin/LambdaHack.exe" ] && mv LambdaHackTheGameInstall/msys32/bin/LambdaHack.exe LambdaHackTheGame) || exit 0
#	cabal new-build --disable-library-profiling --disable-profiling --disable-documentation --only-dependencies .
#	cabal new-install --disable-library-profiling --disable-profiling --disable-documentation --datadir=. --datasubdir=. --install-method=copy --installdir=LambdaHackTheGame --enable-executable-stripping exe:LambdaHack
	cp GameDefinition/config.ui.default LambdaHackTheGame/GameDefinition
	cp GameDefinition/fonts/16x16xw.woff LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/16x16xw.bdf LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/16x16x.fnt LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/8x8xb.fnt LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/8x8x.fnt LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/LICENSE.16x16x LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/source-code-pro-v9-latin_latin-ext-700.woff LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/source-sans-pro-v12-latin_latin-ext-700.woff LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/fonts/OFL.txt LambdaHackTheGame/GameDefinition/fonts
	cp GameDefinition/InGameHelp.txt LambdaHackTheGame/GameDefinition
	cp GameDefinition/PLAYING.md LambdaHackTheGame/GameDefinition
	cp README.md LambdaHackTheGame
	cp CHANGELOG.md LambdaHackTheGame
	cp LICENSE LambdaHackTheGame
	cp COPYLEFT LambdaHackTheGame
	cp CREDITS LambdaHackTheGame

build-binary-ubuntu: build-binary-common
	LambdaHackTheGame/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	tar -czf LambdaHack_$${LH_VERSION}_ubuntu-16.04-amd64.tar.gz LambdaHackTheGame

build-binary-macosx: build-binary-common
	LambdaHackTheGame/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	OS_VERSION=$$(sw_vers -productVersion); \
	tar -czf LambdaHack_$${LH_VERSION}_macosx-$${OS_VERSION}-amd64.tar.gz LambdaHackTheGame

new-build-dev:
	cabal new-build --datadir=. --disable-optimization -j1
