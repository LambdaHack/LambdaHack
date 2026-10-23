play:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs

shot:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix play --dumpInitRngs --printEachScreen

expose-lore:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode crawl --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

dig-lore:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode dig --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

see-caves:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode see --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

short-caves:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix know --newGame 5 --dumpInitRngs --gameMode short --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples --benchmark --noAnim --maxFps 1000

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions -fwith_expensive_assertions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions --enable-optimization

chrome-log:
	google-chrome --enable-logging --v=1 file://$(CURDIR)/../lambdahack.github.io/index.html &

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" ../lambdahack.github.io/index.html

# Low delay to display animations swiftly and not bore the public too much.
# Delay can't be lower than 2, because browsers sometimes treat delay 1
# specially and add their extra delay.
create-gif :
	find ~/.LambdaHack/screenshots/ -name 'prtscn*.bmp' -print0 | xargs -0 -r mogrify -format gif
	../gifsicle/src/gifsicle -O3 --careful -d2 --colors 255 --no-extensions --no-conserve-memory -l ~/.LambdaHack/screenshots/prtscn*.gif -o ~/.LambdaHack/screenshots/screenshot.gif

frontendRaid:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --benchMessages --gameMode raid --exposeActors

frontendBrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode brawl --benchMessages

frontendShootout:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode shootout

frontendHunt:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode hunt

frontendFlight:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 3 --dumpInitRngs --automateAll --gameMode flight

frontendZoo:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode zoo --exposeActors

frontendAmbush:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode ambush

frontendCrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl --exposeItems --exposeActors

frontendCrawlEmpty:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawlEmpty --maxFps 100000 --benchmark

frontendCrawlSurvival:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode crawlSurvival --maxFps 100000 --benchmark

frontendSafari:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode safari --exposeActors

frontendSafariSurvival:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode safariSurvival --exposeActors --maxFps 100000 --benchmark

frontendBattle:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 4 --dumpInitRngs --automateAll --gameMode battle --exposeActors

frontendBattleDefense:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 6 --dumpInitRngs --automateAll --gameMode battleDefense --exposeActors --maxFps 100000 --benchmark

frontendBattleSurvival:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 6 --dumpInitRngs --automateAll --gameMode battleSurvival --exposeActors --maxFps 100000 --benchmark

frontendDefense:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defense --exposeItems --exposeActors

frontendDefenseEmpty:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defenseEmpty --maxFps 100000 --benchmark

fastCrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl --exposeItems --exposeActors --showItemSamples --noAnim --maxFps 100000 --benchmark

slowCrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode crawl --exposeItems --exposeActors --showItemSamples

# different benchmarks use different arguments
RNGOPTS=--setDungeonRng "SMGen 123 123" --setMainRng "SMGen 123 125"
RNGOPTS1=--setDungeonRng "SMGen 127 123" --setMainRng "SMGen 127 125"
RNGOPTS2=--setDungeonRng "SMGen 129 123" --setMainRng "SMGen 129 125"

benchMemoryAnim:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --maxFps 100000 --benchmark --benchMessages --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS2) --frontendLazy +RTS -s -A1M -RTS

benchBattle:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchAnimBattle:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 3 --maxFps 100000 --frontendLazy --benchmark --benchMessages --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchFrontendBattle:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --benchmark --benchMessages --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)

benchCrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

benchFrontendCrawl:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --benchmark --benchMessages --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

benchDig:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 1 --automateAll --keepAutomated --gameMode dig $(RNGOPTS)

benchNull: benchBattle benchAnimBattle benchCrawl

bench: benchBattle benchAnimBattle benchFrontendBattle benchCrawl benchFrontendCrawl

nativeBenchCrawl:
	$$(cabal list-bin exe:LambdaHack)		   --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

nativeBenchBattle:
	$$(cabal list-bin exe:LambdaHack)		   --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)

nativeBench: nativeBenchBattle nativeBenchCrawl

nodeBenchCrawl:
	node $$(cabal list-bin exe:LambdaHack).jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)

nodeBenchBattle:
	node $$(cabal list-bin exe:LambdaHack).jsexe/all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)

nodeBench: nodeBenchBattle nodeBenchCrawl

nodeMinifiedBench:
	node ../lambdahack.github.io/lambdahack.all.js --dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle $(RNGOPTS)
	node ../lambdahack.github.io/lambdahack.all.js --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode crawl $(RNGOPTS)


test: test-short test-medium benchNull

test-gha: test testCrawl-medium testCrawl-stopAfterGameOver testDefense-medium test-sniff

test-short: test-short-new test-short-load

test-medium: testRaid-medium testBrawl-medium testShootout-medium testHunt-medium testFlight-medium testZoo-medium testAmbush-medium testCrawlEmpty-medium testCrawl-medium-know testSafari-medium testSafariSurvival-medium testBattle-medium testBattleDefense-medium testBattleSurvival-medium testDig-medium testDefenseEmpty-medium testMany-teletype

test-sniff:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterFrames 1  --dumpInitRngs --automateAll --keepAutomated --gameMode raid --sniff > /tmp/teletypetest.log 2>&1

testMany-teletype:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 50 --dumpInitRngs --automateAll --keepAutomated 2> /tmp/teletypetest.log

testMany-sdlInit:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 0 --boostRandomItem --newGame 9 --maxFps 100000 --benchmark --benchMessages --stopAfterSeconds 50 --dumpInitRngs --automateAll --keepAutomated 2> /tmp/teletypetest.log

testRaid-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/teletypetest.log

testBrawl-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/teletypetest.log

testShootout-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode shootout 2> /tmp/teletypetest.log

testHunt-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode hunt 2> /tmp/teletypetest.log

testFlight-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 3 --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode flight 2> /tmp/teletypetest.log

testZoo-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 2 --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode zoo 2> /tmp/teletypetest.log

testAmbush-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/teletypetest.log

testCrawl-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 600 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --assertExplored 5 2> /tmp/teletypetest.log


testCrawl-stopAfterGameOver:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --stopAfterGameOver 2> /tmp/teletypetest.log

testCrawlEmpty-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode crawlEmpty 2> /tmp/teletypetest.log

testCrawl-medium-know:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix know --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --knowItems --exposePlaces --exposeItems --exposeActors --showItemSamples 2> /tmp/teletypetest.log

testSafari-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/teletypetest.log

testSafariSurvival-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode safariSurvival 2> /tmp/teletypetest.log

testBattle-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/teletypetest.log

testBattleDefense-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode battleDefense 2> /tmp/teletypetest.log

testBattleSurvival-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode battleSurvival 2> /tmp/teletypetest.log

testDefense-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 9 --noAnim --maxFps 100000 --frontendLazy --benchmark --stopAfterSeconds 600 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/teletypetest.log

testDefenseEmpty-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --benchMessages --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode defenseEmpty 2> /tmp/teletypetest.log

testDig-medium:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterFrames 100 --dumpInitRngs --automateAll --keepAutomated --gameMode dig 2> /tmp/teletypetest.log

testCrawl-appveyor:
	./LambdaHack --dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterGameOver --stopAfterSeconds 300 --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --assertExplored 5

testDefense-appveyor:
	./LambdaHack --dbgMsgSer --logPriority 4 --newGame 9 --noAnim --maxFps 100000 --frontendNull --benchmark --benchMessages --stopAfterSeconds 600 --dumpInitRngs --automateAll --keepAutomated --gameMode defense

test-short-new:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix hunt --dumpInitRngs --automateAll --keepAutomated --gameMode hunt --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix flight --dumpInitRngs --automateAll --keepAutomated --gameMode flight --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode safariSurvival --showItemSamples --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battle --showItemSamples --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battleDefense --dumpInitRngs --automateAll --keepAutomated --gameMode battleDefense --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode battleSurvival --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log

# $(RNGOPTS) is needed for determinism relative to seed
# generated before game save
test-short-load:
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix hunt --dumpInitRngs --automateAll --keepAutomated --gameMode hunt --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix flight --dumpInitRngs --automateAll --keepAutomated --gameMode flight --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix crawl --dumpInitRngs --automateAll --keepAutomated --gameMode crawl --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode safariSurvival --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battleDefense --dumpInitRngs --automateAll --keepAutomated --gameMode battleDefense --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log
	$$(cabal list-bin exe:LambdaHack) --dbgMsgSer --logPriority 4 --boostRandomItem --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode battleSurvival --frontendTeletype --stopAfterSeconds 2 $(RNGOPTS) 2> /tmp/teletypetest.log


build-binary-v1:
	cabal v1-install --force-reinstalls --disable-library-profiling --disable-profiling --disable-documentation --enable-optimization --only-dependencies
	cabal v1-configure --disable-library-profiling --disable-profiling --enable-optimization
	cabal v1-build exe:LambdaHack

copy-binary-v1:
	cabal v1-copy --destdir=LambdaHackTheGameInstall

copy-binary:
	cp $$(cabal list-bin exe:LambdaHack) LambdaHackTheGame

configure-binary-v2:
	cabal configure --disable-tests --disable-library-profiling --disable-profiling --enable-optimization

build-binary-v2:
	cabal build --only-dependencies . --enable-optimization
	cabal build exe:LambdaHack --enable-optimization

copy-directory:
	mkdir -p LambdaHackTheGame/GameDefinition
	cp GameDefinition/InGameHelp.txt LambdaHackTheGame/GameDefinition
	cp GameDefinition/PLAYING.md LambdaHackTheGame/GameDefinition
	cp README.md LambdaHackTheGame
	cp CHANGELOG.md LambdaHackTheGame
	cp LICENSE LambdaHackTheGame
	cp COPYLEFT LambdaHackTheGame
	cp CREDITS LambdaHackTheGame

build-binary-common: build-binary-v1 copy-directory copy-binary-v1

build-binary-windows: configure-binary-v2 build-binary-v2 copy-directory

build-directory: configure-binary-v2 build-binary-v2 copy-directory copy-binary

build-binary-ubuntu: build-directory
	LambdaHackTheGame/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	tar -czf LambdaHack_$${LH_VERSION}_ubuntu-16.04-amd64.tar.gz LambdaHackTheGame

build-binary-macosx: build-directory
	LambdaHackTheGame/LambdaHack --version > /dev/null; \
	LH_VERSION=$$(cat ~/.LambdaHack/stdout.txt); \
	OS_VERSION=$$(sw_vers -productVersion); \
	tar -czf LambdaHack_$${LH_VERSION}_macosx-$${OS_VERSION}-amd64.tar.gz LambdaHackTheGame

build-wasm:
	. ~/.ghc-wasm/env; \
	wasm32-wasi-cabal build exe:LambdaHack --enable-optimization

# Chained with && so the log ends at the command that failed; otherwise
# a build failure runs on into post-link and the test driver, and their
# complaints about the missing binary bury the real error. The trap both
# removes the temporary directory and lets the last command's status stand.
# The npm ci installs lz-string, which the driver's LZString stub loads.
# Each wasm list-bin repeats its build's --enable-optimization: the path
# it names follows that setting, and under the development config's
# `optimization: False` a bare list-bin names a noopt/ build instead.
test-wasm:
	. ~/.ghc-wasm/env; \
	T=$$(mktemp -d); \
	trap 'rm -rf "$$T"' EXIT; \
	wasm32-wasi-cabal build test --enable-optimization && \
	W=$$(wasm32-wasi-cabal list-bin test --enable-optimization) && \
	~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$$W" --output "$$T/ghc_wasm_jsffi.mjs" && \
	(cd ts-src && npm ci) && \
	node ts-src/run-wasm-test.mjs "$$W" "$$T/ghc_wasm_jsffi.mjs"

build-ts:
	. ~/.ghc-wasm/env && \
	cp GameDefinition/index.html GameDefinition/fonts/*.ttf.woff ../lambdahack.github.io && \
	cd ts-src && \
	npm ci && \
	BUILD_OUTDIR=../../lambdahack.github.io npm run build && \
	W=$$(cd $(CURDIR) && wasm32-wasi-cabal list-bin exe:LambdaHack --enable-optimization) && \
	~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$$W" --output ../../lambdahack.github.io/ghc_wasm_jsffi.mjs && \
	cp "$$W" ../../lambdahack.github.io/LambdaHack.wasm

test-ts:
	cd ts-src && \
	npm run typecheck && npm run format:check && npx vitest run

# The deployed-page gate: the page as build-ts would deploy it,
# in a scratch directory, loaded in headless Firefox and headless
# Chromium and required to boot, paint and repaint on a key in each
# (ts-src/page-gate.mjs says how). Local only, by ruling; run before
# every browser landing.
page-gate: build-wasm
	. ~/.ghc-wasm/env && \
	T=$$(mktemp -d) && \
	trap 'rm -rf "$$T"' EXIT && \
	cp GameDefinition/index.html GameDefinition/fonts/16x16xw.woff GameDefinition/fonts/*.ttf.woff "$$T" && \
	(cd ts-src && npm ci && BUILD_OUTDIR="$$T" npm run build) && \
	cp ts-src/node_modules/lz-string/libs/lz-string.min.js "$$T" && \
	W=$$(wasm32-wasi-cabal list-bin exe:LambdaHack --enable-optimization) && \
	~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$$W" --output "$$T/ghc_wasm_jsffi.mjs" && \
	cp "$$W" "$$T/LambdaHack.wasm" && \
	node ts-src/page-gate.mjs "$$T"

serve-wasm:
	cd ../lambdahack.github.io && \
	node serve.mjs . 8080

BROWSER ?= firefox

run-wasm:
	$(BROWSER) http://localhost:8080/

# The frontend smokes: a few frames of benchFrontendBattle through the
# event loop of a shipped frontend other than teletype, whose loop the
# playtests enter. Exit status alone cannot tell a game that ran from one
# that quit at frame zero, so each smoke also requires the frame count
# of the benchmark report to reach --stopAfterFrames, read where stdout
# went: GameDefinition/Main.hs redirects it to ~/.LambdaHack/stdout.txt
# unless it is a terminal.
SMOKEFRAMES=60
SMOKEOPTS=--dbgMsgSer --logPriority 4 --newGame 3 --noAnim --maxFps 100000 --benchmark --benchMessages --stopAfterFrames $(SMOKEFRAMES) --automateAll --keepAutomated --gameMode battle $(RNGOPTS1)
smokeFrames=N=$$(sed -n 's/.*Session time: .*; frames: \([0-9]*\)\..*/\1/p' $(1)) && echo "frames: $$N" && test "$$N" -ge $(SMOKEFRAMES)

# Under script the pty is a terminal, so the report is in its capture.
smokeANSI:
	script -e -q -c '$$(cabal list-bin exe:LambdaHack) $(SMOKEOPTS) --frontendANSI' /tmp/smokeANSI.log < /dev/null > /dev/null
	$(call smokeFrames,/tmp/smokeANSI.log)

# Under xvfb-run stdout is not a terminal, and > /dev/null keeps it so
# when make runs in one, so the report is in ~/.LambdaHack/stdout.txt,
# deleted first lest an earlier game's report pass for this one's.
# Xvfb needs /tmp/.X11-unix, which the dev machine's /tmp lacks, and
# segfaults there in GLX init through NVIDIA's EGL unless EGL is pinned
# to Mesa.
MESAEGL=/usr/share/glvnd/egl_vendor.d/50_mesa.json
smokeSdl:
	mkdir -p /tmp/.X11-unix
	rm -f ~/.LambdaHack/stdout.txt
	$(if $(wildcard $(MESAEGL)),__EGL_VENDOR_LIBRARY_FILENAMES=$(MESAEGL)) LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a $$(cabal list-bin exe:LambdaHack) $(SMOKEOPTS) > /dev/null
	$(call smokeFrames,~/.LambdaHack/stdout.txt)
