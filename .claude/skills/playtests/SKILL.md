---
name: playtests
description: LambdaHack's automated AI-vs-AI playtest and benchmark Makefile targets (test-short, test-medium, test-gha, frontend*, bench*) and the headless test-frontend flags. Use before running, interpreting or adding a playtest or benchmark target.
---

# Playtests and benchmarks (Makefile)

The Makefile has a large battery of automated AI-vs-AI playtest and benchmark
targets: `make test-short`, `make test-medium`, `make test` (those two plus
`benchNull`), `make test-gha` (a larger aggregate plus `test-sniff`; run by CI
on each push), `make frontendCrawl`/`make frontendBattle` etc. (interactive
AI-vs-AI games in the SDL2 frontend, useful to visually confirm a change),
and `make bench*` targets for performance. `test<Mode>-medium` targets run one
game mode each -- `testRaid-medium`, `testBrawl-medium`, `testShootout-medium`,
`testHunt-medium`, `testFlight-medium`, `testZoo-medium`, `testAmbush-medium`,
`testCrawl-medium`, `testSafari-medium`, `testBattle-medium`,
`testDefense-medium`, `testDig-medium` and a few variants -- through
the teletype frontend with `--automateAll`, bar `testDefense-medium`, which runs
`--frontendLazy`. Note the capitalised mode and the single hyphen: there
is no `test-raid-medium`. Grep the Makefile for a mode name to find its exact
invocation before adding a new one.

The headless targets select test frontends by flag: `--frontendNull` (frames
forced but not displayed), `--frontendLazy` (frames not even computed),
`--frontendTeletype` (line-printer output).

Such runs look completely silent, because the main game executable redirects
its own stdout and stderr to files --- see "Playtests and headless runs"
in the repo-root `CLAUDE.md` for how to harvest the output. The wasm reactor's
runs are not silent: `make nodeBench` plays `make nativeBench`'s two games, flag
for flag, on the reactor under `ts-src/run-wasm-game.mjs`,
and `make nodeDeployedBench` on the reactor `make build-ts` deployed, each
printing the benchmark report to the terminal.
