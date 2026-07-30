---
name: playtests
description: LambdaHack's automated AI-vs-AI playtest and benchmark Makefile targets (test-short, test-medium, test-gha, frontend*, bench*) and the headless test-frontend flags. Use before running, interpreting or adding a playtest or benchmark target.
---

# Playtests and benchmarks (Makefile)

The names cited here — Makefile targets, frontend flags, document paths — were verified against the tree at commit `ef4d44732` (2026-07-30); the reference pass proves a cited name exists, this stamp that it still does what the claim around it needs. The wording differs from the stamp in `CLAUDE.md` and `test/CLAUDE.md` because this file cites no line numbers.

The Makefile has a large battery of automated AI-vs-AI playtest and benchmark
targets: `make test-short`, `make test-medium`, `make test` (those two plus
`benchNull`), `make test-gha` (a larger aggregate plus `test-sniff`; run
by CI on each push), `make frontendCrawl`/`make frontendBattle` etc.
(interactive AI-vs-AI games in the SDL2 frontend, useful to visually
confirm a change), and `make bench*` targets for performance.
`test<Mode>-medium` targets run one game mode each -- `testRaid-medium`,
`testBrawl-medium`, `testShootout-medium`, `testHunt-medium`,
`testFlight-medium`, `testZoo-medium`, `testAmbush-medium`,
`testCrawl-medium`, `testSafari-medium`, `testBattle-medium`,
`testDefense-medium`, `testDig-medium` and a few variants -- through the
teletype frontend with `--automateAll`. Note the capitalised mode and the
single hyphen: there is no `test-raid-medium`. Grep the Makefile for a mode
name to find its exact invocation before adding a new one.

The headless targets select test frontends by flag: `--frontendNull`
(frames forced but not displayed), `--frontendLazy` (frames not even
computed), `--frontendTeletype` (line-printer output). The `nodeBench*`
and `nodeMinifiedBench` targets are dead GHCJS remnants — they invoke a
`.jsexe` that nothing builds anymore; repurposing them for WASM is
Phase 3 of `docs/wasm-frontend-unified-plan.md`.

Such runs look completely silent, because the main game executable
redirects its own stdout and stderr to files — see "Playtests and
headless runs" in the repo-root `CLAUDE.md` for how to harvest the
output.
