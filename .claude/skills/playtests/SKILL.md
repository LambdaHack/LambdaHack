---
name: playtests
description: LambdaHack's automated AI-vs-AI playtest and benchmark Makefile targets (test-short, test-medium, test-gha, frontend*, bench*) and the headless test-frontend flags. Use before running, interpreting or adding a playtest or benchmark target.
---

# Playtests and benchmarks (Makefile)

The Makefile has a large battery of automated AI-vs-AI playtest and benchmark
targets: `make test-short`, `make test-medium`, `make test` (those two plus
`benchNull`), `make test-gha` (a larger aggregate plus `test-sniff`; run
by CI on each push), `make frontendCrawl`/`make frontendBattle` etc.
(interactive AI-vs-AI games in the SDL2 frontend, useful to visually
confirm a change), and `make bench*` targets for performance.
`test-*-medium` targets run one game mode each
(raid, brawl, shootout, hunt, flight, zoo, ambush, crawl, safari, battle,
defense, dig...) through the teletype frontend with `--automateAll`. Grep the
Makefile for a mode name to find its exact invocation before adding a new
one.

The headless targets select test frontends by flag: `--frontendNull`
(frames forced but not displayed), `--frontendLazy` (frames not even
computed), `--frontendTeletype` (line-printer output). The `nodeBench*`
and `nodeMinifiedBench` targets are dead GHCJS remnants — they invoke a
`.jsexe` that nothing builds anymore; repurposing them for WASM is the
plan's Phase 3.

Such runs look completely silent, because the game redirects its own
stdout and stderr to files — see "Playtests and headless runs" in the
repo-root `CLAUDE.md` for how to harvest the output.
