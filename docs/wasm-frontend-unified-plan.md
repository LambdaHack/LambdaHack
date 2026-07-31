# WASM Frontend Plan: SDL2 Parity on a Shared-Haskell Architecture

## Goals and approach

This plan supersedes `wasm-frontend-haskell-alignment-design.md` and
`sdl2-to-wasm-parity-plan.md`. It pursues two goals at once:

- **G1 — one canonical definition.** Knowledge that has a canonical
  Haskell definition must not be hand-ported into TypeScript or duplicated
  between frontends. It is shared via common pure Haskell modules,
  generated tables, and generated behavioral fixtures.
- **G2 — SDL2 parity.** Close every real gap between the SDL2 frontend and
  the WASM/browser build: pointer cursor, screenshots, fullscreen
  (including scaling), display scale, and multi-font (proportional + mono)
  rendering — plus the input/rendering fidelity fixes in 0.0.

The ordering follows from G1: multi-font done naively would re-implement in
TypeScript the layout logic that lives in `Sdl.hs`'s
`drawPropOverlay`/`drawPropLine`/`drawPropChunk` (`Sdl.hs:670-713`) —
a much larger instance of the hand-porting that already produced real bugs
(a DeadKey set ported with 11 of 13 values missing; a palette color slip).
So: **shared-Haskell foundations first (Phase 0), small parity wins on top
(Phase 1), multi-font as a shared-module extraction (Phase 2), and the
Node benchmark tooling port (Phase 3).** Related goals (R1–R6) and adopted
multi-frontend practices follow the phases.

File:line references were verified against the tree at commit
`4d762337b` (2026-07-30), then machine-checked — re-run
`python3 tools/check-plan-citations.py docs/wasm-frontend-unified-plan.md`
after landing work that touches cited files, and re-verify
universally-quantified claims ("only X does
Y", "exactly two") by repo-wide grep, never by re-reading one file.
Thirteen file basenames are duplicated among this repo's tracked `.hs`
files; the four this plan cites are therefore qualified wherever they
appear: `Server/LoopM.hs` vs `Client/LoopM.hs`, the engine's vs the
game's `Content/Input.hs`, the engine's vs the game's
`Client/UI/Content/Screen.hs`, and `GameDefinition`'s vs
`definition-src`'s `Content/RuleKind.hs`. Decisions *against* work,
deferrals and their rationale are collected in Appendix B; non-gaps from
the SDL2-vs-wasm audit in Appendix C; the GHCJS→JS-backend port
investigation in Appendix A. Which half of this document a passage is in
is answered by the ledger below, and by whether its item carries an
outcome line.

> **What this record is for, and what is frozen in it.** It is kept
> indefinitely, for two readers: the one who wonders why the web stack reads
> as it does, and the one who proposes a browser-side change and needs to
> know whether it was already ruled out. It is never deleted, and it is not
> yet frozen: it matures, and the ledger below says how far. The pointman
> records are the genre it converges on, each already mostly frozen around a
> live carve-out; this one is still live throughout.
>
> **The state vocabulary is `docs/leader-desync-bug.md`'s, borrowed whole.**
> Where the two ever disagree, that record is right and this one is the
> error: it is the older discipline, it is the one `CLAUDE.md` describes,
> and a vocabulary decision taken there needs no glance at this file. Three
> states, and a fourth this campaign needs and a post-mortem does not:
>
> - **landed**, with the commit that did it — the item describes the code as
>   of that commit and is not maintained against later trees, bar one
>   further outcome line per claim that *resolves*. Ordinary drift in a
>   landed item is left alone; rewriting one to match a later tree is the
>   mistake this rule exists to prevent.
> - **open** — the code is wrong *now*, and the item says how. A drifted
>   claim here is an error to fix, and "fixing" it by writing the defect up
>   as solved is the one thing that must not happen.
> - **not applied** — the design is specified and the artifact does not
>   exist. Its claims about *today's* tree are held to a plan's standard.
> - **standing** — the item never lands, because it recurs (1.4, R2) or
>   because it is a review rule with nothing to execute (explicit widths,
>   functional core). It retires when the campaign does. The asymmetry is
>   deliberate: the pointman records have no such case and need no such word.
>
> The distinction a reader will trip on is **open** against **not applied**,
> because the ledger sorts by item number and not by it: 0.0 and 2.1 sit
> four rows apart and mean different things — AltGraph chords are lost in
> the shipped browser build today, while `OverlayLayout` is a module nobody
> has written. The first is a bug report, the second a specification, and
> only the second may be edited to match a tree that moved.
>
> **Landing an item** does two things and nothing else: it appends an
> outcome line, and it flips that item's ledger row from `open` or `not
> applied` to `landed` in that hash. The
> outcome line goes **immediately after the item's opening paragraph**,
> before any subsection, so that opening the item at its head answers the
> question; its shape is the doctest bullet's under Ground rules. A claim
> the landing falsified is corrected in a *separate* commit, so review can
> tell a relocation from a correction — which commit `a7e825c2d` could not,
> having rewritten R2 from "Add a job to…" into "Partly landed, in…" and
> bent a factual claim in the same diff.
>
> **Landing part of an item takes no outcome line**, and the row stays
> live: a partial landing is recorded in the item's body as specification
> for what remains, which is what R2 does today. **A standing item never
> lands** — 0.3, 1.4, R2, explicit widths and functional core are ongoing
> or recurring by their own size cells — so it retires when the campaign
> does, rather than acquiring a hash.
>
> An item that proposes an artifact may take its outcome line **only once
> its entries in `tools/doc-refs-allow.txt` can be deleted**; that file's
> comment already asks for exactly that deletion, and gating on it makes
> "landed" a checked claim rather than a declared one. That gate reaches
> five items — 0.1, 0.2, 2.1, 2.2 and 3.2, whose twelve entries are listed
> there. Every other item is ungated, its outcome line resting on the same
> reading as any other claim here; and an item whose artifact turns out
> unneeded clears the gate by deleting the entry together with the sentence
> that named it.
>
> When every item has landed or retired, the four campaign-live sections
> freeze with them — Goals and approach, Repo facts, Build & verification
> loop, Sequencing — the ledger's own rows stop moving, and this callout
> becomes one line saying the specification landed in full. Restamping does
> not stop: the standing checks quantify over every document in the repo,
> and a frozen record still restamps when the files it cites move, as the
> pointman records do.

## How this lands

The ledger says what the items are and the blocks say how to execute one;
this says how they reach master, because the review is the scarce input and
the fleet cannot decide it.

**One review unit per ledger item, not per agent and not one for the
campaign.** A single pull request carrying Phase 0 through 2 would be
unreadable, and one per agent would make the bottleneck worse rather than
better. `gh` is unauthenticated here, so the unit is a branch off local
master read with `git log -p master..<branch>`; a GitHub PR exists only
after an authorized push.

**3.1 goes first, as a shakedown.** It is the smallest item whose
correctness a machine can settle end to end, so it proves the whole ritual
— the wasm loop, the gates, the outcome line, the ledger flip, the
allowlist deletion, the restamp — before any item whose review needs
judgment. Phase 3 then continues in parallel with Phases 0-2, sharing no
file with them.

**One item is reviewed entirely alone: the `Sdl.hs` drawing loop** (2.1's
refactor together with 0.2's `CellStyle` adoption, which rewrite abutting
regions of one hot loop). It rewrites shipped native rendering; SDL2 is
exercised in CI nowhere beyond the init-and-quit backdoor; expensive
assertions are off in every workflow; and its real gate is display-bound.
Its branch carries `Sdl.hs` and nothing else, and its citation repair is a
separate commit — 83 of this document's 108 `Sdl.hs` citations sit in the
region it rewrites, and they will all still resolve.

**2.4 splits**, so that each half gets the review it needs: the capability
mechanism lands early, provably behaviour-preserving with
`Wasm.supportsMultiFont = False`, and the one-boolean flip is reviewed
alone as a product decision. Unsplit it would arrive as mechanism plus
risk in one diff.

**Never autonomously:** push, open or force-update a pull request, or take
an outcome line on a partial landing.

## Ledger — every item's state

[Repo facts](#repo-facts-the-plan-builds-on) ·
[Ground rules](#ground-rules) ·
[Build & verification loop](#build--verification-loop) ·
[Phase 0 — shared foundations](#phase-0--shared-foundations) ·
[Phase 1 — small parity wins](#phase-1--small-parity-wins) ·
[Phase 2 — multi-font](#phase-2--multi-font-as-extraction-rather-than-re-implementation) ·
[Phase 3 — Node benchmarks](#phase-3--port-the-node-benchmark-targets-from-ghcjs-to-wasm) ·
[Related goals](#related-goals) ·
[Multi-frontend practices](#multi-frontend-practices-adopted) ·
[Out of scope](#out-of-scope) ·
[Sequencing](#sequencing) ·
Appendices
[A](#appendix-a--investigation-porting-the-ghcjs-target-to-ghcs-in-tree-javascript-backend)
/ [B](#appendix-b--decisions-against-and-deferrals)
/ [C](#appendix-c--verified-non-gaps-sdl2-vs-wasm-audit-record)

Sections holding no items are stated once, here, and the two lists are
exhaustive over the sections the per-item rule does not cover. **Frozen
from the start:** Ground rules, Out of scope, and Appendices A, B and C —
frozen does not mean silent, and a claim in one that later resolves takes
an outcome line, as the doctest bullet under Ground rules has. **Live
until every item has landed or retired**, because the work falsifies them:
Goals and approach, Repo facts, Build & verification loop, Sequencing, and
this ledger. Everything else is per item, and the row is the unit of rollback:
each
item is one commit, or the **Split** run of commits its block names, every
one of them leaving the suite green — which is what makes a row something
that can be reverted rather than unpicked.

| § | delivers | size | depends on | state |
|---|---|---|---|---|
| 0.0 | AltGraph fixes (keys, mouse/wheel); highlight-outline rule | tiny | — | open |
| 0.1 | `InputDecision` shared module; sync `lhKey` | small | 0.3 baseline | not applied |
| 0.2 | `CellStyle` + TS-table/fixture generator | medium | — | not applied |
| 0.3 | FFI-coverage battery (baseline before 0.1, then per-commit) | ongoing | — | not applied · standing |
| 1.1 | crosshair cursor (CSS keyword; then generated SVG cursor) | trivial | 0.2 for the final form | not applied |
| 1.2 | working `Ctrl+P` screenshots | small–medium | 0.2 helps | open |
| 1.3 | fullscreen toggle with scaling | small | — | not applied |
| 1.4 | banner/title truthfulness | trivial, recurring | feature landings | open · standing |
| 1.5 | `allFontsScale` honored in browser | small | 2.2's startup call (or a precursor); R4 for player control | open |
| 2.1 | `OverlayLayout` extraction + `Sdl.hs` on it | medium–large | 0.2 for `CellStyle`; determinism goldens (native harness) | not applied |
| 2.2 | browser canvas overlay renderer + font wiring | medium | 2.1, 0.2 | not applied |
| 2.3 | overlay transport over JSFFI | medium | 2.1, 2.2; 0.3 for the end-to-end battery | not applied |
| 2.4 | multi-font capability flip | tiny diff, big review | 2.1–2.3; determinism goldens | not applied |
| 2.5 | post-flip QA | small | 2.4 | not applied |
| 3.1 | `lhStart` reads WASI argv | small (+spike) | — | not applied |
| 3.2 | Node driver for the game reactor | small | 3.1 | not applied |
| 3.3 | `nodeBench*`/`nodeDeployedBench` targets | small | 3.2 | not applied |
| R1 | save robustness: staging keys plus a generation pointer | medium | 3.2, for the lag re-measurement | open |
| R2 | browser-and-frontend CI | ongoing | grows per phase | standing |
| R3 | retire GHCJS support | one commit | 2.5 (parity) | not applied |
| R4 | URL-parameter options | small | 3.1 | not applied |
| R5 | performance pass | exploratory | 2.5, 3.3; R4 + 3.1 for the browser-side instrument | not applied |
| R6 | screenshot/overlay coherence | small | 1.2, 2.2 | not applied |
| capability constants | named constants in place of behavioral CPP | — | — | not applied |
| sum-typed selection | one frontend field replacing four `Bool`s | mechanical, moderate churn | before R4 | not applied |
| RawFrontend contract | haddock contract, tasty harness, add-a-frontend checklist | medium | input side with 0.1; rest with 0.3 | not applied |
| determinism goldens | fixed-seed final-state digests | medium | native harness before 2.1 | not applied |
| frontend CI smokes | xvfb SDL, pty ANSI, a short nodeBench run | small each | 3.3 for the wasm one | not applied |
| explicit widths | `punindex`, never the `Enum` instance, in frontend code | review rule; its one live violation is 0.2's | — | open · standing |
| functional core | the standing review bar for frontend modules | review rule; nothing to execute | — | standing |

## Log

One line per surprise or re-plan, newest last, so that resuming this
campaign needs this section rather than a re-read of three and a half
thousand lines. Log-worthy: an item that turned out larger or smaller than
its row says, a design question reopened, a count or classification here
found wrong, an ordering constraint discovered, a **Decide first** ruled
on. Not log-worthy: doing an item as written, or editing this file before
the work starts — an entry recording only that the plan was written is one
the next reader has to skip.

- 2026-07-30 · plan restructured to mature rather than be split or
  deleted; execution blocks added to all thirty-one items. Twenty-five of
  them carry an unanswered **Decide first**, which is the campaign's real
  blocking front — not the code. Three are hard blockers rather than
  preferences: 0.3b's exports cannot be called after `wasi.start()`
  returns, the RTS having exited; 0.2c cannot reach `cursorXhair`, which
  `Sdl.hs` does not export; and 3.3 must choose between the node on PATH
  and the ghc-wasm-bundled one, which differ in `node:wasi`.
- 2026-07-30 · the RawFrontend contract item's gate was
  `-p "/contract/"`, which is the pointman campaign's tag over the same
  suite: vacuous as a gate here, and it would have moved a count that
  document requires never to move. Retagged `[fe-contract]`, and the
  namespace is now stated in the gates above.
- 2026-07-30 · that retag did not separate the namespaces. `-p` matches a
  substring of the test path — `-p "/ontract/"` selects the same tests as
  `-p "/contract/"`, and `/^contract/` selects none — so `[fe-contract]`
  sat inside the pointman campaign's filter, and the first frontend
  contract test to land would still have moved that campaign's count.
  Half the fix had been made and the whole one recorded. The series is
  `[fe-invariant]` now, and the namespace paragraph states containment
  rather than equality as the test.

## Repo facts the plan builds on

- **How the wasm frontend works today, in five lines.** The game compiles
  to a wasm *reactor* (`-no-hs-main`) exporting `lhStart`, `lhKey`,
  `lhWheel`, `lhMouseUp`; `ts-src/src/loader.ts` instantiates it with a
  WASI shim and calls `lhStart`. Each frame, `Wasm.hs`'s `display` passes
  the `Word32` cell buffer's address to `globalThis.lhSubmitFrame`;
  `terminal.ts` snapshots it and repaints a DOM grid of `<span>` cells
  (diffing against the previous frame) via the pure `styledCell` in
  `terminal-core.ts`. Input flows the other way: DOM events →
  `lhKey`/`lhWheel`/`lhMouseUp` exports → the engine's key channel.
  Saves bypass all of this — `WasmFile.hs` talks to `localStorage`
  directly over JSFFI.
- **`Dom.hs`/`JSFile.hs` (the GHCJS frontend) are dead code** — the repo
  requires GHC 9.10+ while standalone GHCJS ended at GHC 8.10 (Appendix
  A.1). They still appear throughout this plan as the *canonical origin*
  of web-frontend logic: `Wasm.hs` and `terminal.ts` were ported from
  them, so where the web stack's intent is in question, `Dom.hs` is the
  reference. Their fate is decided (R3): no revival — they stay as
  documented-dead example code, and every other trace of GHCJS is ripped
  out once WASM reaches parity.
- **Layout and deployment.** The TS harness lives in `ts-src/src/`
  (`terminal.ts`, `terminal-core.ts` + tests, `loader.ts`, `serve*.ts`).
  The game page is `GameDefinition/index.html`. `make build-ts` builds
  `ts-src/` and deploys bundle + wasm + JSFFI glue + `index.html` into the
  sibling checkout `../lambdahack.github.io` (Makefile:322-330), the
  GitHub Pages site players actually load.
- **Fonts.** `config.ui.default` sets `chosenFontset = "dejavuBold"`
  (line 67); its `[fonts]` section names the web fonts directly:
  `dejavuBold = FontProportional "DejaVuLGCSans-Bold.ttf.woff" 13
  HintingHeavy`, `dejavuMono = FontMonospace "Hack-Bold.ttf.woff" 13
  HintingHeavy` (lines 89-103). The `.woff` files live in
  `GameDefinition/fonts/`.
- **The browser build has the fontset config but not the font bytes.**
  `config.ui.default` is embedded at compile time via Template Haskell
  (`rcfgUIDefault`, `GameDefinition/Content/RuleKind.hs:34`; merged with
  the on-disk user config in `UIOptionsParse.hs:mkUIOptions`), so
  `schosenFontset`/`sfontsets`/`sfonts` are populated even with no argv —
  the multi-font gate's `not (T.null (fontPropRegular chosenFontset))`
  conjunct already passes for the default fontset; only the frontend check
  blocks it. However, `GameDefinition/game-src/Client/UI/Content/Screen.hs`
  sets `rFontFiles = []` under `USE_BROWSER` (natively `$(embedDir
  "GameDefinition/fonts")`), deliberately keeping font bytes out of the
  browser payload. In the browser, fonts are **static web assets by
  design**: Haskell knows their names and sizes (`sfonts :: [(Text,
  FontDefinition)]`, `Common/ClientOptions.hs:35`; `FontDefinition`
  itself, whose constructors carry filename and size, at
  `Common/Misc.hs:28`) but cannot supply bytes.
  Phase 2.2's font wiring is built around exactly that split.
- **Font deployment gap.** `../lambdahack.github.io` currently contains
  only `16x16xw.woff` — committed by hand in 2019, not produced by any
  build — plus `lz-string*.js`, used by `WasmFile.hs`'s save compression.
  `make build-ts` copies no fonts, so the other ten files in
  `GameDefinition/fonts/` never reach the pages repo, and the deployed
  `index.html` references none of them. Any step adding font usage must
  extend `build-ts` to copy the needed
  `GameDefinition/fonts/*.ttf.woff` files (prefer the Makefile over
  committing to the pages repo, so the repo of record stays this one).

## Ground rules

- Every step leaves `make build-wasm && make build-ts && make serve-wasm`
  producing a game that runs with no new console errors, and `make test-ts`
  / `make test-wasm` / `cabal test` green. Steps are independently
  shippable.
- Pure logic goes where it can be tested: shared decisions in pure Haskell
  modules with tasty tests; unavoidably-TS pure logic in `*-core.ts` with
  vitest tests; DOM/FFI wiring stays thin and is verified by running the
  game. One narrow exception to wiring-by-hand: input event *forwarding*
  in `terminal.ts` gets jsdom-driven vitest coverage (synthetic
  `KeyboardEvent`/`MouseEvent`s in, forwarded argument tuples out) — the
  AltGraph class of bug lives exactly there, and this is the test that
  would have caught it. Focus/bfcache behavior stays manual (jsdom can't
  emulate it).
- Prefer additive JSFFI exports/imports over changing existing signatures.
  One deliberate exception: Phase 0.1 changes `lhKey` (sync + an event
  parameter), because `preventDefault` from Haskell requires it. That
  signature change lands first, in isolation, with `loader.ts` updated in
  the same commit; everything after it is additive again.
- Every test suite, old or new, runs in CI — a test that only runs on a
  developer's machine is treated as not existing. Each new test surface
  this plan introduces lands in CI in the same commit that introduces it;
  the one pre-existing gap was doctests.
  *Landed in `a7e825c2d`: they run as their own job in the hand-written
  workflow, following CLAUDE.md's recipe.*
- Follow the module-as-interface convention: new shared modules go under
  `engine-src/Game/LambdaHack/Client/UI/Frontend/` and are reached only
  via the `Frontend` subtree's existing interfaces.

## Build & verification loop

`make build-wasm`, `make build-ts`, `make serve-wasm`, browser at
`localhost:8080` (`make run-wasm`). TS tests: `make test-ts` (vitest).
Native Haskell tests: `cabal test`. Wasm-compiled Haskell tests:
`make test-wasm` (drives the tasty binary through Node via
`ts-src/run-wasm-test.mjs`; the `common options` stanza applies
`USE_BROWSER`/`USE_WASM`/`USE_WASMFILE` to the test-suite under
`os(wasi)`, so the suite is *compiled and linked* in the real browser
configuration — `WasmFile.hs` as the file backend, `Wasm.hs` as the
chosen frontend — which catches wasm-only compile and link breakage.
Neither module's code is *executed*: the integration test passes
`--frontendNull` and every fixture sets `sfrontendNull = True`, so
`nullStartup` stands in for `Chosen.startup`. Running them is exactly
what 0.3's FFI battery adds).


## Handing an item to a session

Every item ends with the same execution block: four labels in a fixed
order — **Owns**, **Done**, **Hands back**, **Decide first** — each
written out even when it has nothing to say, as `nothing`, so that a
missing label is a defect rather than a shrug. A fifth, **Split**, comes
first where it applies and is omitted where it does not; that is the one
deliberate asymmetry, and its absence says the item is a single commit.

- **Split** — the landing order for an item worked as several commits,
  and which of them carries the outcome line and the
  `tools/doc-refs-allow.txt` deletions.
- **Owns** — the files the item writes, exhaustively, and a locking
  discipline over them: while the item is in flight nothing else writes
  those files, and the item writes nothing else. A file in two items'
  **Owns** is therefore a sequencing constraint, not a merge to attempt.
  Where two sub-items of one item cannot run concurrently — a shared new
  file, `package-lock.json`, a citation block that renumbers — **Owns**
  says so and says why.
- **Done** — the gates below that apply, named rather than spelled out,
  plus whatever is this item's own. It is the definition of finished for
  everything a session *can* verify.
- **Hands back** — the acceptance no session can perform, opening with
  the word `display`, `browser` or `judgement`, followed by the
  substitute gate that *is* in **Done**. An item whose **Done** covers
  its whole acceptance says "hands back nothing", and the label exists
  so that the rest cannot be mistaken for one of those.
- **Decide first** — the questions an executing session must not answer
  for itself, with their branches. `nothing` means the item is ready to
  hand out as written.

**The gates, once.** Every **Done** is built from these, run from the repo
root. They are spelled out here and nowhere else: thirty-one items
repeating one command chain is the second definition G1 forbids, and the
chain that drifts is the one nobody re-reads.

```
native   cabal build && cabal test && hlint .
         && stylish-haskell -i <the item's Haskell files>
         && git diff --exit-code <those paths>     # stylish left them alone
ts       make test-ts && (cd ts-src && npx tsc --noEmit)
wasm     make build-wasm && make test-wasm
deploy   make build-ts        # UNSANDBOXED: writes into the pages checkout
docs     python3 tools/check-plan-citations.py $D    # $D = this document
         && python3 tools/check-doc-refs.py $D
```

**Read the counts, not the exit status alone** — the rule the migration
plan states for its own battery, and it binds here too: a suite that
silently loses a test still passes. `cabal test --test-options='--list-tests'`
is what settles a count claim, and a `-p` pattern selects on the test
*name*, so a renamed test leaves its series without failing anything.
Neither may be piped into `head`/`tail`: a pipeline exits with its last
command's status, so a failed build reads as success, and a truncated
listing is how a wrong count gets quoted.

**Tasty tags are a repo-global namespace, shared with the pointman
campaign.** `-p` filters the whole suite by name, not by module or by
campaign, and it matches a *substring*, with no anchor available:
`-p "/ontract/"` selects the same 26 tests as `-p "/contract/"`, and
`-p "/^contract/"` selects none. A tag is therefore claimed by
containment, not by equality, and a marker that merely *contains* another
campaign's is not a fresh claim but a silent join. `[contract]` and
`[LR-flip]` are the pointman campaign's, and
`docs/leader-desync-migration.md` pins the size of each series step by
step, so a marker of ours falling inside either filter moves a count that
document treats as a finding. This campaign's frontend contract series is
`[fe-invariant]`. A new series here claims a marker containing no other
campaign's, and the item that mints it proves that by listing —
`cabal test --test-options='--list-tests -p "/<marker>/"'` must select
nothing before the series exists, which is the same command that reports
26 for an existing marker and so is not a vacuous check. `[frontend]` is
not available, incidentally: it is already a substring of eleven test
names. Reusing — or containing — a tag breaks two things at once and
neither loudly: the other campaign's cardinality invariant, and this
campaign's gate, which would otherwise pass by selecting tests it did not
write.

The logistics live at the item and nowhere else. A central table of
owned files, gates and open questions was proposed and is ruled out: it
would duplicate every item's name and half its body; it would drift from
the bodies the first time an item was edited without it, in the
direction that reads as authoritative; and, decisively, it could not
freeze. The maturation rule is per item — a landed item's logistics are
frozen history like the rest of it — whereas a table's rows would go on
sitting beside live ones, current-looking long after the commits that
consumed them, which is exactly the aged record the freeze callout at
the top of this document exists to prevent.

**Owns** is a checked field, not prose. `tools/check-doc-refs.py`
resolves the backticked paths in this document, so naming an artifact
the item has not built yet fails the run until the file exists; such a
name carries an entry in `tools/doc-refs-allow.txt`, with its reason,
alongside the entries the item bodies already need. The existing landing
gate closes the loop: an item that proposes an artifact takes its
outcome line only once its allowlist entries can be deleted, and
**Owns** is what makes them findable from the commit that lands it.

Three traps have each already produced a wrong result in this repo or
this campaign, so a session is told all three rather than trusted to
know them.

- A **Done** line piped into `tail` or `head` reports the *pipe's* exit
  status, so a failed build reads as success — which is how this
  campaign recorded one false positive already. **Done** lines are
  `&&`-chains, never pipelines; long output is scrolled, not trimmed.
- `npx tsc --noEmit` — the `typecheck` script in `ts-src/package.json` —
  runs in no Makefile target and in no CI job, and esbuild strips types
  without checking them, so a TypeScript regression compiles, bundles
  and deploys through `make build-ts` in silence. Every TS-touching
  item's **Done** runs it explicitly, in the shape
  `(cd ts-src && npx tsc --noEmit)`.
- A green `tools/check-plan-citations.py` proves that every `file:line`
  resolves, not that the line still says what the claim needs: the file
  is long enough and something else slid into the slot. An item that
  moves lines in a cited file re-reads the snippets the checker prints
  and repairs the citations in a *separate* commit, so review can tell a
  relocation from a correction.

This is not a fan-out plan, and the width it does have is spent before
any code is. The hot files are small: `ts-src/src/terminal.ts` is 219
lines with twelve writers across the campaign, `Wasm.hs` 138 with six of
them rewriting the same four-line `startup` body, `ts-src/src/loader.ts`
102, and `GameDefinition/index.html` 95 with no automated gate on it at
all. "Different regions of the same file" is real concurrency in a large
module and applies nowhere here — two items in one of these files are
two items in the same paragraph. Landing concurrency accordingly runs
about five items at its widest and two or three typically, widest where
**Owns** sets are genuinely disjoint: Phase 3 against Phases 1-2, the
standalone refactors against everything. The parallel effort that pays
is upstream of the code, on the open **Decide first** questions, each
blocking an item that is otherwise ready to hand out.

Worktree isolation helps exactly where a collision would be loud: a new
module no one else has a copy of (0.1's `InputDecision`, 2.1's
`OverlayLayout`), and A/B builds wanting two trees and two binaries at
once. It is actively harmful on this plan's central risk. Git never
reports a conflict between `Sdl.hs` and `Wasm.hs`, or between either and
`terminal.ts` — they are different files — so two isolated sessions each
fixing the same behavior in its own frontend, differently, merge clean
and produce precisely the near-duplicate drift this plan exists to
eliminate. Isolation also removes the one mechanism that catches it:
diffing analogous positions in a single tree. Parity work stays in one
checkout.

Three things a session never does on its own authority: push; open or
force-update a pull request; and take an outcome line on a partial
landing. Permission to make a change is not permission to publish it,
and a green **Done** line is evidence about one commit, not the
judgement that an item is finished — where **Split** applies, the
outcome line waits for the commit it names.

---

## Phase 0 — shared foundations

### 0.0 Immediate fixes (ship before anything else)

- **AltGraph, keyboard.** `terminal.ts:203` forwards `e.altKey` only;
  `Dom.hs:126-127` ORs `getAltKey || getAltGraphKey`, so AltGraph-only
  chords (common on European layouts) are lost in the WASM build. Forward
  `e.altKey || e.getModifierState("AltGraph")`; 0.1's redesign then
  subsumes the fix.
- **AltGraph, mouse and wheel.** SDL folds AltGr (and the GUI keys) into
  Alt for mouse events too — `modTranslate` reads `keyModifierAltGr` and
  applies to button/wheel events via `getModState`
  (`Sdl.hs:353,362,771-781`). The wasm handlers pass only `e.altKey`, so
  e.g. the `A-MiddleButtonRelease` binding
  (`GameDefinition/.../Content/Input.hs:202`) can't fire with AltGr.
  Same one-line fix per handler.
- **Highlight outlines follow SDL2.** The rule (also encoded in 0.2's
  `CellStyle` and pinned by its fixtures), read off
  `chooseAndDrawHighlight` (`Sdl.hs:504-518`): *every* kind gets a
  four-sided `SDL.drawRect` over the whole cell box, but the colour
  differs. Kinds other than
  `HighlightNone`/`HighlightBackground`/`HighlightNoneCursor` go through
  `drawHighlight`, which sets `highlightToColor bg` and resets to
  `blackRGBA` afterwards (`Sdl.hs:498-503`). Those three instead take
  `workaroundOverwriteHighlight`, drawing the same rect in the renderer's
  *current* colour — always `blackRGBA` outside `drawHighlight` — because
  rectangle drawing is broken in SDL 2.0.16 (issue #281) and a stale
  rect must be erased rather than left for the glyph to cover. So the
  browser rule is **black border**, not "no border", for the three kinds.
  It reads as no border on the black map background, but
  `HighlightBackground` is the vision backlight
  (`DrawM.hs:377-381,416-417`, on by default via `smarkVision = 1`,
  `SessionUI.hs:207`), and its grey wash is blitted *before*
  `chooseAndDrawHighlight` runs (`Sdl.hs:666-669`) — so SDL really shows
  a grey cell ringed in black there, and today's `terminal.ts:135`
  `inset 0 0 0 1px` box-shadow is wrong only in its `BrBlack` colour, not
  in drawing a ring at all. Fix in `terminal-core.ts` (border colour
  black for the three kinds) with a `terminal-core.test.ts` case.
  Background for why the wasm build behaved this way, and the ruling on
  GHCJS's two-edge rendering, is in Appendix B.

All three fixes land with tests: the two AltGraph fixes get the jsdom
forwarding tests (the ground-rule exception above), the highlight rule its
`terminal-core.test.ts` case.

**Split** — the jsdom harness first (`ts-src/src/terminal-input.test.ts`,
`npm i -D jsdom`, a per-file `// @vitest-environment jsdom` docblock),
then the two AltGraph fixes as **one** commit, with the highlight fix
concurrent. The three fixes are one ledger row, so the document is edited
once.

**Owns** — `ts-src/src/terminal.ts`, `ts-src/src/terminal-input.test.ts`,
`ts-src/src/terminal-core.ts` and its test, `ts-src/package.json` and
`package-lock.json`. The two AltGraph fixes are not concurrent with each
other: they share the new test file and the lock, which does not merge.

**Done** — `ts`, `docs`.

**Hands back** — *browser*: one AltGr chord on a European layout and one
`A-MiddleButtonRelease` with AltGr held, since jsdom synthesises
`getModifierState` and so proves forwarding, not that the browser reports
it. The highlight rule hands back nothing: the vitest case pins it.

**Decide first** — nothing.

### 0.1 `InputDecision`: one Haskell brain for keyboard/mouse decisions

**New module** `Game.LambdaHack.Client.UI.Frontend.InputDecision`:

```haskell
data KeyDecision = KeyDecision
  { kdModifier       :: K.Modifier  -- squashed modifier to actually send
  , kdKey            :: K.Key
  , kdResetChan      :: Bool        -- True for Esc
  , kdPreventBrowser :: Bool        -- True unless an allowed passthrough
  }

decideKey :: K.Modifier -> K.Key -> KeyDecision
decideWheel :: Double -> Maybe K.Key      -- WheelNorth/WheelSouth/Nothing
decideMouseButton :: Int -> K.Key         -- DOM button code -> *ButtonRelease
```

This subsumes four currently-independent copies of the same decisions:
`Dom.hs`'s keydown/wheel/mouseup handlers (the canonical origin),
`Wasm.hs`'s `lhKey`/`lhWheel`/`lhMouseUp`, `terminal.ts`'s
`CTRL_PASSTHROUGH_KEYS`/`DEAD_KEYS` (the cross-language copy that produced
the 11-of-13-DeadKeys bug), and `Sdl.hs`'s verbatim copy of the
modifier-squash + Esc-reset block (`Sdl.hs:343-349`), which is replaced by
a `decideKey` call that ignores `kdPreventBrowser` — the module is
frontend-universal, not web-specific. The browser-zoom passthrough
(`C-+`/`C--`/`C-0`) stays allowlisted here; it coexists with 1.5's
`allFontsScale`.

**`Wasm.hs` changes:**

- `lhKey` becomes a **sync** export taking the raw `KeyboardEvent` as a
  `JSVal`. Sync is required: async exports resolve on a microtask after
  the keydown dispatch has returned, too late for `preventDefault`. This
  was validated empirically against the real GHC wasm toolchain (a sync
  export's `preventDefault` demonstrably suppressed Tab's default; the
  browser makes a few shortcuts like Ctrl+T non-preventable by design, so
  don't test with those). Known trade-off: sync exports don't yet
  propagate uncaught Haskell exceptions to the JS call site — wrap the
  short body so nothing can throw past the boundary.
- Modifiers and `.key` are read off the event from Haskell via `unsafe`
  imports (`"$1.key"`, `"$1.ctrlKey"`, `"$1.altKey ||
  $1.getModifierState('AltGraph')"`, …), putting the AltGraph OR in one
  place, next to where `Dom.hs` does the same.
- One new import `js_preventDefaultAndStop :: JSVal -> IO ()` (`Dom.hs`
  never calls one without the other), called when `kdPreventBrowser`.
- `lhWheel`/`lhMouseUp` keep their numeric signatures (TS already
  prevents unconditionally for wheel/contextmenu/mouseup, exactly like
  `Dom.hs`) but their bodies shrink to `decideWheel`/`decideMouseButton`
  calls.

**TS changes:** `CTRL_PASSTHROUGH_KEYS` and `DEAD_KEYS` are deleted; the
keydown listener shrinks to forwarding the event. `loader.ts`'s
`LhExports.lhKey` type updates (sync, takes the event).

**`Dom.hs`** is not refactored — it stays as a documented-dead example
file (R3).

**Verify:**

- tasty tests for `decideKey`: each passthrough char, DeadKey, Esc, each
  squash case;
- table-driven tasty tests for the key-translation layer itself —
  `keyTranslateWeb` and SDL's `keyTranslate` (`Key.hs:472+`,
  `Sdl.hs:783-890`), pure functions at the heart of input, untested today
  and the home of the DeadKey bug class;
- the jsdom forwarding tests for the `terminal.ts` listener (ground-rule
  exception);
- the input-side RawFrontend contract cases (key delivered while a frame
  is pending, Esc reset, `FrontPressed` — see the contract practice),
  landing with this step since they guard exactly what it rewires;
- then run the game: Ctrl+- zoom, Tab, Esc, AltGraph chords, right-click,
  wheel.

**Split** — three commits. `InputDecision` and its tasty tests with no
consumer wired; then `Sdl.hs`'s verbatim squash/Esc block
(`Sdl.hs:343-349`) replaced by a `decideKey` call that ignores
`kdPreventBrowser` — the smallest consumer, native, no FFI; then the
`lhKey` rewrite with `terminal.ts` and `loader.ts` in the same commit and
nothing else in it, as the ground rules require. The second and third are
disjoint and could run concurrently once the first lands; they are
serialized only so the signature change arrives alone. The outcome line,
and the deletion of `Client/UI/Frontend/InputDecision.hs` and of this
item's new `test/InputDecisionUnitTests.hs` entry from
`tools/doc-refs-allow.txt`, ride the third.

**Owns** — new
`engine-src/Game/LambdaHack/Client/UI/Frontend/InputDecision.hs` and
`test/InputDecisionUnitTests.hs`; `LambdaHack.cabal` and `test/Spec.hs`
for their registration; `engine-src/Game/LambdaHack/Client/UI/Frontend/`'s
`Sdl.hs` and `Wasm.hs`; `ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`ts-src/src/terminal-input.test.ts`; `tools/doc-refs-allow.txt`. Nothing
else. The input-side `RawFrontend` contract cases the practice assigns
here land in `test/InputDecisionUnitTests.hs` — the contract harness file
belongs to the contract item and is not created by this one. Across
items: `LambdaHack.cabal` and `test/Spec.hs` are also written by 0.2 and
0.3, `Sdl.hs` by 0.2 and 2.1, `terminal.ts` by 0.0 and 0.2 — one owner at
a time, and 0.0 lands first or is dropped, since the third commit
rewrites the very listener it fixes.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/InputDecision.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`test/InputDecisionUnitTests.hs`), `ts`, `wasm`, `docs`.

**Hands back** — *browser*: `preventDefault` from Haskell is the point of
the change and no headless gate reaches it — Tab not moving focus,
`C-+`/`C--`/`C-0` still zooming the page, Esc, AltGraph chords,
right-click and wheel, after an unsandboxed `make build-ts` and a
background `make serve-wasm`. The substitute in Done is `make test-wasm`
carrying 0.3's export coverage, which is the stated reason 0.3's baseline
is sequenced before this item, plus the jsdom forwarding cases. The
`Sdl.hs` commit hands back a *display* look as well (`make
frontendCrawl`): `cabal test`'s SDL cases exercise init-and-quit and font
decoding, never the event loop where `decideKey` now sits.

**Decide first** — three. (1) The sync-export spike: the plan asserts a
synchronous `foreign export javascript` was "validated empirically" but
records neither its syntax nor whether it coexists with the reactor's
`-optl-Wl,--export=` wiring and with the command-linked test suite. If
the spike holds, proceed as written; if it does not, land the first two
commits and leave `lhKey` async with `preventDefault` staying in TS,
recording the ruling. (2) The module-as-interface carve-out: the test
suite — and, in 0.2, a generator executable outside the subtree — must
reach `InputDecision` directly, so either `Frontend.hs` re-exports it or
the convention takes a stated exception; whichever, it binds 0.2 and 2.1
too. (3) Whether the table-driven test of SDL's `keyTranslate` is in
scope here: it is exported only under `EXPOSE_INTERNAL` (`Sdl.hs:4-8`),
which `release` no longer defines by default, so a test reaching it does
not compile at all — the trap `test/CLAUDE.md` records, now immediate,
and reaching it needs an sdl2 build-depend plus a `#ifndef USE_BROWSER`
guard, because `Frontend.Sdl` is not a module under `os(wasi)` and `make
test-wasm` links the same suite. Either accept that cost or test
`keyTranslateWeb` alone and say so.

### 0.2 `CellStyle` + a build-time generator for TS tables and fixtures

**New module** `Game.LambdaHack.Client.UI.Frontend.CellStyle` holding the
pure per-cell decision currently in `Dom.hs`'s `setChar`
(`Dom.hs:251-273`): decode `AttrCharW32`, even-row `White`→`AltWhite`,
glyph substitution (space→nbsp, dim floor→`⋅`), highlight→border and
background color (per 0.0's rule). `Sdl.hs`'s `setSquareChar`
(`Sdl.hs:645-669`) contains the *same* AltWhite and floor-substitution
rules (with a bitmap-font variant, `'\x0007'`), while `setMonoChar`
(`Sdl.hs:603-624`) shares only the AltWhite rule — which is also the
honest statement of what each call site consumes. So the module is
written for and consumed by **both** native
and browser frontends, parameterized by the per-frontend floor-glyph choice
(`'\x0007'` for SDL bitmap fonts, `'\x22C5'` for SDL scalable and the
web, `'.'` in ANSI and Teletype — `ANSI.hs:273`, `Teletype.hs:57`, which
makes those two frontends candidate consumers of the substitution rule
too, though they use neither AltWhite nor highlights).

Two riders on the `Sdl.hs` side of this work: derive `colorToRGBA`
(`Sdl.hs:904-922`, a hand-maintained palette copy by its own comment)
from `Color.colorToRGB`, closing the Haskell-vs-Haskell copy alongside
the Haskell-vs-TS ones; and since `Sdl.hs`'s per-cell drawing is a hot
path, gate its adoption of `CellStyle` on before/after `make bench` runs
(`benchFrontendBattle`/`benchFrontendCrawl` exercise exactly that path,
with fixed seeds).

**Build-time generator** — a small native executable (new `.cabal`
executable stanza; regenerate `haskell-ci.yml` afterwards rather than
hand-editing, per repo policy) that emits into `ts-src/src/generated/`:

1. `palette.ts`: `PALETTE` / `HIGHLIGHT_TO_COLOR` / page-chrome colors
   from the real `colorToRGB`/`highlightToColor` (`Color` has no
   `Bounded` — enumerate `[Black .. BrWhite]` explicitly; `Highlight` via
   `[minBound .. maxBound]`).
2. `fixtures.json`: behavioral fixtures from `CellStyle.styleCell` over a
   representative input sample (floor bright/dim, space, ordinary glyph,
   even/odd row × `White`, every `Highlight`), plus `(i, w) → (col, row)`
   index-decoding fixtures pinning the `toEnum`-vs-`punindex` invariant
   (guaranteed by the engine's `Client/UI/Content/Screen.hs`'s
   `rwidth == RK.rWidthMax` assertion —
   which the fixtures make checkable from the TS side too; frontend code
   itself stops relying on it per the explicit-widths practice below).
3. `cursor.ts`: an SVG data-URI rendering of `Sdl.hs`'s `cursorXhair`
   bitmap (see 1.1), so the browser pointer is generated from the same
   definition the native cursor is built from.

`terminal-core.ts` imports the generated palette instead of its
hand-written one; `terminal-core.test.ts` asserts `styledCell` matches
every fixture — a mismatch means TS drifted from `CellStyle.hs`, full
stop.

**Freshness enforcement:** a Makefile target (`make gen-ts`) that
`build-ts` depends on, plus a CI check that regenerates and fails on
`git diff --exit-code` — an out-of-date generated file must be
structurally unable to reach a deployed artifact.

**Split** — four commits. `CellStyle` plus its tasty tests, consumed by
nobody; then the generator executable, `make gen-ts`, the committed
`ts-src/src/generated/` files, `terminal-core.ts` consuming the generated
palette and asserting `styledCell` against every fixture, and the
regenerate-and-diff CI step; then `cursor.ts`, which is also 1.1's final
form; then `Sdl.hs` adopting `CellStyle`, `colorToRGBA` derived from
`Color.colorToRGB`, and the `Sdl.hs:590` `punindex` fix, behind the bench
gate. `gen-ts` must be a prerequisite target with its own shell, never an
inline step in `build-ts`: that recipe sources `~/.ghc-wasm/env`, which
points `CC`/`LD`/`AR` at wasi-sdk while leaving `cabal` and `ghc` native,
so a native build inside it links with `wasm-ld` and fails. The outcome
line, and the deletion of this item's eight `tools/doc-refs-allow.txt`
entries — `CellStyle.hs`, `palette.ts`, `fixtures.json`, `cursor.ts`,
`ts-src/src/generated/`, `make gen-ts`, plus the two it adds — ride the
last to land, which cannot precede the `cursor.ts` commit.

**Owns** — new
`engine-src/Game/LambdaHack/Client/UI/Frontend/CellStyle.hs`,
`test/CellStyleUnitTests.hs`, `GameDefinition/gen-src/GenTsTables.hs`,
`ts-src/src/generated/palette.ts`, `ts-src/src/generated/fixtures.json`
and `ts-src/src/generated/cursor.ts`; modified `LambdaHack.cabal`,
`test/Spec.hs`, `Makefile`, `ts-src/tsconfig.json` (`resolveJsonModule`,
absent today, without which importing `fixtures.json` fails
`tsc --noEmit`), `ts-src/src/terminal-core.ts` and its test,
`ts-src/src/terminal.ts`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`.github/workflows/lint-and-test-suites.yml` and
`tools/doc-refs-allow.txt`. The generated files are **committed**, not
ignored: `git diff --exit-code` is vacuous on untracked files, so an
untracked `ts-src/src/generated/` would defang the freshness check the
item exists to install. `haskell-ci regenerate` is run as policy requires
and measurably produces no diff — the generated workflow builds `all` at
package granularity and never enumerates components — so
`.github/workflows/haskell-ci.yml` is checked, not edited; and the
generator carries no haddock examples, the doctest recipe naming exactly
four components, in which a fifth's examples would run nowhere. The four
commits are strictly serial: each consumes what the previous adds, and
the first and last both encode the highlight rule. `Sdl.hs` here is not
concurrent with 0.1's second commit or with 2.1, and `terminal-core.ts`
not with 0.0.

**Done** — `native`, `ts`, `wasm`, `docs`, plus `make gen-ts` && `git diff
--exit-code ts-src/src/generated` && `haskell-ci regenerate` && `git diff
--exit-code .github/workflows/haskell-ci.yml`.

**Hands back** — *display*: the bench gate on the `Sdl.hs` commit.
`make bench` runs `benchFrontendBattle`/`benchFrontendCrawl` through the
real SDL2 frontend, so it opens a window, and the game redirects its own
stdout to `~/.LambdaHack/stdout.txt` whenever stdout is not a terminal,
so each report must be harvested between runs. Also *browser*, for
`cursor.ts`: pixel parity against the native pointer is the claim and
only an eye settles it. Done's substitutes are the fixture equality
between `CellStyle.styleCell` and `styledCell` — which is the whole
correctness claim, the bench gate being about cost — and `cabal test`'s
SDL cases, which run font discovery and decoding but never the renderer.

**Decide first** — four. (1) `styleCell`'s result type, never given, and
it must serve consumers with incompatible needs: `setSquareChar` and
`setMonoChar` want `Color`s and an atlas key, the DOM grid wants hex
strings, 2.2's canvas wants both. Either one record of `Color`s with a
thin per-frontend rendering step, or a type parameterized on the colour
representation — decide once, since 2.1 and 2.2 inherit it. (2) Whether
the highlight-border rule is a `CellStyle` output field or stays TS-side,
and note the trap either way: `HIGHLIGHT_TO_COLOR` is exactly what the
generator emits from `Color.highlightToColor`, where
`HighlightBackground -> BrBlack` is correct, so the rule — which is
`chooseAndDrawHighlight`'s (`Sdl.hs:504-518`) — must be a branch over the
three kinds, or 0.0's fix is silently reverted by the generator while its
vitest case goes on passing. (3) The bench gate's acceptance: no
threshold, repetition count or regression rule is stated, and the target
is display-bound; either state one and hand the gate to a human run, or
rule the gate unnecessary and record why. (4) The `cursor.ts` blocker:
`cursorXhair` (`Sdl.hs:421-455`) is not in `Sdl.hs`'s export list, not
even inside its `EXPOSE_INTERNAL` block, and `Frontend.Sdl` is not a
module under `os(wasi)`, so the generator cannot reach it as written —
either the ASCII art moves into an SDL-free shared module that `Sdl.hs`
then consumes (which also gives the SVG per-pixel art rather than the two
packed 1-bpp `Word8` vectors `cursorXhair` returns), or the generator
takes sdl2/sdl2-ttf dependencies.

### 0.3 FFI-coverage test (baseline first, then per-commit)

GHC's wasm JSFFI is a trust-the-embedded-string mechanism with no arity or
behavior checking against the declared Haskell type (an ecosystem gap, not
fixable here: no typed-binding generator exists for the wasm backend, and
even one that did would still pay the per-call wasm↔JS boundary cost — it
could not reproduce ghcjs-dom's zero-cost typed bindings, because GHCJS
never crosses a runtime boundary at all). Mitigation: extend the
`make test-wasm` battery (`run-wasm-test.mjs`) so every
`foreign import/export javascript` in the wasm build — today spread over
`Wasm.hs`, `WasmFile.hs`, *and* `GameDefinition/Main.hs`
(the `lhStart` export) — is exercised at least once with a known
input/output. Sequenced deliberately: the **baseline battery for the
existing surface lands before 0.1**, so the riskiest FFI change in the
plan (0.1's `lhKey` signature rewrite) is made against tested ground.
After that, each new FFI declaration lands with its coverage case in the
same commit.

**Split** — the import half first: seven of the eleven declarations that
a repo-wide grep finds outside the dead `Dom.hs`/`JSFile.hs` —
`WasmFile.hs`'s six, reachable only through `Common/File.hs`'s seven
exported functions since `WasmFile` is a hidden other-module, and
`Wasm.hs`'s `js_submitFrame`, reached by `Frontend.Wasm.startup` plus
`fdisplay` on a constructed `SingleFrame` — together with the JS stubs
(`globalThis.localStorage`, `globalThis.LZString`,
`globalThis.lhSubmitFrame`) that `run-wasm-test.mjs` must install before
`wasi.start` and does not have today. Then the export half, once its
mechanism is decided. The standing half — every new FFI declaration lands
with its coverage case — is a review rule, not a commit; by its own size
cell 0.3 retires with the campaign rather than acquiring a hash, so there
is no outcome line and no allowlist deletion to schedule.

**Owns** — `ts-src/run-wasm-test.mjs`, new `test/FfiCoverageUnitTests.hs`,
`test/Spec.hs`, `LambdaHack.cabal`, `ts-src/package.json` and
`package-lock.json` (lz-string as a devDependency — it must come from
npm, never from the sibling checkout, whose `lz-string*.js` is a deployed
artifact). Nothing else. The two commits are serial: the export half
reuses the driver and the stubs the import half installs. Across items:
not concurrent with 0.0 or 3.2, which write the same lock file, and it
does not merge; the stubs are defined once here and exported for 3.2's
`run-wasm-game.mjs` to import rather than copy.

**Done** — `native` (stylish over `test/FfiCoverageUnitTests.hs`), `wasm`.

**Hands back** — *judgement*, and only for the standing half: a reviewer
confirms each FFI-touching commit carries its case. The substitute in
Done is `make test-wasm`, which fails when a covered declaration breaks
but never when an uncovered one is added — the cheap mechanization, a
checker asserting that the count of `foreign import/export javascript`
declarations equals the battery's case count, is not proposed by this
plan and would close the gap if written. The two executable halves hand
back nothing.

**Decide first** — three. (1) The export mechanism, and the measurement
that forces the question: the wasm test binary already exports `lhKey`,
`lhWheel` and `lhMouseUp` — DCE spares them because `Frontend.Wasm` is
statically reachable from `initUI` — but calling any of them after
`wasi.start` returns fails with "newBoundTask: RTS is not initialised;
call hs_init() first", a command's `_start` running `hs_exit` on the way
out, and `lhStart` is not among them at all, living in
`GameDefinition/Main.hs`, the executable's `main-is`, which the test
suite does not link. Three branches: (a) call the exports re-entrantly
from JS while the RTS is live, from the driver's `lhSubmitFrame` stub
during a Haskell-initiated crossing — cheapest, and needs a spike on
re-entering the RTS from an `unsafe` import; (b) a second, reactor-linked
test component, which contradicts the deliberate `.cabal` comment keeping
the test-suite out of the reactor treatment; (c) fold export coverage
into 3.2's `run-wasm-game.mjs`, which reorders it behind Phase 3 and
leaves this baseline covering imports only — i.e. not covering 0.1's
`lhKey` rewrite, the plan's own stated reason for landing 0.3 first. (2)
Whether `test/FfiCoverageUnitTests.hs` is CPP-guarded wasm-only or also
runs natively over `HSFile.hs`, where the same calls touch the real
filesystem. (3) What "a known input/output" means per declaration, since
it is an instruction and not an acceptance criterion; proposed, and to be
ruled on rather than reinvented per session: an `encodeEOF`/
`strictDecodeEOF` round trip returning the original value;
`doesFileExist` False before a write and True after; `renameFile` leaving
the new key holding the old bytes and the old key absent
(`WasmFile.hs:107-114`); a damaged EOF marker rejected rather than
silently decoded, which is also R1's stated wasm round-trip test; and,
for `js_submitFrame`, the stub recording exactly one call per frame with
`(addr, w, h)` matching `rwidth`/`rheight` and the first cell word equal
to the fixture's.

---

## Phase 1 — small parity wins

Independent of each other; any order. 1.1 and 1.3 don't depend on Phase 0;
1.2 benefits from 0.2's generated palette but can precede it.

### 1.1 Crosshair cursor over the map (trivial)

`ts-src/src/terminal.ts` `buildGrid` already sets a batch of container
styles; add `container.style.cursor = "crosshair"` — the zero-dependency
interim step, and a good toolchain warm-up. The final form comes with
0.2: its generator emits an SVG data-URI cursor from SDL2's own bitmap
definition (`cursorXhair`'s ASCII-art alpha/BW pair, `Sdl.hs:421-455`;
32×27, hotspot (13,13) per the `createCursor` call at `Sdl.hs:211-213`),
and `terminal.ts` switches to
`cursor: url(<data-URI>) 13 13, crosshair` — the keyword staying as the
fallback. Pixel-parity with the native pointer, sourced from the one
canonical definition. Verify by hovering, both before and after the 0.2
upgrade.


**Split** — two commits, in this order: the `crosshair` keyword line
first, then the generated SVG data-URI form once 0.2 has emitted
`cursor.ts`. The keyword commit takes **no** outcome line and leaves the
row live, per the partial-landing rule; the SVG commit carries the
outcome line and flips the ledger row. Neither deletes a
`tools/doc-refs-allow.txt` entry — `cursor.ts` and `ts-src/src/generated/`
there are 0.2's, and 1.1 is ungated.

**Owns** — `ts-src/src/terminal.ts`, and, in the landing commit only,
`docs/wasm-frontend-unified-plan.md`: a line inserted in `buildGrid`
shifts `terminal.ts:135` and `terminal.ts:203`, both cited above, so even
this one-liner obliges a citation repair and a restamp. `terminal.ts` is
contended by 1.2 and 1.5, so the three serialize; take 1.1 first, it
clears the file fastest. `../lambdahack.github.io` is redeployed by
**Done**, not owned, and that command needs unsandboxed Bash.

**Done** — `ts`, `wasm`, `deploy`, `docs`, plus `grep -qF crosshair
../lambdahack.github.io/bundle.js`.

**Hands back** — *display*: that the pointer reads as a crosshair over the
map, and, for the SVG form, that the browser accepted the data URI rather
than falling back to the `crosshair` keyword — a failure with no console
error, no failing test and no trace in the bundle. The substitute gate in
**Done** is the `grep` over the deployed `bundle.js`, which proves the
string shipped and nothing about how it painted.

**Decide first** — nothing.

### 1.2 Screenshots: make `Ctrl+P` real (small–medium)

Today `C-P` → `PrintScreen` → `printScreenHuman` shows *"Screenshot
printed."* and calls `fprintScreen`, which for wasm is the `Common.hs:67`
dummy. The fix follows `Sdl.hs:273`'s own pattern (override the field
after `createRawFrontend`), with the **filename scheme living in
Haskell**, shared with SDL2:

1. Extract `Sdl.hs:743-755`'s timestamp scheme (`"prtscn" <> dateText`,
   spaces→`_`, `:`→`.`) into a small shared pure helper (e.g. in
   `Frontend.Common` or a sibling), unit-tested in tasty against a fixed
   time; `Sdl.hs` calls it too. Timezone: UTC in the browser — under
   wasi `getTimeZone` resolves to UTC, and that divergence from SDL's
   local-time names is accepted (document it in the helper's haddock; a
   `js_tzOffsetMinutes` FFI import was considered and rejected as not
   worth the extra FFI surface).
2. `Wasm.hs`: `foreign import javascript unsafe
   "globalThis.lhPrintScreen($1)" js_printScreen :: JSString -> IO ()`;
   `startup` sets `fprintScreen` to compute the name (`.png`, not `.bmp`
   — canvas exports PNG natively) and call it.
3. TS: `lhPrintScreen(name)` (wired in `loader.ts` next to
   `lhSubmitFrame`) rasterizes the current cell buffer (`prev`, `cols`,
   `rows` in `terminal.ts`) to an offscreen `<canvas>` via `styledCell` +
   the generated palette, then triggers an `<a download>` click.
   Structure it as a functional core over **draw commands**: a pure
   function emits the op list (fill rect, draw glyph at position with
   color), and a thin interpreter executes it on the canvas — the op list
   is vitest-testable with no canvas dependency, and 2.2's overlay
   renderer reuses the same shape.

Verify: tasty test for the filename helper; vitest asserting the draw-op
list for a small fixture frame; run the game, press `Ctrl+P`, confirm the
downloaded PNG matches the screen. **Forward dependency (R6):** after
Phase 2 lands, this rasterizer must also draw the overlay layers, or
screenshots silently regress to map-only — tracked in 2.5's checklist.


**Split** — three commits. (1) The filename helper plus its tasty test,
with `Sdl.hs` switched onto it: native only, no TS and no wasm in the
diff. (2) The pure rasterizer — `ts-src/src/screenshot-core.ts` and
`ts-src/src/screenshot-core.test.ts`, emitting the draw-op list and
nothing else — which also deletes this item's two
`tools/doc-refs-allow.txt` entries, since the files it names now exist.
(3) The wiring: `Wasm.hs`'s `js_printScreen` import and its
`fprintScreen` override, `lhPrintScreen` in `loader.ts`, the frame
snapshot out of `terminal.ts`, the `<a download>` interpreter, and 0.3's
coverage case in `ts-src/run-wasm-test.mjs`. Commits 1 and 2 are disjoint
and may be written concurrently; 3 needs 2, and carries the outcome line
and the ledger flip.

**Owns** — `engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs` and
`Wasm.hs` beside it, the helper's home module under
`engine-src/Game/LambdaHack/Client/UI/Frontend/`, a new tasty module
registered in `test/Spec.hs` and `LambdaHack.cabal`,
`ts-src/src/screenshot-core.ts` and its test, `ts-src/src/terminal.ts`,
`ts-src/src/loader.ts`, `ts-src/run-wasm-test.mjs`,
`tools/doc-refs-allow.txt`, and `docs/wasm-frontend-unified-plan.md` —
the `Sdl.hs` citations, `Wasm.hs:79`, `loader.ts:56`, `terminal.ts:135`
and `terminal.ts:203` all shift. Cannot run concurrently with 1.5: four
shared files, three of them at the same lines — `startup`'s body and the
`foreign import javascript` block in `Wasm.hs`, `loader.ts`'s `declare
global` and its `globalThis.lh*` wiring, `terminal.ts`'s `Terminal`
interface and returned object — nor with 1.1, which writes `terminal.ts`.
`../lambdahack.github.io` is redeployed by **Done**, not owned, and that
command needs unsandboxed Bash.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/*.hs`, `test/*.hs`), `ts`,
`wasm`, `deploy`, `docs`.

**Hands back** — *display*: press `Ctrl+P` in a browser and compare the
downloaded PNG with the screen. That comparison is not merely eye-only,
it is undefined — canvas `measureText`/`fillText` placement is not the
DOM grid's `1ch`/`1em` placement and glyph rasterization differs per
browser, so "matches" has no strict reading (see **Decide first**) — and
a second failure mode is invisible with every gate green: `lambdaHackFont`
exists only as the `@font-face` in `GameDefinition/index.html`, so a
rasterizer that does not await `document.fonts.ready` silently draws the
PNG in a fallback font. The substitute gates in **Done** are the tasty
test pinning the filename against a fixed time and the vitest assertion
on the draw-op list; `make test-wasm` adds nothing here, since it passes
`--frontendNull`, so `Wasm.hs` never executes and an undefined
`globalThis.lhPrintScreen` would go unnoticed.

**Decide first** — four, none of them an executing session's to settle.
(a) What "the PNG matches the screen" means: a tolerance against a
captured reference, a golden draw-op list captured from a real frame, or
an explicit ruling that the acceptance is a human glance and the item
ships with no machine criterion. (b) Whether the rasterizer honors 1.5's
`font-size` and 1.3's fullscreen transform or always renders at the
native 16px — SDL's `printScreen` reads the live `SDL.windowSize`, so
parity argues for the former, and nothing in the plan says so. (c) Where
the shared filename helper lives: `Frontend.Common`, which is not a pure
module today, or a new pure sibling under
`engine-src/Game/LambdaHack/Client/UI/Frontend/` — the choice fixes the
new tasty module's name and its `LambdaHack.cabal` stanza. (d) Whether
0.3's per-commit FFI-coverage rule binds `js_printScreen`: nothing
headless executes `Wasm.hs` today, so the rule either waits for 3.2's
reactor driver or is satisfied by a link-check the plan does not call
coverage.

### 1.3 Fullscreen toggle with scaling (small, no Haskell)

Page-level only (`sfullscreenMode` is a startup-time SDL choice,
`ClientOptions.hs:18-39`, with no in-game command even natively). Add a
"⛶ Fullscreen" button to `GameDefinition/index.html` next to the banner;
`click` → `document.documentElement.requestFullscreen()` /
`document.exitFullscreen()` toggled on `document.fullscreenElement`; keep
the label synced via `fullscreenchange` (covers browser-`Escape` exit).
A button beats telling players to press F11: several browsers leave some
chrome visible under F11, and the Fullscreen API is the closer analogue
of SDL2's `BigBorderlessWindow`.

**Scaling is part of parity, not a nicety:** SDL fullscreen sets
`rendererLogicalSize` so the whole frame scales up to fill the screen with
aspect ratio preserved (`Sdl.hs:237-240`); a bare `requestFullscreen()`
would just center the fixed 16px grid in a sea of black. While fullscreen,
scale `#screen` to fit — e.g. a CSS `transform:
scale(min(vw/gridW, vh/gridH))` computed on `fullscreenchange`/`resize`
(transform keeps the DOM layout untouched; bitmap-font blur at non-integer
scales is the same trade-off SDL's scaler makes). Factor any pure
scale-computation helper into a `*-core.ts` for vitest; the
`requestFullscreen` call itself is a run-the-game check (jsdom can't).


**Owns** — `GameDefinition/index.html`, `ts-src/src/fullscreen-core.ts`
and `ts-src/src/fullscreen-core.test.ts`, the one import and one call
they need in `ts-src/src/loader.ts`, `tools/doc-refs-allow.txt` (its two
entries go in the same commit that creates the files), and
`docs/wasm-frontend-unified-plan.md` — the import shifts `loader.ts:56`,
cited twice, so this item is not citation-safe either. Not concurrent
with 1.4, which rewrites the same `<table id="banner">` region, nor with
1.5, which rewrites the `#screen` rule's scaling-blur comment, nor with
1.2 or 1.5 on `loader.ts`; where those hold `loader.ts`, hand them the
import as a request rather than writing it here.
`../lambdahack.github.io` is redeployed by **Done**, not owned, and that
command needs unsandboxed Bash.

**Done** — `ts`, `wasm`, `deploy`, `docs`, plus `grep -qF Fullscreen
../lambdahack.github.io/index.html` && `grep -qF fullscreenchange
../lambdahack.github.io/bundle.js`.

**Hands back** — *display*: that the page really enters fullscreen, that
the scaled grid is centred and aspect-correct rather than clipped, and
that bitmap blur at non-integer scales is acceptable. Doubly so —
`requestFullscreen()` demands a *trusted* user gesture, so no page script
and no `element.click()` can raise it, and even a browser-automation
channel would need a synthetic trusted click. `#screen` is laid out
`width: max-content; margin: 0 auto`, so a `transform: scale()` grows the
painted box while the layout box stays where it was; whether that reads
as centred is exactly the part no gate sees. The substitute gates in
**Done** are the vitest assertion on the pure scale arithmetic and the
two greps over the deployed page and bundle — and note that `make
build-ts` merely `cp`s `index.html`, so malformed HTML deploys with
everything green.

**Decide first** — two. (a) Where the wiring lives: `npm run build`
bundles `src/loader.ts` alone, so a new module is dead code unless
`loader.ts` imports it or the build script gains a second entry point,
and the third option — an inline `<script>` in `index.html` — is
unbundled and untestable. The plan says "page-level only" and resolves
none of that. (b) Whether 1.5 lands first. "Any order,
parallel-friendly" is false for this pair: a scale computed off a
hardcoded 16px while 1.5 has set the grid to `16 × scale` px overflows
the viewport, silently, with every test green. Either sequence 1.5 before
1.3, or rule that the helper measures the live grid with
`getBoundingClientRect()` and never assumes a cell size.

### 1.4 Banner truth maintenance (trivial, recurring)

`GameDefinition/index.html`'s banner and status text still claim save
games aren't persistent (they are, via `WasmFile.hs`/localStorage) and
will go staler as features land ("For proportional fonts, fullscreen, and
persistent save games, use the native binary"). Update the text as each
feature ships. The "savefiles are prone to corruption" caveat stays until
R1 addresses it. (The GHCJS-era page's community features are
deliberately not restored, and the page `<title>` stays hardcoded —
Appendix B.)


**Split** — recurring by design: one commit per capability landing, each
deleting only the clause its feature falsified — 1.3 for "fullscreen",
2.4 for "proportional fonts" and for the `#status` div's "limited to the
square font", R1 for "Savefiles are prone to corruption". Each runs the
**Done** line below with only its own phrase in the alternation. No
commit carries an outcome line and the ledger row never flips: 1.4 is a
standing item and retires with the campaign. It deletes no
`tools/doc-refs-allow.txt` entry.

**Owns** — `GameDefinition/index.html`, and
`docs/wasm-frontend-unified-plan.md` wherever this item's own body quotes
the banner: 1.4 is `open · standing`, so a quote that no longer matches
the page is an error to fix, not drift to leave alone. Single-writer on
`index.html` — 1.3 adds its button to the same `<table id="banner">` and
1.5 rewrites the `#screen` comment, so neither may be in flight.
`../lambdahack.github.io` is redeployed by **Done**, not owned, and that
command needs unsandboxed Bash.

**Done** — `wasm`, `deploy`, `docs`, plus `! grep -qE 'proportional
fonts|limited to the square font' ../lambdahack.github.io/index.html`.

**Hands back** — *judgement*: whether the replacement sentence is true of
the shipped build. No display is needed and no command decides it — a
grep proves a stale phrase is gone, never that what replaced it is
accurate, and the `#status` div carries two further claims of the same
kind whose truth is owned by 2.4 and R1 rather than by whoever edits the
page. The substitute gate in **Done** is exactly that absence grep, run
over the deployed `index.html` rather than the source, so a forgotten
`make build-ts` fails it.

**Decide first** — one, and no fleet can dispatch this item without it:
who writes the banner. The document reads two ways. The body's "update
the text as each feature ships" makes 1.4 a post-condition of every
capability item, which turns `GameDefinition/index.html` into a shared
write target needing a fleet-level lock and puts an `index.html` line in
1.3's, 1.5's, 2.4's and R1's own **Owns**. Sequencing's "banner last"
makes it a single terminal agent and `index.html` a single-writer file,
at the price of shipping a page that lies for the whole interval. Pick
one before dispatch; picking neither loses the edits.

### 1.5 `allFontsScale` in the browser, alongside browser zoom

The browser build supports *both* mechanisms, as complementary:
`allFontsScale` (the game's own display-scale setting, as in SDL2) *and*
the browser's native zoom (`C-+`/`C--`/`C-0`, allowlisted in 0.1).

Mirror SDL2's semantics (`Sdl.hs:169-183`): at scale 1.0 use the
pixel-perfect rendering — for the web that means the map font's `.woff`
drawn at its native 16px, exactly what the page does today (the
`index.html` comment about avoiding scaling blur) — and at any other
scale fall back to scalable rendering, i.e. the same outline font at
`16 × scale` px, accepting the browser's rasterization the way SDL
accepts FreeType's. Auxiliary (prop/mono) font sizes multiply by the
scale too, which 2.2's font wiring already specifies.

Mechanics: `sallFontsScale` is already in the embedded config/options;
pass it to TS in the same startup call that 2.2 introduces for fonts (or
a trivial precursor if this lands first) and set the grid container's
`font-size` accordingly — cell metrics stay consistent because 2.2
derives them by measuring the rendered grid rather than hardcoding 16px.
Until R4 lands there is no way for a *player* to change the value in the
browser (no config file, no argv), so the full player-facing feature is
`1.5 + R4 (?allFontsScale=)`; 1.5 alone makes the engine-side value
honored instead of silently ignored.


**Split** — two commits: the startup channel and the `font-size` it sets
(`Wasm.hs` binding the `ClientOptions` its `startup` discards today,
`loader.ts` wiring, `terminal.ts` applying the scale inside `buildGrid`
so it survives every dimension change, and 0.3's coverage case in
`ts-src/run-wasm-test.mjs`), then the `#screen` comment repair in
`GameDefinition/index.html`, split off because that file is 1.4's and
that repair is 1.4's rule. The first commit carries the outcome line and
flips the ledger row: 1.5's player-facing half belongs to R4, so
withholding it is not a partial landing of this item. Neither commit
deletes a `tools/doc-refs-allow.txt` entry.

**Owns** — `engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`ts-src/src/loader.ts`, `ts-src/src/terminal.ts`,
`ts-src/run-wasm-test.mjs`, `GameDefinition/index.html` (only the
`#screen` rule's blur comment, which a scaled `font-size` no longer
wholly describes) and `docs/wasm-frontend-unified-plan.md` — `Wasm.hs:79`,
`loader.ts:56`, `terminal.ts:135` and `terminal.ts:203` all shift. Cannot
run concurrently with 1.2 (four shared files, three at the same lines),
with 1.1 (the same `container.style.*` batch in `buildGrid`), or with 1.3
and 1.4 on `index.html`. Land it before 1.3, whose scale arithmetic is
wrong if written against a hardcoded 16px. `../lambdahack.github.io` is
redeployed by **Done**, not owned, and that command needs unsandboxed
Bash.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/*.hs`), `ts`, `wasm`, `deploy`,
`docs`.

**Hands back** — *display*: that the grid renders larger. The item body
carries no `Verify:` step — alone among 1.1, 1.2 and 1.3 — and the gap is
not one a browser channel would close, because a session cannot even
*produce* the interesting case: `allFontsScale = 1.0` sits in
`config.ui.default`, which is embedded at compile time via
`rcfgUIDefault`, and the browser has no argv and no config file until R4,
so exercising any other scale means a throwaway edit there plus a full
wasm rebuild — an edit that must not be committed, since it changes
native behavior too. The substitute gate in **Done** is `make
test-wasm`, which link-checks the new import string and nothing more.

**Decide first** — one, and it is 2.2's design rather than 1.5's. The
startup call is undesigned in name, direction (a Haskell-side `foreign
import javascript` of a `globalThis.lh*` function, versus a new reactor
export JS calls) and payload, and 2.2 needs the same call to carry font
information; a scale-only `globalThis.lhSetFontScale(s)` invented here
forces 2.2 into the signature change the ground rules forbid outside
0.1's sanctioned `lhKey` exception, whereas one extensible startup
payload designed now leaves 1.5 filling in a single field. One thing that
looks like a decision and is not: SDL's "bitmap at 1.0, scalable
otherwise" branch (`Sdl.hs:169-183`) needs no browser counterpart,
because the `dejavuBold` fontset points both `fontMapScalable` and
`fontMapBitmap` at `16x16xwScalable`, i.e. at the same `16x16xw.woff` the
page already loads — so `16 × scale` px on the deployed font is correct
and deploys no new font.

---

## Phase 2 — multi-font, as extraction rather than re-implementation

Strict chain 2.1 → 2.2 → 2.3 → 2.4; each independently shippable and
visually a no-op until 2.4 flips the switch. (Until then, nothing is
missing on screen: under `singleFontSetup` the engine pre-flattens all
overlay text into the square-font `singleArray`, which is why the wasm
build shows menus and the log today, just in the map font.)

Before 2.2 lands, capture a browser frame-timing **baseline** — with R5's
harness if it exists by then, else a temporary `performance.now()` probe —
and re-measure at 2.5, so multi-font's rendering cost is an attributed,
measured change rather than a guess.

### 2.1 Extract `OverlayLayout`: the pure half of `Sdl.hs`'s overlay drawing

**New module** `Game.LambdaHack.Client.UI.Frontend.OverlayLayout`, holding
every per-line and per-chunk decision that today sits interleaved with SDL
IO in `Sdl.hs:593-713`. It is cut along the *measurement* line, and that
seam is the design: chunking, colours, faces and the two fixed-pitch line
cuts are pure and take no metrics at all, while the proportional pen — the
one quantity that depends on rendered width — advances through two small
pure functions: a measurement-free cutoff test, called *before* a chunk is
rendered, and a fit function called with the width just measured. The
ruler stays in the frontend; the cutoff rule, the clamp rule and the
trimmed-line marker get one definition.

**Metrics.** Everything device-space is parameterized, never baked to SDL's
numbers:

```haskell
-- | Cell metrics in one consumer's own device units: SDL pixels of the
-- loaded map font, or the browser's CSS pixels measured off the rendered
-- span grid (2.2).
data LayoutMetrics = LayoutMetrics
  { lmHalfSize :: Int  -- ^ one 'PointUI' step, and the mono pitch
                       --   (@Sdl.hs:623@)
  , lmBoxSize  :: Int  -- ^ one square cell: the square pitch and the row
                       --   height of all three layers
                       --   (@Sdl.hs:623@, @Sdl.hs:666@, @Sdl.hs:708@)
  , lmWidth    :: Int  -- ^ @rwidth coscreen@, screen width in squares
  }
  deriving (Show, Eq)

-- | The only constructor, so that "map font determines cell size for all
-- others" (@Sdl.hs:183-184@) cannot come apart in a consumer. @Sdl.hs@
-- passes half the loaded map font's size, that font having been opened at
-- a point size already multiplied by @sallFontsScale@ (@Sdl.hs:152@); the
-- browser passes half its measured grid box.
mkLayoutMetrics :: Int -> Int -> LayoutMetrics
mkLayoutMetrics halfSize width =
  assert (halfSize > 0 && width > 1)
  $ LayoutMetrics halfSize (2 * halfSize) width
```

**The metric-free half.** Its invariant is visible in the signatures: none
of these takes a `LayoutMetrics`, so none of them can emit a device-space
number. `cellPitch` is the one exception, labelled as such: it interprets
a `CellLayer` into a device advance, and is here only to sit beside the
type it interprets.

```haskell
-- | Which of the two proportional faces draws a chunk
-- (@Sdl.hs:700-702@, over the two fonts loaded at @Sdl.hs:186-187@).
--
-- Deliberately not 'Overlay.DisplayFont': that type names the three
-- overlay *layers* (@Overlay.hs:56@), is kept abstract on purpose so that
-- 'FontSetup' overrides cannot be bypassed (@Overlay.hs:44-51@), and
-- refuses width questions outright (@textSize PropFont@ is an @error@,
-- @Overlay.hs:64-67@) — which is the one question this module poses. It
-- has no bold constructor either, so it cannot express this choice.
--
-- Two constructors and no square or mono ones: those layers have exactly
-- one face each, so their per-item font choice does not exist.
data PropFont = PropRegular | PropBold
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | The face rule, on the already even-row-adjusted colour. @AltWhite@
-- sorts between @White@ and @BrBlack@ (@Color.hs:33-51@), so the even-row
-- substitution provably cannot flip a chunk from regular to bold — which
-- is why 'chunkPropLine' may apply it before reading the colour here, as
-- @Sdl.hs:693-694,700@ does.
propFontOf :: Color.Color -> PropFont
propFontOf fg | fg >= Color.White && fg /= Color.BrBlack = PropRegular
              | otherwise = PropBold

-- | One maximal same-colour run of a proportional line. It carries no
-- position: its x is the running sum of the *measured* widths of every
-- chunk before it (@Sdl.hs:696-697@), which is the whole reason the
-- placement half of this module exists.
data PropChunk = PropChunk
  { pcFont     :: PropFont      -- ^ 'propFontOf' of 'pcColor'
  , pcColor    :: Color.Color   -- ^ even-row adjusted (@Sdl.hs:693-694@)
  , pcText     :: Text          -- ^ colours dropped (@Sdl.hs:695@);
                                --   never empty
  , pcAllSpace :: Bool          -- ^ @T.all isSpace pcText@
                                --   (@Sdl.hs:703@). Not a convenience: it
                                --   decides whether an overrunning run may
                                --   claim the last cell (@Sdl.hs:553@) and
                                --   whether the cut raises the marker
                                --   (@Sdl.hs:577@), so it is the one
                                --   measurement-rule input that must reach
                                --   the consumer, and 2.3 carries it.
  }
  deriving (Show, Eq)

-- | One proportional line, still at its logical 'PointUI' start.
data PropLine = PropLine
  { plStartX :: Int  -- ^ 'PointUI' x, in half-cells; multiplied by
                     --   @lmHalfSize@ exactly once, in 'startPropLine'
                     --   (@Sdl.hs:673@)
  , plRow    :: Int
  , plChunks :: [PropChunk]
  }
  deriving (Show, Eq)

-- | Which fixed-pitch layer a run belongs to. Total, unlike a face tag:
-- these two layers are the ones with a pitch.
data CellLayer = LayerSquare | LayerMono
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Device advance of one cell: a box for square (@Sdl.hs:666@), a half
-- for mono (@Sdl.hs:623@).
cellPitch :: LayoutMetrics -> CellLayer -> Int
cellPitch LayoutMetrics{lmHalfSize, lmBoxSize} = \case
  LayerSquare -> lmBoxSize
  LayerMono -> lmHalfSize

-- | A run of cells in one fixed-pitch layer, already cut to the screen.
--
-- The cells stay raw 'Color.AttrCharW32' words, @bg@ included, because the
-- square layer really carries highlights: @bg@ enters the atlas key when
-- it is @HighlightBackground@ (@Sdl.hs:644-646@), picks the grey fill
-- (@Sdl.hs:657-659@) and drives @chooseAndDrawHighlight@ (@Sdl.hs:669@,
-- defined at @Sdl.hs:504-518@). The even-row and floor-glyph substitutions
-- are *not* applied here — they are 0.2's `CellStyle` rules, applied by
-- the painter per cell; restating them would be the second definition G1
-- forbids.
data CellRun = CellRun
  { crLayer :: CellLayer
  , crRow   :: Int
  , crCol   :: Int  -- ^ leftmost cell, in units of 'cellPitch': square
                    --   columns after @uiToSquare@'s @div 2@
                    --   (@PointUI.hs:39-41@, applied at @Sdl.hs:628@),
                    --   'PointUI' half-cells for mono (@Sdl.hs:595@)
  , crCells :: AttrString  -- ^ non-empty
  }
  deriving (Show, Eq)

-- | One frame's overlays. The field order is the draw order — prop, then
-- square, then mono last, so a mono overrun warning wins over the
-- proportional text beneath it (@Sdl.hs:728-733@, and the engine says the
-- same of the layers at @Overlay.hs:38-42@) — and 2.3's payload carries
-- its three sections in that order, so neither consumer has to know the
-- rule to obey it. @singleArray@ is not here: the map is not an overlay
-- and is drawn by diffing (@Sdl.hs:720-723@).
data FrameLayout = FrameLayout
  { flProp   :: [PropLine]
  , flSquare :: [CellRun]
  , flMono   :: [CellRun]
  }
  deriving (Show, Eq)

-- | Lay out a whole frame. The 'Int' is @rwidth coscreen@.
layOutFrame :: Int -> SingleFrame -> FrameLayout

-- | Split a line into maximal same-colour chunks (@Sdl.hs:679-695@),
-- resolving two rules that a hand-port would drift on: a *leading* space
-- run takes the colour of the next non-space character, or its own if the
-- line is all spaces (@Sdl.hs:683-687@), while an *interior* space run
-- joins the run to its left, spaces matching any colour in @sameAttr@
-- (@Sdl.hs:688-689@). Even-row @White@ becomes @AltWhite@
-- (@Sdl.hs:693-694@) via `CellStyle`, not by a second copy of the rule.
--
-- Asserts each cell's @bg@ is @HighlightNone@ or @HighlightNoneCursor@
-- (@Sdl.hs:691-692@) — an assertion that today can only fire with SDL
-- running and after this extraction fires under @cabal test@ too.
--
-- Cuts nothing: cutting is 'propFit''s business. That is what turns the
-- concatenation property below into an equality.
chunkPropLine :: Int          -- ^ row, for the even-row rule
              -> AttrString   -- ^ the line
              -> [PropChunk]

-- | @take (rwidth - col)@ after @uiToSquare@ (@Sdl.hs:628-629@); drops
-- runs left empty.
layOutSquare :: Int -> OverlaySpace -> [CellRun]

-- | @take (2 * rwidth - x)@, in half-cells (@Sdl.hs:596@), plus the mono
-- @bg@ assertion (@Sdl.hs:611-612@).
layOutMono :: Int -> OverlaySpace -> [CellRun]

-- | The half of the overrun cutoff that *is* expressible in logical units:
-- may a line starting at this 'PointUI' x be drawn at all? Exactly
-- equivalent to 'propCutoff' on the line's first chunk, because
-- @lmBoxSize == 2 * lmHalfSize@ and @lmHalfSize > 0@:
--
-- > xUI * halfSize >= (rwidth - 1) * boxSize  <=>  xUI >= 2 * (rwidth - 1)
propLineStartFits :: Int -> Int -> Bool
propLineStartFits width xUI = xUI < 2 * (width - 1)
```

**The measured half.** Two functions and an opaque pen. `PropCursor` is
constructible only by `startPropLine` and advanceable only by `propFit`, so
no consumer can present the cutoff rule with an x the module did not
compute.

```haskell
-- | Pen state for one proportional line, in device units.
data PropCursor = PropCursor
  { pcurXPx :: Int  -- ^ not exported
  , pcurRow :: Int
  }
  deriving (Show, Eq)

-- | The one place a logical x becomes a device x (@Sdl.hs:673@).
startPropLine :: LayoutMetrics -> PropLine -> PropCursor
startPropLine LayoutMetrics{lmHalfSize} PropLine{plStartX, plRow} =
  PropCursor (plStartX * lmHalfSize) plRow

-- | Has the pen reached the last column? Then this chunk and every later
-- chunk of the line is dropped — "for KISS, reject it" (@Sdl.hs:676-678@).
-- Measurement-free on purpose: @Sdl.hs@ tests it *before* rendering the
-- chunk, so a consumer that calls this first never rasterizes a chunk it
-- then throws away.
propCutoff :: LayoutMetrics -> PropCursor -> Bool
propCutoff LayoutMetrics{lmBoxSize, lmWidth} PropCursor{pcurXPx} =
  pcurXPx >= (lmWidth - 1) * lmBoxSize

-- | Where a measured chunk goes, how much of it survives, and whether the
-- cut raises the trimmed-line marker.
data PropFit = PropFit
  { pfXPx     :: Int            -- ^ device x to draw at (@Sdl.hs:708@)
  , pfWidthPx :: Int            -- ^ device width to draw *and to clip to*:
                                --   the whole chunk when it fits, else the
                                --   cut width. @Sdl.hs@ uses it as both the
                                --   source crop (@Sdl.hs:558-559@) and the
                                --   target width (@Sdl.hs:709@); canvas
                                --   @fillText@ has no width argument, so
                                --   the browser needs it as a clip rect.
  , pfMarker  :: Maybe CellRun  -- ^ draw it *now*, before the next chunk
  , pfNext    :: PropCursor
  }
  deriving (Show, Eq)

-- | Resolve one measured chunk: the three-way clamp of @Sdl.hs:551-554@
-- and the marker condition of @Sdl.hs:577@, which is exactly "the run was
-- cut and it was not blank".
--
-- The consumer supplies the natural rendered width in its own device
-- units, rounded *up*, so that "it fits" is never optimistic: @Sdl.hs@
-- passes the rendered surface's width (@Sdl.hs:549-550@, produced by the
-- @TTF.shaded@ call at @Sdl.hs:704-705@), the browser
-- @ceil (measureText t)@.
--
-- Precondition: @not (propCutoff lm cur)@, which is also what makes the
-- third branch positive — the guard gives @remaining > lmBoxSize@.
propFit :: LayoutMetrics -> PropCursor -> PropChunk -> Int -> PropFit
propFit lm@LayoutMetrics{lmBoxSize, lmWidth} cur PropChunk{pcAllSpace}
        widthRaw =
  assert (not (propCutoff lm cur) && widthRaw >= 0)
  $ PropFit { pfXPx = pcurXPx cur
            , pfWidthPx = width
            , pfMarker = if trimmed
                         then Just (trimmedMarkerRun lm (pcurRow cur))
                         else Nothing
            , pfNext = cur {pcurXPx = pcurXPx cur + width} }
 where
  remaining = lmWidth * lmBoxSize - pcurXPx cur          -- Sdl.hs:551
  width | widthRaw <= remaining = widthRaw               -- Sdl.hs:552
        | pcAllSpace = remaining                         -- Sdl.hs:553
        | otherwise = remaining - lmBoxSize              -- Sdl.hs:554
  trimmed = width /= widthRaw && not pcAllSpace          -- Sdl.hs:577

-- | The @$@ stamped into the last square column of a trimmed row
-- (@Sdl.hs:577-578@; @Color.hs:236-237@ defines it as a @BrBlack@ @'$'@),
-- expressed as an ordinary square-layer run so that no frontend needs
-- marker code of its own. Distinct from the engine-level @$@ that
-- @truncateAttrLine@ splices into an over-long line (@Frame.hs:200@):
-- that one counts logical cells before a frontend sees the string, this
-- one reacts to rendered width. Both use the same word; do not unify them.
trimmedMarkerRun :: LayoutMetrics -> Int -> CellRun
trimmedMarkerRun LayoutMetrics{lmWidth} row =
  CellRun { crLayer = LayerSquare
          , crRow = row
          , crCol = lmWidth - 1
          , crCells = [Color.trimmedLineAttrW32] }
```

**Two invariants the arithmetic gives for free**, worth asserting rather
than commenting. Both clamping branches leave the pen at or past the
cutoff: `pcAllSpace` yields `xPx + width == lmWidth * lmBoxSize`, and the
third branch yields exactly `(lmWidth - 1) * lmBoxSize`, which the `>=` in
`propCutoff` catches. So **a clamped chunk always ends its line**, hence at
most one marker per line and "draw `pfMarker` immediately" is bit-exact
with SDL. And since the precondition gives `remaining > lmBoxSize`, the
third branch's width is always positive: no zero-width blit.

**Where logical units end.** Everything `FrameLayout` emits is logical,
and so is everything on the wire; device space exists only inside
`PropCursor`, which never travels. The pen is where it must: after the
first chunk its x is a running sum of measured widths (`Sdl.hs:696-697`),
which is why the cutoff cannot be stated in logical units past the line
start. The one exactly-logical piece of it is `propLineStartFits` above.

**Rejected: a character-count cutoff.** Replacing the pixel rule with a
column count would make the module self-contained, and it does not work.
The engine already wraps proportional text by character count
(`splitAttrString`, `Overlay.hs:232-235`, reached for prop text through
`indentSplitAttrString`, `Overlay.hs:237-238`) and deliberately refuses to
model proportional width at all — `textSize PropFont` is an `error`
(`Overlay.hs:64-67`) and the wrap's own comment says the space width
"varies wildly" (`Overlay.hs:239-240`). A character-count cutoff would
therefore either duplicate that wrap and never fire, letting a wide-glyph
overrun paint past the screen edge, or fire on lines that fit and truncate
normal menu text. The pixel rule is the safety net for exactly the case the
character model cannot see; it stays, and with it the measured half.

**Refactor `Sdl.hs` to consume the module** — the proof the extraction is
faithful, and what de-risks everything downstream: if the native playtests
(`make test-medium`, `make frontendCrawl` for a visual look) pass with
`Sdl.hs` on the shared module, the browser consumer starts from
known-correct layout data. `drawPropOverlay`/`drawPropLine`/`drawPropChunk`
(`Sdl.hs:670-713`) and the two line cuts (`Sdl.hs:596`, `Sdl.hs:629`) go;
`scaleSurfaceToTextureProp` (`Sdl.hs:546-579`) loses its `x`, `row` and
`allSpace` parameters, its width arithmetic and its `setSquareChar` call,
shrinking to the crop-and-blit it is named for — which is the rule leaving
the texture helper, the place the plan flagged as easiest to lose it in.
What stays in `Sdl.hs`: font handles, `TTF.shaded`, the vertical crop and
centring (`Sdl.hs:555-562` — blit mechanics with no canvas counterpart,
declined deliberately rather than generalized), atlases, `SDL.copy`,
`chooseAndDrawHighlight`, and the per-cell glyph decisions that are 0.2's
business. Perf-gate the adoption as 0.2's is gated: before/after `make
bench` (`benchFrontendBattle`/`benchFrontendCrawl`, fixed seeds), since
`Sdl.hs`'s per-cell drawing is a hot path. Fix `Sdl.hs:590`'s `toEnum`
violation in the same commit if 0.2 has not already, the loop being touched
either way.

**How the marker travels.** It is a rule, not a datum, and it travels as
one. The rule is the single line `trimmed = width /= widthRaw && not
pcAllSpace` inside `propFit`, inseparable from the clamp that produced
`width` — which is why the plan's earlier "output type: chunks" could not
express it: the marker is a function of a measurement, so it can only be
decided when the measurement arrives, and it cannot be known at encode
time. On the wire, therefore, **it has no slot**; what travels is
`pcAllSpace`, one bit per chunk, and whichever `propFit` runs re-derives
the marker. In the consumer it appears as `pfMarker :: Maybe CellRun`, a
fully formed square-layer run at column `lmWidth - 1` — so a frontend draws
it with the same routine it draws every other `CellRun`, and a browser
implementer never has to learn it exists. Returning it from `propFit`
rather than appending it to `flSquare` also preserves the ordering
subtlety: `Sdl.hs` writes it from inside the proportional pass
(`Sdl.hs:577-578`, called from `Sdl.hs:706-707`), *before*
`drawSquareOverlay` runs at all (`Sdl.hs:731-732`), so a genuine
square-overlay cell in the last column still overwrites it. Its content
needs no special casing anywhere: `trimmedLineAttrW32` is a `BrBlack` `'$'`
(`Color.hs:236-237`), so `AltWhite` cannot apply to it and its
`HighlightNone` background takes `workaroundOverwriteHighlight`
(`Sdl.hs:515`) like any other unhighlighted cell.

**2.3's payload, specified here.** 2.3 owns the mechanism — an additive
`js_submitOverlays`, a packed `Word32` buffer passed by address, no string
marshalling, no serialization dependency — and points here for the payload,
so the field list has one definition. Four properties fix it:

1. **Three sections, in draw order**, behind a header of three counts —
   one per `FrameLayout` field. Making the sections structural is what lets
   a decoder obey the layer rule without knowing it, and it is the only
   shape that carries the two cell layers at all.
2. **Cell layers ship raw `AttrCharW32` words**: per run, layer tag, row,
   col, count, then the already-cut cells verbatim. `fontKind|colorIdx`
   plus codepoints would silently drop `bg`, which the square layer needs
   three times over (`Sdl.hs:644-646,657-659,669`); an `AttrCharW32`
   already packs char, fg and bg into one word, and `terminal-core.ts`
   decodes exactly that word for the map grid today. Lossless *and* less
   encoder work.
3. **The prop chunk header splits into three fields.** Layer is no longer
   part of it, the layers being separate sections, so the face is a
   one-bit choice between the two proportional fonts (`Sdl.hs:700-702`);
   the colour index is its own field; and `pcAllSpace` gets a bit, because
   `propFit` consumes it (`Sdl.hs:553`, `Sdl.hs:577`) and it is
   measurement-free, so the module owns it and the consumer needs it.
4. **`xStart` is logical**, and load-bearing rather than stylistic: it is
   logical precisely because the receiving consumer's `startPropLine` is
   what computes the device x. Nothing device-space is encoded.

The encoder still lives next to `OverlayLayout` and is still pure; tasty
still round-trips it on fixed cases and on QuickCheck-generated
`OverlaySpace` values; the 0.2 generator still emits decoder fixtures.

**The browser's share of the rule**, decided here because it decides what
this module exports: TS re-implements `propCutoff` and `propFit` — two
comparisons and a three-way `min`, with no string, colour or chunking
knowledge — pinned by 0.2-generated branch-complete fixtures, a table of
`(halfSize, width, cursorX, allSpace, widthRaw) -> (stop | x, width,
marker)` run by vitest in CI, exactly as `CellStyle`'s TS twin is pinned.
This keeps the boundary-cost ruling this section already makes against a
sync `js_measureText` crossing per chunk per frame, rather than arguing it
down. Recorded alternative, with
its trigger: batch instead — one crossing per frame carrying every chunk's
text out and every measured width back, then run the real `propFit` in
Haskell. It costs a round trip and a frame of latency, and it is worth
taking only if the twin ever needs to grow past pure integer arithmetic.
Today it does not, and the subtle knowledge — space inheritance, face
choice, the recolouring, the two cell cuts, the layer order — never crosses
under either option.

**Tests.** Nothing above touches SDL or a canvas, so
`test/OverlayLayoutUnitTests.hs` (the harness's `<Module>UnitTests.hs`
naming, wired into `test/Spec.hs`'s list beside the others) needs no
`SessionUIMock`; `chunkPropLine` is reached through the repo's
`EXPOSE_INTERNAL` idiom (`PointUI.hs:5-8`), and QuickCheck is already in
the suite (`test/SessionUIUnitTests.hs:10`).

Fixed cases, chunking: the empty line; one character; leading spaces before
a coloured run (the next-non-space inheritance); trailing spaces, the only
way an all-space chunk arises; a multi-space run *between* two
differently-coloured runs, which joins the left one — the case a hand-port
gets backwards; an all-space line, taking the `[] -> w` fallback
(`Sdl.hs:686`); a colour change with no space at the boundary; a line whose
spaces are *coloured*, which pins the deliberate difference between the
chunker's `(== spaceAttrW32)` (`Sdl.hs:680`; a default-attribute space,
`Color.hs:230-231`) and `pcAllSpace`'s `Char.isSpace` (`Sdl.hs:703`) —
unifying those two would silently change which lines get the marker; and
even against odd rows over a `White` cell, paired with the assertion that
both still choose `PropRegular`.

Fixed cases, placement — one row per branch of `Sdl.hs:552-554` and
`Sdl.hs:676-678`: the pen below the limit; the pen exactly at
`(lmWidth - 1) * lmBoxSize`, which the `>=` stops; `widthRaw` under
`remaining`; `widthRaw == remaining`, fitting exactly with no marker, the
case an off-by-one in a port flips; `widthRaw` over, not all-space (clamp
to `remaining - lmBoxSize`, marker fires); the same all-space (clamp to
`remaining`, *no* marker — the asymmetry that makes `pcAllSpace`
load-bearing); a clamped chunk followed by more chunks, asserting the tail
is dropped and the line ends; and `remaining == lmBoxSize + 1`, the
tightest case in which the third branch stays positive.

QuickCheck properties, over arbitrary `AttrString`s built from a legal fg
colour, a `HighlightNone`/`HighlightNoneCursor` bg and a printable char, so
that `Sdl.hs:691-692`'s assertion holds by construction:

- *chunking is total*: `concatMap (T.unpack . pcText) (chunkPropLine row
  al) == attrStringToString al` (`Overlay.hs:111-112`), for all rows and
  lines. Separating chunking from fitting is what strengthens the plan's
  earlier "concatenate back to the input minus the overrun cut" into a
  plain equality — the cut is now a different function's claim.
- *chunks are single-coloured under space inheritance*: splitting the input
  by the chunk lengths, every cell of a chunk's slice satisfies
  `fgFromW32 ac == pcColor c || ac == spaceAttrW32` (`Sdl.hs:688-689`).
- *no chunk is empty, and the lengths sum to the input's.* This is also the
  termination argument: `Sdl.hs:697` recurses on the remaining string, not
  on the pen, so a zero-advance measurement cannot loop — worth a property
  rather than a comment, since the naive reading of that recursion says
  otherwise.
- *the derived fields agree*: `pcFont c == propFontOf (pcColor c)` and
  `pcAllSpace c == T.all isSpace (pcText c)`, so neither can be supplied
  independently across the boundary.
- *placement stays on screen*: `pfWidthPx <= widthRaw`; `pfXPx +
  pfWidthPx <= lmWidth * lmBoxSize`; `isJust pfMarker` implies both
  `pfWidthPx < widthRaw` and `pfXPx + pfWidthPx <= (lmWidth - 1) *
  lmBoxSize`, i.e. the marker cell is left unpainted; and the pen is
  monotonic.
- *scale invariance*, the formal content of "not baked to SDL's pixel
  values": for all `k > 0`, `propFit (mkLayoutMetrics (k * h) w)` on a
  `k`-scaled cursor and a `k`-scaled `widthRaw` gives the `k`-scaling of
  `propFit (mkLayoutMetrics h w)` on the originals. The metric-free half
  is invariant by construction, taking no metrics.
- *logical and device cutoffs agree*: for all `w`, `h > 0` and `xUI >= 0`,
  `propLineStartFits w xUI` equals `not (propCutoff (mkLayoutMetrics h w)
  (startPropLine …))` on that line's first chunk.
- *the cuts are the module's*: `layOutSquare`/`layOutMono` emit no run
  longer than `width - crCol` / `2 * width - crCol`, and emit the input
  unchanged when it fits.
- *transport round trip*: `decode . encode == id` on generated
  `FrameLayout`s (2.3).

Non-vacuity, per the repo's rule: each of the six placement rows must be
shown to fail when its own line of `propFit` is perturbed — drop the
`- lmBoxSize`, flip the `not pcAllSpace`, weaken the `>=` to `>` — and the
proof recorded next to the table, since a table built on a wrong
`remaining` passes six ways at once. The refactor's own proof stays the
native one: `make test-medium`, a `make frontendCrawl` look, and before and
after `make bench`.

**Split** — four commits. (1) the module and its tests with no consumer:
`engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs`, both of `LambdaHack.cabal`'s module
lists and `test/Spec.hs`; the module goes in the library's
*unconditional* `exposed-modules`, not the `else` branch that carries
`Sdl.hs`, so `make test-wasm` compiles it too and the wasm consumer
inherits a module already proved to build there. (2) the `Sdl.hs`
refactor onto it, carrying `Sdl.hs:590`'s `toEnum` fix if 0.2 has not
already. (3) the citation repair of this plan and of the repo-root
`CLAUDE.md` against the shrunken `Sdl.hs` — mandatory and separate, per
the freeze callout, and unskippable because `tools/check-plan-citations.py`
proves a line exists and nothing more, so every surviving citation into
the deleted region re-points silently and the pass stays green. (4) the
outcome line naming (2)'s hash, the ledger flip, and the deletion of this
item's two `tools/doc-refs-allow.txt` entries. (3) before (4), so a
correction is never in the same diff as the landing record.

**Owns** — `engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs`, `test/Spec.hs`, `LambdaHack.cabal`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`docs/wasm-frontend-unified-plan.md`, `CLAUDE.md` and
`tools/doc-refs-allow.txt`. It does not write the wire encoder: that lands
in the same module file under 2.3, so the two items are never in flight
together. `Sdl.hs` has two other claimants — 0.2's `setSquareChar`/
`setMonoChar` rewrite, whose region abuts (2)'s, and 2.4's
`supportsMultiFont` export — so exactly one of the three holds the file at
a time; 0.2 first is preferable, since then (2) inherits the `toEnum` fix
instead of carrying it.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`test/OverlayLayoutUnitTests.hs`, `test/Spec.hs`), `wasm`, `docs`, plus `make
test-medium` and the `make bench` pair under `xvfb-run`.

**Hands back** — hands back nothing on display, as of 2026-07-31. `make
bench` (`Makefile:123`) includes `benchFrontendBattle`/`benchFrontendCrawl`
(`Makefile:109-110`, `Makefile:115-116`), which pass neither
`--frontendNull` nor `--frontendLazy` and so drive the real SDL renderer —
exactly the per-cell hot path (2) rewrites, and the reason the headless
`benchBattle`/`benchCrawl` cannot substitute: they never enter that path
at all. `xvfb-run` is installed now, so that pair runs here, and so does
the `make frontendCrawl` look, frame by frame through `xwd` — subject to
the two conditions a bare install does not supply, which are CLAUDE.md's
to state. One limit is not lifted: the headless renderer is llvmpipe, so a
before/after pair must be measured wholly under it and never against a
number from the real display, which is what (2)'s before/after script is
still for.

**Decide first** — three. (a) `layOutFrame`'s contract, which has a
signature and no body: `Sdl.hs` applies the start cutoff inside the
drawing recursion (`Sdl.hs:676-678`, re-entered per chunk at
`Sdl.hs:697`), so the transcription is genuinely two-way — either
`layOutFrame` filters with `propLineStartFits` and drops rejected lines,
or it emits every `PropLine` and each consumer applies `propCutoff` to the
first chunk. Left open, SDL and the browser diverge on precisely the case
the module exists to unify. (b) how `chunkPropLine` reaches its tests:
§2.1 routes it through `EXPOSE_INTERNAL` (`PointUI.hs:5-8`), while
`test/CLAUDE.md:15` rules that a name a unit test consumes must sit
*outside* that block, because `release` defaults to `False`
(`LambdaHack.cabal:83-85`, `LambdaHack.cabal:135-136`) and the idiom
therefore breaks the suite in the ordinary build. That default was `True`
when this was written, which left the breakage to a flag nobody passed;
flipping it forces the first branch below rather than merely favouring
it. Two repo
documents disagree; either correct §2.1's sentence to `Common/Kind.hs:15-16`'s
"internal and used in unit tests" group in commit (1), or record why the
block is acceptable here — but do not silently pick. (c) the name and
signature of the even-row `White`→`AltWhite` recolour `chunkPropLine`
consumes: 0.2 specifies `styleCell` as one whole-cell function, and this
module needs only the recolour, applied to a `Color.Color` and a row
before any chunking. 0.2 must name that finer export before either item
starts, or both invent one and §2.1's ban on a second copy of the rule is
broken by construction.

### 2.2 Browser overlay renderer, in isolation

A single absolutely-positioned `<canvas>` over the existing `<span>` grid
(the grid can't host variable-width text; canvas `fillText`+`measureText`
is the direct analogue of SDL2's measure-then-blit). New
`overlay-core.ts`/`overlay-core.test.ts` for the pure parts — the
`propCutoff`/`propFit` twin 2.1 specifies, which takes widths as plain
numbers, measurement staying in the canvas shell — pinned by 2.1's
0.2-generated branch table rather than by hand-written cases. The renderer
draws all three of 2.1's sections, not only the proportional one: a
`CellRun` is drawn at its layer's `cellPitch` with `CellStyle`'s per-cell
rules applied by the painter, in the prop, square, mono order 2.1 fixes.
Like 1.2's rasterizer, the renderer is a functional core
emitting draw commands executed by a thin canvas interpreter, so the
tests assert op lists — no canvas dependency.

Text drawn on canvas is not selectable or visible to screen readers; that
trade-off is accepted — the DOM grid keeps those affordances where they
come free, and no effort is spent recreating them for overlay text
(Appendix B).

Fonts, the G1 way — following the `rFontFiles = []`-under-`USE_BROWSER`
split: the Haskell side owns *which* fonts at *what size* (from `sfonts` /
the chosen fontset, sizes multiplied by `sallFontsScale` exactly as
`Sdl.hs:loadFontFile` does), the web side owns the *bytes* (static `.woff`
assets). Concretely: at startup `Wasm.hs` resolves the chosen fontset and
passes file names + effective px sizes to JS via a one-shot
`js_setupFonts` call; TS declares them with the `FontFace` API from
`./<filename>` URLs. **TS hardcodes nothing about fonts**; `make build-ts`
is extended to copy the referenced `GameDefinition/fonts/*.ttf.woff` into
the pages checkout (only `16x16xw.woff` is deployed today).

Known browser pitfalls to handle here, not discover in 2.4:

- **Measure only after fonts load.** `measureText` before the `FontFace`
  finishes loading silently measures a fallback font; await
  `FontFace.load()` (or `document.fonts.ready`) before the first overlay
  draw, and don't cache widths measured earlier.
- **devicePixelRatio.** Size the canvas backing store at `cssSize × dpr`
  and scale the context, or overlay text is blurry on any HiDPI screen.
- **Cell metrics from one source.** Derive the browser's `boxSize` (and
  `halfSize = boxSize / 2`) by measuring the actual rendered grid (a
  span's `getBoundingClientRect`), not by re-hardcoding `16px` — keeps
  zoom, `allFontsScale`, and any future font-size change coherent, and
  mirrors how `Sdl.hs` takes the cell size from the loaded font rather
  than from config.

Fixtures for the browser's `propCutoff`/`propFit` twin come from the 0.2
generator calling `OverlayLayout` — no hand-written fixtures. Chunking
itself never crosses, so there is nothing there to pin.

Verify: vitest green; live game pixel-identical to before (nothing wired).

**Split** — three code commits plus the record commit. (1) the font
wiring: `Wasm.hs`'s one-shot `js_setupFonts`, its `globalThis` hook in
`loader.ts`, the `FontFace` declarations in `terminal.ts`, and the
`Makefile` copy of the referenced `GameDefinition/fonts/*.ttf.woff` into
the pages checkout. It depends on nothing else in Phase 2 — `sfonts` and
`schosenFontset` are already populated in the browser build — so it lands
first and unblocks 1.5, which names it as its dependency. (2)
`ts-src/src/overlay-core.ts`'s `propCutoff`/`propFit` twin and its vitest
table; §2.1 gives both functions as complete Haskell bodies, so it can be
*written* before 2.1 lands, but it may only *land* once 0.2's generator
emits the branch-complete table, hand-written fixtures being forbidden
here. (3) the canvas shell: the absolutely-positioned `<canvas>`, the
devicePixelRatio-sized backing store, the grid-box measurement, and the
draw-command core all three of 2.1's sections pass through. (4) the
outcome line naming (3)'s hash, the ledger flip, and the deletion of
`overlay-core.ts` and `overlay-core.test.ts` from
`tools/doc-refs-allow.txt`.

**Owns** — `ts-src/src/overlay-core.ts`, `ts-src/src/overlay-core.test.ts`,
`ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`, `Makefile`,
`GameDefinition/index.html` (only the `#screen` positioning context the
canvas overlays), `docs/wasm-frontend-unified-plan.md` and
`tools/doc-refs-allow.txt`. Sub-commits (2) and (3) are not concurrent —
they share the two new `overlay-core` files — and neither is (1) with (3),
which share `terminal.ts`. `Wasm.hs` is written by 2.3 and 2.4 as well, so
one item holds it at a time. The generator extension that emits (2)'s
branch table is 0.2's stanza and 0.2's file, not this item's: schedule it
with 0.2's owner rather than editing the generator here, or two sessions
edit one executable.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`), `ts`, `wasm`,
`deploy`, plus `test -f ../lambdahack.github.io/DejaVuLGCSans-Bold.ttf.woff`
&& `test -f ../lambdahack.github.io/Hack-Bold.ttf.woff`.
Run it unsandboxed: the build-ts step writes into the sibling pages
checkout. The two file tests are the deployment gate and are non-vacuous
today — neither font is in that checkout, which is the whole font
deployment gap this item closes.

**Hands back** — *browser*: that the declared `FontFace`s actually load
and that the canvas sits pixel-aligned over the span grid. §2.2's own
"live game pixel-identical to before" needs no human — `getFontSetup`
still gates on `"sdl"` (`MonadClientUI.hs:329`), so the overlays are
provably `[]` and the vitest case asserting zero draw calls on an empty
payload decides it, which is why the substitute gate in Done is `make
test-ts` plus the two font-deployment tests rather than a look. The HiDPI
and browser-zoom checks are deferred to 2.5 by the plan itself, not by
this block.

**Decide first** — three. (a) the pre-Phase-2 frame-timing baseline. Phase
2's intro requires it captured *before* 2.2 lands and re-measured at 2.5,
and no item owns capturing it; the moment (1) or (3) merges, the
left-hand side is unrecoverable and 2.5 finds the comparison has no other
half. Decide who captures it, with which probe — R5's harness if it exists
by then, else a temporary `performance.now()` one — and where the number
is written down, since recording it in this plan is an edit to a live
section. (b) the browser's argument to `mkLayoutMetrics`, which
reconstructs `lmBoxSize = 2 * halfSize`: a measured grid box that is odd
or fractional — the normal case under devicePixelRatio scaling, not the
corner case — makes `lmBoxSize` disagree with the grid's actual pitch.
Round the box down to even, round the half, or reject and fall back? (c)
`js_setupFonts`'s marshalling. A font *name* list is unavoidably strings,
which is the one thing 2.3's payload design exists to avoid, so the
one-shot call needs a stated encoding before it is written rather than an
invented one per session.

### 2.3 Transport: overlays across the JSFFI boundary

`Wasm.hs`'s `display` currently drops three of `SingleFrame`'s four fields
(`Wasm.hs:79`; `OverlaySpace = [(PointUI, AttrString)]`, `Frame.hs:100`).
Add an **additive** `js_submitOverlays` alongside `js_submitFrame`, using
the same idiom the frame already uses: a packed `Word32` buffer passed by
address — everything is numeric and fits `Word32`, no string marshalling,
no new serialization dependency. The payload's shape is specified once, in
2.1, under "2.3's payload, specified here": three sections in draw
order behind a header of counts, cell layers shipping raw `AttrCharW32`
words, the prop chunk header split into face, colour index and the
`pcAllSpace` bit, and `xStart` logical because the receiving consumer's
`startPropLine` is what turns it into a device x. Restating it here is the
second definition G1 forbids, and the field list is exactly what drifts.
The encoder lives next to `OverlayLayout` and is pure;
tasty round-trips it on fixed cases and on QuickCheck-generated arbitrary
`OverlaySpace` values, and the 0.2 generator emits encode fixtures the TS
decoder is tested against. All three overlay kinds (prop/square/mono)
travel and draw through the same path, on the same canvas, in the 2.1
layer order.

Because `getFontSetup` still gates on `"sdl"`, the overlays are provably
`[]` in the live game — this step is a visual no-op, verified by playing.
The real-data path is exercised by tests instead: a tasty case constructs
a `SingleFrame` with non-empty overlays and checks the encoding; a vitest
case asserts the 2.2 renderer makes zero draw calls on an empty overlay
payload (the live game's case until 2.4); and the `run-wasm-test.mjs`
battery (0.3) drives `js_submitOverlays` end-to-end.

**Split** — three commits. (1) the pure encoder and its tasty round trip,
both inside 2.1's module and test files — no FFI, no TS, so the wire
layout is frozen and reviewable on its own. (2) the transport: the
additive `js_submitOverlays`, `Wasm.hs`'s `display` filling the buffer,
the TS decoder in `ts-src/src/overlay-core.ts`, its `globalThis` hook in
`loader.ts`, the hand-off in `terminal.ts`, and the `run-wasm-test.mjs`
coverage case — one commit, because 0.3's rule is that a new `foreign
import/export` lands with its coverage case in the same diff. (3) the
outcome line naming (2)'s hash and the ledger flip; this item has no
`tools/doc-refs-allow.txt` entries, 2.1's and 2.2's having gone with their
own items.

**Owns** — `engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`ts-src/src/overlay-core.ts`, `ts-src/src/overlay-core.test.ts`,
`ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`ts-src/run-wasm-test.mjs` and `docs/wasm-frontend-unified-plan.md`. It
overlaps 2.1 on the module file, 2.2 on all four TS files and 2.4 on
`Wasm.hs`, so it runs alone: this is the strict chain's one real join.
The 0.2 generator emitting the decoder's encode fixtures is 0.2's file,
not this item's — schedule it there, as 2.2 does for its branch table.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`test/OverlayLayoutUnitTests.hs`), `ts`, `wasm`, `deploy`. Run it unsandboxed:
the build-ts step writes into the sibling
pages checkout.

**Hands back** — hands back nothing. §2.3's "verified by playing" argues
its own case away in the next sentence: `getFontSetup` still gates on
`"sdl"` (`MonadClientUI.hs:329`), the overlays are provably `[]`, and
playing proves nothing the zero-draw-call vitest case and the
`run-wasm-test.mjs` end-to-end case do not. Phase 2's browser budget is
spent at 2.4 instead.

**Decide first** — three. (a) the bit-level wire layout. §2.1 fixes the
payload's four structural properties and no more: no field widths, no
packing order inside a `Word32`, no statement of whether the header's
three counts are element counts or word counts. An encoder session and a
decoder session each invent one and only integration reveals the
mismatch, so freeze it here as a haddock table on the encoder and pin the
TS side with generator-emitted fixtures rather than a second reading of
that table. (b) whether the Haskell `decode` that §2.1's `decode . encode
== id` property needs is a shipped export or a test-only helper — the
answer changes the module's export list and reopens 2.1's
`EXPOSE_INTERNAL` question, so it is 2.1's ruling to extend, not this
item's to invent. (c) whether "the encoder lives next to `OverlayLayout`"
means the same module file, which is how this block reads it and what its
Owns assumes; a *sibling* module instead needs a
`tools/doc-refs-allow.txt` entry added before the name is written
anywhere, or `tools/check-doc-refs.py` fails on it.

### 2.4 Flip the switch — as a capability, not a name list

Each frontend module exports a constant `supportsMultiFont :: Bool`
(`Sdl.hs`, `Wasm.hs`: `True`; ANSI/Teletype: `False`; `Dom.hs` is a dead
example file no configuration compiles, R3), and `Frontend.hs` dispatches
it exactly like `frontendName` already dispatches per-frontend values
(`Frontend.hs:186-196`, including the null/lazy/teletype/ANSI option
guards). A name list like `frontendName soptions `elem` ["sdl", "wasm"]`
would be a drift-prone string list of the same shape G1 fights. Then
`getFontSetup` (`MonadClientUI.hs:329`) becomes:

```haskell
multiFont = Frontend.supportsMultiFont soptions
            && not (T.null (fontPropRegular chosenFontset))
```

Re-check `test/MonadClientUIUnitTests.hs`'s `getFontSetup works in stub`
after the flip. It is platform-independent today, and stays so only as
long as the stub pins a frontend: `stubClientOptions` sets
`sfrontendNull = True` (`test/UnitTestHelpers.hs:207`), which is the
*first* guard in `frontendName`, so it never reaches
`Chosen.frontendName` and `multiFont` is `False` under `cabal test` and
`make test-wasm` alike. Add a case that pins `supportsMultiFont` itself,
so the flip is covered rather than merely not broken.

Small diff, large blast radius: this is the step that changes what players
see and it touches shared engine code — the one to review hardest, and to
re-run the native playtest battery on (`make test-medium` at minimum).

**Split** — three commits, and the split is the point. (1) the capability
mechanism, behaviour-preserving: `supportsMultiFont` exported by `Sdl.hs`
(`True`), `Wasm.hs` (`False`, for now), `ANSI.hs` and `Teletype.hs`
(`False`), the `Frontend.hs` dispatch mirroring `frontendName`'s
CPP-guarded guard chain (`Frontend.hs:186-196`), `getFontSetup` reading it
(`MonadClientUI.hs:329`), and the new `MonadClientUIUnitTests.hs` case
pinning it. This is provably a no-op: today's gate is `frontendName
soptions == "sdl"`, already `False` for wasm, ANSI, Teletype, null and
lazy alike, so a dispatch true only for `Sdl.hs` reproduces it exactly —
which is why (1) may land at any time, even before 2.1. (2)
`Wasm.supportsMultiFont = True`, one line and nothing else. (3) the
outcome line naming (2)'s hash and the ledger flip; this item has no
`tools/doc-refs-allow.txt` entries.

**Owns** — `engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/ANSI.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Teletype.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs`,
`engine-src/Game/LambdaHack/Client/UI/MonadClientUI.hs`,
`test/MonadClientUIUnitTests.hs` and
`docs/wasm-frontend-unified-plan.md`. `Dom.hs` is deliberately not here: a
dead example file no configuration compiles, R3. Two files are shared —
`Sdl.hs` with 2.1 and 0.2, `Wasm.hs` with 2.2 and 2.3 — and although (1)
touches only their export lists, they do not merge, so serialize.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/ANSI.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Teletype.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs`,
`engine-src/Game/LambdaHack/Client/UI/MonadClientUI.hs`,
`test/MonadClientUIUnitTests.hs`), `ts`, `wasm`, `deploy`, plus `make
test-medium`. Run it unsandboxed: the build-ts step writes into the sibling
pages
checkout.

**Hands back** — *browser*: commit (2) is the moment players see something
different, and it is the whole human-review budget of Phase 2 concentrated
into a one-line diff — menus, message log, help and item descriptions
rendering in the proportional fonts, and the mono-overwrites-prop overrun
behaving. That concentration is what the split buys: the mechanism is
reviewed in (1) with no behaviour change, the risk in (2) with no
mechanism left to re-read. In Done instead: `cabal test` pins the
capability itself through the new `MonadClientUIUnitTests.hs` case — which
is what stops the stub's `sfrontendNull = True` from making the flip
merely not-broken rather than covered — and `make test-medium` is the
native playtest battery §2.4 asks for.

**Decide first** — nothing.

### 2.5 Post-flip QA checklist

- Side-by-side visual comparison with SDL2: menus, message log, help,
  item descriptions; mono-overwrites-prop overrun behavior.
- Screenshot rasterizer (1.2) extended to draw the overlay layers (R6).
- Banner text updated again (1.4): proportional fonts come off the "use
  the native binary for…" list.
- HiDPI and browser-zoom spot checks (the 2.2 pitfalls, exercised for
  real).
- Frame-timing re-measurement against the pre-Phase-2 baseline (Phase 2
  intro; R5's harness).

**Split** — three commits around one human checklist. (1) R6 closure: the
single shared TS drawing entry point that the live overlay renderer and
1.2's screenshot rasterizer both call, plus the vitest case rendering a
fixture frame with overlays through both and comparing draw-command lists
rather than pixels. (2) the banner update, 1.4's recurring rule applied
once more — proportional fonts come off `GameDefinition/index.html:63-65`'s
"use the native binary for…" list. (3) after the human checklist is signed
off, the outcome line naming (1)'s hash and the ledger flip. Nothing in
`tools/doc-refs-allow.txt`: 2.1's and 2.2's four entries went with their
own items, and any survivor here is the gate telling you a proposed
artifact was never built.

**Owns** — `ts-src/src/overlay-core.ts`, `ts-src/src/overlay-core.test.ts`,
the `*-core.ts` 1.2 creates for its screenshot draw-command core — 1.2
fixes that name and this item must not invent it, so 2.5 cannot start
before 1.2 has landed — `GameDefinition/index.html` and
`docs/wasm-frontend-unified-plan.md`. Nothing else. The two `overlay-core`
files belong to 2.2 and 2.3 as well, but both sit behind 2.4 in the chain,
so there is no window in which two items hold them.

**Done** — `ts`, `wasm`, `deploy`, `docs`, plus `! grep -qF 'proportional
fonts' GameDefinition/index.html`. Run it
unsandboxed: the build-ts step writes into the sibling pages checkout.
The banner grep is non-vacuous today — the phrase is still on the page —
so it fails until (2) lands.

**Hands back** — *judgement*: four of the five checklist items are
irreducibly a human's. The side-by-side visual comparison with SDL2
(menus, message log, help, item descriptions, and the mono-overwrites-prop
overrun), the HiDPI and browser-zoom spot checks 2.2's pitfalls defer
here, and the frame-timing re-measurement — which additionally needs a
left-hand side no item owns; see 2.2's Decide first. What is in Done is
the fifth and the R6 case: the banner grep, and the draw-command
comparison that is the one checklist line with a machine verdict and the
one that would otherwise regress in silence, screenshots quietly going
map-only.

**Decide first** — one. Whether the pre-Phase-2 frame-timing baseline
exists at all. If 2.2 landed without capturing it, the re-measurement has
no other half, and this item must either strike that checklist line with a
recorded reason or re-derive a baseline from a build at the pre-2.2
commit — not quietly measure against nothing and call the number a
comparison.

---

## Phase 3 — port the Node benchmark targets from GHCJS to WASM

The Makefile still carries `nodeBenchCrawl`/`nodeBenchBattle`/`nodeBench`/
`nodeMinifiedBench` (Makefile:133-143), but they invoke the GHCJS
`.jsexe/all.js`, which the Makefile can no longer build — they are dead as
written. What they did matters: headless AI-vs-AI benchmarks of the
browser-targeted build under Node (`--frontendNull --benchmark
--stopAfterFrames N --automateAll ...`, same flag sets as the
`nativeBench*` targets, sharing `RNGOPTS`), with `nodeMinifiedBench`
additionally benchmarking the *deployed* artifact from
`../lambdahack.github.io` rather than the build tree. This phase
repurposes those targets for the wasm build, restoring the browser half of
the native-vs-browser benchmark comparison.

Independent of Phases 1–2; only 3.1 touches the engine, and it's the same
mechanism R4 needs anyway.

### 3.1 Argv into the reactor: `lhStart` reads WASI args

`lhStart` (`GameDefinition/Main.hs:82-89`) parses an empty list where
`main` parses real argv. Change it to parse `getArgs`: GHC's wasm RTS gets
argv from WASI `args_get`, which every host shim controls — the browser
loader passes `["LambdaHack"]` (`loader.ts:56`), so `getArgs` returns `[]`
there and browser behavior is provably unchanged, while a Node harness can
pass real flags. Update the stale "there is no argv in the browser"
comment to describe the new contract (argv comes from the WASI host; the
browser host passes none). **Spike first**: confirm with a one-liner that
`getArgs` in reactor mode does surface Node-WASI-supplied args — the one
assumption here not yet validated against the toolchain. This entry point
is what R4 later feeds from URL parameters, so 3.1 is shared
infrastructure, not bench-only plumbing.

**Split** — two commits, then a third that is not part of the landing.
First `GameDefinition/Main.hs`: `getArgs` in place of the literal `[]`,
and the comment rewritten to the new contract. Second the landing —
outcome line plus the ledger row. Third, separate because the landing
falsified them, the corrections this item's own text needs: re-range the
`GameDefinition/Main.hs:82-89` citation and retire "the one assumption
here not yet validated against the toolchain", which the spike has since
answered.

**Owns** — `GameDefinition/Main.hs` and this document. Deliberately not
`CLAUDE.md`: its "there is no argv and no config file on disk" stays true
after the change, since the loader still passes the one-element argv
`["LambdaHack"]` (`loader.ts:56`), and touching it would put another work
stream's file under this item's lock for nothing.

**Done** — `native`, `wasm`, `docs`, plus `cabal build exe:LambdaHack` &&
`diff -q <(stylish-haskell GameDefinition/Main.hs) GameDefinition/Main.hs`.

**Hands back** — *browser*: one `make serve-wasm` page load showing the
game still starts on default options, no in-session gate running `lhStart`
under a browser WASI shim. The substitute in Done is `make build-wasm`,
and it is the load-bearing half: `lhStart` sits inside `#ifdef USE_WASM`,
so `cabal build` and `cabal test` compile none of it and a native-only
done-check would verify nothing — `hlint .` does see it, `.hlint.yaml`
passing `--cpp-simple`. The argv claim hands back nothing: the spike runs
in a session, and 3.2's driver turns it into a standing check.

**Decide first** — nothing. The spike is already answered: a reactor
linked with this repo's flags and driven through `node:wasi` reports
`getArgs=["--newGame","1",...]` with `getProgName` holding argv[0]
separately, so `getArgs` needs no argv[0] drop of its own and the
browser's one-element argv yields `[]`, exactly as the item predicts.

### 3.2 A Node driver for the game reactor

`run-wasm-test.mjs` runs WASI *commands* (`wasi.start`, the test binary);
the game exe is a *reactor* (`_initialize` + exported `lhStart`). Extend
it (or add a sibling `run-wasm-game.mjs` sharing the setup code) to: pass
`[args...]` as WASI argv as it already does, `wasi.initialize(instance)`,
then call and await `lhStart()`. Two integration points to cover:

- **`WasmFile.hs` reaches for `globalThis.localStorage` and
  `globalThis.LZString` unconditionally** — neither exists under Node.
  Provide a tiny in-memory `localStorage` stub and load `lz-string` (as a
  `ts-src` devDependency rather than reaching into the sibling checkout).
  With `--frontendNull`, `Wasm.hs`'s frontend (and thus `lhSubmitFrame`
  etc.) never starts, so no display stubs are needed — don't add any
  silently.
- **Exit propagation:** `make` must see failures. Await the `lhStart()`
  promise; a rejection (uncaught Haskell exception crossing the async
  export) sets a nonzero `process.exitCode`. Check how
  `--stopAfterFrames` termination surfaces (normal return vs
  `ExitSuccess`-shaped rejection) during the 3.1 spike and handle both.
- **Measurement for free:** the driver reports peak `WebAssembly.Memory`
  size at exit — the wasm counterpart of `benchMemoryAnim`'s `+RTS -s`,
  and memory was precisely the GHCJS era's pain point (the old
  `GHCJS_GC_INTERVAL` knobs existed for a reason) — and the localStorage
  stub times and reports `setItem` durations and payload sizes, so R1's
  save-lag re-measurement falls out of any nodeBench run.

**Split** — three commits. First the correction Decide first (b) rules
on, because a session implementing the bullet as written builds a wrong
driver. Then the driver and its package wiring. Then the landing: outcome
line, ledger row, and the `run-wasm-game.mjs` deletion from
`tools/doc-refs-allow.txt` — 3.2 is one of the gated items, so that
deletion is what entitles it to an outcome line, and it belongs in the
same commit as the line.

**Owns** — `ts-src/run-wasm-game.mjs`, `ts-src/package.json`,
`ts-src/package-lock.json`, `tools/doc-refs-allow.txt` and this document;
`ts-src/run-wasm-test.mjs` as well under the extend branch of Decide first
(a), where no new file appears and the allowlist entry goes together with
the sentence naming it. It writes no display stub: with `--frontendNull`
the frontend never starts, so a `globalThis.lhSubmitFrame is not a
function` TypeError means the reactor being driven predates 3.1 and
dropped the flag, not that a stub is missing. Nothing runs concurrently
with anything here — one new file, one lock file that does not merge, one
allowlist line.

**Done** — `ts`, `wasm`, `docs`, plus `. ~/.ghc-wasm/env` && `T=$(mktemp -d)`
&& `W=$(wasm32-wasi-cabal list-bin exe:LambdaHack)` &&
`~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$W" --output
"$T/ghc_wasm_jsffi.mjs"` && `node ts-src/run-wasm-game.mjs "$W"
"$T/ghc_wasm_jsffi.mjs" --newGame 1 --gameMode crawl --noAnim --maxFps 100000
--frontendNull --benchmark --stopAfterFrames 200 --automateAll
--keepAutomated` && `! node ts-src/run-wasm-game.mjs "$W"
"$T/ghc_wasm_jsffi.mjs" --stopAfterFrames notanumber`.

**Hands back** — hands back nothing. Every integration point in the item
is an exit status, the negated second run being the non-vacuity control
that proves the driver's success rule can still fail. *Judgement* survives
only in the third bullet — whether the reported peak `WebAssembly.Memory`
and `setItem` timings measure what R1 wants — and that is R1's gate, not
this one's; 3.2 owes the numbers, not their reading.

**Decide first** — three. (a) Sibling file or extension of
`run-wasm-test.mjs`: the item leaves it open and it is not cosmetic —
`run-wasm-game.mjs` has an entry in `tools/doc-refs-allow.txt`, so a
sibling deletes it when the file appears while an extension deletes it
together with the sentence that proposed it. (b) The exit-propagation
bullet is wrong as written. Measured on this toolchain, a Haskell `error`,
`exitWith (ExitFailure 3)` and `exitWith ExitSuccess` all surface as the
same rejected promise carrying a `WebAssembly.RuntimeError` whose message
is the shown exception, so the class carries nothing and the success rule
has to read "resolved, or rejected with the message `ExitSuccess`"; and a
fault raised in the RTS scheduler loop on a later tick escapes the awaited
promise entirely, arriving as an uncaught exception, so
`process.on('uncaughtException')` is needed too — Node exiting 1 there is
luck, not the mechanism the bullet names. A live item's wrong claim is an
error to fix, so the default is to correct the bullet in its own commit
before the driver; rule otherwise if the correction should ride with the
landing. (c) Whether `lz-string` is added now. The probe reached
`WasmFile.hs`'s startup on an empty store without ever touching
`globalThis.LZString`, so only a run that *saves* needs it — R1's
measurement, not the benchmark, which leaves through
`ReqUIGameDropAndExit`. Adding it also needs the npm registry, whose
reachability is unproven in a session and which a populated
`ts-src/node_modules` hides until `npm i` runs.

### 3.3 Repurpose the Makefile targets

- `nodeBenchCrawl` / `nodeBenchBattle`: same flags and
  `RNGOPTS` as today (mirroring
  `nativeBenchCrawl`/`nativeBenchBattle`), but invoking the 3.2 driver on
  `wasm32-wasi-cabal list-bin exe:LambdaHack` plus post-linked glue (the
  `test-wasm` target shows the exact `post-link.mjs` recipe,
  Makefile:313-320). `nodeBench` stays the aggregate of both.
- `nodeMinifiedBench` → rename to `nodeDeployedBench` (there is no
  "minified" wasm; the honest name is "the deployed artifact"): run the
  same two benchmarks against `../lambdahack.github.io/LambdaHack.wasm` +
  its `ghc_wasm_jsffi.mjs`, exactly the role the minified `all.js` played
  — and a coherence check that the two deployed files match each other,
  for free. If a `wasm-opt` step is ever added to `build-ts`, this target
  is what validates it.

**Verify:** both benchmarks run to completion under Node with plausible
frame counts; compare against `nativeBench` on the same machine for a
first real wasm-vs-native data point (feeding R5). A crash or hang must
fail the target (3.2's exit propagation). Once stable, a short
`--stopAfterFrames` variant is a natural cheap addition to R2's CI job —
it exercises the whole engine headless under wasm, which nothing else in
CI does.

**Split** — three commits. The `Makefile` rewrite first: both
`nodeBench*` targets onto the 3.2 driver, and `nodeMinifiedBench` renamed.
Then, separate because the rewrite falsified them, the claims elsewhere
that these targets are dead — the `nodeBench*` sentence in
`.claude/skills/playtests/SKILL.md`, and `docs/leader-desync-migration.md`
if the block's line count moved. Last the landing: outcome line, ledger
row, and the `make nodeDeployedBench` deletion from
`tools/doc-refs-allow.txt`.

**Owns** — `Makefile`, `.claude/skills/playtests/SKILL.md`,
`tools/doc-refs-allow.txt` and this document, plus
`docs/leader-desync-migration.md` conditionally. That last is the fleet
hazard: it cites, in §02 step 3, `Makefile:146-148` — `test:`, a blank line,
`test-gha:` — three lines below the block this item rewrites, so any
change in that block's line count slides the citation onto other lines
while it still *resolves*, leaving `tools/check-plan-citations.py` green
over a document that has started to lie. The `Makefile` is also 0.2's
(`make gen-ts`) and R2's, so 3.3 holds it alone.

**Done** — `wasm`, `docs`, plus `make nodeBench` && `make nodeDeployedBench`
&& `grep -q nodeDeployedBench .claude/skills/playtests/SKILL.md` && `! grep -q
nodeMinifiedBench Makefile`.

**Hands back** — *judgement*: "plausible frame counts" and the
wasm-vs-native ratio are readings, not exit statuses, and the two halves
are harvested asymmetrically — the native binary redirects its own stdout
to `~/.LambdaHack/stdout.txt` whenever stdout is not a terminal
(`GameDefinition/Main.hs`, under `#ifndef USE_BROWSER`), overwritten on
every launch, while the wasm run is exempt from that `#ifdef` and prints
to the terminal, so a session comparing the two reads one from a file and
one from stdout, or silently compares nothing. Done gates the half that is
mechanical: both benchmarks run to completion, and a crash or hang fails
the target.

**Decide first** — three. (a) Which Node. `node` on `PATH` is v18.19.1
and `~/.ghc-wasm/env`'s is v26.4.0; `node:wasi` is experimental and
version-sensitive, `make test-wasm` sources the env and `make test-ts`
does not. The new targets must name one, or two sessions reach two
verdicts on one driver. (b) The deployed half cannot pass until
`../lambdahack.github.io/LambdaHack.wasm` is rebuilt from a post-3.1 tree
— the artifact deployed today drops `--frontendNull` and dies in the
frontend. Either the executing session runs `make build-ts` itself, which
needs unsandboxed Bash and dirties the sibling checkout's working tree, or
that redeploy is the author's and the session runs Done after it; Done is
the same line either way. (c) The flags, and what the numbers mean.
`--dbgMsgSer --logPriority 4` are inherited verbatim from the GHCJS lines,
and the browser build additionally hardwires `sdumpInitRngs = True`
(`TieKnot.hs:140`), so every wasm run prepends a seed dump the native side
does not emit — keep the debug flags or drop them. And the development
`cabal.project.local` this machine builds with carries
`+with_expensive_assertions`, which reaches the wasm build as much as the
native one, so every number 3.3 produces is an assertions-on number:
symmetric, hence honest for the ratio, but the item should say so rather
than leave a session to re-run the comparison with assertions off.

---

## Related goals

**R1 — Save robustness.** The banner's "savefiles are prone to corruption
when the browser is closed mid-save" caveat is the last banner claim with
no plan behind it. Grounding (from `WasmFile.hs`): each file maps to one
localStorage key written by a single synchronous `setItem` of the
lz-string-compressed payload with an `"OK"` EOF marker — a *single* key is
already effectively atomic, and truncation is detected on read. The real
residual risk is **cross-key consistency**: a game save writes several
files (server + per-faction clients), and a tab killed mid-sequence leaves
a mixed-generation set of individually-valid saves. Fix shape: write each
save cycle to staging keys, then commit with a single generation-pointer
key flip; readers resolve through the pointer. Test plan (runnable under
`make test-wasm` with 3.2's localStorage stub): simulate an interrupted
cycle — staging keys written, pointer not flipped — and assert the reader
serves the previous generation intact; plus an
`encodeEOF`/`strictDecodeEOF` round-trip in the wasm environment.

Two engine-side facts shape this goal:

- **Periodic autosave is disabled entirely in browser builds** —
  `Server/LoopM.hs:335-342` (not to be confused with `Client/LoopM.hs`)
  skips the periodic `writeSaveAll` under
  `USE_JSFILE`/`USE_WASMFILE`, with the comment *"Saving on the browser
  causes a huge lag"*. That measurement is GHCJS-era; re-measure under
  wasm as soon as 3.2 exists — its instrumented localStorage stub reports
  `setItem` durations from any nodeBench run. If it's now acceptable,
  re-enabling periodic saves shrinks the loss window far more than any
  atomicity work; if it's still slow, a save triggered from JS
  `pagehide`/`visibilitychange` is the alternative (needs investigation
  of what the server monad allows mid-turn).
- The exit path already contains a browser-specific mitigation to be
  preserved (or obsoleted knowingly): `WatchUpdAtomicM.hs:585-592` waits
  2s at `UpdKillExit` because *"some browsers seem to trash Local Storage
  when page reloaded or closed … while they still internally finish the
  saving in the background"*.

No SDL2 analogue (SDL's own window-close path deliberately exits without
a fresh save — `Sdl.hs:475-484`), but this is the browser-build equivalent
of "your progress is safe", which is what parity is *for*.

**Split** — three commits. R1a re-measures the browser save lag under wasm
from 3.2's instrumented stub and records the number here, editing this
document alone. R1b is the staging-key/generation-pointer change in
`WasmFile.hs` plus its interrupted-cycle test, and carries the
`test/WasmFileUnitTests.hs` deletion from `tools/doc-refs-allow.txt`. R1c
acts on R1a's number — periodic autosave re-enabled, or a save driven from
`pagehide`/`visibilitychange` — and carries the outcome line.

**Owns** — `engine-src/Game/LambdaHack/Common/WasmFile.hs`, including the
header's "Mirrors JSFile.hs … exactly in storage format" claim, which
R1b's layout change falsifies and which R3 does not touch; the new
`test/WasmFileUnitTests.hs` (CPP-guarded to an empty group natively, the
way `test/Spec.hs` already guards its SDL cases), `test/Spec.hs`, and the
test-suite `other-modules` list in `LambdaHack.cabal`;
`Server/LoopM.hs:336-342` and `WatchUpdAtomicM.hs:585-595`. It does not
write `ts-src/run-wasm-test.mjs` — the localStorage stub is 3.2's, and a
stub behaviour the test needs lands there first, as a 3.2 change. R1c is
not concurrent with R3 or with capability constants, all three rewriting
those same two `#if` sites; the `LambdaHack.cabal` edit is not concurrent
with 0.1, 0.2, 2.1, 2.2 or R3.

**Done** — `native` (stylish over `$(git`, `ls-files`, `'*.hs')`), `wasm`,
`docs`, plus `git diff --exit-code`.

**Hands back** — *judgement*: whether R1a's measured `setItem` cost is low
enough to re-enable periodic saving, which is R1c's entire branch and has
no threshold anywhere in this document; and a real tab kill mid-cycle,
which the Node stub can only simulate. The substitute gate in Done is the
interrupted-cycle case under `make test-wasm` — staging keys written,
pointer not flipped, the previous generation still served intact — plus
the `encodeEOF`/`strictDecodeEOF` round-trip in the wasm environment.

**Decide first** — three. Where the generation-pointer flip lives:
`WasmFile.hs` cannot know which write ends a cycle, since `writeSaveAll`
(`Server/CommonM.hs:254`) drives the server save and every client save, so
either a File-layer commit hook is called from `writeSaveAll`, or the
pointer is per file and there is no cycle to commit. What becomes of saves
already sitting in players' localStorage under today's flat `path`-as-key
layout — read as generation zero, or abandoned. And whether R1c is the
autosave re-enable or the `pagehide` save, which R1a's number decides and
which the item flags as needing investigation of what the server monad
allows mid-turn.

**R2 — Browser-and-frontend CI.** None of Phase 0's drift protection fires
unless CI runs it. Partly landed, in
`.github/workflows/lint-and-test-suites.yml` (the hand-written workflow — do
not touch the generated haskell-ci one): a `test-wasm` job that installs
ghc-wasm-meta and runs `make test-wasm`, and `make test-ts` split off as
its own Node-only job, so TS regressions fail in seconds. Still to add,
each when it exists: `make build-wasm`, the 0.3 FFI-coverage battery and
the 0.2 generated-file freshness check. Still to do regardless: cache
`~/.ghc-wasm` and the wasm cabal store, the toolchain being the expensive
part and bootstrapped from scratch on every run today. The
frontend-CI-matrix practice (below) widens this with xvfb SDL and pty
ANSI smokes.

**Completeness requirement: everything runs in CI.** What runs there today
is CLAUDE.md's to state, not this document's; a third copy of that
inventory is what drifted. The doctest gap
is closed the second of the two ways weighed here — a job following
CLAUDE.md's recipe — because that recipe is known to work here, a run
per library with `--with-repl=doctest`; haskell-ci's own doctest support
was not evaluated against it. And the
standing rule from the ground rules applies to everything this plan
adds: the vitest suites (including the jsdom forwarding tests),
`make test-wasm` (including the FFI battery and the RawFrontend contract
harness), the generated-file freshness check, the determinism goldens
(native under `cabal test`, cross-backend under `make test-wasm`), and
the frontend smokes — each in CI from the commit that introduces it.

**Split** — one commit per job, and the row never flips: R2 is a standing
item, so no commit carries an outcome line and none has
`tools/doc-refs-allow.txt` entries to delete. Two jobs are ready today —
caching `~/.ghc-wasm` and the wasm cabal store, and a `make build-wasm`
job; the rest land with the artifact each runs: 0.2's generated-file
freshness check, 0.3's FFI battery, a short `nodeBench` smoke after 3.3,
and the practice's `xvfb` SDL and pty ANSI smokes.

**Owns** — `.github/workflows/lint-and-test-suites.yml`, plus the
`Makefile` where a job needs a target of its own (the `--stopAfterFrames`
variant of `benchFrontendBattle` the xvfb smoke drives). Never
`.github/workflows/haskell-ci.yml`, which `haskell-ci regenerate` owns.
The jobs are not concurrent with each other: they share one YAML file,
which does not merge.

**Done** — `wasm`, plus `python3 -c "import yaml;
yaml.safe_load(open('.github/workflows/lint-and-test-suites.yml'))"` — the
parse, plus the job's own payload run locally, the
second half substituted per commit.

**Hands back** — *judgement*: whether the job is green on a GHA runner,
and whether the toolchain cache pays for itself, which takes two
authorized pushes to see — one cold, one warm — and which a session may
not arrange for itself. There is no substitute gate, a workflow edit
having no local exit status beyond parsing; Done runs the payload and
nothing more, and CI status is read afterwards via `curl -s` against
`api.github.com`, as the standing checks describe.

**Decide first** — which job this commit adds; R2 is a track, not a unit,
and hands out one job at a time. Separately, before any `make build-ts`
job is contemplated: whether CI gets a parameterized output directory, a
throwaway destination, or no such job at all — the target hardcodes
`../lambdahack.github.io` and `cd ~/r/LambdaHack`, and chains with `;`
rather than `&&`, so its exit status does not report a failed post-link or
copy and it cannot serve as a gate as written.

**R3 — Retire GHCJS support (one browser target is enough).** The
original GHCJS target is unbuildable, permanently: this codebase requires
GHC 9.10+ and the standalone GHCJS compiler died at GHC 8.10. A port to
GHC's in-tree JavaScript backend was investigated and found feasible at
1.5–3 weeks, but is **not happening** (rationale in B.8; Appendix A is
kept as the resurrection manual). Instead, once WASM reaches parity
(after 2.5), in one commit:

- `Dom.hs` and `JSFile.hs` **stay in the tree as examples** of an
  alternative frontend/file-backend pair, each gaining a prominent header
  comment marking it as dead, bitrotten, GHCJS-only code that no
  configuration compiles. (They remain the historical origin of the web
  stack's logic, which is why this plan cites them.)
- **Everything else GHCJS goes**: the `impl(ghcjs)` cabal conditionals
  (`ghcjs-dom`/`ghcjs-base` deps and the exposed/other-module lines,
  `LambdaHack.cabal:148-152` and `LambdaHack.cabal:380-400`), the
  `ghcjs-options` knobs (`GHCJS_GC_INTERVAL`, `GHCJS_BUSY_YIELD`,
  `-dedupe`, `GHCJS_BROWSER`, `LambdaHack.cabal:165-173`) and the fifth,
  `-DREMOVE_TELETYPE` (`LambdaHack.cabal:152`) — which unlike the other
  four has source consumers, `Frontend.hs:86` and `Frontend.hs:190`'s
  `#ifndef REMOVE_TELETYPE` guards, permanently true once no
  `ghcjs-options` line can define the macro, so those go too. Then the
  `supportNodeJS` flag entirely, it having no non-GHCJS use — including
  the tracked `cabal.project.local.js`, whose payload is
  `flags: -supportNodeJS`. Then `Frontend.hs:43-44`'s
  `USE_GHCJS` import branch, `File.hs`'s `USE_JSFILE` branch,
  `TieKnot.hs:114-118`'s GHC.Compact escape hatch, and the `USE_JSFILE`
  halves of the browser conditions in
  `Server/LoopM.hs`/`WatchUpdAtomicM.hs`/`HandleHumanLocalM.hs` and
  `TieKnot.hs:138` (the `sdumpInitRngs` hardwiring plus main-thread
  workaround skip, a second `TieKnot.hs` site distinct from the
  GHC.Compact one above) — or of
  their capability-constant successors, if that practice lands first.
  Update CLAUDE.md's GHCJS mentions in the same commit. The README needs
  no GHCJS edit — it contains the string nowhere (checked repo-wide); what
  it carries is browser-era prose (`README.md:79-81`) that R1 and R5 own,
  not this rip-out.

Timed after parity, not before, so the rip-out doesn't tangle with
Phase 0–2 diffs touching the same cabal stanzas and CPP sites.

**Owns** — `LambdaHack.cabal` (the `supportNodeJS` flag, the `impl(ghcjs)`
`cpp-options` stanza and the comment above it, `-DREMOVE_TELETYPE`, the
`ghcjs-options` knob block, and the two `impl(ghcjs)` module/dependency
branches), `cabal.project.local.js` (deleted), `Frontend.hs` (the
`USE_GHCJS` import branch and both `#ifndef REMOVE_TELETYPE` guards),
`File.hs`'s `USE_JSFILE` branch, `TieKnot.hs` (both sites), the
`USE_JSFILE` halves at `Server/LoopM.hs:336`,
`HandleHumanLocalM.hs:815` and `WatchUpdAtomicM.hs:586`, header comments
only in `Dom.hs` and `JSFile.hs`, and `CLAUDE.md` and this document. It
must hold `LambdaHack.cabal` exclusively — 0.1, 0.2, 2.1 and 2.2 all add
`exposed-modules` there and 2.2 extends the very `os(wasi)` block R3
collapses — and the three engine `#if` sites are also R1's and the
capability-constants practice's, whichever lands first shaping R3's diff.

**Done** — `native` (stylish over `$(git`, `ls-files`, `'*.hs')`), `wasm`,
`docs`, plus `make test-gha` && `git diff --exit-code` && `! git grep -q
'USE_GHCJS\|USE_JSFILE\|REMOVE_TELETYPE\|supportNodeJS\|ghcjs-options' --
':!*.md' ':!LambdaHack.cabal.bkp' ':!LambdaHack.cabal.flattened'
':!engine-src/Game/LambdaHack/Client/UI/Frontend/Dom.hs'
':!engine-src/Game/LambdaHack/Common/JSFile.hs'`. The
grep half is non-vacuous: run against today's tree it finds matches and
fails the line, so it can only pass after the rip-out.

**Hands back** — *browser*: one page load after `make build-ts` and
`make serve-wasm`, per the ground rules — a formality here, since every
branch deleted is compiled by no supported configuration, so native and
wasm object code are unchanged by construction. The substitute gate in
Done is `make test-gha`, which drives `--frontendTeletype` through whole
games and so covers the only removal with live source consumers,
`-DREMOVE_TELETYPE`. The `CLAUDE.md` rewording is authored text and gets a
human read rather than a mechanical edit.

**Decide first** — four. (a) The README clause: `README.md` contains no
occurrence of "GHCJS" at all, so either the clause is dropped or a
specific browser-era sentence is named — `README.md:107-108`'s
Chrome/Local-Storage line is the only close candidate. (b) Whether
`Dom.hs` and `JSFile.hs` stay in the sdist: dropping the `impl(ghcjs)`
stanzas removes them from every `exposed-modules`/`other-modules` list, so
they leave the tarball unless `extra-source-files` gains them; `hlint` and
stylish keep covering them either way, both quantifying over tracked
`.hs` files. (c) The GHCJS mentions R3 does not claim and nothing else
does: `Makefile:133-143`'s `nodeBench*` targets (3.3's) and
`.claude/skills/playtests/SKILL.md`'s description of them as dead GHCJS
remnants, `GameDefinition/Main.hs:38`'s comment (still true of wasm),
`Point.hs:32`'s "not supported yet by GHCJS", and the `impl(ghcjs)`
stanzas in the two tracked cabal archives `CLAUDE.md` calls kept verbatim.
(d) Whether the citation renumbering rides this commit: deleting
`Frontend.hs:43-44` and the two guard pairs shifts later lines by two,
four or six, and nine citations here point into that file —
`Frontend.hs:148`, `159-183`, `186-196` and `84-92` in live items,
`41-48` and `93` inside the frozen appendices, where renumbering is
neither the drift the freeze rule protects nor a correction.

**R4 — URL-parameter options.** Server/client options sit at defaults in
the browser for lack of argv. After 3.1, `lhStart` parses whatever WASI
argv the host supplies — so the whole feature reduces to the *loader*
translating an allowlist of URL query parameters (e.g. `?fontset=`,
`?allFontsScale=` — the player-facing half of 1.5 — `?benchmark=`, debug
flags) into the args array it already passes to the WASI shim
(`loader.ts:56`), reusing the real options parser with no engine-side
parsing code at all. This gives browser users what argv gives native ones
with no new UI surface — an address-bar knob. `?fontset=` only becomes
meaningful once 2.2's font wiring exists, and non-default fontsets need
their `.woff` files deployed too — deploy all six once rather than
special-casing the default set.

**Owns** — the new `ts-src/src/url-options-core.ts` and
`ts-src/src/url-options-core.test.ts`, `ts-src/src/loader.ts` (the
`new WASI(["LambdaHack"], [], fds)` argv construction at `loader.ts:56`),
`GameDefinition/index.html` if the knobs get on-page documentation, and
this document. Not concurrent with R5: R5's `?benchmark` mode is one of
this allowlist's own entries and edits the same `loader.ts` call. Not
concurrent with 1.3 or 1.4 for `index.html`.

**Done** — `ts`, `docs`.

**Hands back** — *browser*: that a parameter typed into the address bar
actually reaches the engine. No headless path exists — 3.2's Node driver
hands argv to the WASI shim directly and never loads `loader.ts` — so
end-to-end confirmation is one served page per parameter. The substitute
gate in Done is the vitest case over `url-options-core.ts`: query string
in, args array out, which is everything between the URL and `loader.ts:56`.

**Decide first** — three, all player-visible. Which debug options are
exposed from an address bar (`LambdaHack --help` lists all of them, and
most are not knobs a player should hold). The failure policy for an
unknown, duplicated or malformed parameter — ignore silently,
`console.warn`, or refuse to start — sharpened rather than softened by the
item's own selling point, since handing the string to the real options
parser makes a bad URL abort `lhStart` rather than degrade. And whether
the loader validates values at all or delegates every one of them.

**R5 — Performance pass (exploratory, after Phases 2 and 3).** The banner
says the game "runs rather slowly in the browser". Phase 3's `nodeBench*`
targets supply the game-logic half of the measurement (headless wasm vs
`nativeBench` on the same machine, same seeds); the remaining half is
browser-side rendering, which nothing in Phase 3 touches
(`--frontendNull` never starts the wasm frontend). Its instrument: after
R4 + 3.1, a `?benchmark` URL mode running the same `--automateAll`
AI-vs-AI game with the real DOM frontend — the browser analogue of
`benchFrontendCrawl` — while `terminal.ts` collects `submitFrame`-to-paint
timings (rAF timestamps) and reports count/mean/p95 at game end. Used
three times: the pre-Phase-2 baseline, the 2.5 re-measurement, and
whatever R5 then decides to chase. No committed scope beyond that —
measure first; the frame path itself (one buffer address per frame) is
already about as cheap as the boundary allows.

**Split** — R5a is the instrument: the `?benchmark` URL mode plus the rAF
frame-timing collector, its pure half in a `*-core.ts` under vitest, and
it carries the deletion of its two `tools/doc-refs-allow.txt` entries.
R5b is the three measurement runs the item names, each landing as a
recorded number in this document and nothing else; its third run carries
the outcome line. R5c is whatever the numbers justify chasing, which the
item deliberately leaves uncommitted — not schedulable, and no commit
here.

**Owns** — the new `ts-src/src/frame-timing-core.ts` and
`ts-src/src/frame-timing-core.test.ts`, `ts-src/src/terminal.ts` (the
`submitFrame`-to-paint instrumentation), `ts-src/src/loader.ts` (the
`?benchmark` entry), and this document; R5b writes this document alone.
Not concurrent with R4 (`loader.ts`, and `?benchmark=` sits in R4's
parameter allowlist) nor with 2.2 (`terminal.ts`).

**Done** — `ts`, `docs`, which decides
R5a only.

**Hands back** — *browser*: the whole rendering half, R5b, and no
substitute exists anywhere in this plan. Phase 3's `nodeBench*` targets
run `--frontendNull`, so `Wasm.hs`'s frontend never starts and no frame is
ever painted; the three runs are `make build-ts` (unsandboxed) plus
`make serve-wasm` and a human at the page, three times. What Done does
gate is the collector's arithmetic — count/mean/p95 over a fixed
timestamp list — and beside it sits the game-logic half, `make nativeBench`
against `make nodeBench` on one machine with one seed set, which a session
can run but not judge, and which must not be parallelized across agents.

**Decide first** — three. Whether §2's pre-2.2 baseline uses R5a or the
sanctioned temporary `performance.now()` probe: the first pulls R5a ahead
of R4 and 3.1 onto Phase 2's critical path, the second leaves R5 where the
ledger puts it and throws the probe away at 2.5. Who takes the three
browser measurements, given that no session can. And what result is worth
chasing — "runs rather slowly" has no threshold here, and R5c exists only
if someone sets one.

**R6 — Screenshot/overlay coherence.** Every renderer of "the current
screen" (the live grid, the 1.2 screenshot rasterizer, any future export)
must consume the same four `SingleFrame` fields after Phase 2. Enforced by
putting the shared drawing entry point in one TS function both paths call,
and by a vitest case rendering a fixture frame with overlays through both —
comparing draw-command lists (per 1.2/2.2's functional-core structure),
not pixels.

**Owns** — the new `ts-src/src/render-coherence.test.ts` and 2.5's
checklist line here, and nothing else — *if* the shared drawing entry
point is a stated acceptance criterion of 1.2 and 2.2. The op-list
representation R6 compares is defined by 1.2 and reused by 2.2, and R6
states no shape of its own; where that criterion is not pinned in
advance, R6 degrades into a two-file refactor of `ts-src/src/terminal.ts`
and `ts-src/src/overlay-core.ts`, code neither of whose owners it is, and
must then hold both exclusively. Decide first settles which of the two
Owns lists is the real one.

**Done** — `ts`, `docs`.

**Hands back** — nothing. The comparison is over draw-command lists, not
pixels, so no canvas, no fonts and no browser are involved; that a real
`Ctrl+P` PNG matches a real screen is 1.2's residue, and that the overlays
look right is 2.5's.

**Decide first** — two, both to be answered before 1.2 starts rather than
after. Whether "both renderers reach the four `SingleFrame` fields
(`Frame.hs:93-97`) through one shared TS entry point" is handed to the 1.2
and 2.2 agents as an acceptance criterion — the branch that keeps R6 a
small test — or left for R6 to impose afterwards. And which module holds
that entry point: 2.2's `overlay-core.ts`, or a module of its own that 1.2
creates and 2.2 imports. R6 cannot pick it after the fact without moving
1.2's code.

## Multi-frontend practices (adopted)

G1 is one practice among several that make a codebase with N frontends
cheap to extend. These are adopted; the rejected companions are in
Appendix B.

**Capability constants instead of behavioral CPP.** Module *selection* by
CPP is fine (`File.hs:9-15`, `Frontend.hs:41-48`); behavioral forks
scattered around the engine are not: the autosave skip (`Server/LoopM.hs:336`),
the history-dump key omission (`HandleHumanLocalM.hs:815`), and the 2s
exit flush wait (`WatchUpdAtomicM.hs:586`) are each an `#if` over the same
two macros, whose *reason* lives in a comment far from the backend it
describes — and not even in the same *sense*: the first two are
`#if !defined(USE_JSFILE) && !defined(USE_WASMFILE)`, guarding the native
behaviour, while the exit wait is the positive
`#if defined(USE_JSFILE) || defined(USE_WASMFILE)`, browser 2s in the
`#if` branch and native 200ms in the `#else`. That inversion is exactly
what makes a mechanical conversion dangerous and a named constant safer.
Let the storage backend
module itself export named constants (e.g. `savesCauseLag :: Bool`,
`filesAreDumpable :: Bool`, `needsExitFlushDelay :: Bool`) re-exported
through `File.hs`, and let the engine branch on values, not macros. A
fourth backend then answers named questions instead of hoping every
negated macro conjunction got updated; R1's re-measurement flips one
constant instead of editing ifdefs. (Server code consuming a File-layer
constant respects the client-server split — it's the storage backend's
property, not the frontend's.)

**Split** — two commits, exports before uses: first the File-layer modules
gain the constants (`Common/HSFile.hs`, `Common/WasmFile.hs`,
`Common/JSFile.hs`, re-exported through `Common/File.hs`, whose selection
`#ifdef` at `File.hs:9-15` stays untouched), then the three engine
consumers branch on values and lose their `#if`s. The second commit
carries the outcome line; the item sits in no `tools/doc-refs-allow.txt`
gate, so it deletes nothing there.

**Owns** — `Common/File.hs`, `Common/HSFile.hs`, `Common/WasmFile.hs`,
`Common/JSFile.hs`, `Server/LoopM.hs`, `Client/UI/HandleHumanLocalM.hs`,
`Client/UI/Watch/WatchUpdAtomicM.hs`. The two commits are strictly
ordered, not concurrent — the second does not compile without the first.
Nothing else may hold these files meanwhile: R1's whole point is flipping
two of the constants (`Server/LoopM.hs:336`, `WatchUpdAtomicM.hs:586`)
and R3 deletes their `USE_JSFILE` halves, so both follow this item rather
than overlapping it.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Common/{File,HSFile,WasmFile,JSFile}.hs`,
`engine-src/Game/LambdaHack/Server/LoopM.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs`,
`engine-src/Game/LambdaHack/Client/UI/Watch/WatchUpdAtomicM.hs`), `wasm`,
`docs`, plus `! git grep -lE 'USE_JSFILE|USE_WASMFILE' --
engine-src/Game/LambdaHack/Server/LoopM.hs engine-src/Game/LambdaHack/Client`.
That `git grep` names three files
today, which is what makes its later silence evidence rather than a
vacuous search.

**Hands back** — *browser*: nothing in-session enters the page-unload
path, so that `needsExitFlushDelay` still buys `localStorage` its 2s on a
real tab close is unverified here. The substitute gate in Done is
`make test-wasm`, which proves only that the wasm build compiles, links
and runs its suite with the constants in place; the number itself is
re-examined by R1, not by this item.

**Decide first** — three, none of them an agent's to settle. (1) Whether
the two `GameDefinition/game-src/TieKnot.hs` sites join: `TieKnot.hs:114`
is an availability question (`GHC.Compact` absent under GHCJS) that
cannot become a runtime `Bool`, while `TieKnot.hs:138` (`sdumpInitRngs`
plus the main-thread-workaround skip) is behavioral and could. Branches:
in scope → a fourth constant, and `TieKnot.hs` joins **Owns**; out of
scope → the item says so, and R3 removes them. (2) Whether
`Common/JSFile.hs` gets the constants at all — no configuration compiles
it, so whatever is written there is checked by nothing. Branches: mirror
them for symmetry and let R3 delete them, or leave the module alone and
accept that `File.hs`'s `USE_JSFILE` arm stops compiling even in
principle. (3) The polarity and the layering. The three names are given
as "e.g.", and two of the three sites guard the *native* behaviour
through a negated conjunction, so the choice between a browser-true
constant read under `unless` and a native-true counterpart is open — the
doc names the inversion as the hazard without ruling on the sense. With
it goes the objection the doc pre-empts only for the client-server split:
`needsExitFlushDelay` is a page-lifecycle property exported from a
storage backend.

**Sum-typed frontend selection.**
`sfrontendANSI`/`sfrontendTeletype`/`sfrontendNull`/`sfrontendLazy` are
four independent `Bool`s (`ClientOptions.hs:62-68`) whose simultaneous
truth is resolved by guard order in two places (`Frontend.hs:84-92` and
`Frontend.hs:186-196`). One sum-typed field (`FrontendDefault | FrontendANSI |
FrontendTeletype | FrontendNull | FrontendLazy`) makes the guard chains
total cases and turns conflicting flags into a parse error. Mechanical,
moderate churn (options parser, UnitTestHelpers' stub options). Do before
R4, so URL parameters parse into the sum type, not the Bools.

**Owns** — `Common/ClientOptions.hs` (the field), `Server/Commandline.hs`
(the four parsers at `Commandline.hs:316-334` and their bindings at
`:82-85`), `Client/UI/Frontend.hs` (both guard chains),
`Client/UI/DrawM.hs` and `Client/UI/MonadClientUI.hs` — the two
`frontendName soptions ==` string compares the sum type subsumes
(`DrawM.hs:597`, `MonadClientUI.hs:329`), which this item's churn list
omits and which `git grep 'frontendName soptions =='` shows are the only
two in the tree — and `test/UnitTestHelpers.hs`, whose `:207` is the sole
fixture setting an `sfrontend*` field. Serialize against 2.4 and R3
rather than running beside them: 2.4 rewrites `MonadClientUI.hs:329` and
dispatches through `Frontend.hs:186-196`, and R3 deletes the
`#ifndef REMOVE_TELETYPE` guards at `Frontend.hs:86` and `:190` — the
exact lines this item turns into cases.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Common/ClientOptions.hs`,
`engine-src/Game/LambdaHack/Server/Commandline.hs`,
`engine-src/Game/LambdaHack/Client/UI/{Frontend,DrawM,MonadClientUI}.hs`,
`test/UnitTestHelpers.hs`), `wasm`, plus `make test-short` && `! git grep -q
'frontendName soptions ==' -- '*.hs'` && `LH=$(cabal list-bin exe:LambdaHack)`
&& `! "$LH" --frontendNull --frontendTeletype --newGame 1 --gameMode dig
--benchmark --stopAfterFrames 1 --automateAll`. Both
negated checks pass *today* and so are non-vacuous: the grep finds two
sites, and the double `--frontend` invocation exits 0 because guard order
silently picks null. `make test-short` plays whole games — minutes, not
seconds.

**Hands back** — *judgement*: `--help` text and error wording change when
four independent `switch`es become alternatives of `flag'`, and nothing
reads either. The substitute gates in Done are the nonzero exit on
`--frontendNull --frontendTeletype` and `make test-short`, which drives
`--frontendTeletype` end to end through the rewritten parser.

**Decide first** — (1) the save-format ruling. `ClientOptions` derives
`Generic` with `instance Binary ClientOptions`
(`ClientOptions.hs:79-81`), is written into every client save
(`Client/State.hs:193` and `:210`) and is carried by `UpdRestart`
(`CmdAtomic.hs:130`), so four `Bool`s becoming one constructor changes
the wire format — while `compatibleVersion` compares only the first three
version-branch components (`Save.hs:142-143`) and the package is
`0.11.0.1`, so a bump to `0.11.0.2` still *accepts and misdecodes* old
saves; only `0.11.1.0` rejects them. Branches: bump the third component
in this commit, or declare the breakage acceptable and record that in the
item. (2) Constructor availability under CPP. Both chains are themselves
pruned — `Frontend.hs:86`/`:190` under `#ifndef REMOVE_TELETYPE`,
`Frontend.hs:89`/`:193` under `#ifndef USE_BROWSER` — while
`Server/Commandline.hs` carries no build CPP and offers all four
`--frontend*` switches in every build, so under `USE_BROWSER` an
`--frontendANSI` value parses with no branch to take. Branches: five
constructors always, the browser build falling through to
`FrontendDefault`; five always, erroring at startup on an unavailable
one; or a CPP-pruned type, which makes the `Binary` and `Show` instances
differ per build and so feeds back into (1). (3) Whether "conflicting
flags become a parse error" may reshape the parser: four `switch`es
cannot express exclusion, so it takes an `<|>` of `flag'`s, which
rewrites `--help`.

**The RawFrontend contract, written down and tested.** The engine-side
protocol every frontend must fit is real but implicit: the `fshowNow`
handshake's four numbered steps are split between `display`
(`Frontend.hs:148`) and `frameTimeoutThread` (`Frontend.hs:159-183`) with
a third participant in `saveKMP` (`Common.hs:80`); `fdisplay` semantics
legitimately differ per frontend (SDL blocks until drawn via its
frame-queue handshake, wasm snapshots-and-returns with painting deferred
to rAF, Dom schedules a rAF callback, lazy does nothing) — yet nothing
states which properties are *required* and which are incidental; the
threading rules (SDL needs the bound main thread via
`startupBound`/`workaroundOnMainThreadMVar`, wasm is a single-threaded
reactor) are likewise folklore the wasm port had to reverse-engineer.
Deliverables: a haddock contract on `RawFrontend`'s fields; a tasty
contract-test harness run against `nullStartup`/`lazyStartup`/`Teletype`
natively and, via `make test-wasm`, against the real `Wasm.hs` (key
delivered while a frame is pending; Esc reset; shutdown with a frame
queued; `FrontPressed` semantics); and a "how to add a frontend"
checklist. The input-side cases land together with 0.1 — they guard
exactly what it rewires in `Wasm.hs`; the rest pairs with 0.3.

**Split** — three commits. The haddock contract on `RawFrontend`'s fields
(`Frontend/Common.hs:24-31`) first, since it is what the harness then
asserts against; the native harness second, driving
`nullStartup`/`lazyStartup`/`Teletype.startup`; the wasm case third, and
not before 0.3's baseline battery, because `ts-src/run-wasm-test.mjs`
supplies no `globalThis.lhSubmitFrame` today — 73 lines wiring WASI and
`ghc_wasm_jsffi` and nothing else (`run-wasm-test.mjs:34-67`) — so the
first foreign call out of `Wasm.hs` traps. The third commit carries the
outcome line and the `tools/doc-refs-allow.txt` deletion.

**Owns** — `Client/UI/Frontend/Common.hs` (haddock),
`Client/UI/Frontend.hs` (export list only), the new
`test/FrontendContractUnitTests.hs`, `test/Spec.hs`, the `test-suite
test` stanza of `LambdaHack.cabal` (`LambdaHack.cabal:481-515`),
`ts-src/run-wasm-test.mjs` and one job in
`.github/workflows/lint-and-test-suites.yml`. Two shared-file caveats.
The input-side cases are specified to land inside 0.1's commit, so
whichever of the two lands first creates the test module and the other
extends it — never concurrently. The cabal stanza's `other-modules` list
is also the determinism goldens' only edit point, and
`ts-src/run-wasm-test.mjs` is extended by 0.3 and by 3.2 as well: one
holder at a time for each.

**Done** — `native` (stylish over
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Common.hs`,
`test/FrontendContractUnitTests.hs`), `wasm`, `docs`, plus `cabal test
--test-options='-p "/fe-invariant/"'`. That used to carry a second
build, `cabal build --builddir=dist-norelease --flags=-release test`,
as the only thing catching a harness leaning on the `EXPOSE_INTERNAL`
block; `release` now defaults `False`, so the ordinary build is that
build and the clause would only re-state the default.

**Hands back** — *display*: SDL2 is the frontend whose folklore the
contract most needs to bind — the bound main thread via
`startupBound`/`workaroundOnMainThreadMVar`, the frame-queue handshake —
and no in-session run enters its event loop, so SDL is covered by the
haddock and by review alone. The substitute gate in Done is the harness
itself, run against null, lazy and Teletype natively and against the real
`Wasm.hs` under `make test-wasm`.

**Decide first** — (1) the export gating. `nullStartup`, `lazyStartup`,
`display` and `frameTimeoutThread` leave `Frontend.hs` only inside
`#ifdef EXPOSE_INTERNAL` (`Frontend.hs:9-13`), which `flag(release)`
defines (`LambdaHack.cabal:83-86`, `:135-136`) and which defaults `False`
— so a harness driving them does not compile at all, which is the trap
`test/CLAUDE.md` records for `emptyUnknownTile` (`Common/Kind.hs:16`) in
the form it takes now that the default no longer hides it until someone
passes `-release`. Branches: hoist the four names into the
unconditional export list, following that precedent, and say so in the
item; or drive only `chanFrontendIO`, which starts a whole frontend chain
and cannot isolate the `fshowNow` handshake. Whether the hoist is this
item's diff or a separate one is part of the same call. (2) The module
layout under CPP: `Sdl.hs`/`ANSI.hs` and `Wasm.hs` are never co-exposed
(`LambdaHack.cabal:375-391`), so one test module cannot import the set
the item names — branches: one module with an `#ifdef USE_WASM` split
over a shared table of cases, or two modules sharing that table. (3) The
checklist's home — haddock in `Frontend/Common.hs`, a new file under
`docs/`, or `README.md`; the plan owns frontend planning, so the guess is
not an agent's to make.

**Determinism goldens — native first, then cross-backend.** The tasty
suite already compiles and runs under both native (`cabal test`) and wasm
(`make test-wasm`), and `test/SessionUIMock.hs` already unwinds key macros
through the real `HandleHumanM` machinery. Add golden tests — fixed seed,
scripted keys, assert a committed final-state digest — in two stages.
The native-only harness lands **before 2.1**: it guards the two most
dangerous shared-code changes in the plan (2.1's `Sdl.hs` refactor and
2.4's flip) regardless of wasm. Then the same goldens run under
`make test-wasm`, routine in CI since R2's first jobs landed — the only
kind of test that catches native-vs-wasm *behavioral* drift (FFI-adjacent
paths, numeric assumptions); per-frontend unit tests can't. Medium
effort.

**Split** — two commits, as the item's own staging says: the native
harness, which must precede 2.1, then the wasm stage, which only adds the
same goldens to `make test-wasm` and to R2's existing job. The second
carries the outcome line and the `tools/doc-refs-allow.txt` deletion.

**Owns** — the new `test/DeterminismGoldenUnitTests.hs`, `test/Spec.hs`,
the `test-suite test` stanza of `LambdaHack.cabal`
(`LambdaHack.cabal:481-515`), and whatever fixtures the digest needs in
`test/UnitTestHelpers.hs` and `test/SessionUIMock.hs`. Not concurrent
with the RawFrontend contract harness: both add a module to the same
`other-modules` list and both extend `test/Spec.hs`. If the digest is
over committed literals rather than a file, it owns no data file; if over
a file, that file joins this list rather than living beside the test.

**Done** — `native` (stylish over `test/DeterminismGoldenUnitTests.hs`,
`test/UnitTestHelpers.hs`, `test/SessionUIMock.hs`, `test/Spec.hs`), `wasm`,
`docs`, plus `cabal test --test-options='-p "/golden/"'`.

**Hands back** — *judgement*: whether the digest is actually sensitive to
what 2.1 and 2.4 change is the entire value of the item, and no run
answers it — a green golden that guards nothing looks exactly like a
green golden that guards everything. The substitute gate is the
non-vacuity proof the standing checks demand of any new self-checking
assertion: perturb one drawing or layout input, re-run `cabal test
--test-options='-p "/golden/"'`, expect failure, and record the
perturbation next to the test.

**Decide first** — (1) what the digest covers. The stated purpose is
guarding 2.1's `OverlayLayout` extraction and 2.4's font flip, neither of
which a *game-state* digest can see, while "final-state digest" reads as
precisely that. Branches: digest the rendered frames and overlays;
digest final client and server state, which guards macro and command
semantics only; or two golden families, one each. (2) Which harness.
`test/` offers `CliMock` over a 3x3 stub board (`UnitTestHelpers.hs:630`,
with `scriptedFchanFrontend` at `:142` and `scriptedCliState` at `:588`),
not a seeded whole game; the seeded-game shape is the `--frontendNull
--benchmark --stopAfterFrames` one the Makefile bench targets use,
through `tieKnot`, which the suite already depends on. (3) The digest
mechanism. The `test-suite test` stanza depends on neither `tasty-golden`
nor `hashable`, and any dependency added must also resolve under
`wasm32-wasi-cabal` for the second commit — branches: a hand-rolled fold
over `encode` bytes with no new dependency, or a new dependency; and
committed literals versus a committed file, files being usable under the
wasm run since `run-wasm-test.mjs:50` preopens `/`.

**A CI smoke for every shipped frontend.** No frontend's event loop runs
in CI at all. Teletype comes closest — the `make test-gha` playtests
drive `--frontendTeletype` through whole games. `make test-wasm` compiles
and links `Wasm.hs` but runs the suite with `--frontendNull`, so the wasm
frontend is never entered, and `make build-wasm`/`make build-ts` run in no
workflow, leaving the reactor's `--export=` wiring exercised nowhere.
SDL2 is reached only via the `slogPriority == Just 0` init-and-quit
backdoor (`Sdl.hs:196-207`) — which does run its font discovery and
decoding, once per configured fontset, ahead of the backdoor branch, but
never its event loop or renderer. Add an
`xvfb-run` job driving a real SDL game for a few frames (a tiny
`--stopAfterFrames` variant of `benchFrontendBattle`), a pty-driven
ANSI startup/shutdown check, and — once 3.2 exists — a short `nodeBench`
run, which is what would first execute `Wasm.hs` in CI. No shipped
frontend goes permanently untested the way `Dom.hs` did.

**Split** — three commits, ordered by which gate can run at all: the
pty-driven ANSI smoke (verifiable in-session), the `xvfb-run` SDL smoke
(likewise, since the display arrived — see Hands back), and the
`nodeBench` smoke, which waits on 3.3.
The third carries the outcome line; the item is in no
`tools/doc-refs-allow.txt` gate beyond the three target names this block
introduces, which its own commit deletes as it lands.

**Owns** — `Makefile`, gaining `smokeANSI`, `smokeSdl` and
`smokeNodeBench`, and `.github/workflows/lint-and-test-suites.yml`, one
job each. Not `.github/workflows/haskell-ci.yml`, which is generated from
the cabal file and whose hand edits vanish on the next regenerate. The
workflow file is the campaign's busiest contention point — R2 grows it
per phase, and 0.2 and 0.3 each add a job — so one holder at a time, and
the three commits here are serialized against each other for the same
reason.

**Done** — `native`, `docs`, plus `make smokeANSI` && `make smokeSdl` &&
`make smokeNodeBench --dry-run`. `--dry-run` gates the one target that
cannot execute here on existing and on expanding, without running it;
`check-doc-refs.py` resolves all three names against the makefile, which
is what makes the allowlist deletion a checked claim.

**Hands back** — hands back nothing. *Display*: `xvfb-run` and `Xvfb` were
installed on 2026-07-31 and a whole game has since played out in the real
SDL2 frontend here, so the SDL smoke is gated in-session like the ANSI
one, rather than first executing in a CI run that would need a push and
so an explicit go-ahead. Two conditions the bare install does not supply
— a missing `/tmp/.X11-unix` and a GLX segfault — are CLAUDE.md's to
state and this item's to obey: the `smokeSdl` recipe carries them or the
target reproduces neither here nor in CI. One limit survives: the
renderer is llvmpipe, so an in-session frame *count* is a gate and an
in-session *timing* is not.

**Decide first** — (1) each smoke's pass criterion, against one repo fact
that makes the obvious guess wrong: the executable redirects its own
stdout and stderr to `~/.LambdaHack/stdout.txt` and
`~/.LambdaHack/stderr.txt` whenever stdout is not a terminal
(`GameDefinition/Main.hs:49-56`). Under `script` the pty *is* a terminal,
so the ANSI smoke's output stays in the job log; under `xvfb-run` it is
not, so an SDL job grepping its log for frame counts reads an empty
stream and passes vacuously. Branches: gate on exit status alone, or
harvest the two files — and if the latter, say which count is asserted.
(2) The frame counts. The item asks for "a tiny `--stopAfterFrames`
variant of `benchFrontendBattle`" (`Makefile:109-110`, which passes no
`--frontend` flag and so is SDL) without naming one, and the SDL job also
needs `xvfb` added to the apt line the playtest job already uses for the
SDL2 dev libraries. (3) The ANSI smoke's driver — `script`, present at
`/usr/bin/script` and propagating the child's exit status under `-e`, or
a python pty helper. No Makefile target names `--frontendANSI` today, so
there is no precedent to follow and the choice is free rather than
derivable.

**Frontends pass widths explicitly.** Frontend code decodes linear
indices with the explicitly-parameterized `punindex (rwidth coscreen)`,
never the `Enum` instance: `toEnum i` depends on the global
`speedupHackXSize` (the *dungeon* width, correct at frontend call sites
only via the engine's `Client/UI/Content/Screen.hs`'s
`rwidth == RK.rWidthMax` assertion). The switch is
the same arithmetic with one *fewer* global read, in a non-hot
once-per-frame loop, so it needs no benchmark. One live violation
exists: **`Sdl.hs:590`** (`setMapChar`'s `let Point{..} = toEnum i`) —
fix it when 0.2 puts
`Sdl.hs`'s per-cell drawing onto `CellStyle`, which touches exactly that
loop. (`Dom.hs:254` has the same pattern next to the correct form at
`Dom.hs:160`, but it's a dead example file, R3 — leave it.) The rule
binds all live and future frontend code from now on. The `Enum`
instance itself and its global stay engine-internal, permanently — the
engine-wide
removal of the hack is rejected outright (Appendix B has the ruling and
the hack's documentation of record).

**Owns** — nothing — this is a review rule, not an item to execute. Its
whole executable residue is `Sdl.hs:590`, owned by 0.2, which rewrites
that per-cell loop onto `CellStyle`; fixing it standalone contradicts the
coupling stated just above and collides with 0.2's diff. The rule
retires with the campaign rather than taking a hash.

**Done** — no gate of its own, only
`! git grep -nE 'Point\{\.\.\} = toEnum' --
'engine-src/Game/LambdaHack/Client/UI/Frontend/*.hs'
':!engine-src/Game/LambdaHack/Client/UI/Frontend/Dom.hs'`. It returns
nonzero today, naming `Sdl.hs:590` — which is what proves it a real
search rather than a silent one — and must return zero from 0.2 onward,
so it is an acceptance criterion on 0.2's diff and a check on every later
frontend diff, not a task of its own. Deliberately not a bare `toEnum`
grep: eleven other `toEnum` sites in `Sdl.hs` are legitimate SDL geometry
conversions, and a gate that flags them would stop being read.

**Hands back** — *judgement*: the grep catches only the shape already
seen, `Point{..} = toEnum`. Whether a newly written decoding site passes
the width explicitly — including one that spells the decode differently,
or one in a frontend not yet in the tree — is a review question with no
mechanical substitute; the grep above is the floor, asked of any diff
touching a frontend module.

**Decide first** — nothing.

**Functional core, imperative shell — per frontend (the review bar).**
`CellStyle`/`InputDecision`/`OverlayLayout` and the `ts-src`
`*-core.ts`-vs-wiring split are instances of one rule: a frontend module
contains only event capture, output mutation, and plumbing; every
*decision* lives in shared pure code under test. The question to ask of
any new line in `Sdl.hs`/`Wasm.hs`/`terminal.ts`: "would a fourth frontend
have to copy this?" If yes, it belongs in core. This is the practice that
generated 0.1/0.2/2.1, stated so it outlives them.

**Owns** — nothing — this is a review rule, not an item to execute. It
generated 0.1, 0.2 and 2.1 and is stated so as to outlive them; handed
out as a task, it becomes an agent hoisting decisions out of `Sdl.hs`,
`Wasm.hs` and `ts-src/src/terminal.ts`, which is those three items' work
done speculatively, in their files, without their specs. It retires with
the campaign rather than taking a hash.

**Done** — none exists, and inventing one is the error. The rule's entire
enforcement is the question asked of every diff touching a frontend
module: "would a fourth frontend have to copy this?"

**Hands back** — *judgement*: all of it, and there is no substitute gate,
because there is no Done. The nearest mechanical neighbours are the other
items' own suites, which pass equally whether or not a decision was left
sitting in the shell.

**Decide first** — nothing to decide, one thing to refuse: generalizing
this rule into a frontend-interface-as-value record is Appendix B.4's
recorded don't-do, deferred with named revisit triggers, so an agent
proposing it is re-opening a ruling rather than extending the practice.
If a trigger has genuinely fired, that is B.4's revision, not this item's.

## What would falsify this

Worth stating, because a campaign this long otherwise reads as
unfalsifiable. Three claims carry it, and each has a check that would sink
it rather than merely inconvenience it.

**G1, that shared knowledge is cheaper than duplicated knowledge.** What
would sink it is a shared module that cannot decide without knowing which
frontend is asking — a `CellStyle` or an `OverlayLayout` whose body
branches on the caller rather than on a parameter it was given. That is
duplication with a worse interface, and the check is 2.1's `Sdl.hs`
refactor: if the native consumer needs the module to know it is native,
G1 has failed for that rule and the rule belongs back in the frontend.
The floor-glyph parameter is the shape that passes; a frontend tag is the
shape that fails.

**G2, that SDL2's behaviour is the bar.** What would sink it is SDL2
being wrong, and it already was once: 0.0 assumed the three fill-only
highlight kinds draw no outline, and SDL2 in fact draws a black ring —
the erase half of a workaround for a 2.0.16 rectangle bug, not a design
choice anyone made. Parity copied it faithfully. So the bar is SDL2's
*intent* where the two can be told apart, and where they cannot, the
audit record in Appendix C is what has to say which was checked. A second
such discovery would not sink G2, but a pattern of them would mean the
bar is a workaround inventory.

**The boundary-cost ruling**, that a per-chunk `js_measureText` crossing
is too expensive to consider. It is the reason 2.1's pen is split into a
measurement-free cutoff and a fit function, and it rests on no
measurement of this codebase. R5's instrument is the check; the recorded
alternative in 2.1 — batch the whole frame's measurements into one
crossing — is what the ruling loses to if the numbers say so. Until then
it stands, and it is a ruling rather than a fact.

What would *not* falsify anything here: an item turning out larger than
its row says, or a **Decide first** resolving against the sketch in its
body. Both are the plan working. The Log is where they go.

## Out of scope

Sound (SDL2 has none either); window resizing (`windowResizable = False`
natively); tile/image graphics (no `sdl2-image` — both frontends are
font-glyph renderers); NumLock/numpad perfection (a `KeyboardEvent`
limitation, already mitigated by on-page guidance); mouse hover / IME
(SDL2 doesn't handle them either); a fontset picker UI (superseded by
R4). Save-file *persistence* was never a gap (it works, via
`WasmFile.hs`); save-file *robustness* is in scope as R1. Further
deliberate exclusions with their rationale: Appendix B.

## Sequencing

```
0.0 immediate fixes: AltGraph on keys AND mouse/wheel; highlight-outline
    rule (fill-only kinds; four-sided outlines)
0.3-baseline — FFI battery for the existing surface, before 0.1
0.1 InputDecision (validated mechanism; fixes the live input-bug class;
    brings the key-translation table tests, jsdom forwarding tests, and
    the input-side RawFrontend contract cases)
0.2 CellStyle + generator (unblocks 1.2's palette reuse and 2.2's fixtures)
1.1 / 1.2 / 1.3 / 1.4 / 1.5 — any order, parallel-friendly, banner last
    (1.5's full player-facing form needs R4; its trivial form is
    independent)
2.1 OverlayLayout extraction + Sdl.hs refactor (native-only,
    playtest- and bench-gated)
2.2 browser overlay renderer (visual no-op)
2.3 overlay transport (visual no-op, additive FFI)
2.4 capability flip (the visible change; heaviest review)
2.5 QA + R6 closure
3.1 lhStart reads WASI argv (spike first; independent of Phases 1-2;
    shared infrastructure with R4)
3.2 Node reactor driver (localStorage/LZString stubs, exit propagation)
3.3 repurpose nodeBench* targets; nodeMinifiedBench -> nodeDeployedBench
    (Phase 3 may run before/alongside Phases 1-2 — it's independent, and
    doing it early unlocks the wasm-vs-native ratio and R1's save-lag
    measurement via 3.2's instrumented stub)
R2 CI jobs — grow them per phase (0.3's battery once it exists; a short
    nodeBench smoke run joins after 3.3; the xvfb SDL + pty ANSI smokes
    complete the matrix)
0.3 FFI coverage — after the baseline, incremental with every
    FFI-touching commit
capability constants / sum-typed selection — standalone refactors, any
    time; the sum type before R4 (URL params parse into it)
RawFrontend contract — input-side cases with 0.1; the rest with 0.3
determinism goldens — native harness before 2.1; cross-backend under
    R2's test-wasm job
R1 / R4 — independent; R4 becomes loader-only after 3.1
R3 GHCJS rip-out — one commit, after 2.5 (parity); Dom.hs/JSFile.hs stay
    as documented-dead examples
R5 — after 2.5 and 3.3, measurement first
```

Rationale, condensed: the input cluster goes first (mechanism already
validated, fixes live bugs, small); the generator second because two later
steps consume its output; multi-font last because it is the largest item
and every earlier phase either de-risks it (0.2 fixtures, 2.1 native-side
proof) or is what it builds on (2.3 uses 0.3's test harness, 2.2 uses
0.2's palette).

---

## Appendix A — Investigation: porting the GHCJS target to GHC's in-tree JavaScript backend

The port was decided **against** (R3, B.8); this appendix is kept as the
resurrection manual — feasibility evidence, work items, effort estimate
and sources — should that decision ever be revisited.

Conducted 2026-07-13. Method: every repo-side claim below was read from
the current working tree (file:line references); every ecosystem claim was
checked against a primary source that day — the GHC user's guide, GHC's
GitLab (release branches and merge-request API), Hackage package pages and
released `.cabal` files fetched verbatim, upstream GitHub `master`
branches, and the ghcup cross-channel metadata. Where a source was
secondary or a claim rests on inference, that's said explicitly. Sources
are listed in A.7 and cited inline by number.

### A.1 The old GHCJS target is unbuildable — waiting was never an option

- The repo requires GHC ≥ 9.10: `tested-with: GHC ==9.10.3 || ==9.12.4 ||
  ==9.14.1` (`LambdaHack.cabal:43`) and `default-language: GHC2024`
  (`LambdaHack.cabal:94`), which only exists in GHC 9.10+.
- The standalone GHCJS compiler's last releases track GHC 8.6/8.10 [8]; it
  was never updated past that — its successor *is* the in-tree backend.
- Therefore `Dom.hs` (283 lines), `JSFile.hs`, and every `impl(ghcjs)`
  block are dead code on any compiler that can build this package. There
  is no "keep the old target limping" option; the only choices are the
  in-tree JS backend, or freezing/deleting the code.

### A.2 The in-tree JavaScript backend: status and toolchain

- Merged into GHC for 9.6 (Nov 2022) [7][9]. As of the current release
  (9.14.1), the user's guide still says: *"included as a technical
  preview. At time of writing, it is being actively developed but is not
  suitable for serious projects and production environments"* and *"not
  distributed in the GHC bindist and requires a manual build"* [2].
- The second half of that quote is out of date in practice: ghcup's cross
  channel ships community bindists for `javascript-unknown-ghcjs` GHC
  9.6.2, 9.6.7, 9.10.2, 9.12.1 and 9.12.2 (no 9.14.x yet, unlike
  wasm32-wasi which has newer coverage) [3]. Emscripten is required as the
  configured C toolchain, at install time and for any C bits [2][7].
  Version fit: the repo's floor (9.10) and middle (9.12) are both
  available; `tested-with`'s 9.14.1 has no JS bindist yet.
- Cabal identifies the new target as `arch(javascript)` (and
  `impl(ghc)`), **not** `impl(ghcjs)` — the old conditionals evaluate
  false under it. Concretely, building this package with the JS backend
  today would fall through to the *native* branch and try to build
  `Sdl.hs` against `sdl2`. The ecosystem's own convention confirms this:
  jsaddle's and ghcjs-base's cabal files gate on
  `impl(ghcjs -any) || arch(javascript)` [10][11].

### A.3 Template Haskell — the make-or-break requirement, and it holds

This repo cannot build without cross-target TH: `rcfgUIDefault` embeds
`config.ui.default` via a TH splice
(`GameDefinition/Content/RuleKind.hs:34`), `Client.UI.Content.Screen`
runs a TH splice that reads and parses `GameDefinition/PLAYING.md` at
compile time (plus `embedDir` for fonts, though that one is `#ifdef`'d
away under `USE_BROWSER`).

- The JS backend runs TH splices through a node-based external
  interpreter: `compiler/GHC/Runtime/Interpreter/JS.hs` ("JavaScript
  interpreter", `spawnJSInterp`, `jsLinkRts`, `jsRunServer`) — verified
  present in both the `ghc-9.10` and `ghc-9.12` release branches by
  fetching the file directly [4]. At the backend's initial merge TH was
  explicitly missing [7]; IOG's 2023 update series documents the
  implementation work in between [9].
- This is the same architecture as the wasm backend's TH, which this
  repo's wasm build already depends on daily — including qRunIO
  file-reading splices, which run on the build host and resolve repo
  paths normally. No new class of TH problem is introduced.

### A.4 FFI differences and their exact blast radius in this repo

- The new backend's documented convention for `foreign import javascript`
  is a **JS function expression** (typically an arrow function); the old
  GHCJS `$r = ...` result-assignment sugar is not documented as supported
  [1]. Repo impact: exactly two import strings, `JSFile.hs:35-39`
  (`LZString.compressToUTF16`/`decompressFromUTF16`) — a mechanical
  rewrite.
- There is no true `foreign export javascript` on the JS backend;
  callback-style quasi-exports go through `GHC.JS.Foreign.Callback`
  (`syncCallback*`/`asyncCallback*`), with `JSVal` and conversions in
  `GHC.JS.Prim` [1]. Repo impact: **none** — `Dom.hs` registers event
  handlers from the Haskell side via ghcjs-dom's `on`/`EventM` and exports
  nothing; unlike `Wasm.hs`, whose whole design is JS-calls-Haskell
  exports. (This asymmetry is *why* `Dom.hs` doesn't need exports: under
  GHCJS/JS-backend the Haskell program owns the page; under wasm the TS
  loader owns the page.)
- An `interruptible` convention exists for async JS calls (continuation
  argument) [1]; nothing in `Dom.hs`/`JSFile.hs` needs it.

### A.5 The library chain Dom.hs sits on — ported, but thinly maintained

`Dom.hs` imports only `GHCJS.DOM.*` (typed bindings); `JSFile.hs` imports
`Data.JSString`/`Data.JSString.Text` (ghcjs-base) plus `GHCJS.DOM.Storage`.
The chain those need:

- **ghcjs-dom 0.9.9.2** (May 2024) — its released `.cabal` selects the
  implementation with: `if flag(jsffi) && (impl(ghcjs <9) || (impl(ghc
  >=9.6.4) && arch(javascript)))`, taking `ghcjs-dom-jsffi` for old GHCJS
  and **`ghcjs-dom-javascript` for the new backend** (fetched verbatim
  from Hackage) [5]. So the new backend was explicitly wired in, despite
  the package description and `tested-with` metadata still reading as
  old-GHCJS-era.
- **ghcjs-dom-javascript 0.9.9.3** (Sept 2024) — the new-backend flavor;
  depends on `ghcjs-base` [6].
- **ghcjs-base 0.8.0.4** — released on Hackage; its `master` cabal carries
  `if !arch(javascript)` conditionals, i.e. it builds natively on the new
  backend; repo activity as recent as 2026-03-26 [11].
- **jsaddle 0.9.9.4** (uploaded 2026-03-26 — actively maintained) —
  `master` cabal: `if impl(ghcjs -any) || arch(javascript)` →
  `ghcjs-base`, so the jsaddle/`ghcjs-dom-jsaddle` route is a working
  fallback if the jsffi flavor misbehaves [10].

Caveats, stated plainly: package descriptions and `tested-with` fields
across this family are stale; Hackage build reports show failures (which
means nothing for a cross target — Hackage builders have no JS toolchain —
but also means there's no positive signal); and real-world mileage on the
new backend is thin next to wasm's. Miso is the flagship consumer of the
backend generally. Budget for small upstream patches and expect to test
everything locally; do not expect the first `cabal build` to succeed.

### A.6 Work items, effort, and risk placement

Total: **roughly 1.5–3 weeks of focused work**, deliberately front-loaded
so the cheapest step retires the most uncertainty.

1. **Spike, 1–2 days.** Install the 9.12.2 JS bindist (ghcup cross
   channel) + emsdk; make the cabal edits from item 2 in rough form; build
   the library and run a `--frontendNull`/teletype game under node. This
   surfaces the two genuine unknowns: (a) whether the full dependency
   tree builds under `arch(javascript)` (pure-Haskell deps like
   `miniutter`, `hsini`, `splitmix`, `enummapset` should; `async` relies
   on threads, which the JS backend implements green-threaded as GHCJS
   did); (b) **`ghc-compact`** — the package is an unconditional
   dependency (`LambdaHack.cabal`), the wasm build really calls `compact`
   at startup, and the old-GHCJS escape hatch `#ifdef USE_JSFILE` (`let
   cops = copsRaw -- until GHCJS implements GHC.Compact`,
   `TieKnot.hs:114-118`) already exists — keep it for the new backend
   until proven unnecessary. Everything after the spike is
   known-shape work.
2. **Cabal + `JSFile.hs`, 1–2 days.** `impl(ghcjs)` → `arch(javascript)`
   everywhere — all three sites: the `common options` cpp-options
   conditional (`LambdaHack.cabal:148-152`), inherited by every
   component, and the `library` stanza's frontend and file-backend
   conditionals (`LambdaHack.cabal:380-400`). The executable has no
   `impl(ghcjs)` conditional of its own; it inherits them. Delete the dead
   old-compiler knobs (`ghcjs-options`: `GHCJS_GC_INTERVAL`,
   `GHCJS_BUSY_YIELD`, `-dedupe`, `GHCJS_BROWSER`,
   `LambdaHack.cabal:165-173` — none has a new-backend equivalent; the
   new backend takes plain `ghc-options`); revisit the `supportNodeJS`
   flag's meaning; rewrite the two `$r =` FFI strings as arrow
   functions. The `USE_GHCJS`/`USE_JSFILE` CPP names can stay (they name
   the *frontend/file-layer choice*, not the compiler), which keeps the
   churn out of the eight source files that test them.
3. **`Dom.hs` against ghcjs-dom, 2–5 days — the tail risk.** In the good
   case it compiles nearly as-is (the API surface `Dom.hs` uses — `on`,
   `EventM`, `RequestAnimationFrameCallback`, table/cell types — exists in
   both flavors). The spread covers patching `ghcjs-dom-javascript`/
   `ghcjs-base` where stale, or falling back to the jsaddle flavor.
4. **Build/deploy harness, 2–3 days.** Makefile targets mirroring
   `build-wasm`/`build-ts` (the compiler emits a `.jsexe` with `all.js`
   [2]); a JS-variant `index.html` (load `all.js` + `lz-string`, the old
   model — `Dom.hs` builds its own DOM, needing only the
   `gameMap`/`pleaseWait` elements); deployment into the pages repo
   alongside the wasm artifacts.
5. **CI, optional, 1 day.** A job in the hand-written workflow with ghcup
   cross + emsdk, building and node-smoke-testing. Without it, the target
   rots again the way the last one did — factor this into any future
   revival decision, not just the initial cost.

If Phase 0 has landed first, `Dom.hs` should consume
`InputDecision`/`CellStyle` as part of step 3 at near-zero marginal cost —
which is why a revival would be cheaper after Phases 0–2 than before.

### A.7 Sources

Primary (fetched/inspected directly on 2026-07-13):

1. GHC User's Guide, "FFI and the JavaScript Backend" —
   <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html>
2. GHC 9.14.1 User's Guide, "GHC Backends" (JS backend status quote) —
   <https://downloads.haskell.org/ghc/latest/docs/users_guide/codegens.html>
3. ghcup cross-channel metadata (bindist inventory) —
   <https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.9.yaml>
4. `GHC.Runtime.Interpreter.JS` in release branches (raw fetch, HTTP 200 +
   content on both) —
   <https://gitlab.haskell.org/ghc/ghc/-/raw/ghc-9.10/compiler/GHC/Runtime/Interpreter/JS.hs>,
   `.../ghc-9.12/...`
5. ghcjs-dom 0.9.9.2 released `.cabal` (backend-selection conditional) —
   <https://hackage.haskell.org/package/ghcjs-dom-0.9.9.2/ghcjs-dom.cabal>
6. ghcjs-dom-javascript —
   <https://hackage.haskell.org/package/ghcjs-dom-javascript>
7. "JavaScript backend merged into GHC" (IOG Engineering, 2022-12-13; TH
   listed as missing at merge) —
   <https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/>
8. GHCJS repository (last supported GHC versions) —
   <https://github.com/ghcjs/ghcjs>
9. IOG GHC update series (2023; JS-backend TH implementation work) —
   <https://engineering.iog.io/tags/ghc/>
10. jsaddle — <https://hackage.haskell.org/package/jsaddle> and
    `master` cabal at <https://github.com/ghcjs/jsaddle>
11. ghcjs-base — <https://hackage.haskell.org/package/ghcjs-base> and
    `master` cabal at <https://github.com/ghcjs/ghcjs-base>

Checked but yielding nothing decisive (recorded so nobody re-treads them):
GHC 9.8.1/9.10.1/9.12.x release notes (no explicit "JS backend TH"
announcement — the release notes are simply sparse on this backend; the
branch-source check in [4] is the authoritative evidence); the GHC wiki's
javascript-backend page (access-blocked at time of writing); GHC GitLab MR
search for the TH merge (the API surfaced only the initial backend merge,
!9133, 2022-11-30, milestone 9.6.1).

---

## Appendix B — Decisions against, and deferrals

Decisions by Mikolaj, 2026-07-13. Recorded so they aren't re-proposed,
and so the reasoning survives if circumstances change.

**B.1 No copying / screen-reader work beyond what the DOM gives free.**
DOM text is incidentally selectable/copyable and visible to screen
readers — affordances SDL2 never had, which canvas-rendered overlay text
(message log, menus, help) loses after Phase 2. Decision: keep the
affordances only where they are free (the DOM `<span>` grid keeps them by
nature; don't disable selection there), but spend no effort preserving
them for overlay text — no hidden DOM mirror, no copy button, no ARIA
layer. SDL2 parity is the bar, and canvas meets it.

**B.2 The GHCJS-era page's community features stay gone.** The old page
(pages-repo `index.html`, GHCJS era) carried a cactus.chat Matrix comments
widget, a feedback invitation, and SEO `meta keywords`; the wasm page
rewrite dropped them. They are deliberately not restored — the old page's
git history is not a to-do list.

**B.3 GHCJS's two-edge highlight rendering is an artifact, not a
feature.** Context for 0.0's highlight rule: the GHCJS-era page CSS gave
every table cell `border:1px solid #000000`, so `Dom.hs`'s `border-color`
writes produced visible outlines — including the spurious one on
`HighlightBackground` cells whose colour the wasm port faithfully
reproduced; and
because of collapsed table borders, a highlight effectively showed only on
a tile's bottom and right edges (verified in a browser). Rulings: SDL2's
look is canonical (a black ring, not none, for
`None`/`Background`/`NoneCursor` — see 0.0); every kind outlines all four
tile sides;
the wasm `inset` box-shadow's four-sided drawing is correct and stays.

**B.4 No frontend-interface-as-value record (yet).** The idea: each
frontend module exports one value, e.g. `frontend :: FrontendImpl` (name,
startup, capability record), and `Frontend.hs` consumes only that — so
adding a capability becomes adding a field the type-checker forces every
frontend to answer, instead of today's parallel conventions (a module
exporting `startup`/`frontendName` wired by CPP as `Chosen`,
`Frontend.hs:41-48`; `fprintScreen` as a dummy field overridden
post-construction by exactly one frontend, `Common.hs:67`/`Sdl.hs:273`;
2.4's `supportsMultiFont` constant). Rejected for now. Revisit if any
trigger fires: capabilities accumulate beyond the two now planned; a bug
appears that this design would have prevented (e.g. a frontend silently
missing a newly-required export); or another adopted item turns out to
depend on it.

**B.5 No hoisting of the frame-diff fold.** The changed-cell fold is
duplicated verbatim in Haskell (`Sdl.hs:722-723`, `Dom.hs:277-279`), but
it's two lines of content-free traversal — not worth the churn.

**B.6 Engine-wide `speedupHackXSize` removal: rejected outright.** The
hack stays permanently: it is set once at startup, it is now documented
(below), and any clean alternative would churn the engine's hottest loops
for zero player-visible value. No rework, no benchmark experiment.
Why the hack exists (confirmed from its own documentation,
`Point.hs:26-40`): a global `NOINLINE` one-element `PrimArray` carries the
dungeon's X size into the `Enum` instances of `Point` and `Vector`,
because `Enum` methods can't take a width argument and these conversions
sit in the engine's hottest loops; `PrimArray` was chosen over `IORef`
explicitly for lower read overhead; it is mutated exactly once, at startup
(`TieKnot.hs:60-63`, `unsafeThaw`/write/`unsafeFreeze`), before first use;
the comment names Backpack as the possible clean alternative, unverified
over GHCJS-era doubts (moot for wasm, untested for the JS backend).
The candidate reworks that were on the table (threading the width through
call sites, or a Backpack signature) would have touched
FOV/pathfinding/`PointArray` hot loops and required before/after runs of
the bench battery to prove no regression — cost with no payoff, hence the
rejection. This paragraph is the hack's documentation of record. (The
*frontend-side* rule — explicit `punindex`, no benchmark needed — is
adopted; see Multi-frontend practices.)

**B.7 No-action findings from the alignment analysis** (kept
self-contained since the superseded document may be deleted):

- *Per-frame cell diffing stays on the TS side.* A `buf[i] === prev[i]`
  check is content-free equality with no canonical second definition to
  drift from, and moving per-cell diffing into Haskell would trade one
  bulk buffer transfer per frame for potentially hundreds of wasm↔JS
  boundary crossings; a Haskell-diffs-then-marshals-changes middle ground
  transfers as much as today in the worst case while adding complexity.
- *DOM/grid construction stays in TS.* Grid dimensions already come from
  Haskell (`rwidth`/`rheight`); the rest (`gridTemplateColumns`,
  `lineHeight`, `whiteSpace`) is CSS mechanics with no Haskell-side
  counterpart, exactly as `Dom.hs`'s own `<table>` CSS choices aren't
  derived from anything deeper.
- *Focus management stays in TS.* The bfcache/`pageshow`/mousedown-refocus
  orchestration in `terminal.ts` is browser-lifecycle work with no
  canonical Haskell value anywhere near it; `Dom.hs`'s single
  `focus divMap` call very likely has the same bfcache gap, just never
  tested.
- *`#screen` font-size (16px) is a web-presentational constant* with no
  canonical Haskell value behind it; nothing to generate. (Its player
  -facing counterpart is now handled properly by 1.5's `allFontsScale`.)
- *Save persistence needs no rework* — `WasmFile.hs` mirrors `JSFile.hs`'s
  exact localStorage format as a proper Haskell sibling module; it is the
  existing exemplar of the pattern this plan applies elsewhere.

**B.8 No JS-backend port — one browser target is enough.** A port of the
GHCJS target to GHC's in-tree JavaScript backend was investigated and
found feasible at roughly 1.5–3 weeks (Appendix A), with real payoffs
(typed DOM bindings — the one structural gap wasm can't close; a no-wasm
fallback browser target). Rejected: a second browser target means a
second cross toolchain (including emscripten) and CI surface maintained
forever, for an engine that only needs one. `Dom.hs`/`JSFile.hs` stay
in-tree as documented-dead examples of an alternative frontend and file
backend; everything else GHCJS is ripped out at parity (R3 has the
inventory). Appendix A remains the resurrection manual if this is ever
revisited.

**B.9 No `document.title` from `stitle`.** SDL sets its window title from
the `stitle` option (`Sdl.hs:107-108`) while the page's `<title>` is
hardcoded in `index.html`. Sourcing it from Haskell at startup was
proposed as a one-line G1 nicety and dropped: the page title is page
chrome, not game knowledge worth an FFI call.

---

## Appendix C — Verified non-gaps (SDL2-vs-wasm audit record)

From a full line-by-line read of `Sdl.hs` (all 922 lines) against
`Wasm.hs`, `terminal.ts`, `loader.ts` and the shared
`Common.hs`/`Frontend.hs` machinery, hunting for anything SDL2 does that
the wasm stack doesn't. Beyond the gaps in this plan (cursor, screenshots,
fullscreen incl. scaling, `allFontsScale`, multi-font, the 0.0 fixes),
everything else checked out as parity or not-applicable. Recorded with
evidence so nobody re-treads it:

- **Mouse position attached to keyboard events.** SDL sends the *current*
  mouse position with every keypress (`getAbsoluteMouseLocation`,
  `Sdl.hs:348-350`); web frontends send `PointUI 0 0`, and every KMP
  overwrites the session's `spointer` (`MonadClientUI.hs:166`) — so in the
  browser, each keystroke clobbers the remembered pointer. Traced every
  `spointer` consumer (`HandleHumanGlobalM.hs:119,1271,1359`;
  `HandleHumanLocalM.hs:698,1303,1330`; `HandleHelperM.hs:244`;
  `SlideshowM.hs:350`) — the list is exhaustive, by repo-wide grep for
  `getsSession spointer`. Every one of them reads `spointer` only on a
  mouse-button release, and the triggering mouse KMP itself sets
  `spointer` correctly first, so there is no observable difference today.
  Six reach that read from the mouse bindings themselves
  (`GameDefinition/.../Content/Input.hs:188-236` — the mouse
  section plus the `"safe1".."safe6"` `CmdInternal` pseudo-keys, which are
  not typeable on any keyboard and exist only for the mouse machinery and
  macros to reference). The other two are entered from the *keyboard* and
  gate the read on a `K.LeftButtonRelease` case inside their own confirm
  loop: `pickPoint` (`HandleHumanGlobalM.hs:1359`, reached from
  `alterDirHuman`/`closeDirHuman`, bound to `M`/`m`) and
  `displayChoiceScreen`'s `interpretKey` (`SlideshowM.hs:350`). The
  invariant is therefore about the *read*, not about the binding. This is
  a booby trap: **any future key binding for a `*WithPointer`/`ByArea`
  command would behave differently on web.** 0.1's `InputDecision` should
  document the invariant.
- **Key auto-repeat.** Neither side filters it: `Sdl.hs:334-336` accepts
  every `Pressed` event without consulting the repeat flag; the browser
  keydown listener forwards repeats likewise. Parity by mutual omission.
- **Natural-scrolling wheel direction.** SDL branches on
  `mouseWheelEventDirection`/`ScrollFlipped` (`Sdl.hs:363-370`); browsers
  normalize direction into `deltaY` before the event reaches script.
  Handled at different layers, same result.
- **Window-close semantics.** SDL's close/quit path deliberately exits
  without a fresh save (`forceShutdown`; the `display` comment at
  `Sdl.hs:475-484` spells out why) — so a closed browser tab losing
  unsaved progress is parity, not a regression. Improving on it is R1.
- **Redraw on expose/restore/resize** (`Sdl.hs:376-385`): repainting after
  occlusion is the DOM's job in a browser; nothing to port. Same for the
  texture-invalidation workarounds.
- **VSync and frame pacing.** SDL uses `AcceleratedVSyncRenderer`
  (non-benchmark) plus its own poll loop; the web side batches through
  `requestAnimationFrame` — the browser analogue. The `--maxFps` logic
  itself lives in shared code (`Frontend.hs:93`), not in any frontend.
- **HighDPI.** `windowHighDPI` (`Sdl.hs:227`) has no DOM-grid counterpart
  to port (browser text is DPR-aware natively); the canvas-DPR concern is
  a 2.2 pitfall item.
- **PrintScreen key (as opposed to `C-P`).** SDL maps the key
  (`Sdl.hs:841`) but no binding uses `K.PrintScreen` bare
  (`GameDefinition/.../Content/Input.hs:185` binds only `C-P`), so
  `keyTranslateWeb`'s lack of a `"PrintScreen"` case changes nothing.
- **Key-translation coverage.** `keyTranslateWeb` (`Key.hs:472+`) covers
  the same command-relevant key set as SDL's `keyTranslate`
  (`Sdl.hs:783-890`) including F-keys, navigation, KP digits with the
  shift convention, and dead keys; the residual differences are the
  layout/NumLock quirks declared out of scope.
- **Frame-delivery blocking.** SDL's `display` blocks until the frame is
  drawn (`sframeQueue`/`sframeDrawn` handshake); wasm's returns after the
  synchronous buffer snapshot with painting deferred to rAF. A timing
  nuance inside the same shared `fshowNow` protocol, not a feature gap;
  if it ever matters, it will show up in R5's measurements.

The GHCJS files (`Dom.hs`, `JSFile.hs`, the `impl(ghcjs)` cabal stanzas,
and the GHCJS-era page) were likewise audited line-by-line against SDL2:
every difference is a browser-platform limitation, a DOM-instead-of-
textures mechanism, a JS-performance workaround (the disabled autosave,
the `GHCJS_GC_INTERVAL`/`-dedupe` knobs), or plain missing effort (the
known parity gaps) — with faithful adaptations elsewhere (`JSFile.hs`
preserves `HSFile.hs`'s exact save envelope with lz-string in place of
zlib; the browser-zoom passthrough is deliberate). The GHCJS features
that exceeded SDL2 (runtime zoom, free DOM text affordances) are handled
by 1.5, 0.1's passthrough, and B.1.
