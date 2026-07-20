# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this is

LambdaHack is a Haskell game engine library for ASCII roguelike games,
bundled with a sample dungeon crawler (`GameDefinition/`). It targets
native (SDL2/ANSI/teletype frontends) and WASM (the browser build); a
defunct GHCJS browser build survives only as dead example code (see the
Frontends section below). The TypeScript browser-side harness lives in
`ts-src/`; built WASM artifacts are deployed into the
`lambdahack.github.io` repo, expected as a sibling checkout at
`../lambdahack.github.io` (visible to sessions only when the wrapper
mounts it — sandboxing notes at the end of this file).

Frontend work is planned in `docs/wasm-frontend-unified-plan.md`: the
SDL2-parity roadmap for the WASM frontend, recorded decisions — including
don't-do rulings, so ideas aren't re-proposed — and verified repo facts with
file:line citations. Consult it before frontend-touching changes.

A second reference document, `docs/leader-desync-bug.md`, covers UI
client state: the post-mortem of the pointman-desync bug family (the crash
worked around in commit 4a6eca154), the de-risked live-read fix design —
make `sleader` the single source of truth, read at point of use — and the
test battery pinning it. Its companion, `docs/promptgetkey-hygiene.md`,
holds the abort-split design — strictly after live-read and assuming it,
split `promptGetKey`'s interrupted-macro cleanup into a pure decision plus
a named abort action — plus everything joint to the two designs, notably
the [contract] test series that must stay green across both refactors. The
dependency is one-way by design: the post-mortem never references the
abort-split design or its document, so don't "fix" that by adding a link.
Consult them before touching pointman/leader plumbing, macro playback, run
continuation or the item-dialog code.

The closing portable-notes section holds the author-generic conventions
and the machine-specific session facts — the sandbox's misleading "No such
file" errors, phantom dotfiles in `git status`, `git`/`cabal` writes
needing to run unsandboxed — skim it before debugging anything
environment-related.

## Build

```
# one-time setup of a fast dev build: no optimization, expensive
# assertions on, tests enabled; guarded not to clobber an existing config
[ -f cabal.project.local ] ||
  cp cabal.project.local.development cabal.project.local
cabal build
```

Run `cabal` (like all `git` writes and `gpg`) unsandboxed — `~/.cabal` is
read-only under the inner sandbox (sandboxing notes below). A full build
takes long: give Bash a generous timeout or run it in the background
rather than concluding it hung.

Run the sample game (opens an SDL2 window, so for a human at a display —
not from a headless session):

```
make play               # normal play
make shot               # play, printing each screen (debugging frontends)
```

The WASM (browser) build requires `~/.ghc-wasm/env` (ghc-wasm-meta toolchain):

```
make build-wasm         # wasm32-wasi-cabal build exe:LambdaHack
make build-ts           # build the TS harness in ts-src/; deploy it, the
                        # wasm binary, the JSFFI glue and
                        # GameDefinition/index.html into ../lambdahack.github.io
make serve-wasm         # serve ../lambdahack.github.io locally on :8080
make run-wasm           # open in firefox
```

## Tests, lint, CI

```
cabal test
```

To run a single unit test (tasty), pass a pattern via `-p`:

```
cabal test --test-options='-p "<test name substring>"'
```

Doctests are a manual-only recipe, not run in CI (closing that gap is part
of the plan's R2). The README also lists a `definition` doctest component,
but that internal library only exists in the original `LambdaHack.cabal.bkp`
— the flattened `LambdaHack.cabal` in use has a single library, so this
covers everything:

```
cabal install doctest --overwrite-policy=always && cabal build
cabal repl --build-depends=QuickCheck --build-depends=template-haskell \
  --with-ghc=doctest lib:LambdaHack
```

### Haskell unit-test harness (`test/`)

Hard-won facts for writing and driving tests with this harness.

#### The mock and frontend stubs

`test/UnitTestHelpers.hs` provides `CliMock`, a real `MonadClientUI`
implementation over `StateT CliState IO`, plus two frontend stubs: the
default answers every `FrontKey` request with ESC;
`scriptedFchanFrontend` (wired into a fixture by `partyCliStateScripted`)
plays a scripted key list first, then falls back to ESC.

#### The stub world: board and party fixtures

- The stub board is 3x3 unknown, unwalkable tiles: aiming/projection
  pipelines fail deterministically ("aiming obstructed by terrain"), but
  code that checks stores before aiming (e.g. `projectHuman`) is testable.
  Anything that indexes `ltile` by `Point` must keep positions on row 0:
  the `Enum` width hack (`speedupHackXSize`, see the gotcha below) keeps
  its default 80 in the test binary, so on the 3x3 board only row-0
  lookups stay in bounds.
- Party fixtures `partyCliState`/`partyCliState3`/`partyCliStateBanned`
  model the sample game's hero faction. `emptyUIFaction` defaults
  `fhasPointman = False`, which alone forces `noRunWithMulti` and disables
  the run machinery — set it `True` in any faction fixture that must run or
  restore the pointman.

#### Driving keys, commands and dialogs

- `promptGetKey` runs under the mock with blank frames (`onBlank = True`)
  and with rendered ones (`drawHudFrame` over the stub board — pinned by
  `FrameMUnitTests.AS7`). Even whole dialogs can be driven — see the ESC
  store-dialog test in `test/HandleHumanLocalMUnitTests.hs` — given two
  things: an item both held by the actor and registered in `sitemD` (a
  separate `updateItemD` step; without it the store reads as empty), and
  a screen wider than 4 (dialog prompts assert that — enlarge `coscreen`
  per-test; the level can stay 3x3).
- Real key bindings come from the *sample game's* `standardKeysAndMouse`
  (module `Client.UI.Content.Input` — the game's, per the
  duplicate-basename gotcha below) via `IC.makeData Nothing`, baked into
  the fixture CCUI by `stubSessionUI` — no hand-rolled `InputContent`
  needed. To run a whole `HumanCmd` the way the key loop does, use
  `dispatchCmd` in `test/HandleHelperMUnitTests.hs`, which resolves the
  command's real key from those bindings and feeds both to
  `cmdSemInCxtOfKM`.

#### Characterization tags: `[LR-flip]` and `[contract]`

Some tests are deliberate characterizations of known-buggy behaviour,
tagged `[LR-flip]` in comments with the post-fix expectation stated
inline; `[contract]`-tagged tests pin behaviour that must survive both
planned designs (live-read, then abort-split — see the two `docs/`
documents above) unchanged. Don't "fix" a green `[LR-flip]` test — flip
it together with the engine change it documents, and verify the flip by
temporarily applying the candidate fix before committing either.

### WASM and TypeScript test suites

The WASM test suite drives the compiled wasm test binary through
Node/wasmtime via `ts-src/run-wasm-test.mjs`; the TypeScript tests run
vitest over `ts-src/`:

```
make test-wasm
make test-ts
```

### Playtests (Makefile)

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
one. These targets play out whole AI-vs-AI games — expect minutes, not
seconds, and set Bash timeouts accordingly; the `frontend*` targets
additionally open an SDL2 window, so they are for a human at a display.

The headless targets select test frontends by flag: `--frontendNull`
(frames forced but not displayed), `--frontendLazy` (frames not even
computed), `--frontendTeletype` (line-printer output). The `nodeBench*`
and `nodeMinifiedBench` targets are dead GHCJS remnants — they invoke a
`.jsexe` that nothing builds anymore; repurposing them for WASM is the
plan's Phase 3.

`LambdaHack --help` lists all debug options. Of these, `--sniff` (verbose,
initially cryptic) prints the client-server traffic — useful when debugging
the request/response flow.

When stdout is not a terminal — tool-driven sessions, CI — the game
redirects its own stdout and stderr to `~/.LambdaHack/stdout.txt` and
`~/.LambdaHack/stderr.txt` (`GameDefinition/Main.hs`, a Windows
desktop-launch workaround), superseding any shell redirection. Such
runs look completely silent: the handles are duplicated at startup, so
everything the process writes to either stream from then on — the
`--benchmark` report, `+RTS -s` summaries, teletype/debug output —
lands in those two files, freshly overwritten on each launch; harvest
them between runs.

### Linting and formatting

Two tools: `hlint .` (using the liberal `.hlint.yaml`) and
stylish-haskell (`.stylish-haskell.yaml`: 2-space indent, 80 columns,
spaces not tabs; the editor runs it on save). The `arguments:` in
`.hlint.yaml` pass `--cpp-simple` (keeps the
`#ifdef`-guarded code — the expensive assertions, the wasm branches —
visible to the linter; hlint's default no-defines CPP pass would drop
it, un-linting it and producing false positives in what remains) and
`-XNoStarIsType` (as in horde-ad; honored by the hlint on PATH, built
from hlint master though it self-reports v3.10 — earlier binaries
mishandled the flag). Repo-wide `hlint .` is not expected to come out
clean under that master build: it reports ~12 pre-existing hints that
the release binary CI installs doesn't, plus a parse error on
`JSFile.hs` (unsupported `JavaScriptFFI` extension) that the CI job
sidesteps with `--ignore-glob` — so judge hlint output on touched files
only.

### CI

Two workflows: `.github/workflows/haskell-ci.yml` (auto-generated
by `haskell-ci`; runs `cabal build`/`cabal test`/`haddock` across GHC
versions) and `.github/workflows/lint-and-playtest.yml` (hand-written;
runs `hlint .` with `JSFile.hs` ignore-globbed, plus the `make test-gha`
playtests).

### Standing checks

For a generic "run checks" request — each with its trigger:

- Always: `python3 tools/check-plan-citations.py DOC` over CLAUDE.md, the
  three `docs/` documents and every draft staged in the repo root; then,
  for any document edited since it was last verified, passes 2 and 3 of
  the three-pass discipline (portable notes below — pass 3, the
  quantified-claims grep, is the one that keeps finding real errors).
- When Haskell code changed: build, then `cabal test` (the Makefile
  playtests when the change warrants), and stylish-haskell and hlint on
  touched files.
- After a push: CI status via `curl -s` against `api.github.com`, path
  `/repos/LambdaHack/LambdaHack/actions/runs?branch=BRANCH&per_page=5`,
  reading each run's `head_sha`, `status` and `conclusion` (`gh` is
  unauthenticated); both workflows above report there, so expect two runs
  per push.
- When a new self-checking assertion or checker tool is born: prove it
  non-vacuous by deliberately breaking it (portable notes), and record
  the proof next to it.

## Architecture

### Three source trees, one library

The `LambdaHack` library stanza in `LambdaHack.cabal` combines three
`hs-source-dirs`, all under module namespace `Game.LambdaHack.*` (except
`GameDefinition`, which is unprefixed):

- `definition-src/` — pure data: dice, frequencies, RNG, and the abstract
  `Content.*Kind` definitions (`CaveKind`, `FactionKind`, `ItemKind`,
  `ModeKind`, `PlaceKind`, `RuleKind`, `TileKind`) plus `Definition.*`
  (abilities, colors, flavour). No game logic, no IO.
- `engine-src/Game/LambdaHack/` — the actual engine: `Atomic/` (state-changing
  command representation), `Client/` (UI + AI client logic), `Common/`
  (shared types/state), `Server/` (game arbiter, dungeon generation, FOV).
- `GameDefinition/` — the *sample game's* concrete content
  (`GameDefinition/Content/*.hs`, module namespace `Content.*`) plus
  game-specific client wiring (`game-src/Client/UI/Content/{Input,Screen}.hs`)
  and the `Implementation.Monad{Client,Server}Implementation` modules that
  pick concrete monad transformer stacks for the abstract client/server
  monads. `TieKnot.hs` wires content + engine + frontend into a runnable
  game (`tieKnot`/`tieKnotForAsync`); `Main.hs` is the executable entry
  point. Also holds `index.html`, the game's WASM browser page.

Separately, `ts-src/` (not part of the Haskell library) holds the TypeScript
browser-side harness for the WASM build (loader, terminal emulation, dev
server, their vitest tests) and `run-wasm-test.mjs`, the Node driver for
`make test-wasm`.

**Module-as-interface convention** (stated in the .cabal description): if a
module has the same name as a directory (e.g. `Game.LambdaHack.Client` vs.
`Game.LambdaHack.Client.*`), that module is the *exclusive* interface to
everything in the directory — other modules in the library must not reach
past it into the directory's internals. This is enforced by convention, not
by `.cabal`-exposed/hidden boundaries (nearly all modules are exposed so
that downstream games can override things — the only hidden ones are the
per-backend file module actually compiled in, `HSFile`/`WasmFile`/`JSFile`,
and `Paths_LambdaHack`), so respect it when adding imports.

### Client-server architecture

The engine is strictly split, with types enforcing the separation, into:

- **Server** (`Server/`): the sole authority over full game state; never
  trusts clients.
- **Clients** (`Client/`): one per faction. Each client only sees its own
  perception-limited view of the world. A client may be UI-driven (human, via
  `Client/UI/`) or AI-driven (`Client/AI/`); the human/UI client is a client
  like any other, just with extra `Client.UI` capabilities layered on.

Flow: the UI client turns keystrokes into `HumanCmd`s
(`Client/UI/HumanCmd.hs`, interpreted in `HandleHumanM.hs`/
`HandleHumanGlobalM.hs`/`HandleHumanLocalM.hs`). Purely local UI actions
resolve in the client; anything that would change game state becomes a
`Request` sent to the server. The server validates it (rejecting with a
`ReqFailure` if impossible, e.g. moving into a wall) or applies it and
broadcasts the resulting atomic updates (`UpdAtomic`/`SfxAtomic`, see
`Atomic/`), wrapped in `Response`s, to every client that can perceive the
event (`Response` also carries the server's queries prompting a client for
its next move). AI clients run the same request/response loop, generating
requests from perceived state via `Client/AI/`
(`PickActorM`, `PickTargetM`, `PickActionM`, `Strategy`).

Six command datatypes recur across the codebase, each with its own
interpreters (mostly in `Handle*` modules): `HumanCmd`, `Effect`,
`UpdAtomic`, `Request` (a family: `ReqUI`/`ReqAI`/`RequestTimed`, in
`Client/Request.hs`), `Response`, `FrontReq`. Most command semantics live
in custom monads (`MonadClient`, `MonadServer`, `MonadClientUI`, etc.) —
these are state monads, so a command's semantics is a state transformer
plus side effects (e.g. frontend drawing). Each monad's concrete
transformer stack is chosen in the `*Implementation` modules
(`GameDefinition/game-src/Implementation/`), not in the engine itself —
the engine only depends on the abstract monad classes.

Naming mismatch: the in-game/UI concept called "pointman" is called
`leader` in the source code (and there are a few more such mismatches).
Keep source-code naming and UI naming each internally consistent, but
don't expect them to match each other.

### Frontends and build backends

Frontend selection and native-vs-browser backend are both compile-time,
driven by cabal flags/conditionals in `LambdaHack.cabal` and CPP macros
(`USE_BROWSER`, `USE_WASM`, `USE_WASMFILE`, `USE_GHCJS`, `USE_JSFILE`) defined
once in the `options` common stanza and consumed throughout `engine-src` and
`GameDefinition/game-src/TieKnot.hs`:

- Native (default): SDL2 frontend (`Client/UI/Frontend/Sdl.hs`) plus an ANSI
  terminal frontend (`Frontend/ANSI.hs`, screen-reader friendly, via
  `--frontendANSI`) and a monochrome teletype frontend (`Frontend/Teletype.hs`,
  via `--frontendTeletype`, used by the Makefile playtests). File I/O via
  `Common/HSFile.hs`.
- `os(wasi)`: WASM build for the browser (current target). Frontend in
  `Frontend/Wasm.hs`; file storage via `Common/WasmFile.hs`; the executable is
  built as a wasm *reactor* — a persistent instance whose exports JS calls
  repeatedly — rather than a run-once command. A command's `_start` is the
  root the linker anchors dead-code elimination on; a reactor has none, so
  each JS-callable entry point (`lhStart`, `lhKey`, `lhWheel`, `lhMouseUp`)
  must be named explicitly with `-optl-Wl,--export=` or DCE strips it, and
  `-no-hs-main` drops the now-unused C `main` stub.
- `impl(ghcjs)`: defunct GHCJS build — dead code, since standalone GHCJS
  ended at GHC 8.10 and this repo requires 9.10+. `Frontend/Dom.hs` and
  `Common/JSFile.hs` stay in the tree as documented examples of an
  alternative frontend/file-backend pair (and as the historical origin the
  WASM port was written from); the remaining GHCJS wiring (`impl(ghcjs)`
  cabal stanzas, `ghcjs-options`, CPP branches) is scheduled for removal
  once WASM reaches SDL2 parity — see `docs/wasm-frontend-unified-plan.md`, R3.

The test-suite stanza shares the same CPP flags (so `USE_BROWSER`/`USE_WASM`
etc. are consistent across library/executable/test-suite) but is deliberately
excluded from the reactor linker treatment — tasty's normal
`exitcode-stdio` main doesn't fit that model.

Browser-build runtime differences: there is no argv and no config file on
disk — server/client options sit at their defaults and the UI config comes
from `config.ui.default`, embedded at compile time via TH (`rcfgUIDefault`
in `GameDefinition/Content/RuleKind.hs`), with user overrides read from
localStorage. Periodic autosave is disabled under the browser file backends
(`Server/LoopM.hs`) — saves happen only on explicit save/exit.

### Coding conventions (beyond hlint/stylish-haskell defaults)

Author-generic style conventions are collected in the portable-notes
section at the end of this file; what follows is LambdaHack-specific.

- Frontend code follows functional-core/imperative-shell: rendering and
  input *decisions* belong in shared pure modules under
  `Client/UI/Frontend/` (tested against fixtures), while frontend modules
  keep only event capture, output mutation and plumbing. The review
  question for any new line in a frontend module: would another frontend
  have to copy it? (The shared modules — `InputDecision`, `CellStyle`,
  `OverlayLayout` — are being established by the plan's Phases 0 and 2;
  until then the rule binds new code.)

## Gotchas

- Duplicate basenames: `Server/LoopM.hs` vs `Client/LoopM.hs`, and the
  engine's vs the game's `Client/UI/Content/Input.hs` (the key bindings are
  in the game's). Qualify paths when grepping or citing.
- The `Enum` instances of `Point` and `Vector` read a global dungeon width
  (`speedupHackXSize`, written once at startup in `TieKnot.hs`) — a
  deliberate, permanent performance hack; see the comment at `Point.hs:26`.
  Frontend code decodes screen indices with the explicit
  `punindex (rwidth coscreen)` instead of `toEnum` (one legacy violation
  at `Sdl.hs:590` awaits its scheduled fix — see the plan).
- Several frontends carry near-duplicate logic (SDL2, WASM, the dead Dom,
  ANSI, Teletype) — the prime local instance of the analogous-variant
  families that make single-file only/every/never generalizations
  treacherous (see the grep rule in the portable working-style notes).
- The pointman desync (next bullet) stayed hidden for years because one
  `sleader` writer sat inside an input primitive (`promptGetKey`) nobody
  suspected of mutating game-relevant state — the local cautionary tale
  for "this state cannot change here" assumptions.
- The pointman is denormalized: the authoritative `sleader` lives in client
  state, but many UI functions thread `ActorId` copies of it. Multi-actor
  runs rotate `sleader` through the party (`RunM.hs:90`) and `promptGetKey`
  silently restores the run leader when it interrupts macro playback
  (`FrameM.hs:157`), so a leader value held across an interactive wait can
  go stale — the root of the assertion disabled in commit 4a6eca154. Until
  the live-read design lands (see `docs/leader-desync-bug.md`), don't
  cache the pointman across a `promptGetKey` call; re-read `sleader`
  at the point of use.
- `noRunWithMulti` has three disjuncts (`SkMove` skill, `fspawnsFast`,
  `fhasPointman`); misreading it as two once produced a test fixture where
  the run-leader restore silently never fired.
- `updateCOpsAndCachedData` recomputes only the actor max-skills cache; a
  fixture that swaps tile content must rebuild `coTileSpeedup` itself
  (`Tile.speedupTile False cotile`).

## Orientation for new contributors

A good entry point for understanding command flow:
`GameDefinition/game-src/Client/UI/Content/Input.hs` (key → command bindings,
also drives auto-generated in-game help) leads to
`engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs` (how the UI client
interprets those commands). From there, see the Client-server architecture
section above for how a command becomes a `Request`/`Response` round trip.

## Portable notes: same author, same machine

Nothing in this section is LambdaHack-specific: it should hold for other
projects by the same author, in the same coding style, developed on the
same machine behind the same outer sandbox. Examples are from LambdaHack
unless attributed.

### Coding style

- Haddocks are expected on all module headers and on functions/types in
  "major"/interface modules. Minor internal helpers get no haddocks; their
  comments, if any, must not be haddocks and may describe implementation
  details and go out of date — don't treat every comment as authoritative
  documentation.
- Prefer assertions over comments to document invariants, unless that would
  be too verbose.
- `-fno-ignore-asserts` stays on in the cabal `common options` stanza, so
  failed `assert`s crash release builds too — crash reports from released
  code can name assertions.
- Lens libraries are deliberately avoided; state lives in plain records
  (with record punning).
- GHC2024 is the default language; each project's default-extensions live
  in the cabal `common options` stanza. Projects normally set `StrictData`
  — assume it unless the project's notes say otherwise.
- Formatting (also spelled out in the README's Coding style section):
  2-space indent, 80 columns, spaces not tabs, spurious whitespace avoided,
  spaces around arithmetic operators encouraged. Inline comments (`--`) are
  prefixed with exactly two spaces, unless indented to match other
  comments. Operators such as `(` and `,`, `<$>` and `<*>`, comment starts,
  etc. on consecutive lines either align or, if that would make lines too
  long, indent by 2 spaces from the previous indentation level. Generally,
  relax and stick to the style apparent in the file being edited.
- Put large, mechanical formatting changes in their own commit, separate
  from substantive changes.
- If hlint is still too naggy, adding more exceptions to `.hlint.yaml` is
  fine — don't contort code to appease it.
- **Uniformity across analogous positions is itself a review tool.**
  Parallel code (e.g. the near-duplicate logic across the SDL2, WASM,
  ANSI and Teletype frontends) and its comments should be identical
  modulo names and shapes; diffing analogous positions is how bugs
  surface, and a drifted one is normalized toward the cleaner form, not
  the first draft. One level up, things meant to be compared (benchmark
  variants, test cases) are designed as one-to-one counterparts —
  measuring the same stage, differing only along the compared axis,
  adjacent in the output — not accreted one probe at a time.
- **Order definitions as they are used, and let every summary span its
  whole subject.** Auxiliary definitions, do-bindings, list entries and
  top-level functions follow the order in which their consumers run,
  print or assert, wherever that order is deliberate or visible; an
  overview (module haddock, section comment) covers every member of what
  it describes, not the subset that existed when it was written. Both
  properties decay silently under accretion — after adding to a set,
  re-normalize the whole set, not just the new member.
- **One meaning per name, and label deliberate asymmetries.** A letter or
  abbreviation keeps a single meaning per vocabulary (in horde-ad: not
  `S`/`H` as both the pipelines and the gather orientations they produce,
  nor `c` as both concrete and contracted); and where uniformity is
  intentionally broken — a counterpart deliberately absent, a definition
  that is a fixture rather than a candidate — the site says so and why:
  an unexplained asymmetry reads as drift and costs a review round-trip.
- **Comments.** A substantial note that sibling sites would repeat with
  only names changed is stated once, at its canonical occurrence; tiny
  notes, by contrast, are repeated identically at every analogous
  position. Match the codebase's spelling and hyphenation (in horde-ad:
  "poor man's", not "poor-man's") and keep terminal punctuation consistent
  across parallel clauses (don't end one with ";" and its sibling with
  "."). A comment must still match the code after refactors — watch for
  notes invalidated by later changes. Leave pre-existing comments alone
  unless asked — flag them instead.
- In tests, an expected crash may never fire due to laziness: an assertion
  or lookup error is swallowed if the offending value is never forced (in
  LambdaHack this turned an expected dangling-`ActorId` crash into a silent
  arbitrary result). Catch real assert failures with
  `Control.Exception.try`, rather than pattern-matching on output; the
  `blame`/`swith` details (from the `assert-failure` package) go to the
  trace output, not into the exception.
- **Prove a self-checking assertion is non-vacuous.** An invariant check
  — e.g. horde-ad's verification that scatter is the adjoint of gather
  via `sdot0 (sgather x f) y == sdot0 x (sscatter y f)` — should be shown
  to actually fail when the property is deliberately broken, or it may be
  passing vacuously.
- Share the objective between a test and its benchmark via one exported
  helper, parameterized by what differs, so the benchmark provably
  measures what the test validates — share code, not configuration.

### Working style

- **Scope discipline.** On an ambiguous request ("the *new* tests") take
  the narrower reading, do it, and flag the boundary with an offer to
  expand — don't silently touch pre-existing code. Split unrelated work
  into separate PRs.
- **Verify before claiming done.** "Uniform" / "correct" / "passes" must
  rest on an actual line-by-line cross-check or a test run, not on the
  fact that it compiled; a claim about a touched file covers its
  pre-existing code too, so audit that as well.
- **Only/every/never claims must rest on repo-wide grep, not on the file
  where the pattern was first noticed** — analogous-variant families
  (e.g. the near-duplicate frontends) make single-file generalization
  treacherous. The same discipline applies before concluding "this cannot
  happen here": grep for *every* site that could do it.
- **Commits should be clean and logical, not a diary of the work.**
  File-partition them, order them so exports precede uses, and fold a
  follow-up refactor into the commit where the code is logically born
  rather than adding "add then move" churn.
- **Never push, or open/force-update a PR, without an explicit
  go-ahead.** Permission to make a change is not permission to publish
  it.
- **Record don't-do rulings next to the do's.** Refuted designs live in
  the working documents together with the evidence that killed them,
  precisely so they aren't re-proposed later; when a new idea dies to
  evidence, write the ruling down where the next reader will look. (In
  LambdaHack: the recorded decisions, including don't-do rulings, in
  `docs/wasm-frontend-unified-plan.md`.)
- **Drafting GitHub-bound texts**: one file per destination (issue, its
  design comment, PR description, upstream PR), staged in the repo root
  until posted; keep the *design* implementation-ignorant and put
  measured results in the PR description; deliberate overlap between
  files is fine to make each self-contained. Don't attribute design
  intent to code that is merely generic ("assumes irregular indexing") —
  state observed granularity and cost, not motives. Prefer
  reference-style markdown links to keep the prose readable. Wrapping is
  per destination: GitHub renders single newlines as hard `<br>` in
  issue/PR/comment *bodies* (though not in rendered `.md` files), so a
  draft meant to be pasted into an issue body, PR description or comment
  is kept *unwrapped* — one long line per paragraph (blank line between
  paragraphs; tables, code fences, list items, reference-link
  definitions and `Co-Authored-By` trailers each still on their own
  line) — so the browser soft-wraps it, and it is never re-wrapped to 80
  columns; a doc read as a repo file (e.g. README, the docs/ documents)
  keeps that file's normal wrapping instead.
- **Links that cite source code — in GitHub-bound texts and the
  reference documents — must be GitHub permalinks pinned to a commit
  hash that is on `master`** (`…blob/<commit>/….hs#L12-L34`): branch-name
  and unpinned links drift or die when branches move, while a master hash
  survives rebases and stays verifiable — the citation checker validates
  such permalinks against the pinned commit. Deliberately-living
  whole-file links (e.g. the README's `blob/master` pointers) are outside
  the rule, as are links to foreign repos, which the checker cannot
  verify.
- **State mathematical properties in the code's surface notation, not
  abstract math** (in docs and comments alike). E.g. (from horde-ad)
  write the gather/scatter adjoint law as
  `sdot0 (sgather x f) y == sdot0 x (sscatter y f)`, not
  `⟨gather x, y⟩ = ⟨x, scatter y⟩`: keep the index function `f` explicit
  rather than hidden in the operator name, and put quantifiers first
  (*for all `f`, `x`, `y`*). It reads in the vocabulary of the code and
  keeps the shapes checkable.

### Document verification: the three-pass discipline

Planning/reference documents (this file, the three `docs/` documents
listed at the top and any GitHub-bound drafts staged in the repo root)
make checkable claims. Whenever such a document (or code it cites) is
edited, run three passes:

1. `python3 tools/check-plan-citations.py [DOC]` (from the repo root) —
   re-validates `file:line` citations, resolving each, checking the line
   range and printing the first cited line to eyeball against the
   surrounding claim, and pinned GitHub permalinks against the commit
   they name via `git show`, so those never drift; foreign-repo links
   can't be verified locally. DOC defaults to this file; pass
   `docs/wasm-frontend-unified-plan.md`, `docs/leader-desync-bug.md` or
   `docs/promptgetkey-hygiene.md` to check those.
2. Check that paths, Makefile targets and flags named in prose exist.
3. Re-verify only/every/never claims by repo-wide grep.

The passes are ordered by yield: the mechanical ones catch drift, but the
quantified-claims grep has found real, long-standing errors every time it
has been run, so it is the pass never to skip.

A fourth pass, the **heading-scope check**, applies whenever a document's
heading structure is edited — adding, moving, removing or re-levelling a
heading. Markdown nests every block under the nearest preceding heading
until an equal-or-higher one appears, so a heading edit silently rescopes
the trailing content. Re-derive the outline with `python3
tools/heading-outline.py DOC...` (handles ATX and Setext, code-fence-aware)
and, block by block, confirm each sits under a heading it is actually
about, at the right level. The tell is a topic that shifts mid-section with
no heading change — e.g. a new last subsection quietly swallowing the
sections meant to follow it, or content left dangling under a heading a
level too deep.

### Sandboxing on the dev machine (outer wrapper + inner sandbox)

Claude Code sessions on Mikolaj's machine run inside an outer bwrap
sandbox wrapper (PID 1 is `bwrap`), beyond Claude Code's own (inner)
sandbox. Current state and its implications:

- Run `git` writes, `cabal` and `gpg` unsandboxed — the inner sandbox
  mounts `.git/config`, `~/.cabal` and `~/.gnupg` read-only.
  `git checkout -b` / `branch -f` fail to lock the config, though the ref
  often moves anyway — verify refs afterwards. GPG signing
  (`commit.gpgsign` is on) fails sandboxed and works unsandboxed, so
  never fall back to `--no-gpg-sign`; SSH pushes likewise work only
  unsandboxed. Other read-only HOME mounts (e.g. `~/.claude/projects`)
  also need unsandboxed commands for deletions.
- An unsandboxed (or otherwise permission-gated) command sits at the
  approval prompt until answered — on the user's screen indistinguishable
  from a hung long-running command — so before issuing an optional or
  expensive one (a haddock run, an extra rebuild), say what is about to
  appear.
- Paths the wrapper blocks report "No such file or directory", not
  "Permission denied". If a path outside the repo seems missing —
  especially one documented as expected, like the `../lambdahack.github.io`
  sibling checkout — suspect the wrapper and ask, rather than record the
  path as absent. `dangerouslyDisableSandbox` bypasses only the inner
  sandbox, never the wrapper.
- The wrapper mounts a hand-picked, changeable subset of HOME, curated
  per-path rather than per-directory: that an entry is visible says
  nothing about how much of its contents is (at times `~/r` has held
  only the current repo, and `~/.ssh` carries public material only).
  Don't infer access from a directory listing and don't record mount
  inventories — they go stale; verify the specific path at the moment it
  matters. One standing casualty: `~/.ghc-wasm` is not mounted (as of
  2026-07), so LambdaHack's WASM targets (`make
  build-wasm`/`build-ts`/`test-wasm` source `~/.ghc-wasm/env`) fail with
  "cannot open ... No such file" even unsandboxed; they need the wrapper
  extended or a plain terminal.
- Inside the inner sandbox, HOME appears to be the repo root, so sandboxed
  `git status`/`ls` show phantom untracked dotfiles (`.bashrc`,
  `.gitconfig`, `.vscode`, ...) that don't exist on the real filesystem.
  Ignore them; verify with an unsandboxed command before deleting any.
- System state under wrapper-hidden paths (e.g. `/etc/apparmor.d`) cannot
  be inspected from inside a session; such diagnosis must be done by
  Mikolaj in a plain terminal.
- The nested inner sandbox works only because three things hold: the
  Ubuntu AppArmor userns restriction is off
  (`kernel.apparmor_restrict_unprivileged_userns=0`, no
  `bwrap-userns-restrict` profile), the wrapper mounts the repo
  read-write, and `enableWeakerNestedSandbox: true` is in effect —
  inherited from the `sandbox` block of the user-level
  `~/.claude/settings.json` (a project `sandbox` block wholesale
  replaces the user-level one — settings objects don't deep-merge — so
  a project that defines one must repeat the flag). If sandboxed
  commands start failing at startup, check these three first.

### Git and GitHub in sessions

- Interactive git is unavailable (tool commands run without a TTY, so
  editor and prompt loops hang): no `rebase -i`, no `add -i`. Rewrite
  history with `git reset --mixed <base>` + re-`add`/`commit` per file
  group, reusing messages via `git commit -C <hash>` / `-F <file>`.
- Amending a non-HEAD commit (no `rebase -i`): save working-tree edits as
  a patch, `git reset --hard <target>` (spares untracked files), apply,
  `--amend`, then `git cherry-pick` the successors (conflict-free when
  they don't touch the amended files); update recorded hashes.
- The repo root accumulates many untracked scratch files (`log*`,
  `*.prof`, `cabal.project.local.bkp*`, `.emacs.desktop*`, etc.); leave
  them alone and never `git add` them wholesale.
- `gh` is not authenticated; for GitHub reads use `curl` against
  `api.github.com` (whitelisted in horde-ad's sandbox network config —
  verify it is here too).

### Build and shell tooling in sessions

- `awk` works through `~/.local/bin/awk → mawk` (added 2026-07-21):
  `/usr/bin/awk` is an alternatives symlink that dangles in sessions,
  `/etc/alternatives` being wrapper-hidden. `hlint` and
  `stylish-haskell` are on PATH (via `~/.cabal/bin`) and work in-session,
  sandboxed included — run both on touched `.hs` files before committing;
  code written outside the editor misses its on-save stylish pass.
- To combine several tasty `-p` patterns, tasty wants awk-style syntax:
  `-p "/foo/ || /bar/"`.
- GHC emits warnings only on *recompilation*: a cached, up-to-date build
  can hide warnings (e.g. `-Wredundant-constraints`) that a full rebuild
  would surface — don't infer "no warnings" from a clean second build.
- Keep one set of cabal flags across a session — changing flags (e.g.
  toggling `--enable-optimization` or `--enable-profiling`) forces a full
  rebuild of the local packages, though each flag set's dependency builds
  stay cached in the store. Pass such flags on the command line rather
  than editing `cabal.project.local`.
- The `/tmp` scratchpad is wiped by a machine restart — put anything that
  must survive (patches, notes) in the repo tree; binaries just get
  rebuilt.
- Exact dependency sources are always available: unpack
  `~/.cabal/packages/hackage.haskell.org/<pkg>/<ver>/` matching
  `dist-newstyle/cache/plan.json`.
- `.github/workflows/haskell-ci.yml` is generated by `haskell-ci` from
  the `.cabal` file — regenerate it rather than hand-editing, and
  re-apply any hand-maintained steps afterwards (in horde-ad: the `tests`
  and `benchmarks` steps, marked by a comment in the workflow).
- Toggle-based A/B builds: make a rule's guard unsatisfiable (e.g.
  `x /= x` on some scrutinee), build and copy the binary aside, restore +
  rebuild + copy again, then run the two preserved binaries in
  interleaved pairs (no rebuild between A and B).
