# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository. Humans read it too, arriving from the file listing rather than from `README.md`, which doesn't link here by design — so don't "fix" that by adding a link. Read README first: this file assumes it and doesn't retell its tour of the codebase. The coding-style rules are the deliberate exception, restated in the portable notes below so a session has them resident; README is where a human should read them.

File:line references were verified against the tree at commit `1d55df2a4` (2026-07-29); the citation pass proves a cited line exists — this stamp, that it still says what the claim around it needs.

## What this is

LambdaHack is a Haskell game engine library for ASCII roguelike games, bundled with a sample dungeon crawler (`GameDefinition/`). It targets native (SDL2/ANSI/teletype frontends) and WASM (the browser build); a defunct GHCJS browser build survives only as dead example code (see the Frontends section below). The TypeScript browser-side harness lives in `ts-src/`; built WASM artifacts are deployed into the `lambdahack.github.io` repo, expected as a sibling checkout at `../lambdahack.github.io` (visible to sessions only when the wrapper mounts it — sandboxing notes at the end of this file).

## Where to look next

Frontend work is planned in `docs/wasm-frontend-unified-plan.md`: the SDL2-parity roadmap for the WASM frontend, recorded decisions — including don't-do rulings, so ideas aren't re-proposed — and verified repo facts with file:line citations. Consult it before frontend-touching changes.

The closing portable-notes section holds the author-generic conventions and the machine-specific session facts — skim it before debugging anything environment-related.

## Build

```
# one-time setup of a fast dev build: no optimization, expensive
# assertions on, tests enabled; guarded not to clobber an existing config
[ -f cabal.project.local ] ||
  cp cabal.project.local.development cabal.project.local
cabal build
```

Run `cabal` unsandboxed (sandboxing notes below). A full build takes long: give Bash a generous timeout or run it in the background rather than concluding it hung.

Run the sample game (opens an SDL2 window, so for a human at a display — not from a headless session):

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

Doctests are a manual-only recipe, not run in CI (closing that gap is part of the plan's R2). The README also lists a `definition` doctest component, but that internal library only exists in the original `LambdaHack.cabal.bkp` — the flattened `LambdaHack.cabal` in use has a single library, so this covers everything:

```
cabal install doctest --ignore-project --overwrite-policy=always && cabal build
cabal repl --build-depends=QuickCheck --build-depends=template-haskell --with-compiler=doctest --repl-options='-w -Wdefault' lib:LambdaHack
```

### Haskell unit-test harness (`test/`)

The hard-won facts for writing and driving tests with this harness are in `test/CLAUDE.md` — read it when working under `test/`, where a session loads it automatically.

### WASM and TypeScript test suites

The WASM test suite drives the compiled wasm test binary through Node/wasmtime via `ts-src/run-wasm-test.mjs`; the TypeScript tests run vitest over `ts-src/`:

```
make test-wasm
make test-ts
```

### Playtests and headless runs

The Makefile's battery of automated AI-vs-AI playtest and benchmark targets (`make test-short`, `test-medium`, `test-gha`, `frontend*`, `bench*`) and the headless test-frontend flags are covered by the `playtests` skill — read `.claude/skills/playtests/SKILL.md`, in this repo. Those targets play out whole games — expect minutes, not seconds, and set Bash timeouts accordingly; the `frontend*` targets open an SDL2 window, so they are for a human at a display.

`LambdaHack --help` lists all debug options. Of these, `--sniff` (verbose, initially cryptic) prints the client-server traffic — useful when debugging the request/response flow.

When stdout is not a terminal — tool-driven sessions, CI — the main game executable redirects its own stdout and stderr to `~/.LambdaHack/stdout.txt` and `~/.LambdaHack/stderr.txt` (`GameDefinition/Main.hs`, a Windows desktop-launch workaround), superseding any shell redirection. Such runs look completely silent: the handles are duplicated at startup, so everything the process writes to either stream from then on — the `--benchmark` report, `+RTS -s` summaries, teletype/debug output — lands in those two files, freshly overwritten on each launch; harvest them between runs.

### Lint and format

Two tools: `hlint .` (using the liberal `.hlint.yaml`) and stylish-haskell (`.stylish-haskell.yaml`; the editor runs it on save). The `arguments:` in`.hlint.yaml` pass `--cpp-simple` (keeps the `#ifdef`-guarded code — the expensive assertions, the wasm branches — visible to the linter; hlint's default no-defines CPP pass would drop it, un-linting it and producing false positives in what remains) and `-XNoStarIsType` (as in horde-ad; honored by the hlint on PATH, built from hlint master though it self-reports v3.10 — earlier binaries mishandled the flag).

### CI

Two workflows: `.github/workflows/haskell-ci.yml` (auto-generated by `haskell-ci`; runs `cabal build`/`cabal test`/`haddock` across GHC versions) and `.github/workflows/lint-and-playtest.yml` (hand-written; runs `hlint .` with `JSFile.hs` ignore-globbed, plus the `make test-gha` playtests).

### Standing checks

For a generic "run checks" request — each with its trigger:

- Always, over every `.md` document in the repo (`git ls-files '*.md'` plus untracked drafts, minus `CHANGELOG.md`, a historical record rather than a live claim set):
  - the mechanical passes, `python3 tools/check-plan-citations.py DOC`, `python3 tools/check-doc-refs.py DOC` and, where the document shows example code, `python3 tools/check-doc-examples.py DOC`;
  - the remaining passes, over any document edited since it was last verified — pass 3, the quantified-claims grep, is the one that keeps finding real errors.

  The `doc-verification` skill holds them and their order. It is user-scope, so it applies in every project and its only copy lives outside this repo, at `~/.claude/skills/doc-verification/SKILL.md`: a reader who wants the passes themselves must open it there. `tools/heading-outline.py` belongs to the heading-scope pass, which fires only when a document's heading structure changes, so it is not in the always-run set above.
- When Haskell code changed: build, then `cabal test` (the Makefile playtests when the change warrants), and stylish-haskell and hlint on touched files (both run in-session, sandboxed included; code written outside the editor misses its on-save stylish pass, so run them by hand). Hlint should report no suggestions and no errors when run with `--ignore-glob "engine-src/Game/LambdaHack/Common/JSFile.hs"` (unsupported `JavaScriptFFI` extension).
- After a push: CI status via `curl -s` against `api.github.com`, path `/repos/LambdaHack/LambdaHack/actions/runs?branch=BRANCH&per_page=5`, reading each run's `head_sha`, `status` and `conclusion` (`gh` is unauthenticated); both workflows above report there, so expect two runs per push.
- When a new self-checking assertion or checker tool is born: prove it non-vacuous by deliberately breaking it (portable notes), and record the proof next to it.

## Architecture

### Three source trees, one library

The `LambdaHack` library stanza in `LambdaHack.cabal` combines three source trees, all under module namespace `Game.LambdaHack.*` (except `GameDefinition`, which is unprefixed):

- `definition-src/` — pure content-definition data (`Content.*Kind`, `Definition.*`). No game logic, no IO.
- `engine-src/Game/LambdaHack/` — the actual engine: `Atomic/` (state-changing command representation), `Client/` (UI + AI client logic), `Common/` (shared types/state), `Server/` (game arbiter, dungeon generation, FOV).
- `GameDefinition/` — the *sample game's* concrete content (module namespace `Content.*`) plus its client wiring and the `Implementation.Monad{Client,Server}Implementation` modules that pick concrete monad transformer stacks for the abstract client/server monads. `TieKnot.hs` wires content + engine + frontend into a runnable game (`tieKnot`/`tieKnotForAsync`); `Main.hs` is the executable entry point.

Separately, `ts-src/` (not part of the Haskell library) holds the TypeScript browser-side harness for the WASM build and `run-wasm-test.mjs`, the Node driver for `make test-wasm`.

**Module-as-interface convention** (stated in the .cabal description): if a module has the same name as a directory (e.g. `Game.LambdaHack.Client` vs. `Game.LambdaHack.Client.*`), that module is the *exclusive* interface to everything in the directory — other modules in the library must not reach past it into the directory's internals. This is enforced by convention, not by `.cabal`-exposed/hidden boundaries (nearly all modules are exposed so that downstream games can override things — the only hidden ones are the file-backend module compiled into this build, `HSFile`/`WasmFile`/`JSFile`, and `Paths_LambdaHack`), so respect it when adding imports.

### Client-server architecture

The engine is strictly split, with types enforcing the separation, into:

- **Server** (`Server/`): the sole authority over full game state; never trusts clients.
- **Clients** (`Client/`): one per faction. Each client only sees its own perception-limited view of the world. A client may be UI-driven (human, via `Client/UI/`) or AI-driven (`Client/AI/`); the human/UI client is a client like any other, just with extra `Client.UI` capabilities layered on.

Flow: the UI client turns keystrokes into `HumanCmd`s (`Client/UI/HumanCmd.hs`, interpreted in `HandleHumanM.hs`/ `HandleHumanGlobalM.hs`/`HandleHumanLocalM.hs`). Purely local UI actions resolve in the client; anything that would change game state becomes a `Request` sent to the server. The server validates it (rejecting with a `ReqFailure` if impossible, e.g. moving into a wall) or applies it and broadcasts the resulting atomic updates (`UpdAtomic`/`SfxAtomic`, see `Atomic/`), wrapped in `Response`s, to every client that can perceive the event (`Response` also carries the server's queries prompting a client for its next move). AI clients run the same request/response loop, generating requests from perceived state via `Client/AI/` (`PickActorM`, `PickTargetM`, `PickActionM`, `Strategy`).

Six command datatypes recur across the codebase, each with its own interpreters (mostly in `Handle*` modules): `HumanCmd`, `Effect`, `UpdAtomic`, `Request` (a family: `ReqUI`/`ReqAI`/`RequestTimed`, in `Client/Request.hs`), `Response`, `FrontReq`. Most command semantics live in custom monads (`MonadClient`, `MonadServer`, `MonadClientUI`, etc.) — these are state monads, so a command's semantics is a state transformer plus side effects (e.g. frontend drawing). Each monad's concrete transformer stack is chosen in the `*Implementation` modules (`GameDefinition/game-src/Implementation/`), not in the engine itself — the engine only depends on the abstract monad classes.

Naming mismatch: the in-game/UI concept called "pointman" is called `leader` in the source code (and there are a few more such mismatches). Keep source-code naming and UI naming each internally consistent, but don't expect them to match each other.

### Frontends and build backends

Frontend selection and native-vs-browser backend are both compile-time, driven by cabal flags/conditionals in `LambdaHack.cabal` and CPP macros (`USE_BROWSER`, `USE_WASM`, `USE_WASMFILE`, `USE_GHCJS`, `USE_JSFILE`) defined once in the `options` common stanza and consumed throughout `engine-src` and `GameDefinition/game-src/TieKnot.hs`:

- Native (default): SDL2 frontend (`Client/UI/Frontend/Sdl.hs`) plus an ANSI terminal frontend (`Frontend/ANSI.hs`, screen-reader friendly, via `--frontendANSI`) and a monochrome teletype frontend (`Frontend/Teletype.hs`, via `--frontendTeletype`, used by the Makefile playtests). File I/O via `Common/HSFile.hs`.
- `os(wasi)`: WASM build for the browser (current target). Frontend in `Frontend/Wasm.hs`; file storage via `Common/WasmFile.hs`; the executable is built as a wasm *reactor* — a persistent instance whose exports JS calls repeatedly — rather than a run-once command. A command's `_start` is the root the linker anchors dead-code elimination on; a reactor has none, so each JS-callable entry point (`lhStart`, `lhKey`, `lhWheel`, `lhMouseUp`) must be named explicitly with `-optl-Wl,--export=` or DCE strips it, and `-no-hs-main` drops the now-unused C `main` stub.
- `impl(ghcjs)`: defunct GHCJS build — dead code, since standalone GHCJS ended at GHC 8.10 and this repo requires 9.10+. `Frontend/Dom.hs` and `Common/JSFile.hs` stay in the tree as documented examples of an alternative frontend/file-backend pair (and as the historical origin the WASM port was written from); the remaining GHCJS wiring (`impl(ghcjs)` cabal stanzas, `ghcjs-options`, CPP branches) is scheduled for removal once WASM reaches SDL2 parity — see `docs/wasm-frontend-unified-plan.md`, R3.

The test-suite stanza shares the same CPP flags (so `USE_BROWSER`/`USE_WASM` etc. are consistent across library/executable/test-suite) but is deliberately excluded from the reactor linker treatment — tasty's normal `exitcode-stdio` main doesn't fit that model.

Browser-build runtime differences: there is no argv and no config file on disk — server/client options sit at their defaults and the UI config comes from `config.ui.default`, embedded at compile time via TH (`rcfgUIDefault` in `GameDefinition/Content/RuleKind.hs`), with user overrides read from localStorage. Periodic autosave is disabled under the browser file backends (`Server/LoopM.hs`) — saves happen only on explicit save/exit.

### Coding conventions (beyond hlint/stylish-haskell defaults)

Author-generic style conventions are collected in the portable-notes section at the end of this file; what follows is LambdaHack-specific.

- Frontend code follows functional-core/imperative-shell: rendering and input *decisions* belong in shared pure modules under `Client/UI/Frontend/` (tested against fixtures), while frontend modules keep only event capture, output mutation and plumbing. The review question for any new line in a frontend module: would another frontend have to copy it? (The shared modules — `InputDecision`, `CellStyle`, `OverlayLayout` — are being established by the plan's Phases 0 and 2; until then the rule binds new code.)

## Gotchas

- Duplicate basenames: `Server/LoopM.hs` vs `Client/LoopM.hs`, and the engine's vs the game's `Client/UI/Content/Input.hs` (the key bindings are in the game's). Qualify paths when grepping or citing.
- The `Enum` instances of `Point` and `Vector` read a global dungeon width (`speedupHackXSize`, written once at startup in `TieKnot.hs`) — a deliberate, permanent performance hack; see the comment at `Point.hs:26`. Frontend code decodes screen indices with the explicit `punindex (rwidth coscreen)` instead of `toEnum` (one legacy violation at `Sdl.hs:590` awaits its scheduled fix — see the plan).
- Several frontends carry near-duplicate logic (SDL2, WASM, the dead Dom, ANSI, Teletype) — the prime local instance of the analogous-variant families the portable notes' grep rule warns about.
- The pointman desync (next bullet) stayed hidden for years because one `sleader` writer sat inside an input primitive (`promptGetKey`) nobody suspected of mutating game-relevant state — the local cautionary tale for "this state cannot change here" assumptions.
- The pointman is denormalized: the authoritative `sleader` lives in client state, but many UI functions thread `ActorId` copies of it. Multi-actor runs rotate `sleader` through the party (`RunM.hs:90`) and `promptGetKey` silently restores the run leader when it interrupts macro playback (`FrameM.hs:155`), so a leader value held across an interactive wait can go stale — the root of the assertion disabled in commit 4a6eca154. Don't cache the pointman across a `promptGetKey` call; re-read `sleader` at the point of use.
- `noRunWithMulti` has three disjuncts (`Faction.hs:143-151`): the `SkMove` skill, `bannedPointmanSwitchBetweenLevels` — which is `fspawnsFast`, one line below — and `fhasPointman`. Misreading it as two once produced a test fixture where the run-leader restore silently never fired.
- `updateCOpsAndCachedData` recomputes only the actor max-skills cache; a fixture that swaps tile content must rebuild `coTileSpeedup` itself (`Tile.speedupTile False cotile`).

## Portable notes: same author, same machine

Nothing in this section is LambdaHack-specific: it should hold for other projects by the same author, in the same coding style, developed on the same machine behind the same outer sandbox. Examples are from LambdaHack unless attributed.

### Coding style

- Haddocks are expected on all module headers and on functions/types in "major"/interface modules. Minor internal helpers get no haddocks; their comments, if any, must not be haddocks and may describe implementation details and go out of date — don't treat every comment as authoritative documentation.
- Prefer assertions over comments to document invariants, unless that would be too verbose.
- `-fno-ignore-asserts` stays on in the cabal `common options` stanza, so failed `assert`s crash release builds too — crash reports from released code can name assertions.
- Lens libraries are deliberately avoided; state lives in plain records (with record punning).
- GHC2024 is the default language; each project's default-extensions live in the cabal `common options` stanza. Projects normally set `StrictData` — assume it unless the project's notes say otherwise.
- Formatting: 2-space indent, 80 columns, spaces not tabs, spurious whitespace avoided, spaces around arithmetic operators encouraged. Inline comments (`--`) are prefixed with exactly two spaces, unless indented to match other comments. Operators such as `(` and `,`, `<$>` and `<*>`, comment starts, etc. on consecutive lines either align or, if that would make lines too long, indent by 2 spaces from the previous indentation level. Generally, relax and stick to the style apparent in the file being edited.
- Put large, mechanical formatting changes in their own commit, separate from substantive changes.
- If hlint is still too naggy, adding more exceptions to `.hlint.yaml` is fine — don't contort code to appease it.
- **Uniformity across analogous positions is itself a review tool.** Parallel code (e.g. the near-duplicate frontends) and its comments should be identical modulo names and shapes; diffing analogous positions is how bugs surface, and a drifted one is normalized toward the cleaner form, not the first draft. One level up, things meant to be compared (benchmark variants, test cases) are designed as one-to-one counterparts — measuring the same stage, differing only along the compared axis, adjacent in the output — not accreted one probe at a time.
- **Order definitions as they are used, and let every summary span its whole subject.** Auxiliary definitions, do-bindings, list entries and top-level functions follow the order in which their consumers run, print or assert, wherever that order is deliberate or visible; an overview (module haddock, section comment) covers every member of what it describes, not the subset that existed when it was written. Both properties decay silently under accretion — after adding to a set, re-normalize the whole set, not just the new member.
- **One meaning per name, and label deliberate asymmetries.** A letter or abbreviation keeps a single meaning per vocabulary (in horde-ad: not `S`/`H` as both the pipelines and the gather orientations they produce, nor `c` as both concrete and contracted); and where uniformity is intentionally broken — a counterpart deliberately absent, a definition that is a fixture rather than a candidate — the site says so and why: an unexplained asymmetry reads as drift and costs a review round-trip.
- **Comments.** A substantial note that sibling sites would repeat with only names changed is stated once, at its canonical occurrence; tiny notes, by contrast, are repeated identically at every analogous position. Match the codebase's spelling and hyphenation (in horde-ad: "poor man's", not "poor-man's") and keep terminal punctuation consistent across parallel clauses (don't end one with ";" and its sibling with "."). A comment must still match the code after refactors — watch for notes invalidated by later changes. Leave pre-existing comments alone unless asked — flag them instead.
- In tests, an expected crash may never fire due to laziness: an assertion or lookup error is swallowed if the offending value is never forced (in LambdaHack this turned an expected dangling-`ActorId` crash into a silent arbitrary result). Catch real assert failures with `Control.Exception.try`, rather than pattern-matching on output; the `blame`/`swith` details (from the `assert-failure` package) go to the trace output, not into the exception.
- **Prove a self-checking assertion is non-vacuous.** An invariant check — e.g. horde-ad's verification that scatter is the adjoint of gather via `sdot0 (sgather x f) y == sdot0 x (sscatter y f)` — should be shown to actually fail when the property is deliberately broken, or it may be passing vacuously.
- Share the objective between a test and its benchmark via one exported helper, parameterized by what differs, so the benchmark provably measures what the test validates — share code, not configuration.

### Working style

- **Scope discipline.** On an ambiguous request ("the *new* tests") take the narrower reading, do it, and flag the boundary with an offer to expand — don't silently touch pre-existing code. Split unrelated work into separate PRs.
- **Verify before claiming done.** "Uniform" / "correct" / "passes" must rest on an actual line-by-line cross-check or a test run, not on the fact that it compiled; a claim about a touched file covers its pre-existing code too, so audit that as well.
- **Only/every/never claims must rest on repo-wide grep, not on the file where the pattern was first noticed** — analogous-variant families (e.g. the near-duplicate frontends) make single-file generalization treacherous. The same discipline applies before concluding "this cannot happen here": grep for *every* site that could do it.
- **Prove a search non-vacuous before trusting its silence.** A grep that finds nothing has proved nothing until it is known to find something: run it first against a case known to be present. Two wrong answers in one session came of skipping that — `grep '^??'` over colourised `git status` output (the git notes below), and concluding a note was absent from every document after grepping one of its phrasings. It is the rule already stated for assertions and for checkers, applied to searches — and it reaches further than greps: a checker branch with no live control in this repo is a silent search too, which is why the tools under `tools/` name the branches their own recipes cannot exercise here.
- **Commits should be clean and logical, not a diary of the work.** File-partition them, order them so exports precede uses, and fold a follow-up refactor into the commit where the code is logically born rather than adding "add then move" churn.
- **Never push, or open/force-update a PR, without an explicit go-ahead.** Permission to make a change is not permission to publish it.
- **Record don't-do rulings next to the do's.** Refuted designs live in the working documents together with the evidence that killed them, precisely so they aren't re-proposed later; when a new idea dies to evidence, write the ruling down where the next reader will look. (In LambdaHack: the recorded decisions, including don't-do rulings, in `docs/wasm-frontend-unified-plan.md`.)
- **Drafting GitHub-bound texts.**
  - One file per destination (issue, its design comment, PR description, upstream PR), staged in the repo root until posted.
  - Keep the *design* implementation-ignorant and put measured results in the PR description.
  - Deliberate overlap between files is fine, to make each self-contained.
  - Don't attribute design intent to code that is merely generic ("assumes irregular indexing") — state observed granularity and cost, not motives.
  - Prefer reference-style markdown links to keep the prose readable.
  - Wrapping is per destination: GitHub renders single newlines as hard `<br>` in issue/PR/comment *bodies* (though not in rendered `.md` files), so a draft meant to be pasted into an issue body, PR description or comment is kept *unwrapped* — one long line per paragraph (blank line between paragraphs; tables, code fences, list items, reference-link definitions and `Co-Authored-By` trailers each still on their own line) — so the browser soft-wraps it, and it is never re-wrapped to 80 columns; a doc read as a repo file (e.g. README, the docs/ documents) keeps that file's normal wrapping instead.
- **Links that cite source code — in GitHub-bound texts and the reference documents — must be GitHub permalinks pinned to a commit hash that is on `master`** (`…blob/<commit>/….hs#L12-L34`): branch-name and unpinned links drift or die when branches move, while a master hash survives rebases and stays verifiable — the citation checker validates such permalinks against the pinned commit. Deliberately-living whole-file links (e.g. the README's `blob/master` pointers) are outside the rule, as are links to foreign repos, which the checker cannot verify.
- **State mathematical properties in the code's surface notation, not abstract math** (in docs and comments alike). E.g. (from horde-ad) write the gather/scatter adjoint law as `sdot0 (sgather x f) y == sdot0 x (sscatter y f)`, not `⟨gather x, y⟩ = ⟨x, scatter y⟩`: keep the index function `f` explicit rather than hidden in the operator name, and put quantifiers first (*for all `f`, `x`, `y`*). It reads in the vocabulary of the code and keeps the shapes checkable.

### Sandboxing on the dev machine (outer wrapper + inner sandbox)

Claude Code sessions on Mikolaj's machine run inside an outer bwrap sandbox wrapper (PID 1 is `bwrap`), beyond Claude Code's own (inner) sandbox. Current state and its implications:

- Run `git` writes, `cabal` and `gpg` unsandboxed — the inner sandbox mounts `.git/config`, `~/.cabal` and `~/.gnupg` read-only. `git checkout -b` / `branch -f` fail to lock the config, though the ref often moves anyway — verify refs afterwards. GPG signing (`commit.gpgsign` is on) fails sandboxed and works unsandboxed, so never fall back to `--no-gpg-sign`; SSH pushes likewise work only unsandboxed. Other read-only HOME mounts (e.g. `~/.claude/projects`) also need unsandboxed commands for deletions.
- An unsandboxed (or otherwise permission-gated) command sits at the approval prompt until answered — on the user's screen indistinguishable from a hung long-running command — so before issuing an optional or expensive one (a haddock run, an extra rebuild), say what is about to appear.
- Paths the wrapper blocks report "No such file or directory", not "Permission denied". If a path outside the repo seems missing — especially one documented as expected, like the `../lambdahack.github.io` sibling checkout — suspect the wrapper and ask, rather than record the path as absent. `dangerouslyDisableSandbox` bypasses only the inner sandbox, never the wrapper. `tools/check-doc-refs.py` encodes the same rule: it resolves sibling checkouts when present and *stops* (exit 2) when one is not, rather than falling back to a weaker check, so a BLOCKED run means ask for a mount, not work around it.
- The wrapper mounts a hand-picked, changeable subset of HOME, curated per-path rather than per-directory: that an entry is visible says nothing about how much of its contents is (at times `~/r` has held only the current repo, and `~/.ssh` carries public material only). Don't infer access from a directory listing and don't record mount inventories — they go stale; verify the specific path at the moment it matters.
  - One standing casualty: `~/.ghc-wasm` is not mounted (as of 2026-07), so LambdaHack's WASM targets (`make build-wasm`/`build-ts`/`test-wasm` source `~/.ghc-wasm/env`) fail with "cannot open ... No such file" even unsandboxed; they need the wrapper extended or a plain terminal.
- Inside the inner sandbox, HOME appears to be the repo root, so sandboxed `git status`/`ls` show phantom untracked dotfiles (`.bashrc`, `.gitconfig`, `.vscode`, ...) that don't exist on the real filesystem. Ignore them; verify with an unsandboxed command before deleting any.
- System state under wrapper-hidden paths (e.g. `/etc/apparmor.d`) cannot be inspected from inside a session; such diagnosis must be done by Mikolaj in a plain terminal.
- The nested inner sandbox works only because three things hold; if sandboxed commands start failing at startup, check these first:
  - the Ubuntu AppArmor userns restriction is off (`kernel.apparmor_restrict_unprivileged_userns=0`, no `bwrap-userns-restrict` profile);
  - the wrapper mounts the repo read-write;
  - `enableWeakerNestedSandbox: true` is in effect — inherited from the `sandbox` block of the user-level `~/.claude/settings.json` (a project `sandbox` block wholesale replaces the user-level one — settings objects don't deep-merge — so a project that defines one must repeat the flag).

### Git and GitHub in sessions

- `git status --short` is colourised even through a pipe — the bytes are `^[[31m??^[[m path` — so `grep '^??'` silently matches nothing. Use `git status --porcelain`, which is defined as machine-readable and is never colourised.
- Interactive git is unavailable (tool commands run without a TTY, so editor and prompt loops hang): no `rebase -i`, no `add -i`. Rewrite history with `git reset --mixed <base>` + re-`add`/`commit` per file group, reusing messages via `git commit -C <hash>` / `-F <file>`.
- Amending a non-HEAD commit (no `rebase -i`): save working-tree edits as a patch, `git reset --hard <target>` (spares untracked files), apply, `--amend`, then `git cherry-pick` the successors (conflict-free when they don't touch the amended files); update recorded hashes.
- The repo root accumulates many untracked scratch files (`log*`, `*.prof`, `cabal.project.local.bkp*`, `.emacs.desktop*`, etc.); leave them alone and never `git add` them wholesale.
- `gh` is not authenticated; for GitHub reads use `curl` against `api.github.com`, reachable in-session — 200 on 2026-07-28. Nothing shows that host is specially permitted, only that network access was open, so don't treat it as an allowlist entry. If the calls start failing, reachability is the thing to re-establish.

### Build and shell tooling in sessions

- `awk` works through `~/.local/bin/awk → mawk` (added 2026-07-21): `/usr/bin/awk` is an alternatives symlink that dangles in sessions, `/etc/alternatives` being wrapper-hidden. `hlint` and `stylish-haskell` are on PATH (via `~/.cabal/bin`) and work in-session, sandboxed included.
- `grep` on PATH is ugrep (7.5.0), not GNU grep — mostly compatible, with `-P` meaning PCRE2, and it can reject a regex outright with `exceeds complexity limits` where GNU grep would run it. The checkers under `tools/` are unaffected: they shell out to `git grep`.
- To combine several tasty `-p` patterns, tasty wants awk-style syntax: `-p "/foo/ || /bar/"`.
- Criterion (the benchmark suites) filters differently and does *not* accept that syntax — it has no `-p`: bare positional args are **prefix** patterns on the full `group/bench` path, and `-m glob`/`-m pattern`/`-m ipattern` switch the match mode (in horde-ad, this repo having no criterion suites: `--benchmark-options='-m glob "*/S-exec"'` to pick a bench across groups, or a bare `--benchmark-options='cnn-24x24'` prefix).
- GHC emits warnings only on *recompilation*: a cached, up-to-date build can hide warnings (e.g. `-Wredundant-constraints`) that a full rebuild would surface — don't infer "no warnings" from a clean second build.
- Keep one set of cabal flags across a session — changing flags (e.g. toggling `--enable-optimization` or `--enable-profiling`) forces a full rebuild of the local packages, though each flag set's dependency builds stay cached in the store. Pass such flags on the command line rather than editing `cabal.project.local`.
- The `/tmp` scratchpad is wiped by a machine restart — put anything that must survive (patches, notes) in the repo tree; binaries just get rebuilt.
- Exact dependency sources are always available: unpack `~/.cabal/packages/hackage.haskell.org/<pkg>/<ver>/` matching `dist-newstyle/cache/plan.json`.
- `.github/workflows/haskell-ci.yml` is generated by `haskell-ci` from the `.cabal` file — regenerate it rather than hand-editing, and re-apply any hand-maintained steps afterwards (in horde-ad: the `tests` and `benchmarks` steps, marked by a comment in the workflow).
- Toggle-based A/B builds: make a rule's guard unsatisfiable (e.g. `x /= x` on some scrutinee), build and copy the binary aside, restore + rebuild + copy again, then run the two preserved binaries in interleaved pairs (no rebuild between A and B).
