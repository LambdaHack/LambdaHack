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
`8c93e3ba2` (2026-07-13), then machine-checked — re-run
`python3 tools/check-plan-citations.py` after landing work that touches
cited files, and re-verify universally-quantified claims ("only X does
Y", "exactly two") by repo-wide grep, never by re-reading one file. Two
file basenames are ambiguous in this repo and are therefore qualified
wherever cited: `Server/LoopM.hs` vs `Client/LoopM.hs`, and the engine's
vs the game's `Content/Input.hs`. Decisions *against* work, deferrals,
and their rationale are collected in Appendix B; verified non-gaps from
the SDL2-vs-wasm audit in Appendix C; the GHCJS→JS-backend port
investigation in Appendix A.

Contents: [Repo facts](#repo-facts-the-plan-builds-on) ·
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
Appendices [A](#appendix-a--investigation-porting-the-ghcjs-target-to-ghcs-in-tree-javascript-backend)
/ [B](#appendix-b--decisions-against-and-deferrals)
/ [C](#appendix-c--verified-non-gaps-sdl2-vs-wasm-audit-record)

### Work map

| Item | Deliverable | Size | Depends on | Status |
|---|---|---|---|---|
| 0.0 | AltGraph fixes (keys, mouse/wheel); highlight-outline rule | tiny | — | todo |
| 0.1 | `InputDecision` shared module; sync `lhKey` | small | 0.3 baseline | todo |
| 0.2 | `CellStyle` + TS-table/fixture generator | medium | — | todo |
| 0.3 | FFI-coverage battery (baseline before 0.1, then per-commit) | ongoing | — | todo |
| 1.1 | crosshair cursor (CSS keyword; then generated SVG cursor) | trivial | 0.2 for the final form | todo |
| 1.2 | working `Ctrl+P` screenshots | small–medium | 0.2 helps | todo |
| 1.3 | fullscreen toggle with scaling | small | — | todo |
| 1.4 | banner/title truthfulness | trivial, recurring | feature landings | todo |
| 1.5 | `allFontsScale` honored in browser | small | 2.2's startup call (or a precursor); R4 for player control | todo |
| 2.1 | `OverlayLayout` extraction + `Sdl.hs` on it | medium–large | — | todo |
| 2.2 | browser canvas overlay renderer + font wiring | medium | 2.1, 0.2 | todo |
| 2.3 | overlay transport over JSFFI | medium | 2.1, 2.2 | todo |
| 2.4 | multifont capability flip | tiny diff, big review | 2.1–2.3 | todo |
| 2.5 | post-flip QA | small | 2.4 | todo |
| 3.1 | `lhStart` reads WASI argv | small (+spike) | — | todo |
| 3.2 | Node driver for the game reactor | small | 3.1 | todo |
| 3.3 | `nodeBench*`/`nodeDeployedBench` targets | small | 3.2 | todo |

R1–R6 (save robustness, CI, the JS-backend decision, URL parameters,
performance, screenshot coherence) and the adopted multi-frontend
practices are ongoing or unscheduled tracks described after the phases.

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
  sibling checkout `../lambdahack.github.io` (Makefile:318-326), the
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
  the multifont gate's `not (T.null (fontPropRegular chosenFontset))`
  conjunct already passes for the default fontset; only the frontend check
  blocks it. However, `GameDefinition/game-src/Client/UI/Content/Screen.hs`
  sets `rFontFiles = []` under `USE_BROWSER` (natively `$(embedDir
  "GameDefinition/fonts")`), deliberately keeping font bytes out of the
  browser payload. In the browser, fonts are **static web assets by
  design**: Haskell knows their names and sizes (`sfonts :: [(Text,
  FontDefinition)]`, `Common/Misc.hs:28`) but cannot supply bytes.
  Phase 2.2's font wiring is built around exactly that split.
- **Font deployment gap.** `../lambdahack.github.io` currently contains
  only `16x16xw.woff` (plus `lz-string*.js`, used by `WasmFile.hs`'s save
  compression). `make build-ts` copies no fonts. Any step adding font
  usage must extend `build-ts` to copy the needed
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
  the one pre-existing gap (doctests, today a manual-only recipe in
  CLAUDE.md) is closed by R2.
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
`os(wasi)`, so it exercises the real `Wasm.hs`/`WasmFile.hs` paths).

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
  `CellStyle` and pinned by its fixtures): highlight kinds
  `HighlightNone`/`HighlightBackground`/`HighlightNoneCursor` get
  background fill only, **no outline** (`Sdl.hs:504-518`); all other
  highlight kinds get an outline on **all four tile sides**. Today
  `terminal.ts:135` draws its `inset 0 0 0 1px` box-shadow for every
  cell, which is correct four-sided drawing but wrongly includes
  `HighlightBackground` — and `HighlightBackground` is the vision
  backlight (`DrawM.hs:377-381,416-417`, on by default via
  `smarkVision = 1`, `SessionUI.hs:207`), so the whole field of view gets
  spurious `BrBlack` outlines in aiming mode where SDL shows a plain grey
  wash. Fix in `terminal-core.ts` (border color = background fill for the
  three kinds) with a `terminal-core.test.ts` case. Background for why
  the wasm build behaved this way, and the ruling on GHCJS's two-edge
  rendering, is in Appendix B.

All three fixes land with tests: the two AltGraph fixes get the jsdom
forwarding tests (the ground-rule exception above), the highlight rule its
`terminal-core.test.ts` case.

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

### 0.2 `CellStyle` + a build-time generator for TS tables and fixtures

**New module** `Game.LambdaHack.Client.UI.Frontend.CellStyle` holding the
pure per-cell decision currently in `Dom.hs`'s `setChar`
(`Dom.hs:251-273`): decode `AttrCharW32`, even-row `White`→`AltWhite`,
glyph substitution (space→nbsp, dim floor→`⋅`), highlight→border and
background color (per 0.0's rule). `Sdl.hs`'s
`setSquareChar`/`setMonoChar` (`Sdl.hs:603-669`) contain the *same*
AltWhite and floor-substitution rules (with a bitmap-font variant,
`'\x0007'`), so the module is written for and consumed by **both** native
and browser frontends, parameterized by the per-frontend floor-glyph choice
(`'\x0007'` for SDL bitmap fonts, `'\x22C5'` for SDL scalable and the
web, `'.'` in ANSI and Teletype — `ANSI.hs:273`, `Teletype.hs:57`, which
makes those two frontends candidate consumers of the substitution rule
too, though they use neither AltWhite nor highlights).

Two riders on the `Sdl.hs` side of this work: derive `colorToRGBA`
(`Sdl.hs:904-921`, a hand-maintained palette copy by its own comment)
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
   (guaranteed by `Screen.hs`'s `rwidth == RK.rWidthMax` assertion —
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

### 1.4 Banner truth maintenance (trivial, recurring)

`GameDefinition/index.html`'s banner and status text still claim save
games aren't persistent (they are, via `WasmFile.hs`/localStorage) and
will go staler as features land ("For proportional fonts, fullscreen, and
persistent save games, use the native binary"). Update the text as each
feature ships. The "savefiles are prone to corruption" caveat stays until
R1 addresses it. (The GHCJS-era page's community features are
deliberately not restored, and the page `<title>` stays hardcoded —
Appendix B.)

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

---

## Phase 2 — multi-font, as extraction rather than re-implementation

Strict chain 2.1 → 2.2 → 2.3 → 2.4; each independently shippable and
visually a no-op until 2.4 flips the switch. (Until then, nothing is
missing on screen: under `singleFontSetup` the engine pre-flattens all
overlay text into the square-font `singleArray`, which is why the wasm
build shows menus and the log today, just in the map font.)

Before 2.2 lands, capture a browser frame-timing **baseline** — with R5's
harness if it exists by then, else a temporary `performance.now()` probe —
and re-measure at 2.5, so multifont's rendering cost is an attributed,
measured change rather than a guess.

### 2.1 Extract `OverlayLayout`: the pure half of `Sdl.hs`'s overlay drawing

**New module** `Game.LambdaHack.Client.UI.Frontend.OverlayLayout` holding
every per-line decision that today lives interleaved with SDL IO in
`Sdl.hs:593-713`:

- prop-line chunking: split an `AttrString` into same-fg runs, including
  the subtle space-inherits-the-*next*-non-space-chunk's-color rule
  (`drawPropLine`'s `isSpace`/`sameAttr`/`span`, `Sdl.hs:679-697`) —
  exactly the kind of knowledge that would drift in a hand-port;
- prop font choice per chunk: `fg >= White && fg /= BrBlack` → regular,
  else bold (`drawPropChunk`, `Sdl.hs:700-702`);
- even-row `White`→`AltWhite` (shared with/via `CellStyle`);
- the coordinate-scaling *rule* — `xPx = x * halfSize`, `yPx = row *
  boxSize` — parameterized by `halfSize`, **not** baked to SDL's pixel
  values: `Sdl.hs` derives `halfSize` from the loaded map font's real
  height × `sallFontsScale` (`Sdl.hs:150-184`), while the browser's cell
  box comes from CSS. Emitted positions therefore stay in *logical*
  `PointUI` units; each consumer applies its own metrics (see 2.2/2.3);
- line-overrun cutoffs (`take (2 * rwidth - x)` for mono, the
  `x >= (rwidth-1) * boxSize` rejection for prop — restated in logical
  units) — including the **trimmed-line marker**: when a prop chunk is
  cut short, SDL stamps `Color.trimmedLineAttrW32` (a `$` marker) into
  the last square column of that row (`Sdl.hs:577-578`), a rule that's
  easy to lose in extraction because it lives inside the texture-scaling
  helper, not the layout loop;
- the layer-ordering rule: prop, then square, then mono last so overrun
  warnings win (`Sdl.hs:728-733`) — encoded as a documented
  constant/order, not a re-discovery.

Output type: per line, a logical start position plus chunks
`(fontKind, colorIdx, text)`. **Measurement is the one thing that stays
frontend-side**: prop-chunk x-advance depends on rendered width, so the
module emits chunks and each consumer advances its own cursor (SDL from
`TTF.shaded` surface widths as today; the browser from canvas
`measureText`). Mono and square chunks need no measurement at all (fixed
`halfSize`/`boxSize` advance per char). The alternative — Haskell calling
a sync `js_measureText` per chunk — is rejected on boundary-cost grounds
(a wasm↔JS crossing per chunk per frame), unless cursor-advance in the
consumer proves insufficient in practice.

**Refactor `Sdl.hs` to consume the module** — this is the proof the
extraction is faithful, and it de-risks everything downstream: if native
playtests (`make test-medium`, `make frontendCrawl` for visual
confirmation) pass with `Sdl.hs` on the shared module, the browser
consumer starts from known-correct layout data. Perf-gate the refactor
too: before/after `make bench` runs
(`benchFrontendBattle`/`benchFrontendCrawl` exercise the refactored
drawing path with fixed seeds). Tasty tests cover chunking
edge cases (leading/trailing/multi-space runs, color changes at spaces,
overrun cutoffs), plus QuickCheck properties (QuickCheck is already a
library dependency — `Point.hs` imports it): the chunk texts of a line
concatenate back to the input minus the overrun cut, and every chunk is
single-colored under the space-inheritance rule. Fixed cases catch the
known edges; properties catch the unknown ones.

### 2.2 Browser overlay renderer, in isolation

A single absolutely-positioned `<canvas>` over the existing `<span>` grid
(the grid can't host variable-width text; canvas `fillText`+`measureText`
is the direct analogue of SDL2's measure-then-blit). New
`overlay-core.ts`/`overlay-core.test.ts` for the pure parts (cursor
advance, logical-to-pixel placement), with a fake `measureText` injected
in tests. Like 1.2's rasterizer, the renderer is a functional core
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

Fixtures for the chunking logic come from the 0.2 generator calling
`OverlayLayout` — no hand-written fixtures.

Verify: vitest green; live game pixel-identical to before (nothing wired).

### 2.3 Transport: overlays across the JSFFI boundary

`Wasm.hs`'s `display` currently drops three of `SingleFrame`'s four fields
(`Wasm.hs:79`; `OverlaySpace = [(PointUI, AttrString)]`, `Frame.hs:100`).
Add an **additive** `js_submitOverlays` alongside `js_submitFrame`, using
the same idiom the frame already uses: a packed `Word32` buffer passed by
address (per line: `y`, logical `xStart`, chunk count; per chunk:
fontKind|colorIdx, length, then codepoints — everything is numeric and
fits `Word32`, no string marshalling, no new serialization dependency).
Positions stay in logical `PointUI` units per 2.1; TS scales by its own
measured cell box. The encoder lives next to `OverlayLayout` and is pure;
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

Update `test/MonadClientUIUnitTests.hs`'s `getFontSetup works in stub` so
it is correct under both `cabal test` and `make test-wasm` — as written it
only provably holds when compiled for SDL, since `stubClientOptions` falls
through to the compiled-in frontend.

Small diff, large blast radius: this is the step that changes what players
see and it touches shared engine code — the one to review hardest, and to
re-run the native playtest battery on (`make test-medium` at minimum).

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

### 3.3 Repurpose the Makefile targets

- `nodeBenchCrawl` / `nodeBenchBattle`: same flags and
  `RNGOPTS`/`RNGOPTS1` as today (mirroring
  `nativeBenchCrawl`/`nativeBenchBattle`), but invoking the 3.2 driver on
  `wasm32-wasi-cabal list-bin exe:LambdaHack` plus post-linked glue (the
  `test-wasm` target shows the exact `post-link.mjs` recipe,
  Makefile:309-316). `nodeBench` stays the aggregate of both.
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

**R2 — Browser-and-frontend CI.** None of Phase 0's drift protection fires
unless CI runs it. Add a job to `.github/workflows/lint-and-playtest.yml`
(the hand-written workflow — do not touch the generated haskell-ci one)
that installs ghc-wasm-meta, runs `make build-wasm`, `make test-wasm`,
`make test-ts`, the 0.3 FFI-coverage battery, and the 0.2 generated-file
freshness check. Cache `~/.ghc-wasm` and the wasm cabal store
aggressively; the toolchain is the expensive part. `make test-ts` alone
(Node only) is nearly free — split it into its own always-fast job so TS
regressions fail in seconds. The frontend-CI-matrix practice (below)
widens this with xvfb SDL and pty ANSI smokes.

**Completeness requirement: everything runs in CI.** Today's inventory:
the tasty suite and haddock run via the generated haskell-ci workflow;
hlint and the `make test-gha` playtests via the hand-written one;
**doctests run nowhere** — CLAUDE.md documents a manual-only recipe.
Close that gap here: either regenerate haskell-ci with its doctest
support enabled (repo policy: regenerate, never hand-edit) or add a
doctest job to the hand-written workflow following the CLAUDE.md recipe —
whichever survives contact with the flattened-cabal setup. And the
standing rule from the ground rules applies to everything this plan
adds: the vitest suites (including the jsdom forwarding tests),
`make test-wasm` (including the FFI battery and the RawFrontend contract
harness), the generated-file freshness check, the determinism goldens
(native under `cabal test`, cross-backend under `make test-wasm`), and
the frontend smokes — each in CI from the commit that introduces it.

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
  `LambdaHack.cabal:152-156` and `371-391`), the `ghcjs-options` knobs
  (`GHCJS_GC_INTERVAL`, `GHCJS_BUSY_YIELD`, `-dedupe`, `GHCJS_BROWSER`,
  `LambdaHack.cabal:163-177`), the `supportNodeJS` flag's GHCJS
  semantics, `Frontend.hs:43-44`'s `USE_GHCJS` import branch, `File.hs`'s
  `USE_JSFILE` branch, `TieKnot.hs:114-118`'s GHC.Compact escape hatch,
  and the `USE_JSFILE` halves of the browser conditions in
  `Server/LoopM.hs`/`WatchUpdAtomicM.hs`/`HandleHumanLocalM.hs` (or of
  their capability-constant successors, if that practice lands first).
  Update CLAUDE.md's and the README's GHCJS mentions in the same commit.

Timed after parity, not before, so the rip-out doesn't tangle with
Phase 0–2 diffs touching the same cabal stanzas and CPP sites.

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

**R6 — Screenshot/overlay coherence.** Every renderer of "the current
screen" (the live grid, the 1.2 screenshot rasterizer, any future export)
must consume the same four `SingleFrame` fields after Phase 2. Enforced by
putting the shared drawing entry point in one TS function both paths call,
and by a vitest case rendering a fixture frame with overlays through both —
comparing draw-command lists (per 1.2/2.2's functional-core structure),
not pixels.

## Multi-frontend practices (adopted)

G1 is one practice among several that make a codebase with N frontends
cheap to extend. These are adopted; the rejected companions are in
Appendix B.

**Capability constants instead of behavioral CPP.** Module *selection* by
CPP is fine (`File.hs:9-15`, `Frontend.hs:41-48`); behavioral forks
scattered around the engine are not: the autosave skip (`Server/LoopM.hs:336`),
the history-dump key omission (`HandleHumanLocalM.hs:815`), and the 2s
exit flush wait (`WatchUpdAtomicM.hs:586`) are each an
`#if !defined(USE_JSFILE) && !defined(USE_WASMFILE)` whose *reason* lives
in a comment far from the backend it describes. Let the storage backend
module itself export named constants (e.g. `savesCauseLag :: Bool`,
`filesAreDumpable :: Bool`, `needsExitFlushDelay :: Bool`) re-exported
through `File.hs`, and let the engine branch on values, not macros. A
fourth backend then answers named questions instead of hoping every
negated macro conjunction got updated; R1's re-measurement flips one
constant instead of editing ifdefs. (Server code consuming a File-layer
constant respects the client-server split — it's the storage backend's
property, not the frontend's.)

**Sum-typed frontend selection.**
`sfrontendANSI`/`sfrontendTeletype`/`sfrontendNull`/`sfrontendLazy` are
four independent `Bool`s (`ClientOptions.hs:62-68`) whose simultaneous
truth is resolved by guard order in two places (`Frontend.hs:84-92` and
`186-196`). One sum-typed field (`FrontendDefault | FrontendANSI |
FrontendTeletype | FrontendNull | FrontendLazy`) makes the guard chains
total cases and turns conflicting flags into a parse error. Mechanical,
moderate churn (options parser, UnitTestHelpers' stub options). Do before
R4, so URL parameters parse into the sum type, not the Bools.

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

**Determinism goldens — native first, then cross-backend.** The tasty
suite already compiles and runs under both native (`cabal test`) and wasm
(`make test-wasm`), and `test/SessionUIMock.hs` already unwinds key macros
through the real `HandleHumanM` machinery. Add golden tests — fixed seed,
scripted keys, assert a committed final-state digest — in two stages.
The native-only harness lands **before 2.1**: it guards the two most
dangerous shared-code changes in the plan (2.1's `Sdl.hs` refactor and
2.4's flip) regardless of wasm. Then the same goldens run under
`make test-wasm` once it is part of routine CI (R2) — the only kind of
test that catches native-vs-wasm *behavioral* drift (FFI-adjacent paths,
numeric assumptions); per-frontend unit tests can't. Medium effort.

**A CI smoke for every shipped frontend.** CI exercises teletype
(playtests) and, after R2, wasm; SDL2 itself is only ever CI-tested via
the `slogPriority == Just 0` init-and-quit backdoor (`Sdl.hs:196-207`) —
its event loop, renderer, and font pipeline never run in CI. Add an
`xvfb-run` job driving a real SDL game for a few frames (a tiny
`--stopAfterFrames` variant of `benchFrontendBattle`), and a pty-driven
ANSI startup/shutdown check. No shipped frontend goes permanently untested
the way `Dom.hs` did.

**Frontends pass widths explicitly.** Frontend code decodes linear
indices with the explicitly-parameterized `punindex (rwidth coscreen)`,
never the `Enum` instance: `toEnum i` depends on the global
`speedupHackXSize` (the *dungeon* width, correct at frontend call sites
only via `Screen.hs`'s `rwidth == RK.rWidthMax` assertion). The switch is
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

**Functional core, imperative shell — per frontend (the review bar).**
`CellStyle`/`InputDecision`/`OverlayLayout` and the `ts-src`
`*-core.ts`-vs-wiring split are instances of one rule: a frontend module
contains only event capture, output mutation, and plumbing; every
*decision* lives in shared pure code under test. The question to ask of
any new line in `Sdl.hs`/`Wasm.hs`/`terminal.ts`: "would a fourth frontend
have to copy this?" If yes, it belongs in core. This is the practice that
generated 0.1/0.2/2.1, stated so it outlives them.

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
R2 CI job — start as soon as 0.3 has anything to run; grow it per phase
    (the test-ts-only job can land today; a short nodeBench smoke run
    joins after 3.3; the xvfb SDL + pty ANSI smokes complete the matrix)
0.3 FFI coverage — after the baseline, incremental with every
    FFI-touching commit
capability constants / sum-typed selection — standalone refactors, any
    time; the sum type before R4 (URL params parse into it)
RawFrontend contract — input-side cases with 0.1; the rest with 0.3
determinism goldens — native harness before 2.1; cross-backend once
    test-wasm is routine in CI (R2)
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
  ==9.14.1` (`LambdaHack.cabal:47`) and `default-language: GHC2024`
  (`LambdaHack.cabal:98`), which only exists in GHC 9.10+.
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
(`GameDefinition/Content/RuleKind.hs:34`), `Client.UI.Content.Screen` runs a TH splice that reads
and parses `GameDefinition/PLAYING.md` at compile time (plus `embedDir`
for fonts, though that one is `#ifdef`'d away under `USE_BROWSER`).

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
  [1]. Repo impact: exactly two import strings, `JSFile.hs:30-33`
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
   everywhere (library conditionals at `LambdaHack.cabal:152-156` and
   `371-391`, plus the executable's); delete the dead old-compiler knobs
   (`ghcjs-options`: `GHCJS_GC_INTERVAL`, `GHCJS_BUSY_YIELD`, `-dedupe`,
   `GHCJS_BROWSER`, `LambdaHack.cabal:163-177` — none has a new-backend
   equivalent; the new backend takes plain `ghc-options`); revisit the
   `supportNodeJS` flag's meaning; rewrite the two `$r =` FFI strings as
   arrow functions. The `USE_GHCJS`/`USE_JSFILE` CPP names can stay (they
   name the *frontend/file-layer choice*, not the compiler), which keeps
   the churn out of the eight source files that test them.
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
that's the "cheaper and more valuable after Phases 0–2" claim in R3 made
concrete.

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
`HighlightBackground` cells that the wasm port faithfully reproduced; and
because of collapsed table borders, a highlight effectively showed only on
a tile's bottom and right edges (verified in a browser). Rulings: SDL2's
look is canonical (fill-only for
`None`/`Background`/`NoneCursor`); highlights outline all four tile sides;
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
  `SlideshowM.hs:350`): all are reachable only from mouse-button-release
  bindings (`GameDefinition/.../Content/Input.hs:188-236` — the mouse
  section plus the `"safe1".."safe6"` `CmdInternal` pseudo-keys, which are
  not typeable on any keyboard and exist only for the mouse machinery and
  macros to reference), and the triggering mouse KMP itself sets
  `spointer` correctly first. No observable difference today — but this is
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
  (`GameDefinition/.../Content/Input.hs:185` binds only `C-P`), so `keyTranslateWeb`'s lack of a
  `"PrintScreen"` case changes nothing.
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
