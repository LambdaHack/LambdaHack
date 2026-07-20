# The secondary smell: an input primitive that mutates (abort-split)

*LambdaHack · UI client · design — companion to
`docs/leader-desync-bug.md`, the pointman-desync post-mortem*

`promptGetKey` is named and used as "give me a key," yet on the
macro-interrupt path it silently rewrites semantic state. The live-read
design (the post-mortem's §10: make `sleader` the single source of truth,
read at the point of use) makes that mutation safe; the abort-split
described here makes it *visible*, by a functional-core/imperative-shell
split of the interrupted-macro cleanup into a pure decision plus a named
abort action. The document also records everything joint to the two
designs: the sequencing contract between them, and the test series that
must stay green across both refactors. The live-read design itself lives
entirely in the post-mortem; nothing here is needed to understand or land
it.

> design: **functional core / imperative shell** · planned strictly
> *after* the live-read design · file:line citations are to this branch's
> working tree (parked WIP, not yet on master); re-run
> `python3 tools/check-plan-citations.py
> docs/promptgetkey-hygiene.md` after touching cited files, and re-verify
> the only/every/never claims by repo-wide grep, never by re-reading one
> file.

> **⚠ Normative: the abort-split is specified against the post-live-read
> codebase.** The abort-split design is planned strictly *after* the
> live-read design and *assumes* it: every sketch in this document
> presupposes that no caller caches the pointman's identity, so the leader
> restore inside the abort action is safe by construction. Do not implement
> the abort-split against the pre-live-read tree — there the restore still
> invalidates threaded copies, and naming it would only decorate the bug.
> What the abort-split may rely on from live-read: all leader reads are
> live; what it must preserve: `promptGetKey`'s exact observable branch
> behavior (the §01 checklist), which the FrameM contract tests pin.

## 01 · The smell, and the abort-split that removes it

`promptGetKey` — which every dialog calls to draw a frame and read a key —
does interrupted-macro cleanup on one of its paths: restoring the leader,
resetting playback, cancelling the run:

**The hidden mutation** — `FrameM.hs:138-167`

```haskell
    KeyMacro kms -> do
      if null kms then ... -- no macro; nothing to abort
      else do
        resetPlayBack           -- wipe the interrupted macro
        restoreLeaderFromRun    -- <- MUTATES sleader, invisibly to callers
        resetPressedKeys
      frontKeyFrame <- drawOverlay dm onBlank ovs lidV
      recordHistory
      modifySession $ \sess -> sess {srunning = Nothing, ...}  -- more cleanup
      connFrontendFrontKey frontKeyKeys frontKeyFrame
```

That is what made the post-mortem's threaded pointman copies dangerous: a
caller reasonably assumes "reading a key doesn't change who the leader
is." Once the live-read design lands, the mutation is no longer
*dangerous* (everyone reads `sleader` live), but it is still *surprising*.
Eliminating the smell means making the mutation named and separating the
two concerns, following this repo's own functional-core / imperative-shell
rule.

### 1 · The interrupt decision is pure — move it to `InputDecision`

Whether a pending macro should play, abort, or is absent is a pure function of
`sreqQueried`, the report, the legal keys and the macro frame. It belongs in
the shared, fixture-tested `InputDecision` module the plan is establishing
(Phases 0/2), not buried in a frontend-adjacent IO action:

**Functional core** — `Client/UI/Frontend/InputDecision.hs`

```haskell
data MacroStep = VoiceKey K.KM KeyMacro   -- play this key, remaining macro
               | AbortPlayback            -- pending macro, but interrupted
               | NoMacro                   -- nothing pending

macroStep :: Bool      -- sreqQueried
          -> Bool      -- report disturbs resting
          -> [K.KM]    -- keys legal for this frame
          -> KeyMacroFrame
          -> MacroStep
macroStep sreqQueried disturbs frontKeyKeys mf =
  let interrupted = not sreqQueried
                    || (disturbs && keyPending mf /= KeyMacro [K.mkKM "F1"])
  in case keyPending mf of
       KeyMacro (k : ks)
         | not interrupted && (null frontKeyKeys || k `elem` frontKeyKeys)
                     -> VoiceKey k (KeyMacro ks)
       KeyMacro [] -> NoMacro
       KeyMacro _  -> AbortPlayback
```

### 2 · The effect is named — `abortMacroPlayback`

**Named state transition** — `FrameM.hs`

```haskell
-- | Abort in-progress macro playback and the run it was driving.
-- Named and explicit: acquiring a key must not do this behind the
-- caller's back.
-- NOTE: does NOT clear @srunning@ -- restoreLeaderFromRun
-- READS it, and the shell clears it for both branches afterwards.
abortMacroPlayback :: MonadClientUI m => m ()
abortMacroPlayback = do
  resetPlayBack
  restoreLeaderFromRun   -- restore the pointman the run began with
  resetPressedKeys
```

### 3 · `promptGetKey` becomes a thin shell that orchestrates

**Imperative shell** — `FrameM.hs`

```haskell
promptGetKey dm ovs onBlank frontKeyKeys = do
  step <- macroStep <$> getsSession sreqQueried
                <*> (anyInReport disturbsResting . newReport
                     <$> getsSession shistory)
                <*> pure frontKeyKeys
                <*> getsSession smacroFrame
  case step of
    VoiceKey k ks -> do popMacroKey ks; msgAdd MsgMacroOperation ...; return k
    _ -> do
      case step of
        AbortPlayback -> abortMacroPlayback  -- explicit, visible, named
        NoMacro -> specialEventKeyReset dm
          -- the ColorFull/resetPressedKeys logic, kept as today
          -- (see the checklist below)
        _ -> return ()
      frame <- drawOverlay dm onBlank ovs =<< viewedLevelUI
      recordHistory
      -- Common cleanup for EVERY real-key read (both branches above);
      -- must come AFTER abortMacroPlayback, which reads srunning:
      modifySession $ \sess ->
        sess { srunning = Nothing, sxhairGoTo = Nothing
             , sdisplayNeeded = False, sturnDisplayed = True }
      connFrontendFrontKey frontKeyKeys frame
```

The end state is worth stating plainly: `promptGetKey` still mutates. The
macro-frame advance, the voicing message, `recordHistory`, the common
cleanup and — on the abort branch — the leader restore all stay in the
shell; a key-read primitive driving a macro machine cannot be pure. What
leaves is the *decision*; what changes about the mutations is that each is
named, gated on that decision and enumerable — §02's audit step then pins
the enumeration in code.

> **Why keep the leader restore at all.** Restoring the pointman to the run's
> original leader when a run is interrupted is deliberate UX the author
> designed, and it is preserved verbatim — just relocated into a named action
> and gated on the pure `AbortPlayback` decision. After the live-read
> design (the post-mortem's §10), its `sleader` write is simply an update
> to the one source of truth that every subsequent read observes. So the
> live-read design *subsumes* the danger; this document removes the
> remaining *surprise*, and yields a pure, testable interrupt decision as
> a bonus. No defensive re-syncs are introduced anywhere.

### Branch-exactness checklist (verified against `FrameM.hs`)

The split must be branch-exact; these are the invariants the decomposition
preserves, each verified line-by-line against today's `promptGetKey`:

- **Common cleanup belongs to the shell, not the abort action.** `srunning =
  Nothing`, `sxhairGoTo = Nothing`, `sdisplayNeeded`, `sturnDisplayed` run on
  *every* real-key read — in the "no macro" branch (a run that ended naturally)
  just as in the "aborted macro" branch — so the `srunning` clear must not
  move into `abortMacroPlayback`, or the natural-run-end cleanup is silently
  dropped.
- **Ordering invariant:** `restoreLeaderFromRun` *reads* `srunning` to find
  `runLeader`; the shell's clear must therefore come after the abort action.
  Worth an `assert` or a comment stating the read-before-clear dependency — it
  is the kind of implicit ordering a later refactor would happily break.
- The "no macro" branch must *not* call `resetPlayBack` — today it leaves the
  macro stack untouched; wiping it there would be a semantic change hiding as a
  simplification.
- The "no macro" branch keeps its special-event logic: on `dm /= ColorFull`,
  `resetPressedKeys` unless the faction is under AI (shown as
  `specialEventKeyReset` in the shell above).
- The F1-help exemption (`keyPending /= KeyMacro [F1]` keeps a help-displaying
  macro alive through alarming messages) is part of the *interrupt decision*
  and moves into the pure `macroStep` with it (pinned by AS10).
- The legal-key test ``(null frontKeyKeys || km `elem` frontKeyKeys)`` is also
  part of the decision: an illegal macro key aborts playback *even when not
  interrupted*. The pure function must reproduce the guard exactly as today's
  pattern-match does.
- `addToMacro` recording (gated on `sreqQueried`, at the very end) is untouched
  — it is what lets in-game macros capture menu navigation, a feature the
  author values.
- The decision's interrupt inputs are confirmed reachable: `stopPlayBack`'s
  `MsgStopPlayback` is classified `interruptsRunning = True`, hence flows
  through `disturbsResting` into `macroStep` — the watch-event → macro-abort
  pathway goes through the pure function, where it becomes testable (and is
  already pinned end-to-end by AS9).

### Blast radius and testability

`promptGetKey` is called from exactly two engine modules,
`Client/UI.hs:194` and `SlideshowM.hs:421`, and its type does not change,
so the abort-split is entirely internal to `FrameM` plus one new pure
module. Both call sites are already exercised: the §03 contract tests
drive `promptGetKey` directly from `test/FrameMUnitTests.hs`, and the
store-dialog ESC test (`test/HandleHumanLocalMUnitTests.hs`) reaches it
through the `SlideshowM` site. The pure `macroStep` slots into the
existing test style directly:
`test/SessionUIMock.hs` already simulates macro-frame transitions
(`unwindMacros`), so play/abort/no-macro decision tables live next to
established tests rather than requiring new harness machinery.

> **⚠ Sequencing: the abort-split is hygiene, not the fix — and it assumes
> live-read.** The abort-split alone would *not* fix the bug family: the
> leader restore would still happen mid-dialog, merely under a nicer name,
> and every threaded copy would still go stale. The abort-split is therefore
> planned strictly after the live-read design and *assumes* it throughout:
> its correctness argument is "the restore is an ordinary write to the
> single source of truth, observed by every (live) reader" — a statement
> that is only true once live-read has landed. Concretely: land the
> live-read design (steps 1–5 of the post-mortem's §12 migration plan),
> re-enable the assertions, flip the LR-series test expectations, and only
> then extract `macroStep`/`abortMacroPlayback`. The AS-series tests are
> deliberately written as *contract tests* against `promptGetKey`'s
> unchanged type — they must pass before live-read, after it, and after the
> abort-split, so they serve as the refactor's safety net rather than as
> characterizations to flip.

## 02 · Migration step and verification

The abort-split is the final step of the joint migration — strictly after
step 5 of the post-mortem's §12 plan, assuming the live-read design is
complete:

1. Extract `macroStep` into `InputDecision`, with the §01 branch-exactness
   checklist as its test table.
2. Name `abortMacroPlayback`; keep the common cleanup in the shell, with the
   read-before-clear ordering noted in §01.
3. Audit the shell's residual writes: `promptGetKey` stays mutating by
   design (§01), so walk its body and, for every remaining effect — the
   voicing branch's macro-frame advance and `MsgMacroOperation` message,
   the special-event `resetPressedKeys`, `recordHistory`, the common
   cleanup block, the `sreqQueried`-gated `addToMacro` recording — either
   hoist it to the callers (only where all of them want it and the §03
   outcomes survive) or keep it with a comment stating why a key-read
   primitive is its natural home. The recording already carries the model
   comment ("recorded here, not in @UI.humanCommand@, to also capture
   choice of items from menus"). End state: `promptGetKey`'s haddock
   exhaustively lists the state it may write, so nothing about the
   primitive is hidden again.
4. The FrameM contract tests (the AS series of §03) must pass unchanged
   across this step — any diff in their outcomes means the refactor
   altered `promptGetKey`'s observable behavior.

## 03 · The joint part of the test battery

The post-mortem's §13 describes the battery (the live-read series and the
sibling-bug tests); this section holds the part that touches both designs. The
sequencing between the designs is encoded in the battery's two
classifications, whose full — joint — meaning is:

- **[contract]** — behaviour that must survive the live-read *and* the
  abort-split designs unchanged. The whole AS series is deliberately in this
  class: pinned against `promptGetKey`'s unchanged type, required to pass
  before live-read, after it, and after the abort-split — the safety net
  under the abort-split refactor, never flipped.
- **[LR-flip]** — characterizations of the current desync-prone behaviour,
  each with the post-live-read expectation stated inline; they flip when
  the live-read design lands, never as part of the abort-split.

### The abort-split series — `test/FrameMUnitTests.hs` (all [contract])

| test | pins |
|---|---|
| AS1 | `addToMacro`: records bound keys only, never `Record`, no-op when idle |
| AS2 | `dropEmptyMacroFrames`: GCs empty frames, always keeps the last |
| AS3 | voicing: a legal, uninterrupted macro key is consumed — and the run *survives* (the enabler of the crash window, post-mortem §04) |
| AS4 | natural end: no macro → run cancelled, pointman *not* restored (§01 branch-exactness) |
| AS5 | abort via illegal macro key: macro wiped, run cancelled, pointman *restored* to the run leader — the hidden write, pinned through the real `promptGetKey` |
| AS6 | abort via `sreqQueried = False`: same outcome through the other interrupt input that `macroStep` must reproduce |
| AS7 | rendered (non-blank) frames work under the mock: `drawHudFrame` over the stub board — pinned because the post-mortem's §13 full-dialog plan depends on it |
| AS8 | keys voiced from a macro are recorded into an in-game macro being defined — the "recorded here, not in @UI.humanCommand@" semantics that the §02 audit step keeps inside the primitive |
| AS9 | abort via a disturbing report — the third interrupt input, driven by the real `stopPlayBack`: same outcome as AS5/AS6 |
| AS10 | the F1 exemption: a help-displaying macro survives the same disturbing report — voiced, run intact, pointman untouched |
| AS11–AS13 | `restoreLeaderFromRun`'s guards, one each — no-op without a run, no-op for a `noRunWithMulti` faction, no-op when the run leader is gone from the level — pinned because `abortMacroPlayback` relocates the function verbatim |

### Bridge — touching both designs

**X1** replays the whole crash window (post-mortem §04) end-to-end with the
leader restore performed by the *real* `promptGetKey` (the post-mortem's
§07 test, kept as LR3 for its simplicity, calls `restoreLeaderFromRun`
directly instead of through `promptGetKey`): run rotates the pointman,
dialog captures it, macro dies inside the dialog, `promptGetKey` restores
the run leader, stale cycling no-ops.
Its `promptGetKey` observations are [contract]; only its final cycling
outcome is [LR-flip].

**X2** repeats the window with the post-abort keypress arriving as a *real*
key from the scripted frontend stub: `promptGetKey` aborts the macro,
restores the pointman and returns a literal `C-Tab`, which the test
resolves through the fixture CCUI's real bindings and feeds to the dialog's
cycling call with the stale captured leader, as `InventoryM`'s
`cycleLevelKeyDef` would. Same [contract]/[LR-flip] split as X1.

> **✓ What building the abort-split series caught.** The AS5/AS6/X1
> outcomes confirm empirically that the restore-on-abort pathway runs under
> the stock unit-test mock with a blank frame (`onBlank = True`) — no real
> frontend needed — which is what makes the AS contract series cheap enough
> to keep green across both refactors.

---

*LambdaHack · promptGetKey hygiene (the abort-split design) · companion to
`docs/leader-desync-bug.md`. A design recommendation, not applied changes;
§03 holds the joint (live-read + abort-split) part of the implemented
test battery. Verified against GHC 9.12.4, cabal test suite (148 tests,
all green).*
