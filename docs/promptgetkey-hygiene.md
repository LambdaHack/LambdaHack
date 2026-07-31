# `promptGetKey`'s branch behaviour, and the abort-split that would tidy it

*LambdaHack · UI client · specification & design — companion to
`docs/leader-desync-bug.md`, the pointman-desync post-mortem*

`promptGetKey` is named and used as "give me a key," yet on the
macro-interrupt path it silently rewrites semantic state. The live-read
design (the post-mortem's §10: make `sleader` the single source of truth,
read at the point of use) makes that mutation safe; the abort-split
described here makes it *visible*, by a functional-core/imperative-shell
split of the interrupted-macro cleanup into a pure decision plus a named
abort action. The document also records the sequencing contract between
the two designs — live-read first, this split strictly after it — and
names the test series that must stay green across both refactors, which
`docs/leader-desync-migration.md` lists. The live-read design itself lives
entirely in the post-mortem; nothing here is needed to understand or land
it.

> design: **functional core / imperative shell** · planned strictly
> *after* the live-read design · file:line citations were verified against
> the tree at commit **2b20a8284** (2026-07-30) — the newest commit
> touching any file they cite, so the verification stands until one of
> those files moves. The tests they cite are on master, while this design
> is the parked part; the citation pass proves a cited
> line exists — that stamp, that it still says what the claim needs; re-run
> `python3 tools/check-plan-citations.py
> docs/promptgetkey-hygiene.md` after touching cited files, and re-verify
> the only/every/never claims by repo-wide grep, never by re-reading one
> file.

> **What this record is for, and what is frozen in it.** It is kept
> indefinitely as the specification of `promptGetKey`'s branch behaviour:
> the checklist in §01 is what any later refactor of that primitive has to
> preserve, whether or not the abort-split itself ever happens.
>
> The excerpt at the head of §01 describes the code *as of the stamped
> commit* and is not maintained against later trees — after the split
> lands it is history, while the invariants under it are the part that
> survives. The split sketched after those invariants is the live part,
> executed rather than merely read — `docs/leader-desync-migration.md`
> hands it to a session, which will do exactly what it says and nothing it
> implies, so a gap here is a question for that plan and not a judgment
> call at the keyboard. It follows
> the post-mortem's model (its §§10–11 callout): a specification until it
> lands or is abandoned, so its claims about today's tree are re-verified
> with the migration document rather than left to age. Outcome lines:
>
> - §01 · the checklist — **pinned** on master by the AS series
>   (`test/FrameMUnitTests.hs`), which must stay green across both
>   refactors; six of its eight bullets, that is. Two are unpinned, each in
>   its own way, and each bullet now records which: the special-event one,
>   which no AS case enters, and the no-`resetPlayBack` one, which two of
>   them enter and none observes.
> - the split itself — **not applied**; the work list is
>   `docs/leader-desync-migration.md`.

> **⚠ Normative: the abort-split is specified against the post-live-read
> codebase.** The abort-split design is planned strictly *after* the
> live-read design and *assumes* it: every sketch in this document
> presupposes that no caller caches the pointman's identity, so the leader
> restore inside the abort action is safe by construction. Do not implement
> the abort-split against the pre-live-read tree — there the restore still
> invalidates threaded copies, and naming it would only decorate the bug.
> What the abort-split may rely on from live-read: all leader reads are
> live; what it must preserve: `promptGetKey`'s exact observable branch
> behaviour (the §01 checklist), which the FrameM contract tests pin.

## 01 · The branch behaviour to preserve, and the split that would tidy it

`promptGetKey` — which every dialog calls to draw a frame and read a key —
does interrupted-macro cleanup on one of its paths: restoring the leader
(the source's name for what the UI calls the pointman, per `CLAUDE.md`'s
naming-mismatch note; both words appear below), resetting playback,
cancelling the run:

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

### Branch-exactness checklist (verified against `FrameM.hs`)

This is the part of the record that outlives the proposal: the invariants
any refactor of this primitive must preserve, each verified line-by-line
against today's `promptGetKey`. The parenthesised names — `macroStep`,
`abortMacroPlayback`, `specialEventKeyReset` — belong to the split
sketched below; the invariants bind whatever replaces it.

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
  simplification. **Entered by AS4 and AS7, observed by neither**: both run on
  `partyCliState`, whose `keyPending` and macro stack are empty already, and
  neither reads macro state after the call — so a split that wiped it here
  would keep the suite green. One of the two bullets whose preservation the
  split cannot be checked for.
- The "no macro" branch keeps its special-event logic: on `dm /= ColorFull`,
  `resetPressedKeys` unless the faction is under AI (shown as
  `specialEventKeyReset` in the shell below). **No AS case enters this
  branch** — every one of them reads with `ColorFull`. The suite is not
  wholly innocent of it: `test/Spec.hs`'s integration run reaches
  `promptGetKey` with `ColorBW` down the shutdown path (`UpdKillExit` →
  `WatchUpdAtomicM.hs:597`'s `displayMore ColorBW "Done."` →
  `SlideshowM.hs:421`), where `gunderAI` suppresses the effect — entered and
  unobserved, which pins nothing. The other unchecked bullet, and the reason
  the migration document's §04 step 1 asks for the missing case before the
  extraction rather than after.
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

### The split that would preserve them, in three pieces

That excerpt is also what made the post-mortem's threaded pointman copies
dangerous: a caller reasonably assumes "reading a key doesn't change who
the leader is." Once the live-read design lands, the mutation is no longer
*dangerous* (everyone reads `sleader` live), but it is still *surprising*.
Eliminating the smell means making the mutation named and separating the
two concerns, following this repo's own functional-core / imperative-shell
rule. What follows is one refactor that satisfies the list above; the
list, not the refactor, is what binds a later one.

### 1 · The interrupt decision is pure — move it to `InputDecision`

Whether a pending macro should play, abort, or is absent is a pure function of
`sreqQueried`, the report, the legal keys and the macro frame. It belongs in
the shared, fixture-tested `InputDecision` module the plan is establishing
(item 0.1), not buried in a frontend-adjacent IO action:

> **⚠ That module does not exist yet, and belongs to another plan.**
> `Client/UI/Frontend/InputDecision.hs` is an artifact of
> `docs/wasm-frontend-unified-plan.md`'s item 0.1, which has not
> landed; nothing in this repository defines `macroStep`'s intended home
> today. So the abort-split is gated on *two* pieces of work, not one.
> The de-gating is deliberate and cheap: if that module is still absent
> when live-read is done, put `macroStep` in a pure section of `FrameM`
> itself, next to `dropEmptyMacroFrames` (already pure and already
> fixture-tested by AS2), and move it to `InputDecision` when the item
> that creates the module lands. The decision function is the point; its
> address is not, and waiting on a different plan for an address would be
> the wrong dependency.
>
> One thing to settle when that address is chosen, and not before: the
> signature below takes a `KeyMacroFrame`, which lives in
> `Client.UI.SessionUI` (`SessionUI.hs:124`), which imports
> `Client.UI.Frontend` (`SessionUI.hs:28`). A home under
> `Client/UI/Frontend/` therefore sits downstream of the very interface the
> frontends are reached through, and `FrameM` importing it directly reaches
> past `Client.UI.Frontend` into that directory, against `CLAUDE.md`'s
> module-as-interface convention. The `FrameM` fallback has neither
> problem, so "entirely internal to `FrameM` plus one new pure module"
> below is exact only for that home.

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
  km <- case step of
    VoiceKey k ks -> do popMacroKey ks; msgAdd MsgMacroOperation ...; return k
    _ -> do
      case step of
        AbortPlayback -> abortMacroPlayback  -- explicit, visible, named
        NoMacro -> specialEventKeyReset dm
          -- the ColorFull/resetPressedKeys logic, kept as today
          -- (see the checklist above)
        _ -> return ()
      frame <- drawOverlay dm onBlank ovs =<< viewedLevelUI
      recordHistory
      -- Common cleanup for EVERY real-key read (both branches above);
      -- must come AFTER abortMacroPlayback, which reads srunning:
      modifySession $ \sess ->
        sess { srunning = Nothing, sxhairGoTo = Nothing
             , sdisplayNeeded = False, sturnDisplayed = True }
      connFrontendFrontKey frontKeyKeys frame
  -- Reached from BOTH arms, unchanged and unmoved (checklist, last but
  -- one bullet); the only write in this function that is unconditional:
  when sreqQueried $ ... addToMacro ...
  return km
```

Two names in that sketch are proposals, not existing code:
`popMacroKey`, the one-line `smacroFrame` update the voicing branch
performs today, and `specialEventKeyReset`, the `dm /= ColorFull` block
with its `unless (gunderAI fact)` guard. Both are extractions of code
already in `promptGetKey`, named here so the shell reads as a list of
decisions; grep will not find either until the split is done.

One further liberty in the same sketch: the real body binds `lidV <-
viewedLevelUI` as its *first* line, ahead of the branch, so the voicing
path computes it and discards it; the sketch defers the read into the
branch that uses it. Harmless — but not because `viewedLevelUI` is a
getter, which is the wrong property to check: it reads `sleader`, through
`getArenaUI`, and the deferred read now happens *after*
`abortMacroPlayback` has written it. What makes the move safe is
`restoreLeaderFromRun`'s own guard — it switches only to a run leader that
`memActor` still finds on `getArenaUI`'s arena (`FrameM.hs:226-229`, pinned
by AS13) — so the arena, and hence `lidV`, is the same on both sides of the
write. Said out loud, because in a document about branch-exactness an
unremarked move across a write is exactly what a reader should distrust.

The end state is worth stating plainly: `promptGetKey` still mutates. The
macro-frame advance, the voicing message, `recordHistory`, the common
cleanup and — on the abort branch — the leader restore all stay in the
shell; a key-read primitive driving a macro machine cannot be pure. What
leaves is the *decision*; what changes about the mutations is that each is
named, gated on that decision and enumerable — the migration document's
audit step then pins the enumeration in code.

> **Why keep the leader restore at all.** Restoring the pointman to the run's
> original leader when a run is interrupted is deliberate UX the author
> designed, and it is preserved verbatim — just relocated into a named action
> and gated on the pure `AbortPlayback` decision. After the live-read
> design (the post-mortem's §10), its `sleader` write is simply an update
> to the one source of truth that every subsequent read observes. So the
> live-read design *subsumes* the danger; this document removes the
> remaining *surprise*, and yields a pure, testable interrupt decision as
> a bonus. No defensive re-syncs are introduced anywhere.

### Blast radius and testability

`promptGetKey` is called from exactly two engine modules,
`Client/UI.hs:194` and `SlideshowM.hs:421`, and its type does not change,
so the abort-split is entirely internal to `FrameM` plus one new pure
module. One of the two call sites is exercised: the store-dialog ESC test
(`test/HandleHumanLocalMUnitTests.hs`) reaches `promptGetKey` through the
`SlideshowM` site, while the AS contract series calls the primitive
directly from `test/FrameMUnitTests.hs` and so exercises neither site — it
pins the primitive, which is what the split changes. `Client/UI.hs:194`,
inside `stepQueryUI`, is entered by no unit test at all; nothing in the
split reaches it, but a claim of coverage there would be wrong. The pure
`macroStep` slots into the existing test style directly:
`test/SessionUIMock.hs` already simulates macro-frame transitions
(`unwindMacros`), so play/abort/no-macro decision tables live next to
established tests rather than requiring new harness machinery.

### What would falsify this

Worth stating, as the post-mortem does for its own design (§10.7). The
split rests on one claim: that the interrupt decision is a pure function
of exactly four inputs — `sreqQueried`, whether the report disturbs
resting, the keys legal for the frame, and the pending macro frame. A
branch of `promptGetKey` that turns out to consult anything else falsifies
`macroStep`'s *signature*, not merely its body. The near misses are `dm`
and `gunderAI fact`, and they are instructive: both are read inside the
"no macro" branch today, and both stay in the shell precisely because they
gate an *effect* rather than the decision. The instrument for detecting
this is already in the tree — an AS case failing after the extraction is
that finding, which is why the migration document forbids editing one to
make it pass. What would *not* falsify anything: the leader restore
turning out to be unwanted UX. That changes what `abortMacroPlayback`
contains, not whether the decision can be lifted out of the effect.

> **⚠ Sequencing: the abort-split is hygiene, not the fix — and it assumes
> live-read.** The abort-split alone would *not* fix the bug family: the
> leader restore would still happen mid-dialog, merely under a nicer name,
> and every threaded copy would still go stale. The abort-split is therefore
> planned strictly after the live-read design and *assumes* it throughout:
> its correctness argument is "the restore is an ordinary write to the
> single source of truth, observed by every (live) reader" — a statement
> that is only true once live-read has landed. Concretely: land the
> live-read design (steps 1–5 of §02 of `docs/leader-desync-migration.md`),
> re-enable the disabled assertion, flip the LR-series test expectations,
> and only then extract `macroStep`/`abortMacroPlayback`. The AS-series
> tests are deliberately written as *contract tests* against
> `promptGetKey`'s unchanged type — they must pass before live-read, after
> it, and after the abort-split, so they serve as the refactor's safety net
> rather than as characterizations to flip.

## 02–03 · Moved to the migration document

The migration step and the joint test battery now live in
`docs/leader-desync-migration.md` (§04 and §05 there), which is written to
be deleted when the work lands. What stays here is `promptGetKey`'s branch
behaviour, the invariants any refactor of it must preserve, and the split
that would tidy it — the parts that stay true afterwards and explain why
`promptGetKey` ends up shaped as it does.

---

*LambdaHack · promptGetKey hygiene (the abort-split design) · a permanent
record, companion to `docs/leader-desync-bug.md`: the primitive's branch
behaviour, the invariants that outlive any refactor of it, and the split
that would tidy it. The split is a recommendation, not applied changes;
the work list lives in `docs/leader-desync-migration.md` until it lands.
Verified against GHC 9.12.4 and a green suite — the test count lives in
that document's §05, where it is maintained.*
