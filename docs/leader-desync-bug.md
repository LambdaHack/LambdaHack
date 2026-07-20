# The TAB-during-item-manipulation crash (commit 4a6eca154)

*LambdaHack · UI client · post-mortem, reproducer & design*

A stale pointman argument, threaded through the item dialog, collides with
an invisible `sleader` write that happens deep inside a keypress. The
workaround in commit `4a6eca154` guessed the cause correctly; here is the
exact mechanism, why it barely ever fires, a reproducer that reaches the bad
state through keypress-level actions, and — the point of the whole exercise
— the one design decision to change so this class of bug becomes
unrepresentable.

> commit **4a6eca154** · engine-src/…/**HandleHelperM.hs** ·
> ✓ reproduced & fix verified · GHC 9.12.4 · design: **live-read** — one
> source of truth. File:line citations are to this branch's working tree
> (this is parked WIP, not yet on master); re-run
> `python3 tools/check-plan-citations.py
> docs/leader-desync-bug.md` after touching cited files, and re-verify the
> only/every/never claims by repo-wide grep, never by re-reading one file.

**In one paragraph.** Inside the item dialog, the pointman is carried as a
plain function argument (`leader`). Every ordinary path re-reads the real
pointman (`sleader`) after each keystroke, so the two stay in sync. But
`promptGetKey` — which every dialog calls to draw a frame and read a key —
silently restores the pointman to the run leader when it interrupts a
playing macro. That write is invisible to the dialog's captured `leader`.
Press the cycle-on-level key (`A-Tab`/`C-Tab`) right afterwards and, if the
restored leader happens to equal the "next member after the stale one", the
switch is a no-op and the disabled assertion crashes. It is not
platform-specific; it needs a multi-hero run, a live macro, and unlucky
party ordering.

## 01 · What the assertion checked

`pointmanCycleLevel leader …` computes `np`, the next party member after
`leader` on the viewed level (in `keySelected` order, wrapping), calls
`pickLeader np`, and asserted that the switch actually took effect:

**The disabled assertion** — `HandleHelperM.hs`

```haskell
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
```

`pickLeader` returns `False` only when its target *already is* `sleader`.
Since `partyAfterLeader leader` never returns `leader` itself, `np ==
sleader` can only happen when `leader ≠ sleader` — that is, when the argument
threaded through the UI has drifted away from the real client-state pointman.
So the assertion was, in effect: *"the `leader` I was handed is still
current."*

## 02 · Why it normally can't fail

Every ordinary route keeps the two in lockstep:

- Top-level commands read `sleader` fresh in `cmdSemantics`
  (`HandleHumanM.hs`) before dispatching — the `CmdLeader` path.
- The item dialog re-reads `sleader` after *every* keypress, via `recCall`
  in `transition` (`InventoryM.hs`) — introduced in commit `8608d6f9c`, "Fix
  a crash when changing pointman inside menu due to 'leader' refactorings",
  an earlier instance of the very same disease.

Traced statically, every entry into `transition`, `itemMenuHuman` and
`chooseItemDialogMode` refreshes the leader. The assertion looks unbreakable
— which is presumably why the cause stayed hidden and got hacked around
instead of fixed.

## 03 · The hole: an invisible `sleader` write

There are exactly four writers of `sleader` in the UI client. The last is
the culprit:

1. `pickLeader` (`HandleHelperM.hs`) — the expected one, and the one
   `recCall` re-syncs against.
2. **`RunM.hs:90`** — a multi-actor run *rotates* `sleader` through the squad
   every step (`updateClientLeader r`), stashing the original in `runLeader`.
   Mid-run, `sleader ≠ runLeader` *by design*.
3. **`HandleHumanGlobalM.hs:693`** — `multiActorGoTo`, the mouse-driven
   go-to-crosshair run, rotates `sleader` through the squad the same way.
4. **`FrameM.hs:157`** — `promptGetKey`, called by *every* dialog to draw and
   read a key, does interrupted-macro cleanup: if a macro is pending and
   playback is interrupted, it runs `restoreLeaderFromRun`, which snaps
   `sleader` back to `runLeader`. This fires deep inside `displayChoiceScreen`,
   entirely invisible to `transition`'s captured `leader`.

Two supporting facts open the window: running is itself macro-driven
(`macroRun25 = ["C-comma", "C-v"]`), and `srunning` is cleared only when a
*real* (non-macro) key is read — so pure macro playback carries a live run
across command boundaries, right into an item dialog.

> **⚠ The precondition on the faction.** For the run to rotate the pointman
> *and* for the restore to fire, the faction must satisfy `not
> (noRunWithMulti fact)`, which unfolds into **three** conditions: `SkMove`
> in `fskillsOther` negative, not banned from cross-level switching
> (`fspawnsFast = False`), and `fhasPointman = True`. All three hold for the
> sample game's own hero faction (`Explorer`: `fskillsOther = meleeAdjacent`,
> whose `SkMove` is `-10 < 0`; `fspawnsFast = False`; `fhasPointman = True`).
> So this is **reachable in ordinary sample-game play with a multi-hero run**
> — not an exotic faction, and nothing platform-specific. (An earlier
> revision of this callout listed only the first two conditions; building the
> §13 test fixtures caught the omission — `emptyUIFaction` defaults
> `fhasPointman` to `False`, which alone disables the restore.)

## 04 · The crash recipe

Within a single item dialog, while a macro drives a stalled multi-hero run:

```text
  sleader     dialog's `leader`    event
  ───────     ─────────────────    ─────────────────────────────────────────
  A           —                    multi-hero run starts;   runLeader = A
  A → C       —                    run step rotates pointman  (RunM.hs:90)
  C           C  (captured)        dialog opens; transition binds leader = C
  C → A       C  (STALE!)          macro dies in dialog → promptGetKey →
                                     restoreLeaderFromRun  (FrameM.hs:157)
  A           C                    A-Tab / C-Tab pressed →
                                     pointmanCycleLevel C …
                                       "next member after C" = A
                                       pickLeader A → already leader → no-op
                                       ⇒ disabled assert would fire here
```

*The coincidence in the last step — restored `runLeader` equals the
next-in-cycle candidate — is what turns a silent no-op into a crash.*

1. A macro mixing squad-running with menu keys plays back: a run starts
   (`runLeader = A`), run steps rotate `sleader` to `C`; the run stalls but
   `srunning` stays `Just` (no real key read yet).
2. A later macro key opens the item dialog. `transition` captures `leader =
   C` — still in sync at this instant.
3. Inside the dialog, playback gets interrupted. The easiest trigger is right
   there in `promptGetKey`: *"a faulty key in a macro is a good reason to
   interrupt it"* — the macro's next key isn't among the dialog's legal keys.
   Cleanup fires; `restoreLeaderFromRun` sets `sleader := A`. The dialog's
   `leader = C` is now stale.
4. The player, dumped mid-dialog, presses `A-Tab`/`C-Tab`
   (`PointmanCycleLevel`). With the stale `C`, the "next member on this level"
   is computed as `A`. **If that equals the just-restored leader**,
   `pickLeader` no-ops and the assertion crashes the game.

## 05 · Why it "apparently crashes rarely"

Even when the desync happens, `A-Tab` usually picks some *other* actor,
succeeds, and `recCall` silently re-syncs — the bug self-heals with no
visible symptom. The crash needs the restored `runLeader` to be *precisely*
the next-in-cycle candidate: roughly a 1-in-(party-members-on-level) chance,
layered on top of the run + macro + dialog + interrupt timing. That, far more
than platform, is why days of normal play never hit it.

## 06 · Answers to the specific questions

**Q. Was it Windows-only?**
No. Nothing in the mechanism is platform-specific. A report from a
release-binary player is fully consistent, though: `LambdaHack.cabal` sets
`-fno-ignore-asserts` unconditionally, so release builds crash on this assert
too. (No issue in the GitHub tracker documents it — only the CHANGELOG line
for the hack itself.)

**Q. Why couldn't you reproduce it in normal play?**
Normal play never opens the window: any real keypress clears `srunning` at
top level before a dialog opens, and every dialog entry reads `sleader`
fresh. You need a recorded/replayed macro that runs a multi-selected squad
and then navigates into an item menu, playback that dies *inside* the dialog,
and then a same-level cycle key — with party ordering that lands the restored
leader on the next-in-cycle slot.

**Q. Why only `pointmanCycleLevel` and not its twin `pointmanCycle`?**
Both are reachable from the dialog with the same stale leader and both carry
the assertion. It's chance plus arithmetic: the level-filtered candidate list
is short, so the restored `runLeader` lands on the computed "next" far more
often there than in the whole-dungeon ordering.

## 07 · A concrete, keypress-level reproducer

The stale leader only survives *inside* the item dialog — top-level dispatch
always re-reads `sleader`. So a faithful test must reach the bad state through
the same client-state operations that keypresses perform, not by
hand-constructing a broken state. The reproducer builds a standard two-hero
party and walks the exact sequence from §04, using only the real engine
functions each keypress calls.

### Fixtures — `test/UnitTestHelpers.hs`

A "runs as a group" faction and two live heroes on one level, mirroring the
sample game's `Explorer` faction in the three properties the bug depends on
(§03: `fhasPointman = True`; `fskillsOther = meleeAdjacent`, whose `SkMove`
is `-10 < 0`; `fspawnsFast = False` — so `noRunWithMulti` is `False` and
cross-level pointman switching is not banned):

```haskell
partyFaction :: Faction
partyFaction = testFaction
  { gkind = emptyUIFaction { FK.fskillsOther = Ability.meleeAdjacent
                           , FK.fhasPointman = True }
  , gunderAI = False }

-- A = testActorId (the run's original leader); C = testActorId2
-- (the member a multi-hero run rotates the client pointman to).
heroA, heroC :: Actor
heroA = testActor {bhp = 100, bpos = Point 1 1}
heroC = testActor {bhp = 100, bpos = Point 2 1}

-- Distinct symbols keep the keySelected party order stable at [A, C].
actorUIA, actorUIC :: ActorUI
actorUIA = ActorUI { bsymbol = 'a', bname = "Alpha"
                   , bpronoun = "he/him", bcolor = BrCyan }
actorUIC = ActorUI { bsymbol = 'c', bname = "Cadet"
                   , bpronoun = "he/him", bcolor = BrGreen }

-- Two-hero party on one level, no leader set yet (the test sets it).
partyCliState :: CliState   -- via partyCliStateWith: State + sactorUI plumbing

-- The run that rotated the pointman: led by A, with the whole party as
-- members, as a multi-hero run start (moveRunHuman) would set it up.
runParamsA :: RunParams     -- runLeader = testActorId, etc.
```

### The test — `test/HandleHelperMUnitTests.hs`

Each step performs its keypress-driven action via the genuine engine call:
step 2 is the run rotation's write pair (`RunM.hs:90-91`), step 4 the
`restoreLeaderFromRun` that `promptGetKey` runs on interrupted macro playback
(exported for tests the same way `rollFlavourMap` is, rather than only under
`EXPOSE_INTERNAL`) and step 5 the item dialogs' cycling call
(`InventoryM.hs:398`).

```haskell
testCase "LR3: stale leader makes pointmanCycleLevel a no-op" $ do
  let testFn = do
        updateClientLeader testActorId   -- 1. run leader A is the pointman
        -- 2. run rotates pointman to C (the RunM.hs:90-91 write pair):
        updateClientLeader testActorId2
        modifySession $ \sess -> sess {srunning = Just runParamsA}
        leaderCapturedByDialog <-        -- 3. dialog binds C
          fromMaybe testActorId2 <$> getsClient sleader
        restoreLeaderFromRun             -- 4. macro dies: restore to A
        leaderBefore <- getsClient sleader
        -- 5. C-Tab inside the dialog, with the stale captured leader:
        _merr <- pointmanCycleLevel leaderCapturedByDialog False Forward
        leaderAfter <- getsClient sleader
        return (leaderCapturedByDialog, leaderBefore, leaderAfter)
  (result, _) <- executorCli testFn partyCliState
  -- Pins CURRENT (buggy) behaviour so the suite stays green: the pointman
  -- STAYS A, because pointmanCycleLevel, fed the stale C, computes "next
  -- after C" = A (already leader), pickLeader no-ops (assert would crash).
  -- After the live-read design lands, flip the last component to
  -- Just testActorId2.
  result @?= (testActorId2, Just testActorId, Just testActorId)
```

> **Green by design.** The committed test asserts the *current* buggy
> behaviour (pointman stays `A`), so the suite stays green and the reproducer
> is addable immediately. It doubles as a characterization test: flip the last
> tuple component to `Just testActorId2` and it becomes a true regression test
> that fails until the desync is fixed.

## 08 · Verified end-to-end

Both the reproduction and a minimal candidate fix were built and run
(GHC 9.12.4, `+with_expensive_assertions`). The one-line change has
`pointmanCycleLevel` read the real pointman instead of trusting its
argument:

**The minimal fix (verification vehicle)** — `HandleHelperM.hs`

```haskell
pointmanCycleLevel leaderStale verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  lidV <- viewedLevelUI
  mleader <- getsClient sleader        -- read the real pointman;
  let leader = fromMaybe leaderStale mleader   -- ignore the stale argument
  body <- getsState $ getActorBody leader
  hs   <- partyAfterLeader leader
  ...
```

| engine | result tuple | test verdict |
|---|---|---|
| unmodified (hacked around) | (C, Just A, **Just A**) | ❌ reproduces the bug — pointman fails to advance |
| with the one-line fix | (C, Just A, **Just C**) | ✅ fixed — cycling advances A → C |
| full suite (unmodified) | 116 / 116 | ✅ all pass — no regressions |

> **✗ Ruled out as a landing path.** Neither the one-liner above nor its
> sibling (have `restoreLeaderFromRun` skip the restore while a dialog is
> in progress and `sleader` is still a live party member) is to be merged
> on its own: each is one more manual re-sync of the denormalised copy —
> the same move §09 shows has already failed three times. The one-liner's
> role is verification only: it proves the reproducer flips (above) and
> re-verifies the [LR-flip] expectations before the real fix lands. The
> fix that ships is the live-read design (§10).

### How to run

```sh
# the reproducer alone (LR3; the pattern is a test-name substring)
cabal test --test-options='-p "LR3"'

# the whole suite
cabal test
```

## 09 · The deeper question: which decision was faulty

The one-line fixes in §08 work, but they are the same move that has been made
three times before — a manual re-synchronisation of a cached copy against the
real value:

- `8608d6f9c` added `recCall`'s re-read of `sleader` after each keypress in
  the item dialog.
- `chooseItemDialogMode` re-reads `sleader` after `getStoreItem`.
- `4a6eca154` — this bug — is the same class once more: the cycle-key handler
  used the frozen copy and nobody re-synced it.

Each patched one call site and left the next one waiting. That is the
signature of a **structural** fault, not a local one.

### The fault: the pointman is denormalised

"Who is the pointman" is stored in two places at once. The authoritative home
is `sleader` in `StateClient`, written from ~8 sites — `pickLeader`, the run
rotation (`RunM.hs:90`), `restoreLeaderFromRun`, the mouse-run continuation
(`HandleHumanGlobalM.hs:693`), the AI pickers, and — decisively —
`Client/HandleAtomicM.hs:144`, where the *server* reassigns the leader when
the current one dies or is affected. Simultaneously, the same identity is
frozen into a by-value `ActorId` argument threaded through the entire UI
command layer: `chooseItemHuman`, `chooseItemDialogMode`, `transition`,
`getItem`, `itemMenuHuman`, `projectHuman`, `applyHuman`, the `pointmanCycle*`
and `xhair*` families — dozens of functions.

Two representations of one fact, kept in sync by hand. Because both are just
`ActorId`, the compiler offers no help; a stale copy and a live one are
indistinguishable.

> **The core insight: identity vs. existence.** The threaded argument was
> meant to carry **existence** — "there *is* a pointman" — established once at
> the `CmdLeader` boundary so downstream code needn't repeat the `Maybe`
> check. That is "parse, don't validate", done well. But an `ActorId` argument
> also freezes **identity** — "*this* actor is the pointman right now."
> Existence is stable within a command; identity is volatile (runs rotate it,
> deaths reassign it). The fault is refining the `Maybe` on the volatile axis
> (identity) instead of the stable one (existence).

### Two more live bugs of the same family — the fling dialog

The stale copy is not only a crash risk; the same root cause is live today:

**(a) The fling dialog ranges missiles from the wrong actor.**
`chooseItemProjectHuman` computes `psuitReq leader` once — a closure capturing
the leader's position, projection skill and line-of-fire — and bakes it into
the `psuit` that `transition` re-evaluates on every keypress. The fling dialog
permits pointman switching (`maySwitchLeader MStore = True`), and `recCall`
refreshes the dialog's own leader — but not the closure. After a mid-dialog
switch from X to Y, prompts and stores show Y while suitability and range
hints are still computed from **X's position**.

**(b) Selection and execution can act for different actors.** The fling key is
`ComposeUnlessError (ChooseItemProject ts) Project` — two separate boundary
dispatches, each reading `sleader` fresh. Switch pointman inside the choose
dialog: the item was validated for X, then `projectHuman` runs for Y and looks
up the remembered `sitemSel` in **Y's** store — which can fail ("no item to
fling") or silently fling Y's copy of an item that only X's range check
approved. Not a crash; quiet incoherence with the same root cause.
(`alterDirHuman` / `pickPoint` spans an interactive wait the same way.)

Under the live-read design (§10) all three read the live pointman at each
evaluation, so the whole family collapses uniformly — evidence that the
fix targets the disease, not the symptom.

## 10 · The general fix (the live-read design): one source of truth

**Principle.** Volatile singleton UI state has exactly one representation,
read at the point of use, never snapshotted into a value that outlives a
single atomic step. The pointman is not a parameter of a command; it is
ambient, server-influenced, mutable state that the command *consults*.

Author intent is preserved by splitting the two conflated axes: keep the
existence guarantee at the boundary; always read identity live.

The middle layer thereby adopts a discipline the two outermost boundaries
already implement. The command boundary (`cmdSemantics`) reads `sleader`
fresh per command; and the *request* boundary reconciles the leader with
the server from the live value, not from any threaded copy:

**Existing reconciliation** — `Client/UI.hs · stepQueryUIwithLeader`

```haskell
stepQueryUIwithLeader = do
  side <- getsClient sside
  -- the server's idea of the leader, before the whole interactive step:
  mleader <- getsState $ gleader . (EM.! side) . sfactionD
  mreq <- stepQueryUI
  ...
      mleader2 <- getsClient sleader  -- the LIVE value, after
      return $ Just (req, if mleader /= mleader2 && not (saveCmd req)
                          then mleader2 else Nothing)
        -- RequestUI = (ReqUI, Maybe ActorId): the leader switch sent to the
        -- server is derived from sleader at send time, never from an argument.
```

So the design requires **no protocol or server change**, and no change to
either boundary: it makes the middle layer consistent with what the ends
already do. `RequestTimed` constructors carry no acting `ActorId` at all —
the server acts through its own `gleader`, updated from this
reconciliation — so a threaded copy in the middle never had authority
anyway.

### 1 · One accessor, the blessed way to ask

**New accessors** — `MonadClientUI.hs`

```haskell
-- | The current pointman, read live from the single source of truth.
-- The witness proves one exists (minted only by the checking
-- mintHasPointman; see 4 below); identity is still read at the point
-- of use, never cached across an effectful step.
getLeaderUI :: MonadClientUI m => HasPointman -> m ActorId
getLeaderUI _witness = do
  mleader <- getsClient sleader
  return $! fromMaybe
    (error "getLeaderUI: pointman vanished under a total context") mleader

-- | For interactions that outlive a single step (dialogs, runs, aiming):
-- the leader may have died or been reassigned meanwhile, so the caller
-- handles Nothing by exiting the interaction. Not defensive -- correct.
getLeaderUIMaybe :: MonadClientUI m => m (Maybe ActorId)
getLeaderUIMaybe = getsClient sleader
```

The accessor's totality is architectural, not hopeful: `loopUI`
(`Client/LoopM.hs`) is strictly sequential — `receiveResponse` then
`handleResponse`, one message at a time — so while a dialog blocks inside
`promptGetKey`, no `RespUpdAtomic` (hence no death, no server-side leader
reassignment) can be processed, and the only mid-dialog `sleader` writers
are client-local (`pickLeader`, `restoreLeaderFromRun`), both setting
`Just`. With a witness in scope `getLeaderUI` is therefore total in
practice; the `Maybe` variant is needed only at entry points and for
future-proofing, not sprinkled through dialogs.

The future-proofing is not idle, though: the loop machinery already
anticipates state racing commands (`sreqPending`'s warning: *"server
updated game state after
current command was issued…"*; `getArenaUI`'s comment *"the leader
may just be teleporting… so not existent momentarily"*). If the client
ever
becomes more asynchronous, every frozen copy becomes a bug again;
single-source-of-truth is the only shape that survives that evolution.

### 2 · Drop the leader parameter from the interactive layer

The looping/interactive functions stop taking `leader`; they take the
witness and call `getLeaderUI` where they need the identity. The manual
re-syncs (`recCall`'s re-read,
`chooseItemDialogMode`'s post-`getStoreItem` re-read) are *deleted*: reading
live is now the default, not a patch.

**Signature change (representative)**

```haskell
-- before: identity frozen into the argument
pointmanCycleLevel :: MonadClientUI m
                   => ActorId -> Bool -> Direction -> m MError
transition :: ... => ActorId -> ... -> m (Either Text ResultItemDialogMode)
chooseItemDialogMode :: ... => ActorId -> Bool -> ItemDialogMode
                     -> m (FailOrCmd ActorId)

-- after: identity read live inside; the witness carries only existence
pointmanCycleLevel :: MonadClientUI m
                   => HasPointman -> Bool -> Direction -> m MError
transition :: ... => HasPointman -> ... -> m (Either Text ResultItemDialogMode)
chooseItemDialogMode :: ... => HasPointman -> Bool -> ItemDialogMode
                     -> m (FailOrCmd ())
```

### 3 · Not every `ActorId` parameter goes — the partition

The design is *not* "delete every `ActorId` argument." The classification rule:
**does an interactive wait (`promptGetKey`) occur between where the value was
read and its last use?** If yes, the identity must be read live; if no, the
argument is a harmless local. And parameters that mean "some actor" (not "the
pointman now") are correct as parameters and stay:

| meaning of the param | examples | action |
|---|---|---|
| "the pointman now", held across interactive waits | transition, getItem/getFull/getGroupItem/getStoreItem, itemMenuHuman, chooseItem\*Human, chooseItemDialogMode, pointmanCycle(Level), pickLeaderWithPointer, psuitReq closure, projectHuman, applyHuman, alterDirHuman/pickPoint | ❌ **drop / read live** |
| "some actor" — a target, pivot or subject | pickLeader (switch target), partyAfterLeader (rotation pivot), skillsOverlay / skillCloseUp / skillsInRightPane (described subject), accessModeBag (pure), lookAt\* | ✅ **keep** |
| "the pointman", single atomic step, no wait inside | waitHuman, yellHuman, moveRunHuman, selectActorHuman, … | ✅ **optional** — mechanical; convert for uniformity or leave |

Scope: the `CmdLeader`-family boundary (29 cases in `HandleHumanM`) sheds an
argument mechanically; roughly 15–25 middle-layer functions convert, of which
only the dialog chain and the `psuit` protocol carry semantic weight. Return
types simplify too: `chooseItemDialogMode :: … → m (FailOrCmd ActorId)` becomes
`m (FailOrCmd ())` — its `ActorId` result existed only to propagate the switch
that `sleader` now propagates by itself.

### 4 · Keep the boundary; move the refinement to the existence axis

The `CmdLeader` boundary in `cmdSemantics` stays the one place that turns
`Maybe ActorId` into a friendly failure. It stops passing the `ActorId`
down; what it hands downstream instead is the zero-width witness — kept
*abstract*, so the type-checker enforces both that commands hold a proof
of existence and where such proofs come from:

**The witness and its only mint** — `MonadClientUI.hs`

```haskell
module ...MonadClientUI
  ( HasPointman   -- abstract on purpose: no constructor exported
                  -- (EXPOSE_INTERNAL aside), so the checking mint below is
                  -- the only source of witnesses
  , mintHasPointman, getLeaderUI, getLeaderUIMaybe, ...
  ) where

-- | A proof that a pointman exists, minted once per command.
-- Zero-width: carries existence, NOT identity, so it cannot go stale.
data HasPointman = HasPointman

-- | The only way to obtain the witness: check the source of truth.
mintHasPointman :: MonadClientUI m => m (Maybe HasPointman)
mintHasPointman = do
  mleader <- getsClient sleader
  return $! HasPointman <$ mleader
```

**The boundary** — `HandleHumanM.hs`

```haskell
data CmdLeaderNeed m =
    CmdNoNeed (m (Either MError ReqUI))
  | CmdLeader (HasPointman -> m (Either MError ReqUI))   -- was: ActorId ->

cmdSemantics cmd = case cmdSemanticsLeader cmd of
  CmdNoNeed mreq -> mreq
  CmdLeader f -> do
    mwitness <- mintHasPointman
    case mwitness of
      Nothing -> weaveJust
                 <$> failWith "command disabled when no pointman designated…"
      Just witness -> ...remote-level checks... >> f witness
                      -- existence proven, identity live
```

With the constructor unexported, a function cannot conjure the witness out
of thin air: every witness comes, transitively, from a `mintHasPointman`
check — even the boundary cannot mint one without checking. Placing the
type in `MonadClientUI` keeps it importable by the whole interactive layer
(`HandleHumanM` sits atop the UI import graph, so the type cannot live
there). The `EXPOSE_INTERNAL` escape hatch stays, as for every internal in
this codebase — forging then requires importing a name the export list
marks internal, a deliberate act a reviewer sees, not an accident the
type-checker misses.

One hole remains: a witness *stored* across commands outlives the existence
it proved — existence is stable within a command, not between commands. Two
sharpenings close it, should stored witnesses ever appear:

- **Scope the witness like `ST`**: give it a phantom parameter and make the
  boundary rank-2 — `CmdLeader (forall s. HasPointman s -> m …)` — so the
  type-checker rejects any witness escaping its command's scope (no
  `Session`-storable type can mention the `s`). Rank-2 types are already
  common in the UI tree (`Frame.hs`, `Frontend.hs`, `DrawM.hs`).
- **A capability monad instead of a value** — the sound version of "a
  constraint instead of an argument": a newtype (constructor unexported)
  whose *only* eliminator performs the boundary check, with `getLeaderUI` a
  method of its class, so `MonadClientPointman m => …` replaces the token.
  Storage-proof too — a stored computation only ever runs under some
  checked eliminator — and a natural fit for this codebase's monad-class
  architecture, but it threads a whole new layer through the
  `*Implementation` stacks for the same guarantee. The heaviest option;
  reach for it only if witness threading proves too noisy.

And two don't-dos, so they aren't re-proposed: a bare **class constraint**
(`HasPointman m => …` over the production monad) cannot express the
boundary at all — an instance, once written, is global to that monad, so
the boundary cannot grant it to one call site and deny it to another (with
no instance nothing compiles, with one everything does); making the
constraint mean something forces the guarded-newtype eliminator, i.e. the
capability monad above. The per-call-site exceptions, **reflection's
`Given` and implicit parameters**, grant locally but are as forgeable as
an exported constructor — `give`/rebinding is public — while adding a
dependency and a style foreign to this codebase.

> **✓ Decision: the witness ships.** The "there is a pointman" guarantee
> is compiler-enforced; the cost is threading a token through the same
> call sites the `ActorId` used to travel, but a token that *cannot*
> encode a stale identity and *cannot* be minted without the check. The
> witness-free variant — plain `getLeaderUI :: m ActorId` plus the
> documented boundary invariant (with an `assert`) — was considered for
> reading cleaner, and passed over: nothing would stop a new call site
> from appearing outside any `CmdLeader` guard, and the token's carrying
> cost is one parameter on signatures that are all being edited anyway.
> The fix subsumes the whole family: no interactive loop can ever again
> hold an `ActorId` that drifts from `sleader`.

### 5 · Consequences for the test suite: simpler, not harder

The current unit tests call dialog code with a `leader` argument while
`sleader = Nothing` — which is exactly why commit `7e74698af` ("Be more
permissive for running without a mleader, for tests") weakened `recCall` to
`fromMaybe leader mleader`. Under the live-read design, fixtures set the
leader once (`updateLeader` is already exported and its side assertion
holds for the stub actors — verified with the §07 reproducer fixtures,
which do precisely this via `updateClientLeader`). Then three hacks
*delete*: the `7e74698af` permissiveness, `recCall`'s manual re-read, and
`chooseItemDialogMode`'s post-`getStoreItem` re-read. Existing
expectations are unaffected (`getArenaUI` / `viewedLevelUI` return the
same level via the leader as via the fallback).

## 11 · Does the better fix cost performance?

No regression is expected, for three grounded reasons.

- **`getsClient sleader` is free.** In the real monad it is `getsClient f =
  CliImplementation $ gets $ f . cliClient` over `StateT CliState IO`, and it
  is `INLINE`d; reading the leader is then two record-field projections
  (`cliClient`, then `_sleader`) — O(1), no allocation, the same order as
  passing an `ActorId` (a newtype over `Int`) down the stack.
- **None of the changed code is a hot path.** The leader-threaded functions
  are the *human* interactive layer, running at keypress/frame frequency.
  Verified: AI clients never call the item-dialog code, and the benchmarks are
  AI-vs-AI and headless (`--frontendNull`/`--frontendLazy`, `--automateAll`),
  so they don't touch it at all. The throughput-sensitive engine (server loop,
  dungeon gen, FOV) is untouched.
- **The pattern is already idiomatic here.** The author deliberately reads
  ambient state at point of use for genuinely hot data — the `Point`/`Vector`
  `Enum` instances read a global dungeon width (`speedupHackXSize`) rather than
  threading it. If that is acceptable for a hot inner-loop coordinate encoding,
  a leader lookup a few times per keypress is a non-issue.

> **Two implementation caveats (not design costs).** Don't move a
> `getLeaderUI` call *inside* a tight per-item/per-actor inner loop that runs
> each frame — the rule is "don't cache identity *across* interactive steps,"
> not "re-read inside every micro-loop"; still bind it once per step and use
> that binding. And the `HasPointman` witness is erased at compile time — zero
> runtime cost. The existing `bench*` targets won't measure any of this because
> they don't exercise the code; the honest check would be a
> menu-navigation/frame microbenchmark, expected to land in the noise.

**Baseline numbers for the post-landing comparison.** Recorded 2026-07-22
on the parked commit `47e05bf60`, as a no-regression tripwire: per the
above, the migration should leave every figure below unchanged to within
the stated noise. Environment: AMD Ryzen 7 5800X (16 threads), machine
otherwise idle; GHC 9.12.4, `-O1` **with `+with_expensive_assertions`**
(the development `cabal.project.local`) — a future comparison must use
the same flag set, and interleaved A/B runs of pre/post binaries built
back-to-back (CLAUDE.md's toggle-based A/B recipe) beat comparing
against these absolute numbers. Protocol: the `benchNull` trio plus
`benchMemoryAnim`, exact Makefile arguments and RNG seeds; wall/user
seconds are the median of 5 timed reps (3 for `benchMemoryAnim`),
run round-robin, spreads in parentheses; clips/s (the game's time-tick
throughput) and FPS are medians of the same reps, taken from the
`--benchmark` report each run prints. Allocation and residency come
from one added `+RTS -s` run per target. Both output channels are
redirected by the game itself whenever stdout is not a terminal
(`Main.hs`), e.g. under a test harness: the `--benchmark` report lands
in `~/.LambdaHack/stdout.txt` and the RTS summary in
`~/.LambdaHack/stderr.txt` — harvest them there after each run.

| target | wall s | user s | clips/s | FPS | bytes allocated | max residency B |
| --- | --- | --- | --- | --- | --- | --- |
| `benchBattle` | 1.33 (±0.01) | 0.58 | 10013 | 2487 | 1,414,005,328 | 7,943,432 |
| `benchAnimBattle` | 1.64 (±0.01) | 0.57 | 7424 | 7666 | 1,471,375,224 | 7,968,984 |
| `benchCrawl` | 2.73 (±0.01) | 1.95 | 12320 | 3525 | 5,017,126,528 | 9,172,888 |
| `benchMemoryAnim` | 5.12 (±0.01) | 3.21 | 12972 | 7515 | 8,082,879,864 | 12,840,120 |

Productivity was 98–99% in every run of the trio and 89% under
`benchMemoryAnim`'s deliberately tight `-A1M` allocation area. The
clips/s and FPS medians are stable to within 1% (`benchBattle`, whose
session is only 0.6 s, is the noisiest; the others stay within 0.3%),
and the session frame counts are themselves deterministic
(1500/7009/7010/33015) — a free determinism check before comparing
rates. Under the fixed seeds the allocation totals are reproducible to
about 0.001% (verified across two full batteries; six `benchMemoryAnim`
reps spanned 8,082,879,864–8,082,951,864 bytes), so they are the most
sensitive tripwire in the table — max residency wobbles by tens of
kilobytes with GC sample timing: any accidental reach of the migration
into engine-hot code will move the totals far above that band, while
wall-clock noise could mask a small slip.
A genuine measurement of the changed layer still requires the
menu-navigation microbenchmark above — none of these targets enters
the UI dialog code at all.

## 12 · Migration order and verification plan

1. **Add the witness and accessors** to `MonadClientUI`: the abstract
   `HasPointman`, the checking `mintHasPointman`, `getLeaderUI` (witness
   required) and the `Maybe` variant for entry points.
2. **Convert the dialog chain first** (InventoryM: `transition`,
   `getItem`/`getFull`/`getGroupItem`/`getStoreItem`; then `itemMenuHuman`,
   `chooseItemDialogMode` and the `choose*Human` wrappers; then `psuitReq` so
   the fling closure reads live). Delete `recCall`'s re-read, the
   post-`getStoreItem` re-read, and the `7e74698af` test permissiveness as each
   becomes dead.
3. **Re-enable both "same leader" assertions** — restore the one disabled by
   `4a6eca154` in `pointmanCycleLevel`; they are now theorems about a single
   variable rather than hopes about two.
4. **Flip the §07 reproducer's expectation** to `Just testActorId2` — it
   becomes the permanent regression test for the family (already verified to
   pass under the live-read fix). Set `sleader` in the stub fixtures via
   `updateLeader`.
5. **Sweep the remaining `CmdLeader` layer** mechanically (the 29 boundary
   cases and one-shot handlers), guided by the §10 partition table; leave the
   "some actor" parameters alone.
6. **Verification**: full unit suite (the 116 + reproducer); `hlint .`; `make
   test-short` / `test-medium` playtests (AI-driven — they exercise the client
   loop, not the dialogs); a manual session replaying §04's timeline
   (multi-hero run inside a recorded macro that opens the item menu, then
   `A-Tab`/`C-Tab`), plus a fling-dialog pointman switch to confirm the §09
   sibling bugs are gone; `make frontendCrawl` for a visual pass over menus.
   Performance needs no gate — §11: no benchmark reaches this layer.

## 13 · The implemented test battery

The design is now encoded in the test suite (all green on the unmodified
engine; 148 tests total, 32 of them new). Every test carries one of two
classifications:

- **[contract]** — behaviour that must survive the live-read design (and
  any later refactor) unchanged; never flipped.
- **[LR-flip]** — characterizations of the current desync-prone behaviour,
  each with the post-live-read expectation stated inline; flipping them is
  step 4 of the §12 migration.

### The live-read series — `test/HandleHelperMUnitTests.hs` (extensive)

| test | class | pins |
|---|---|---|
| LR1, LR2 | ✅ contract | the target invariant: in-sync cycling advances Forward/Backward correctly |
| LR3 | ❌ LR-flip | the 4a6eca154 reproducer: stale leader → cycling silently no-ops |
| LR4 | ❌ LR-flip | three-member party: stale leader → the *wrong* member is picked |
| LR5 | ❌ LR-flip | the changelog crash itself: stale leader fires `pointmanCycle`'s live "same leader" assertion (caught via `try`) |
| LR6 | ❌ LR-flip | a dangling stale `ActorId` is silently tolerated and yields an arbitrary pick (unrepresentable post-live-read) |
| LR7, LR8 | ✅ contract | `partyAfterLeader` pivot rotation, incl. the unknown-pivot edge that enables `np == sleader` (its parameter survives live-read per the §10 partition) |
| LR9 | ✅ contract | the `pickLeader` primitive: no-op on current, switch otherwise |
| LR10, LR11 | ✅ contract | banned factions: dungeon-wide cycling refused, same-level cycling still allowed — the §10 partition subtlety a live-read rewrite must not change |
| LR12 | ✅ contract | the dungeon-wide twin's non-banned success path: in-sync `pointmanCycle` advances (the same function whose live assertion the desync crashes in LR5) |
| LR13 | ✅ contract | the `CmdLeader` boundary itself: with no pointman designated, dispatch refuses with the friendly failure — the one place that turns `Maybe ActorId` into an `MError`, kept by §10 |

LR1, LR2 and LR10–LR13 drive the command through the real key-loop entry
point (`cmdSemInCxtOfKM`, with the key looked up in the sample game's
bindings — the `dispatchCmd` helper), so the leader the handler cycles from
is read from `sleader` at dispatch time, in sync by construction, as for
any top-level keystroke. LR3–LR5 instead call
`pointmanCycleLevel`/`pointmanCycle` the way the item dialogs do
(`InventoryM.hs:398` and `InventoryM.hs:431`), with a held leader — the
desync's entry point — after the real `restoreLeaderFromRun` has moved the
pointman under them.

### The §09 sibling bugs — `test/HandleHuman{Local,Global}MUnitTests.hs`

| test | class | pins |
|---|---|---|
| fling suitability closure differs per actor | ❌ LR-flip | Sibling bug (a)'s testable ingredient: `permittedProjectClient`'s closure gives a different verdict per actor (`Right True` vs `Left ProjectUnskilled` on the same item), so the dialog reusing the entry actor's captured closure after a pointman switch judges items for the wrong actor. Post-live-read the closure reads the live pointman per evaluation (`psuitReq` loses its argument), so the capture becomes unrepresentable and the test changes shape. |
| psuitReq verdict differs per actor | ❌ LR-flip | sibling bug (a) at the exact captured value: `psuitReq` — what `chooseItemProjectHuman` bakes into the dialog's `psuit` — gives a different failure per actor with the xhair on C's own position ("aiming obstructed by terrain" for A, the degenerate "aiming blocked at the first step" for C), through the real aiming pipeline, no walkable tiles needed |
| Project executed by a different actor than the item selection | ✅ contract | Sibling bug (b), both halves of the seam: with `sitemSel` left by A's choose dialog, `projectHuman` run for A gets past the store lookup (control), run for C fails with "no item to fling" for the item just approved. Deliberately [contract]: the execute-half pinned here is correct in isolation and survives the live-read design — what it fixes is the *choose* half, whose live re-reads make the dialog re-validate for C before the selection is confirmed, closing the seam where the incoherent approval arises. |

Two constraints shaped these tests, both verified against the stub harness:
the full `psuitReq` pipeline deterministically fails on the stub's unknown-tile
board (`"aiming obstructed by terrain"` — already pinned by an existing test),
so bug (a) is pinned through failure verdicts rather than through the whole
dialog; and `projectHuman`'s store lookup precedes all aiming, so bug (b)
is fully drivable without walkable tiles. The remaining §09 pattern mention —
`alterDirHuman`/`pickPoint` spanning an interactive wait — is not separately
tested: its mechanism (a mid-`promptGetKey` restore invalidating a held leader)
is exactly what the FrameM contract tests pin (`test/FrameMUnitTests.hs`).
Driving `pickPoint` to completion is unblocked by the scripted-keys stub
(below), but on the all-unknown board every alter attempt fails identically
regardless of position; a per-actor observable therefore needs walkable
tiles, so that test joins ingredient 4's queue.

### Lifting the (a) limitation: full-dialog coverage

The pin is being upgraded to a true end-to-end reproduction: the fling
dialog itself, a mid-dialog pointman switch by keypress, and the stale
suitability verdict observed in the dialog's result. Three of the four
harness ingredients are built; walkable tiles remain:

1. **Built — the pin lifted from `permittedProjectClient` to `psuitReq`
   itself** (the exact value `chooseItemProjectHuman` captures), by
   exploiting actor-dependent *failures* — the "psuitReq verdict differs
   per actor" test above. Its heroes sit on row 0: `speedupHackXSize`
   (the `Point` `Enum` width hack) keeps its default 80 in the test
   binary, so on the 3×3 stub board only row-0 `ltile` lookups stay in
   bounds.
2. **Built — the scripted-keys frontend stub**: `scriptedFchanFrontend` in
   `UnitTestHelpers` closes over an `IORef [K.KM]`, pops a key per
   `FrontKey` request and falls back to `escKM`; `partyCliStateScripted`
   wires it into a fixture. It already lets the end-to-end window replay
   press a literal `C-Tab` (`FrameMUnitTests.X2`) and unblocks the
   `pickPoint`/`alterDirHuman` sibling.
3. **Built — real key bindings in the fixture CCUI**: `stubSessionUI` now
   carries `IC.makeData Nothing standardKeysAndMouse` (the *sample game's*
   bindings — the game's `Client.UI.Content.Input`, not the engine's, per
   the duplicate-basename gotcha in CLAUDE.md), which bind
   `Tab`/`A-Tab`/`C-Tab` to the cycle commands, so `revCmd`-style lookups
   resolve to real keys with no hand-rolled content (`dispatchCmd` and X2
   use them).
4. **Walkable tiles** — the fiddly one, still open: a `floorTile` with
   `Walkable`/`Clear`, modeled on `emptyUnknownTile` (exported only under
   `EXPOSE_INTERNAL`; move it to the unit-tests export section, or rely on
   the `release` flag defaulting True); `TK.makeData
   [emptyUnknownTile, floorTile] [] []`; the level's `ltile` filled with the
   floor id — and, crucially, `coTileSpeedup = Tile.speedupTile False cotile`
   rebuilt *in the same update*, because `updateCOpsAndCachedData` recomputes
   only the skills cache, not the tile speedup. Budget an iteration against
   `TK.makeData`'s content validation. This ingredient also pays beyond (a):
   it is exactly what the existing `psuitReq` test's TODO ("split the test into
   three, each taking a different branch") has been waiting for.

With walkable tiles in place, the target test drives
`chooseItemProjectHuman A` with scripted keys `[C-Tab, Return]` and
skill-asymmetric heroes (C unskilled): after the real in-dialog switch to
C, the dialog's captured A-closure still marks the item suitable, so the
selection succeeds and `sitemSel` is set — for an item the live pointman
cannot fling. Post-live-read the same key script finds no suitable item
after the switch, flipping the observable.

Everything but the fling closure is already pinned in-suite:

- rendered (`onBlank = False`) frames work under the mock
  (`FrameMUnitTests.AS7`);
- the dialog machinery is drivable end-to-end: "chooseItemHuman: ESC exits
  the real store dialog" (`test/HandleHumanLocalMUnitTests.hs`) runs
  `chooseItemDialogMode` → `getStoreItem` → `displayChoiceScreen` to the
  "never mind" exit, and encodes the two prerequisites it uncovered — the
  item must be held by the actor *and* registered in `sitemD` (a separate
  `updateItemD` step), and dialog prompts need a screen wider than 4
  (`indentSplitAttrString`'s assertion; the screen is enlarged per-test,
  the level stays 3×3);
- the mid-dialog switch by keypress works: "chooseItemHuman: scripted Tab
  switches pointman mid-dialog" runs the dialog's own cycling handler and
  `recCall`'s re-entry (the §02 re-sync from commit `8608d6f9c`,
  previously untested) on the equipment store, which needs no aiming.

What remains for the fling version is only the captured `psuitReq`
closure observable — i.e. walkable tiles.

---

*LambdaHack · pointman-desync post-mortem, reproducer & design · verified
against GHC 9.12.4, cabal test suite (148 tests, all green). Engine
behaviour untouched (the sole engine-source change is one export-list
edit in `FrameM.hs`); fixtures + tests added under `test/`
(`UnitTestHelpers`,
`HandleHelperMUnitTests`, `FrameMUnitTests`, `HandleHumanLocalMUnitTests`,
`HandleHumanGlobalMUnitTests`). §§09–12 are a design recommendation, not applied
changes; §13 documents the implemented test battery.*
