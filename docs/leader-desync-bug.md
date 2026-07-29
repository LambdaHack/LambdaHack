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
> source of truth. File:line citations were verified against the tree at
> commit **2b20a8284** (2026-07-29) — the newest commit touching any file
> they cite, so the verification stands until one of those files moves,
> whatever else lands. The fixtures and tests they cite are on master,
> while the designs below are the parked part; the citation pass proves a
> cited line exists — that stamp, that it still says what the claim needs;
> re-run `python3 tools/check-plan-citations.py docs/leader-desync-bug.md`
> after touching cited files, and re-verify the only/every/never claims by
> repo-wide grep, never by re-reading one file.

> **What this record is for, and what is frozen in it.** It is kept
> indefinitely, for two readers: the one who wonders why the pointman code
> reads as it does — `CLAUDE.md`'s pointman gotcha points here — and the
> one who meets the same shape of bug elsewhere, a cached copy of volatile
> singleton state, and wants the analysis rather than the patch.
>
> §§01–09 describe the code *as of the stamped commit* and are not
> maintained against later trees. Once the live-read design lands,
> `promptGetKey` will no longer restore the pointman where §03 says it
> does, and that is as it should be: a post-mortem describes a past state,
> and the stamp says which one. The only upkeep those sections keep is an
> outcome line per claim that resolves:
>
> - §07–§08 · the reproducer and its verification — **landed** on master,
>   as the LR series and its harness (`3453b1777` through `8b5703e87`).
> - §09 · the two live sibling bugs — **open**, pinned by tests; put the
>   fixing commit here when they close.
> - §10–§11 · the live-read design and its performance argument — **not
>   applied**; the work list is `docs/leader-desync-migration.md`, which is
>   written to be deleted, unlike this file.
>
> **§§10–11 are the exception: not frozen, because not yet history.** They
> are the specification `docs/leader-desync-migration.md` executes, plus
> the argument that executing it needs no performance gate — so their
> claims about *today's* tree (the writer census, the boundary and witness
> counts, which manual re-reads exist, which modules import which) are
> held to the plan's standard, not the post-mortem's: re-verify them
> whenever the plan is re-verified, and read a drifted count here as an
> error to fix rather than as a record that has aged. The seam is worth
> naming because it is not where a reader would guess: the larger half of
> a document labelled post-mortem is the live half, and §10 is the section
> the plan points into more often than any other. The exception inside the
> exception is §11's measured baseline, which ages rather than drifts: a
> measurement stays true, and what a later comparison must repeat is its
> protocol. When the design lands, §§10–11 join the frozen part and this
> paragraph goes.

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

**Two names, one thing.** The UI calls it the *pointman*; the source calls
it the *leader* (`sleader`, `runLeader`, `pickLeader`) — one of this
codebase's deliberate naming mismatches, noted as such in `CLAUDE.md`.
Both words appear below, following whichever side is being described.

**The sections**, and which half of the record each falls in — the frozen
half is history, the live half is specification, per the callout above:

| § | holds | state |
|---|---|---|
| 01–02 | what the assertion checked, and why it looks unbreakable | frozen |
| 03 | the invisible `sleader` write, and the repo-wide writer census | frozen |
| 04–05 | the crash timeline, and the rarity arithmetic | frozen |
| 06 | the questions the report raised | frozen |
| 07–08 | the keypress-level reproducer, and the one-line read that flips it | frozen · **landed** |
| 09 | the diagnosis — the pointman is denormalised — and two live siblings | frozen · siblings **open** |
| 10 | the fix: one accessor, no threaded identity, a witness for existence, and the behavioural rule that follows | **live** · not applied |
| 11 | the performance argument, and the baseline a later comparison repeats | **live**, bar the measurement |
| 12–13 | moved to `docs/leader-desync-migration.md`; the stub at the end says why the numbers stay behind | — |

## 01 · What the assertion checked

`pointmanCycleLevel leader …` computes `np`, the next party member after
`leader` on the viewed level (in `keySelected` order, wrapping), calls
`pickLeader np`, and asserted that the switch actually took effect:

**The disabled assertion** — `HandleHelperM.hs` (shown as it read before
`4a6eca154` commented it out; at the disabled site the binders are now
`_b` and `_success`, so a grep for this text lands instead on the live
twin in `pointmanCycle`, which still reads exactly this)

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

Re-verified by repo-wide grep, as such a claim must be: those four are the
only writers under `Client/UI`. Outside it there are four more —
`Client/HandleAtomicM.hs:144` and three in the AI (`Client/AI.hs` twice,
`Client/AI/PickActorM.hs` once) — the eight that §09 counts.

The AI's three writers are outside this design's scope and cannot go
stale: no interactive wait exists on that side, so nothing an AI client
reads can be invalidated between the read and its use. The whole family is
a property of the *human* client's dialogs.

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
> — not an exotic faction, and nothing platform-specific. All three
> conditions matter to a fixture: `emptyUIFaction` defaults
> `fhasPointman` to `False`, which alone disables the restore.

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

### Where it lives, and what it needs

The reproducer is `LR3` in `test/HandleHelperMUnitTests.hs`, on the
fixtures of `test/UnitTestHelpers.hs`. Rather than reproduce code that has
since grown past this document, the four things that had to be true:

- a faction that runs as a group, mirroring the sample game's `Explorer`
  in the three properties of §03's callout, since any one of them missing
  silences the restore;
- two live heroes on one level with distinct symbols, so the `keySelected`
  order the cycling walks is stable;
- a `RunParams` whose `runLeader` is `A`, as a multi-hero run start would
  leave it;
- every step performed by the genuine engine call a keypress makes —
  `updateClientLeader` for the rotation, the real `restoreLeaderFromRun`
  for the macro death, `pointmanCycleLevel` for the cycle key — because a
  hand-built stale state would prove only that the assertion can be
  tripped, not that play can trip it.

The harness facts behind those fixtures (the stub boards, the party
family, the client caches no fixture used to fill) are documented in
`test/CLAUDE.md`, which is maintained against the code; this section is
not.

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
is `sleader` in `StateClient`, written from eight sites — `pickLeader`, the
run rotation (`RunM.hs:90`), `restoreLeaderFromRun`, the mouse-run
continuation (`HandleHumanGlobalM.hs:693`), the AI pickers, and — decisively
— `Client/HandleAtomicM.hs:144`, where the *server* reassigns the leader
when the current one dies or is affected. Simultaneously, the same identity
is frozen into a by-value `ActorId` argument threaded through the entire UI
command layer: `chooseItemHuman`, `chooseItemDialogMode`, `transition`,
`getItem`, `itemMenuHuman`, `projectHuman`, `applyHuman`, the
`pointmanCycle*` and `xhair*` families — dozens of functions.

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
permits pointman switching (`maySwitchLeader MStore{} = True`), and `recCall`
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

**The seven subsections.** 1–4 are normative: the accessor, the parameter
drop, the partition deciding which arguments go, and the boundary with its
witness. 5–7 are consequences and rulings: the effect on the test suite,
the behavioural rule (read live where an interaction *chooses*, pin where
it *confirms*), and what would falsify the design. Inside 4, the two
type-level sharpenings are held in reserve and the two don't-dos are
recorded as refuted; read those four only if the witness itself is being
re-litigated, and skip to 5 otherwise.

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
  side <- getsClient sside
  return $! fromMaybe
    (error $ "getLeaderUI: pointman vanished under a witness"
             `showFailure` side) mleader
    -- the codebase's idiom, not a bare error string: a crash from a
    -- release build (@-fno-ignore-asserts@ is unconditional) then names
    -- the faction whose pointman went missing

-- | For interactions that outlive a single step (dialogs, runs, aiming):
-- the leader may have died or been reassigned meanwhile, so the caller
-- handles Nothing by exiting the interaction. Not defensive -- correct.
getLeaderUIMaybe :: MonadClientUI m => m (Maybe ActorId)
getLeaderUIMaybe = getsClient sleader
```

The accessor's totality is architectural, not hopeful: `loopUI` is strictly
sequential — `receiveResponse` then `handleResponse`, one message at a
time (`Client/LoopM.hs:153-154`, and again at
`Client/LoopM.hs:188-190`) — so while a dialog blocks inside
`promptGetKey`, no `RespUpdAtomic` (hence no death, no server-side leader
reassignment) can be processed, and the only mid-dialog `sleader` writers
are client-local (`pickLeader`, `restoreLeaderFromRun`), both setting
`Just`. With a witness in scope `getLeaderUI` is therefore total in
practice; the `Maybe` variant is needed only at entry points and for
future-proofing, not sprinkled through dialogs.

How often the `Maybe` variant is actually needed is now counted rather
than predicted. Of the read-live set in the migration document's
inventory, thirteen are entry points with a case in `cmdSemanticsLeader`,
so the boundary mints their witness; the other fifteen inherit it from
their callers, all of which are inside that set bar one — `psuitReq` is
also called by `projectItem`, whose own identity is pinned (6 below), but
which is reached only from the entry point `projectHuman`, so a witness
arrives there too: the witness travels even where the identity does not.
No converted function is reachable without a mint, so `getLeaderUIMaybe`
is needed by none of them — its role is exactly the one stated above,
entry points and future-proofing, and the count says so.

The future-proofing is not idle, though: the loop machinery already
anticipates state racing commands (`sreqPending`'s warning: *"server
updated game state after current command was issued…"*; `getArenaUI`'s
comment *"the leader may just be teleporting… so not existent
momentarily"*). If the client ever becomes more asynchronous, every frozen
copy becomes a bug again; single-source-of-truth is the only shape that
survives that evolution.

### 2 · Drop the leader parameter from the interactive layer

The looping/interactive functions stop taking `leader`; they take the
witness and call `getLeaderUI` where they need the identity. The manual
re-syncs (`recCall`'s re-read, `chooseItemDialogMode`'s
post-`getStoreItem` re-read) are *deleted*: reading live is now the
default, not a patch.

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

**Body change (the same function, end to end)** — the target the spike of
the migration document's §02.0 either reaches or refutes, and the shape
the other conversions copy:

```haskell
pointmanCycleLevel witness verbose direction = do
  side <- getsClient sside
  fact <- getsState $ (EM.! side) . sfactionD
  lidV <- viewedLevelUI
  leader <- getLeaderUI witness      -- was: the caller's frozen copy
  body <- getsState $ getActorBody leader
  hs <- partyAfterLeader leader      -- keeps its parameter: the rotation
  ...                                -- pivot, the "some actor" row of 3
    (np, b, _) : _ -> do
      success <- pickLeader verbose np
      let !_A = assert (success `blame` "same leader"
                                `swith` (leader, np, b)) ()
```

Three things that draft settles. The read may sit at the top here,
because this function performs no interactive wait of its own — the
placement rule of 3's second callout binds only where a wait intervenes,
and this one's caller is what holds the value across one. `partyAfterLeader`
and `pickLeader` are untouched: they take "some actor", and the identity
handed to them is one this body has just read. And the assertion comes
back uncommented, which is the point of the exercise — with a single
source of truth `success` is a theorem about one variable rather than a
hope about two, so `np == sleader` can no longer arise from a stale
argument. The spike stops short of that last step; the migration
document's §02 re-enables it separately, and only after the conversion.

### 3 · Not every `ActorId` parameter goes — the partition

The design is *not* "delete every `ActorId` argument." The classification rule:
**does an interactive wait (`promptGetKey`) occur between where the value was
read and its last use?** If yes, the identity must be read live. If no, the
argument cannot go stale, and what happens to it is decided by position
instead — the ruling under the table. Parameters that mean "some actor" (not
"the pointman now") are correct as parameters and stay throughout:

| meaning of the param | examples | action |
|---|---|---|
| "the pointman now", held across interactive waits | transition, getItem/getFull/getGroupItem/getStoreItem, itemMenuHuman, chooseItem\*Human, chooseItemDialogMode, pointmanCycle(Level), pickLeaderWithPointer, psuitReq closure, the runDefSkills/runDefInventory right-pane callbacks, projectHuman, applyHuman, alterDirHuman/pickPoint | ❌ **drop / read live** |
| "some actor" — a target, pivot or subject | pickLeader (switch target), partyAfterLeader (rotation pivot), skillsOverlay / skillCloseUp / skillsInRightPane (described subject), accessModeBag (pure), lookAt\*, projectItem / meleeAid (the confirmation's subject — 6 below) | ✅ **keep** |
| "the pointman", single atomic step, no wait inside, dispatched *at* the boundary | waitHuman, yellHuman, selectActorHuman, acceptHuman, the `xhair*Human` family, … (fifteen, listed in the migration document's tail) | ✅ **convert** — the ruling below |
| the same, but called *below* the boundary on an identity its caller has just read | alterCommon, closeTileAtPos, applyItem, moveItems, permitted\*Client, goToXhair\*, … (the other nineteen there) | ✅ **keep** — the ruling below |

> **✓ Decision: the mechanical tail is not optional, and it splits.**
> "Convert for uniformity or leave" is not a choice a mechanical sweep can
> defer to taste, and 6 below forbids the outcome it would produce, an
> unexplained `ActorId`; so the two wait-free rows are ruled on here rather
> than site by site. The fifteen dispatched *at* the boundary
> convert: then no `CmdLeader` case passes an `ActorId` at all, which is an
> invariant checkable by reading `cmdSemanticsLeader` alone instead of by
> classifying each handler. The nineteen called *below* it keep the
> parameter, and converting them would be a regression dressed as
> uniformity: the caller has just read the live pointman, so passing it
> down is ordinary parameter passing, whereas a re-read in each callee
> would let one multi-step operation act for two different actors. Their
> parameter means "the actor this step is for" — the "some actor" row —
> and per `CLAUDE.md`'s comments rule that is stated once, at
> `getLeaderUI`, not at nineteen call sites.

> **⚠ Converting a function is not deleting its parameter.** The partition
> says *which* values must stop being threaded; where the live read then
> goes is a second decision, and it is the one that actually fixes the
> bug. The read must sit **after** every interactive wait that precedes
> the use — a read placed at the top of the body and bound to a local is
> the same stale copy under a new name. Two placements, each measured
> against the battery rather than reasoned about:
>
> - **`psuitReq` must be called from inside `psuit`.** Dropping its
>   `ActorId` and leaving `chooseItemProjectHuman`'s single call
>   (`HandleHumanLocalM.hs:367`) where it is still bakes a closure over
>   the entry actor's body and position, because the closure, not the
>   call, is what the dialog re-evaluates; sibling bug (a) would survive
>   the conversion intact. The call belongs in the `psuit` action
>   (`HandleHumanLocalM.hs:389`), which `transition` re-runs on every
>   keypress. Verified: with the call moved there, the fling-dialog test
>   flips exactly as its comment records.
> - **`pickPoint` must read after the key, not before it.** Its wait is
>   `getConfirms` (`HandleHumanGlobalM.hs:1356`) and the leader's last use
>   is the `shift (bpos b)` in the body's final line
>   (`HandleHumanGlobalM.hs:1362`), so the read has to happen between the
>   two, not at the top where the body binds `b` today. Verified the same
>   way, against the `alterDir` test.
>
> The rule generalizes: for each converted function, find the wait, then
> put the read below it.

Scope, counted rather than estimated: the `CmdLeader`-family boundary in
`HandleHumanM` sheds an argument in 29 cases (14 via `weaveLeader`, 12
constructing `CmdLeader` directly, 3 via `addLeader`), and 72 functions in
the UI tree bind an `ActorId` parameter named `leader`, with one more,
point-free, binding none — a census rather than a floor, since
`tools/leader-census.py` derives it from the tree and checks it against
the migration document's buckets in both directions, a function in no
bucket and a bucket entry in no module each failing the run. That
document's inventory lists them and applies the partition;
the dialog chain and the `psuit` protocol are the only entries carrying
semantic weight. Return types simplify too: `chooseItemDialogMode :: … → m
(FailOrCmd ActorId)` becomes `m (FailOrCmd ())` — its `ActorId` result
existed only to propagate the switch that `sleader` now propagates by
itself. That is checkable rather than plausible: the result has exactly one
consumer, `chooseItemMenuHuman` (`HandleHumanGlobalM.hs:1640-1642`), which
passes it straight on to `itemMenuHuman`, so the site becomes `Right () ->
itemMenuHuman witness cmdSemInCxtOfKM`; `chooseItemHuman`
(`HandleHumanLocalM.hs:123`) already throws the `ActorId` away.

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

The 29 cases do not name `CmdLeader` directly, though: all but twelve go
through two one-line helpers, `addLeader` and `weaveLeader`
(`HandleHumanM.hs:220-225`; the third helper of that group, `addNoError`,
sits just above them at `HandleHumanM.hs:217-218` and builds a
`CmdNoNeed`, so it never carries the leader), and three of the
twelve are point-free — `pickLeaderWithPointerHuman`, `xhairUnknownHuman`
and `xhairItemHuman` (`HandleHumanM.hs:170`, `:208`, `:209`). So the
mechanical part of the sweep is two helper signatures plus the cases
that spell the lambda out; the point-free ones need `\witness ->` written
back in wherever the handler stops taking the identity.

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
  **Spiked** (GHC 9.12.4, `-fno-code`, against a stack shaped like
  `CliImplementation`: a newtype over `StateT … IO` behind an abstract
  class): the rank-2 field, `mintHasPointman`, the boundary application, a
  converted handler and a point-free case all typecheck, and the escape is
  rejected as designed — storing the witness in a session-like record dies
  on the rigid phantom (`Couldn't match type 'k' with '*' … rigid type
  variable bound by the type signature`). Not covered by that spike: the
  ergonomics across the real 29 cases, and that `weaveLeader`/`addLeader`
  would need rank-2 signatures of their own — see step 0 of the migration
  document.
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
permissive for running without a mleader, for tests") weakened *both*
manual re-reads, from a `fromMaybe (error …)` to `fromMaybe <the held
leader>`. Under the live-read design, fixtures set the leader once
(`updateLeader` is already exported and its side assertion holds for the
stub actors — verified with the §07 reproducer fixtures, which do
precisely this via `updateClientLeader`). Then those two re-reads
*delete* — `recCall`'s and `chooseItemDialogMode`'s post-`getStoreItem`
one — and the `7e74698af` permissiveness goes with them, being nothing
but the `fromMaybe` in each. Existing expectations are unaffected
(`getArenaUI` / `viewedLevelUI` return the same level via the leader as
via its own fallback).

### 6 · What the design decides about behaviour, not only about staleness

Live-read is not only a bug fix; it settles a question the code never
asked out loud: **a dialog acts for whoever is pointman at the moment it
completes.** That is worth stating, because it is visible. Switching the
pointman inside the fling dialog will re-validate the item list under the
player's hands — the end-to-end test's flipped expectation is exactly
that, the same key script finding nothing suitable after the switch — and
sibling bug (b) closes because the choose half re-validates for the new
actor rather than approving an item the executing actor cannot use.

For dialogs that *select* something, that rule is what a player means: the
prompts, the stores and the item list all follow the pointman, so the
selection should too. The item-menu path already assumes it, at
`HandleHumanGlobalM.hs:1622`, where the code makes an actor the pointman
and then hands that same actor onwards — a live read returns exactly what
the argument carried.

There is a counter-case, and the design should name it rather than
inherit it by accident. `projectItem` (`HandleHumanGlobalM.hs:967`) and
`meleeAid` (`HandleHumanGlobalM.hs:386`) do not select; they *confirm* an
action already chosen for a particular actor, and then call
`updateTarget leader`. If the pointman changes during that confirmation —
which today only the macro-abort restore can do, since a yes/no prompt
offers no switch key — then acting for the new pointman honours the
keystroke but not the intent: the player said yes to flinging *this*
actor's item. So identity is pinned there, and the partition already
provides the vocabulary for it: both keep an explicit `ActorId` parameter,
meaning "the actor this confirmation is about", which is the "some actor"
column rather than an oversight, and the migration document's inventory
lists them under Keep for that reason. It is the one place where this
ruling overrides the wait test of 3 rather than agreeing with it, so an
inventory that applies that test mechanically will misfile them, and one
built for this very refactor did.

So the rule to write into the code is: read live wherever the interaction
*chooses*, pin deliberately wherever it *confirms* — and where a
parameter is pinned *across a wait*, say in a comment that it is pinned
and why, because an unexplained `ActorId` is what produced this document.
A parameter merely handed to a helper inside one atomic step needs no such
note: nothing can intervene to make it stale, and 3's ruling states that
convention once, at `getLeaderUI`, rather than at each of the nineteen
sites it covers.

### 7 · What would falsify this

Worth stating, because the design otherwise reads as unfalsifiable. Two
futures would bear on it, and neither overturns it. If the client ever
becomes concurrent — the loop machinery already anticipates state racing
commands — live-read becomes *more* necessary, since every frozen copy
turns into a race rather than a rarity. And if some interaction genuinely
needs an identity that survives a wait, the answer is the pinning rule
above, an explicit parameter meaning "the actor this is about", not a
return to threading the pointman: the fault was never that identities were
passed, but that a volatile one was passed under a name that promised
currency. What *would* sink the design is a demonstration that reading
`sleader` at point of use returns something other than the pointman a
player would name — i.e. that `sleader` is not in fact the source of
truth. §03's writer census is the check on that, and it is why the census
is repo-wide rather than local.

## 11 · Does the better fix cost performance?

No regression is expected, for three grounded reasons.

- **`getsClient sleader` is free.** In the real monad it is `getsClient f =
  CliImplementation $ gets $ f . cliClient` over `StateT CliState IO`, and
  it is `INLINE`d (`MonadClientImplementation.hs:66-67`); reading the
  leader is then two record-field projections (`cliClient`, then
  `_sleader`) — O(1), no allocation, the same order as passing an `ActorId`
  (a newtype over `Int`) down the stack.
- **None of the changed code is a hot path.** The leader-threaded
  functions are the *human* interactive layer, running at keypress/frame
  frequency. Verified by grep, not by inspection of one file: no module
  under `Client/AI` or `Server` imports `InventoryM`, `HandleHumanLocalM`
  or `HandleHumanGlobalM`, so AI clients never call the item-dialog code
  (`Client/AI/PickActionM.hs`'s `projectItem` is a homonym of the UI's,
  not a call into it), and the benchmarks are AI-vs-AI and headless
  (`--frontendNull`/`--frontendLazy`, `--automateAll`), so they don't
  touch it at all. The throughput-sensitive engine (server loop, dungeon
  gen, FOV) is untouched.
- **The pattern is already idiomatic here.** The author deliberately reads
  ambient state at point of use for genuinely hot data — the
  `Point`/`Vector` `Enum` instances read a global dungeon width
  (`speedupHackXSize`) rather than threading it. If that is acceptable for
  a hot inner-loop coordinate encoding, a leader lookup a few times per
  keypress is a non-issue.

> **Two implementation caveats (not design costs).** Don't move a
> `getLeaderUI` call *inside* a tight per-item/per-actor inner loop that runs
> each frame — the rule is "don't cache identity *across* interactive steps,"
> not "re-read inside every micro-loop"; still bind it once per step and use
> that binding. And the `HasPointman` witness is erased at compile time — zero
> runtime cost. The existing `bench*` targets won't measure any of this because
> they don't exercise the code.

> **✗ Ruled out: the menu-navigation microbenchmark.** The honest check on
> the changed layer would be a microbenchmark driving dialog navigation,
> and earlier drafts of this section asked for one. It is not to be built.
> `LambdaHack.cabal` declares no benchmark component at all — the `bench*`
> targets are whole-game `--benchmark` runs of the executable — so this is
> a new component rather than a follow-up, and it would exist to measure a
> layer that runs at keypress frequency and whose per-call cost is two
> record projections behind an `INLINE`. What a regression here would
> really look like is the migration reaching into engine-hot code by
> accident, and the allocation totals below already catch that: under the
> fixed seeds they reproduce to about 0.001%, orders of magnitude tighter
> than any such slip. The baseline is therefore the gate, and the migration
> document's §02 says the same from the other side — performance needs no
> gate, because no benchmark reaches this layer.

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

| target | wall s | user s | clips/s | FPS | frames | bytes allocated | max residency B |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `benchBattle` | 1.33 (±0.01) | 0.58 | 10013 | 2487 | 1500 | 1,414,005,328 | 7,943,432 |
| `benchAnimBattle` | 1.64 (±0.01) | 0.57 | 7424 | 7666 | 7009 | 1,471,375,224 | 7,968,984 |
| `benchCrawl` | 2.73 (±0.01) | 1.95 | 12320 | 3525 | 7010 | 5,017,126,528 | 9,172,888 |
| `benchMemoryAnim` | 5.12 (±0.01) | 3.21 | 12972 | 7515 | 33015 | 8,082,879,864 | 12,840,120 |

Productivity was 98–99% in every run of the trio and 89% under
`benchMemoryAnim`'s deliberately tight `-A1M` allocation area. The
clips/s and FPS medians are stable to within 1% (`benchBattle`, whose
session is only 0.6 s, is the noisiest; the others stay within 0.3%),
and the `frames` column is itself deterministic — each figure is the
recipe's own `--stopAfterFrames` cap (1500, 7000, 7000, 33000) plus a
fixed overshoot of 0 to 15, so it is a free determinism check before
comparing rates. Under the fixed seeds the allocation totals are
reproducible to about 0.001% (verified across two full batteries; six
`benchMemoryAnim` reps spanned 8,082,879,864–8,082,951,864 bytes), so they
are the most sensitive tripwire in the table — max residency wobbles by
tens of kilobytes with GC sample timing: any accidental reach of the
migration into engine-hot code will move the totals far above that band,
while wall-clock noise could mask a small slip. None of these targets
enters the UI dialog code at all, so the table measures the engine the
migration must not disturb rather than the layer it changes — which, per
the ruling above, is the whole of the intended coverage.

These numbers stay in this record rather than in the migration document,
because a measurement does not expire, it merely ages: the protocol above
is what a post-migration comparison must repeat. As of the stamp they have
**not** been re-measured after any part of the design; when they are, the
comparison belongs here, next to the baseline it answers.

## 12–13 · Moved to the migration document

The migration order, the conversion inventory and the state of the test
battery now live in `docs/leader-desync-migration.md`, which is written to
be deleted when the work lands. What stays here is the reasoning, which
is not.

The numbering keeps its gap deliberately. `§13` and the range "sections
09-13" are cited from shipped code — `test/FrameMUnitTests.hs` and
`test/HandleHelperMUnitTests.hs` point at sections of this document by
number — so renumbering the survivors would break references that no
checker can see. A reader arriving at either number lands here and is
redirected. §13 in particular was the plan for full-dialog coverage, which
is why `test/FrameMUnitTests.hs` cites it from AS7; that plan has since
been carried out, so what it promised now exists as the end-to-end fling
and `alterDir` tests, and the harness facts it turned up are in
`test/CLAUDE.md`. Once the migration document is gone, the battery it
described is documented by `test/CLAUDE.md` and by the tests' own names
and comments.

---

*LambdaHack · pointman-desync post-mortem, reproducer & design · a
permanent record: the crash and its analysis (§§01–09) happened, the
live-read design (§10) and the performance reasoning (§11) are the
recommendation that follows, and the work list they feed lives in
`docs/leader-desync-migration.md` until it lands. Verified against GHC
9.12.4 and a green suite — the test count lives in that document's §05,
where it is maintained; the reproducer and battery are on master, the
design is not yet.*
