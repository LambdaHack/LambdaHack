# The leader-desync migration plan

*LambdaHack · UI client · the work list for two designs — written to be
deleted*

> **This document is temporary by design.** The permanent records are
> `docs/leader-desync-bug.md` (the crash, the analysis, the live-read
> design) and `docs/promptgetkey-hygiene.md` (the abort-split). This file
> holds only what stops being true when the work lands: the ordering, the
> conversion inventory, the artifacts still to write, and the state of the
> test battery. When §04's last step is done, delete this file and
> `tools/leader-census.py`, which serves only this document, note the
> landing in `CHANGELOG.md` (the lines are drafted in §02), and add to each
> record the outcome line it reserves. Nothing here is worth keeping
> afterwards — the records carry the reasoning, the code carries the
> result.
>
> File:line citations were verified against the tree at commit
> `ef2b6b263` (2026-07-29) — the newest commit touching any file they
> cite; re-run `python3 tools/check-plan-citations.py
> docs/leader-desync-migration.md --restamp` after the reading pass, and
> re-verify the only/every/never claims by repo-wide grep. Verify the
> post-mortem's §§10–11 in the same pass: they are the live half of that
> record — the design this plan executes — so they drift with this file
> rather than ageing with its frozen sections.

## 00 · Status, size and per-step checks

Keep this table current as the work proceeds; it is the reason this
document lives in the repository rather than in someone's head. Steps 2–4
of §02 belong in **one** commit — a characterization and the code it
characterizes must not be committed apart — so they share a row.

| step | touches | check when done | state |
|---|---|---|---|
| §02.0 spike | `MonadClientUI` plus the three frames of `PointmanCycleLevel` | it compiles; the witness reads tolerably at a real call site; LR1/LR2 green and LR3 still unflipped | pending |
| §02.1 witness, accessors | `MonadClientUI` only, ~30 lines | build clean; `-p "/contract/"` green | pending |
| §02.2–4 dialog chain, assertions, flips | `InventoryM` (7 functions), `HandleHumanLocalM` (4 wrappers and `psuitReq`'s call site), `HandleHelperM` (the one assertion `4a6eca154` disabled), ~14 test edits across 4 test modules | `-p "/contract/"` green *unchanged*; `-p "/LR-flip/"` green *with the flipped values*, each flip verified first against the candidate one-liner | pending |
| §02.5 sweep | the 29 boundary cases, the remainder of §03's read-live set and the fifteen convert-half of its tail, across 4 modules | build clean; `-p "/contract/"` green; `hlint .` says `No hints`; no `CmdLeader` case passes an `ActorId` | pending |
| §02.6 verification | nothing; it is the gate | full suite; `make test-short`, `make test-medium`; the manual timeline session and a fling-dialog switch | pending |
| §04.1 extract `macroStep` | one pure function, plus the eight-row table in §04 and the missing special-event AS case | the table passes; the new AS case passes; the rest of the AS series untouched and green | pending |
| §04.2 name `abortMacroPlayback` | `FrameM`, ~10 lines | AS4–AS6 green *without edits* | pending |
| §04.3 audit the residual writes | `FrameM` only; the drafted haddock in §04 | the haddock lists every write the body performs | pending |
| §04.4 AS series unchanged | nothing; it is the gate | AS1–AS13 and X1/X2 pass with no edits to them | pending |
| §05 battery | — | landed on master (`3453b1777` through `8b5703e87`) | **done** |

### Log

One line per surprise or re-plan, newest last, so that resuming needs this
section rather than a re-read. Log-worthy: a step that turned out larger
or smaller than its row says, a design question reopened, a count or
classification here found wrong, an ordering constraint discovered. Not
log-worthy: doing a step as written, or editing this file before the work
starts — an entry that records only that the plan was written is one the
next reader has to skip.

- 2026-07-29 · plan split out of the two records; the battery is on master;
  nothing of §02 or §04 started. The open design question is the one §02.0
  exists to answer.
- 2026-07-30 · verification pass found two classification errors in §03,
  both leaving the read-live set at 28 and the 13/15 witness split intact,
  by coincidence rather than by cancellation. A third blind spot in the
  extraction rule — a `leader` bound later in the head — had silently
  dropped `runDefSkills` and `runDefInventory`, which hold the entry
  leader in a right-pane callback the menu loop re-invokes; they are now
  in the read-live table, and §02 step 2 converts them with the dialog
  chain. And `projectItem`/`meleeAid` were listed read-live against the
  post-mortem's §10.6, which rules them pinned; they move to Keep. Read
  the pairing as the standing warning it is: an inventory built from a
  grep proxy inherits the proxy's blind spots, and a bucket assigned from
  the "holds it across a wait" test inherits nothing about whether the
  interaction chooses or confirms.
- 2026-07-30 · §03's census is now `tools/leader-census.py`, which derives
  the surface from the tree and checks it against the buckets both ways;
  the hand rule's 64 becomes a census of 72, and the five DrawM and
  `msgAddDone` entries the old rule dropped get an explicit Keep ruling.
  Mapping §04 step 1's pure cases onto the abort-split checklist turned up
  a hole that is not a documentation one: **the special-event branch
  (`dm /= ColorFull`) is entered by no test**, every AS case reading with
  `ColorFull` — and it is the block the split relocates, so step 1 now
  requires the missing case before the extraction. §11's
  menu-navigation microbenchmark is ruled out rather than left open.

## 01 · Sequencing: two designs, one campaign

Live-read first and in full; the abort-split strictly after it and
assuming it. The abort-split record's own normative callout says why:
against the pre-live-read tree the leader restore still invalidates
threaded copies, so naming it would decorate the bug rather than remove
it.

The test battery (§05) is already in the tree, on master, and stays green
throughout. It is the safety net the conversion runs on, not an output of
it: `-p "/contract/"` must pass at every step, and `-p "/LR-flip/"` is the
set that flips once, with the engine change that earns it.

Every step below leaves the tree buildable, green and shippable, so there
is no rollback procedure to write beyond reverting the commit that
carries the step. The only step that *looks* irreversible is the flip of
the characterizations (§02, step 4), and it reverts together with the
engine change it accompanies — which is exactly why the two belong in one
commit.

## 02 · Live-read: migration order and verification

Steps 1–5 are one logical change and belong in one commit, the flips of step
4 included: a characterization and the code it characterizes must not be
committed apart, or the suite is red in between and the flip loses its
evidence. The abort-split (§04) is a separate change on top; the test
battery (§05) is a separate one below, and has already landed.

Blast radius: six modules. `MonadClientUI` gains the accessors and the
witness, `HandleHumanM` the boundary, and the four modules §03 lists supply
the handlers — `HandleHelperM`, `InventoryM`, `HandleHumanLocalM`,
`HandleHumanGlobalM`. Nothing outside the UI client changes: not the AI
client, not the server, not the frontends, and no type crossing the
client-server boundary (the post-mortem's §10). `promptGetKey` keeps its
type, so `Client/UI.hs` and `SlideshowM` merely recompile. The work is
type-directed — change a signature, follow the errors, apply the partition
to each site the compiler names — which is also why §03's inventory is a
floor rather than a census.

**Step 0: spike one path before converting twenty-eight.** Half a day, and
the only step whose outcome can still change the design. Add `HasPointman`,
`mintHasPointman` and `getLeaderUI` to `MonadClientUI`, then convert a
single path end to end. `PointmanCycleLevel` is the one to pick: the battery
already pins it on both sides of the flip (LR1/LR2 as contract, LR3 as the
characterization), and it is three frames deep — `pointmanCycleLevelHuman`,
`pointmanCycleLevel`, and the dialog's own call at `InventoryM.hs:398` — so
it exercises the boundary, an intermediate and a caller that holds the value
across a wait, which is the whole shape of the change in miniature. What it
settles: whether the token reads tolerably at a real call site — the one
question the post-mortem's §10 leaves open, since it defers the capability
monad to "if witness threading proves too noisy" — and whether the boundary
sketch survives contact with `weaveLeader`'s point-free reality. What would
change the plan: noise at three sites will be noise at twenty-nine, and the
witness-free variant (the post-mortem's §10, considered and passed over)
becomes the live choice *before* the remaining functions are touched, rather
than a regret afterwards. The spike either reverts or becomes the beginning
of step 1.

1. **Add the witness and accessors** to `MonadClientUI`: the abstract
   `HasPointman`, the checking `mintHasPointman`, `getLeaderUI` (witness
   required) and the `Maybe` variant for entry points.
2. **Convert the dialog chain first** (InventoryM: `transition`,
   `getItem`/`getFull`/`getGroupItem`/`getStoreItem`, and
   `runDefSkills`/`runDefInventory`, whose right-pane callbacks capture the
   leader the way `psuitReq` does; then `itemMenuHuman`,
   `chooseItemDialogMode` and the `choose*Human` wrappers; then `psuitReq`
   so the fling closure reads live). Delete the two manual re-reads —
   `recCall`'s and the post-`getStoreItem` one — as each becomes dead; the
   `7e74698af` test permissiveness goes with them, so there is no third
   site to hunt for (the post-mortem's §10 says why).
3. **Re-enable the disabled "same leader" assertion** — the one
   `4a6eca154` commented out in `pointmanCycleLevel`
   (`HandleHelperM.hs:129-130`); its twin in `pointmanCycle`
   (`HandleHelperM.hs:149-150`) is live already, which is what LR5
   catches. Both then become theorems about a single variable rather than
   hopes about two. Know what re-enabling switches on: both are plain
   `assert`s under no `WITH_EXPENSIVE_ASSERTIONS` guard, and
   `-fno-ignore-asserts` sits unconditionally in the cabal `options`
   stanza (`LambdaHack.cabal:155`), so from this step onwards they are
   live in *every* build — the CI playtests, which run with expensive
   assertions off, and release binaries alike, which is how the original
   crash reached a release-binary player at all (the post-mortem's §06).
   Hence the ordering: re-enable after the conversion, never before, and
   expect a surviving desync to crash `make test-gha` rather than to no-op
   quietly.
4. **Flip the whole [LR-flip] series**, verifying each flip by temporarily
   applying the candidate fix first, as `test/CLAUDE.md` requires. The set
   is exactly what `cabal test --test-options='-p "/LR-flip/"'` runs — ten
   tests — and each states its target value inline:

   - LR3–LR6 in `test/HandleHelperMUnitTests.hs`, the post-mortem's §07
     reproducer among them (already verified to pass under the live-read
     fix); LR5 changes *shape* rather than value, since it catches a live
     assertion via `Control.Exception.try` and after the fix the assertion
     no longer fires;
   - the final cycling outcome of the bridge tests X1 and X2
     (`test/FrameMUnitTests.hs`); their `promptGetKey` half is [contract]
     and must not move;
   - the two sibling-(a) pins in `test/HandleHumanLocalMUnitTests.hs`,
     which pin a capture live-read makes unrepresentable, so their two
     per-actor calls become one call before and one after a pointman
     switch;
   - the two end-to-end tests of §05 (the fling dialog, `alterDir`), each
     to the value recorded in its comment.

   Mechanical fallout of the same step, and why it is a restructure rather
   than an edit of expected values: the LR series calls the converted
   functions directly, so LR3–LR6 must obtain a witness (`mintHasPointman`,
   whose export exists for exactly this) before they can call
   `pointmanCycle`/`pointmanCycleLevel` at all, and `psuitReq` losing its
   `ActorId` updates its four branch tests. The stub fixtures set `sleader`
   via `updateLeader` — audit every test that today calls dialog code with
   no pointman designated, rather than assuming the post-mortem's §10
   prediction that the expectations are unaffected. Nothing in the
   [contract] series may move; that is what makes it a contract.
5. **Sweep the remaining `CmdLeader` layer** mechanically: the 29 boundary
   cases and, of §03's tail, the fifteen handlers dispatched at the
   boundary. Leave the "some actor" parameters and §03's other nineteen
   alone — the post-mortem's §10 rules on both, so no site needs a
   judgment call here. The step ends on an invariant worth checking by
   reading `cmdSemanticsLeader` alone: no case passes an `ActorId`.
6. **Verification**: the full unit suite (153 today, all green before the
   change), with `-p "/contract/"` kept green at *every* step of the
   migration rather than only at its end — that series is the safety net
   the conversion runs on; `hlint .`; `make test-short` / `test-medium`
   playtests (AI-driven — they exercise the client loop, not the dialogs);
   a manual session replaying the post-mortem's §04 timeline (multi-hero
   run inside a recorded macro that opens the item menu, then
   `A-Tab`/`C-Tab`) — X1 of §05 already drives that window through the
   real `promptGetKey`, so what the session adds is everything the mock
   supplies instead: a real frontend, a macro recorded by actual
   keypresses rather than a `smacroFrame` seeded in the fixture, and the
   sample game's own bindings and party — evidence that a player can reach
   the window, not only that a fixture can. Plus a fling-dialog pointman
   switch to confirm the
   post-mortem's §09 sibling bugs are gone; `make frontendCrawl` for a
   visual pass over menus. Performance needs no gate — the post-mortem's
   §11: no benchmark reaches this layer.

### The artifacts §02 asks for, drafted

Two steps above end in text that is already known, so here it is, on the
model of §04's drafts: transcription rather than rediscovery.

**The pinned-site comments**, which step 5 owes the post-mortem's §10.6 —
it requires a note wherever a parameter is pinned across a wait, "because
an unexplained `ActorId` is what produced this document". One at
`projectItem` and one at `meleeAid`, differing only in the verb:

```haskell
-- The pointman is pinned here, deliberately: this confirms an action
-- already chosen for @leader@, so a pointman swapped in by the prompt
-- itself -- only the macro-abort restore can do that, a yes/no offering
-- no switch key -- must not inherit the player's "yes". Reading
-- @sleader@ live here would be the bug, not the fix.
```

**The `CHANGELOG.md` lines** the header callout asks for on landing. The
hack being replaced has its own line under `v0.11.0.1` ("Hack around a
crash when TABbing during item manipulation") and no issue documents
either it or this (the post-mortem's §06), so neither entry carries a
link:

```
- Read the pointman live rather than threading it through the UI, fixing the TAB-during-item-manipulation crash and two fling dialog siblings
- Split promptGetKey's interrupted-macro cleanup into a pure decision and a named abort action
```

The second lands with §04, not with this section; both go in together only
if the two changes ship in one release.

## 03 · The conversion inventory

The surface the post-mortem's §10 partition has to be applied to, listed
rather than estimated, so that resuming this work needs no re-derivation.

**How the list was made** — by `python3 tools/leader-census.py`, which is
also how it is re-checked. The tool walks
`engine-src/Game/LambdaHack/Client/UI`, finds the 94 top-level functions
taking a bare `ActorId` parameter, and reports the 72 that bind one named
`leader…` (`HandleHumanGlobalM` 34, `HandleHumanLocalM` 20, `InventoryM`
10, `HandleHelperM` 4, `DrawM` 4) — then cross-checks them against the
buckets below in both directions: a function the tree holds and no bucket
names is a failure, and so is a bucket entry the tree no longer has. Run
it after any change to either side, and delete it together with this
document.

One function it cannot see, and prints for hand-classification instead:
`pickLeaderWithPointerHuman`, point-free and so binding nothing. That is
the whole of the remaining blind spot, and it is named rather than silent.

The tool exists because two grep proxies stand behind this inventory and
each has failed once, neither failure showing up as a wrong count. The
*extraction* proxy read only the first bound parameter, so `runDefSkills`
and `runDefInventory`, which bind `leader` third, were dropped along with
`msgAddDone` and DrawM's four; its 64 was a floor and said so, where the
tool's 72 is a census. The *classification* proxy — which bucket a
function then belongs in — saw a wait only in a function's own body, so
`closeDirHuman`, whose wait sits inside the `pickPoint` it calls, was
filed as harmless until re-read. The tool replaces the first proxy and
cannot replace the second: it checks that every function is in *a*
bucket, never that it is in the right one, which is what its clean run
says out loud. The type-checker is still the final word, this being a
type-directed refactor in which the compiler enumerates what is left.

**Read live** — each holds the pointman across a wait and uses it after:

| module | functions | the wait |
|---|---|---|
| `InventoryM` | `transition`, `getItem`, `getFull`, `getGroupItem`, `getStoreItem`, `runDefSkills`, `runDefInventory` | the dialog loop: `runDefInventory`/`runDefSkills` → `displayChoiceScreenWithDefItemKey`, whose right-pane argument that loop re-invokes per keypress |
| `HandleHelperM` | `pointmanCycle`, `pointmanCycleLevel`, `pickLeaderWithPointer` | none of their own; they are called *from* the dialog loop with a held leader — the crash of the post-mortem's §04 |
| `HandleHumanLocalM` | `chooseItemHuman`, `chooseItemDialogMode`, `chooseItemProjectHuman`, `chooseItemApplyHuman`, `psuitReq`, `pointmanCycleHuman`, `pointmanCycleLevelHuman`, `pickLeaderWithPointerHuman` | `chooseItemDialogMode` waits directly (`displayChoiceScreen`); the rest reach it through the chain above. `psuitReq` is the closure case — see the placement rule in the post-mortem's §10 |
| `HandleHumanGlobalM` | `itemMenuHuman`, `chooseItemMenuHuman`, `projectHuman`, `applyHuman`, `alterDirHuman`, `closeDirHuman`, `pickPoint`, `moveItemHuman`, `moveOrSelectItem`, `selectItemsToMove` | `itemMenuHuman` waits directly (`displayChoiceScreen`), `pickPoint` directly (`getConfirms`), the move family through `getFull` |

Three entries above are additions this inventory found, none of them in
the post-mortem's §09 list of symptoms, and each was missed by a different
blind spot. `closeDirHuman` (`HandleHumanGlobalM.hs:1291`) is
`alterDirHuman`'s twin — same `pickPoint` wait, same use of the held leader
afterwards (`closeTileAtPos leader p`); it was classified late because the
extraction rule sees a wait only in a function's own body, and this one's
wait is inside the `pickPoint` it calls. `runDefSkills`
(`InventoryM.hs:511`) and `runDefInventory` (`InventoryM.hs:623`) were
missed by the third blind spot, and they hold the leader the way sibling
bug (a) does rather than in a plain binding: each hands
`displayChoiceScreenWithDefItemKey` a right-pane callback closed over the
entry leader — `skillsInRightPane leader` in one, a `meleeSkill` derived
from it in the other — and that argument is re-invoked inside the menu
loop, so a mid-dialog restore leaves the pane describing an actor who is
no longer pointman. The skills dialog permits the switch outright
(`maySwitchLeader MSkills = True`, `InventoryM.hs:419`). The placement
rule of the post-mortem's §10 therefore binds here exactly as it does for
`psuitReq`: the read goes inside the callback, not at the top of the body.

Where the witnesses come from, counted: thirteen of the read-live
functions have a case in `cmdSemanticsLeader` and so receive a freshly
minted witness at the boundary — `chooseItemHuman`,
`chooseItemProjectHuman`, `chooseItemApplyHuman`, `pointmanCycleHuman`,
`pointmanCycleLevelHuman`, `pickLeaderWithPointerHuman`, `itemMenuHuman`,
`chooseItemMenuHuman`, `projectHuman`, `applyHuman`, `alterDirHuman`,
`closeDirHuman`, `moveItemHuman`. The other fifteen are internal to the
set and inherit it — all but one from a caller inside the set: `psuitReq`
is called by `chooseItemProjectHuman` (`HandleHumanLocalM.hs:367`) but
also by `projectItem` (`HandleHumanGlobalM.hs:976`), which the Keep group
below holds; `projectItem`'s only caller is the entry point
`projectHuman`, so a witness reaches it and it can pass one on. That is
the shape to expect wherever a pinned function calls a read-live one: the
witness travels even where the identity does not. `closeDirHuman` is on
the list for the same reason it appears above at all — it is
`alterDirHuman`'s twin, entry point included, its only caller being the
boundary case at `HandleHumanM.hs:129`. It is also the reason to re-derive
this count whenever the read-live set changes: a late addition there is a
late addition here, and nothing but a re-count catches it. One call site
deserves a second look during the sweep and turns out to confirm the
design: `HandleHumanGlobalM.hs:1622` calls
`itemMenuHuman newAid` right after `pickLeader False newAid`, i.e. it
hands on the actor it has just made pointman — exactly what a live read of
`sleader` returns, so the argument was carrying what `sleader` now
carries, provided the `pickLeader` stays ahead of the call.

**Keep** — the parameter means "some actor", not "the pointman now":
`pickLeader` (the switch target), `partyAfterLeader` (the rotation
pivot), `skillCloseUp` and `skillsInRightPane` (the described subject),
`accessModeBag` (pure), and every `ActorId` bound as `aid`/`source`/
`target` in `RunM`, `SessionUI`, `WatchCommonM`, `WatchSfxAtomicM` and
`WatchUpdAtomicM`.

Five more are named `leader` and mean it, but are one-step callees whose
caller has just read the pointman, so they keep the parameter for the
reason the tail's second half does: `msgAddDone`
(`HandleHumanGlobalM.hs:1328`, reached from the tile-altering and
door-closing paths once each has the identity it will act for) and DrawM's
`drawLeaderDamage`, `checkWarningHP`, `checkWarningCalm` and
`checkWarnings`, which render one frame from a leader `drawHudFrame` read
for that frame — and which the `Watch*` modules call with an `aid` that
is not the pointman at all, so a live read there would be wrong outright.
They are listed here rather than in the tail because they bind `leader`
later in the head and so fall outside the 64 the tail is counted against.

Two more are kept *across a wait*, and are the one place in this inventory
where the post-mortem's §10.6 overrides the "holds the pointman across a
wait" test rather than being served by it. `projectItem`
(`HandleHumanGlobalM.hs:967`) and `meleeAid` (`HandleHumanGlobalM.hs:386`)
each ask `displayYesNo` and *then* call `updateTarget leader`; but they
*confirm* an action already chosen for a particular actor rather than
*choose* one, and only the macro-abort restore can swap the pointman under
such a prompt, since a yes/no offers no switch key. Acting for the new
pointman would honour the keystroke and not the intent, so both keep an
explicit `ActorId` meaning "the actor this confirmation is about" — the
"some actor" column — and each site carries the comment §10.6 requires
wherever a parameter is pinned across a wait. `projectItem` here means the
UI's; the AI's `Client/AI/PickActionM.hs` has a homonym with a different
signature, which is not a call into this layer.

**The mechanical tail** — the 34 that remain after the two groups above,
so that the census partitions its 72 as 27 read live, 11 kept and these
34, where the read and the last use sit in one atomic step, so nothing
here can go stale (the read-live table lists 28 and the Keep group 12,
each counting one function the census cannot see: the point-free
`pickLeaderWithPointerHuman` and `pickLeader`, which binds `aid`). The
post-mortem's §10 rules on them by position relative to the boundary, and
the two halves get opposite answers.

**Convert** (fifteen) — dispatched *at* the boundary, so once they are
done no `CmdLeader` case passes an `ActorId` at all:

- `HandleHumanGlobalM`: `alterWithPointerHuman`, `continueToXhairHuman`,
  `moveOnceToXhairHuman`, `moveRunHuman`, `runOnceAheadHuman`,
  `runOnceToXhairHuman`, `waitHuman`, `waitHuman10`, `yellHuman`
- `HandleHumanLocalM`: `acceptHuman`, `clearTargetIfItemClearHuman`,
  `selectActorHuman`, `xhairItemHuman`, `xhairStairHuman`,
  `xhairUnknownHuman`

**Keep the parameter** (nineteen) — reached *below* the boundary, the
caller passing the identity it has just read, so a re-read in the callee
would buy nothing and could let one multi-step operation act for two
actors. Verified for ten of the nineteen, `goToXhair` being the clean
case — its only callers are three of the fifteen above; the sweep
confirms the rest as the compiler names them:

- `HandleHumanGlobalM`: `alterCommon`, `alterTileAtPos`, `applyItem`,
  `closeTileAtPos`, `displaceAid`, `goToXhair`,
  `goToXhairExplorationMode`, `goToXhairGoTo`, `moveItems`,
  `moveSearchAlter`, `processTileActions`, `verifyAlters`
- `HandleHumanLocalM`: `endAiming`, `endAimingMsg`,
  `permittedApplyClient`, `permittedProjectClient`, `posFromXhair`,
  `projectCheck`, `xhairLegalEps`

`applyItem` earns a place in this tail at all, rather than in the
read-live table above, by a hair: it also asks `displayYesNo`, but makes
no use of the leader afterwards.

## 04 · Abort-split: migration step and verification

The abort-split is the final step of the joint migration — strictly after
§02, assuming the live-read design is complete and its verification step
passed, and with `macroStep`'s home settled per the callout in the
abort-split record's §01 (either `InputDecision`, if the wasm plan's
Phases 0/2 have landed by then, or `FrameM`'s own pure section until they
do):

1. Extract `macroStep` into `InputDecision`, with the *decision* half of
   the record's §01 branch-exactness checklist as its test table. Four of
   that checklist's eight bullets are decision inputs and become pure
   cases — not queried; a disturbing report; the F1-help exemption
   surviving that same report; and the legal-key guard, which aborts
   playback even when *not* interrupted — to which the table below adds
   the two baseline paths any decision function needs: a voiced key with
   its remaining macro, and the empty-macro `NoMacro` case. The other four
   bullets pin the *shell* rather than the decision — the common cleanup,
   the read-before-clear ordering, the special-event logic and the
   `addToMacro` recording — so a pure table cannot express them and none
   is missing from it. Three of those four are pinned already, by AS4, AS5
   and AS8. **The fourth is not pinned by anything**: every AS case calls
   `promptGetKey` with `ColorFull`, so the `dm /= ColorFull` branch and
   its `unless (gunderAI fact)` guard are entered by no test in the
   suite — and that is exactly the block the split relocates into
   `specialEventKeyReset`. Add an AS case for it *before* step 2, or the
   refactor moves untested code and step 4's "must pass without edits"
   gate has a hole precisely where it is being relied on. The pure cases
   are pure, so they sit beside the AS series or in a module of their own,
   wherever the function lands;
   `test/SessionUIMock.hs` already simulates macro-frame transitions, so no
   new harness machinery is needed. These are additions, not replacements:
   the AS cases keep driving the same decisions through the real
   `promptGetKey`, which is what makes step 4 meaningful.
2. Name `abortMacroPlayback`; keep the common cleanup in the shell, with
   the read-before-clear ordering noted in the record's §01.
3. Audit the shell's residual writes: `promptGetKey` stays mutating by
   design (the record's §01), so walk its body and, for every remaining
   effect — the voicing branch's macro-frame advance and
   `MsgMacroOperation` message, the special-event `resetPressedKeys`,
   `recordHistory`, the common cleanup block, the `sreqQueried`-gated
   `addToMacro` recording — either hoist it to the callers (only where all
   of them want it and the §05 outcomes survive) or keep it with a comment
   stating why a key-read primitive is its natural home. The recording
   already carries the model comment ("recorded here, not in
   @UI.humanCommand@, to also capture choice of items from menus"). End
   state: `promptGetKey`'s haddock exhaustively lists the state it may
   write, so nothing about the primitive is hidden again.
4. The FrameM contract tests (the AS series of §05) must pass *without
   edits* across this step — any diff in their outcomes is the finding,
   evidence that the refactor altered `promptGetKey`'s observable
   behavior, and not something to edit away. Re-run the bridge tests X1/X2
   too: their `promptGetKey` observations are [contract] as well, and the
   one part of them that does change, the cycling outcome, has already
   changed by then — in the live-read step (§02, step 4).

### The haddock that step 3 asks for, drafted

Step 3 ends with "`promptGetKey`'s haddock exhaustively lists the state it
may write". That list is already known, so here it is, making the step
transcription rather than rediscovery:

```haskell
-- | Draw a frame and obtain a key: either voiced from a playing macro or
-- read from the frontend. This mutates, deliberately; what it may write,
-- exhaustively:
--
-- * always: @recordHistory@, and the cleanup every real-key read
--   performs (@srunning@, @sxhairGoTo@, @sdisplayNeeded@,
--   @sturnDisplayed@);
-- * voicing branch: the macro-frame advance (@smacroFrame@) and the
--   @MsgMacroOperation@ message;
-- * abort branch, all inside @abortMacroPlayback@: @resetPlayBack@,
--   @restoreLeaderFromRun@ -- which READS @srunning@, hence runs before
--   the common cleanup clears it -- and @resetPressedKeys@;
-- * no-macro branch: @resetPressedKeys@, when the colour mode is not
--   @ColorFull@ and the faction is not under AI;
-- * at the very end, gated on @sreqQueried@: @addToMacro@, recording the
--   key into an in-game macro being defined.
```

### The decision table that step 1 asks for, drafted

One row per bullet of the record's branch-exactness checklist. The inputs
are the four `macroStep` takes — queried, disturbing report, keys legal
for the frame, pending macro — and the expected output is a `MacroStep`:

| queried | disturbs | legal keys | pending | expected | the bullet it pins |
|---|---|---|---|---|---|
| yes | no | none | x, y | voice x, leaving y | the ordinary playback path |
| yes | no | x | x, y | voice x, leaving y | a legal key is voiced |
| yes | no | z | x, y | abort | an illegal macro key aborts even when not interrupted |
| no | no | none | x, y | abort | the not-queried interrupt input |
| yes | yes | none | x, y | abort | the disturbing-report interrupt input |
| yes | yes | none | F1 | voice F1 | the help exemption survives the same report |
| yes | no | none | empty | no macro | the empty-macro branch, which must not reset playback |
| no | yes | none | empty | no macro | no macro pending, so no interrupt to speak of |

Each row is one call of a pure function, so the table transcribes
directly into a test list; the AS series keeps driving the same decisions
through the real `promptGetKey`, which is what makes step 4 meaningful.

## 05 · The test battery as it stands

The design is encoded in a test suite already on master (all green on the
unmodified engine; 153 tests total, 38 of them new). Every test that pins
a design decision carries a `[contract]` or `[LR-flip]` tag — 35 do, 25
and 10 respectively; the four `psuitReq` branch tests are plain coverage
and carry neither. The meaning of the two tags, the tasty patterns that
run each series and the discipline for flipping one are defined once in
`test/CLAUDE.md`, not restated here. What matters below is which test
carries which tag, and that flipping the `[LR-flip]` set is step 4 of §02.

### The live-read series — `test/HandleHelperMUnitTests.hs` (extensive)

| test | class | pins |
|---|---|---|
| LR1, LR2 | ✅ contract | the target invariant: in-sync cycling advances Forward/Backward correctly |
| LR3 | ❌ LR-flip | the 4a6eca154 reproducer: stale leader → cycling silently no-ops |
| LR4 | ❌ LR-flip | three-member party: stale leader → the *wrong* member is picked |
| LR5 | ❌ LR-flip | the changelog crash itself: stale leader fires `pointmanCycle`'s live "same leader" assertion (caught via `try`) |
| LR6 | ❌ LR-flip | a dangling stale `ActorId` is silently tolerated and yields an arbitrary pick (unrepresentable post-live-read) |
| LR7, LR8 | ✅ contract | `partyAfterLeader` pivot rotation, incl. the unknown-pivot edge that enables `np == sleader` (its parameter survives live-read per the post-mortem's §10 partition) |
| LR9 | ✅ contract | the `pickLeader` primitive: no-op on current, switch otherwise |
| LR10, LR11 | ✅ contract | banned factions: dungeon-wide cycling refused, same-level cycling still allowed — the partition subtlety a live-read rewrite must not change |
| LR12 | ✅ contract | the dungeon-wide twin's non-banned success path: in-sync `pointmanCycle` advances (the same function whose live assertion the desync crashes in LR5) |
| LR13 | ✅ contract | the `CmdLeader` boundary itself: with no pointman designated, dispatch refuses with the friendly failure — the one place that turns `Maybe ActorId` into an `MError`, kept by the post-mortem's §10 |

LR1, LR2 and LR10–LR13 drive the command through the real key-loop entry
point (`cmdSemInCxtOfKM`, with the key looked up in the sample game's
bindings — the `dispatchCmd` helper), so the leader the handler cycles from
is read from `sleader` at dispatch time, in sync by construction, as for
any top-level keystroke. LR3–LR5 instead call
`pointmanCycleLevel`/`pointmanCycle` the way the item dialogs do
(`InventoryM.hs:398` and `InventoryM.hs:431`), with a held leader — the
desync's entry point — after the real `restoreLeaderFromRun` has moved the
pointman under them.

### The sibling bugs and dialog contracts — the `HandleHuman*MUnitTests` pair

The sibling bugs of the post-mortem's §09, and the two contracts pinning
the dialog path they run through, in `test/HandleHumanLocalMUnitTests.hs`
and `test/HandleHumanGlobalMUnitTests.hs`:

| test | class | pins |
|---|---|---|
| fling suitability closure differs per actor | ❌ LR-flip | Sibling bug (a)'s testable ingredient: `permittedProjectClient`'s closure gives a different verdict per actor (`Right True` vs `Left ProjectUnskilled` on the same item), so the dialog reusing the entry actor's captured closure after a pointman switch judges items for the wrong actor. Post-live-read the closure reads the live pointman per evaluation (`psuitReq` loses its argument), so the capture becomes unrepresentable and the test changes shape. |
| psuitReq verdict differs per actor | ❌ LR-flip | sibling bug (a) at the exact captured value: `psuitReq` — what `chooseItemProjectHuman` bakes into the dialog's `psuit` — gives a different failure per actor with the xhair on C's own position ("aiming obstructed by terrain" for A, the degenerate "aiming blocked at the first step" for C), through the real aiming pipeline, no walkable tiles needed |
| Project executed by a different actor than the item selection | ✅ contract | Sibling bug (b), both halves of the seam: with `sitemSel` left by A's choose dialog, `projectHuman` run for A gets past the store lookup (control), run for C fails with "no item to fling" for the item just approved. Deliberately [contract]: the execute-half pinned here is correct in isolation and survives the live-read design — what it fixes is the *choose* half, whose live re-reads make the dialog re-validate for C before the selection is confirmed, closing the seam where the incoherent approval arises. |
| fling dialog: a mid-dialog switch keeps A's closure | ❌ LR-flip | Sibling bug (a) end to end, on the walkable board: a scripted `C-Tab` switches the pointman to C inside the real fling dialog, whose captured A-closure still calls the item suitable, so `Return` selects it and `sitemSel` is set — for an item the unskilled C cannot fling. Post-live-read the closure judges for C, nothing is suitable and the dialog exits "never mind" with `sitemSel` unset (flip verified by temporarily re-reading the pointman in the dialog's `psuit`). |
| alterDir: the held leader picks the square to modify | ❌ LR-flip | The remaining site of the post-mortem's §09, `alterDirHuman`/`pickPoint`, driven through the real crash window (the post-mortem's §04): a macro dies inside the wait, `promptGetKey` restores the pointman to A, and the command modifies from the actor it was *handed* — the run holding A targets C's floor, the one holding C the wall past it, and the two failures name the two tiles. Post-live-read both read the restored A and both name the floor (flip verified the same way). |
| chooseItemHuman: ESC exits the real store dialog | ✅ contract | that a whole dialog is drivable under the mock: `chooseItemDialogMode` → `getStoreItem` → `displayChoiceScreen` to the "never mind" exit, reaching `promptGetKey` through its `SlideshowM` call site — the path the end-to-end fling row above runs on |
| chooseItemHuman: scripted Tab switches pointman mid-dialog | ✅ contract | the dialog's own cycling handler and `recCall`'s re-entry — the re-sync of the post-mortem's §02, from commit `8608d6f9c`, previously untested — on the equipment store, which needs no aiming |

Two constraints shaped the three pins written first — the two (a) verdict
rows and the (b) seam row — both verified against the stub harness: the
full `psuitReq` pipeline fails deterministically on the default
unknown-tile board (`"aiming obstructed by terrain"`), so bug (a) was
first pinned through per-actor failure verdicts rather than through a
whole dialog; and `projectHuman`'s store lookup precedes all aiming, so
bug (b) is drivable with no walkable tiles at all. The walkable board
lifted both limitations, and the two end-to-end rows — the fling dialog
and `alterDir` — run on it. Its construction, and every other harness
fact these tests rest on down to why `emptyUnknownTile` is exported the
way it is, is documented in `test/CLAUDE.md`, which is maintained against
the code; none of it is restated here.

### The abort-split's own part, and what the tags mean jointly

The series above pin the live-read design; the two below pin the
abort-split. Stated jointly, the classifications mean:

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
| AS7 | rendered (non-blank) frames work under the mock: `drawHudFrame` over the stub board — pinned because the end-to-end fling test above depends on it |
| AS8 | keys voiced from a macro are recorded into an in-game macro being defined — the "recorded here, not in @UI.humanCommand@" semantics that §04's audit step keeps inside the primitive |
| AS9 | abort via a disturbing report — the third interrupt input, driven by the real `stopPlayBack`: same outcome as AS5/AS6 |
| AS10 | the F1 exemption: a help-displaying macro survives the same disturbing report — voiced, run intact, pointman untouched |
| AS11–AS13 | `restoreLeaderFromRun`'s guards, one each — no-op without a run, no-op for a `noRunWithMulti` faction, no-op when the run leader is gone from the level — pinned because `abortMacroPlayback` relocates the function verbatim |

### Bridge — touching both designs

**X1** replays the whole crash window (post-mortem §04) end-to-end with the
leader restore performed by the *real* `promptGetKey` (the post-mortem's
§07 test, kept as LR3 for its simplicity, calls `restoreLeaderFromRun`
directly instead of through `promptGetKey`): run rotates the pointman,
dialog captures it, macro dies inside the dialog, `promptGetKey` restores
the run leader, stale cycling no-ops. Its `promptGetKey` observations are
[contract]; only its final cycling outcome is [LR-flip].

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

*LambdaHack · the migration plan for the pointman-desync work · temporary
by design: delete it when §04's last step lands. The reasoning lives in
`docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md`; the result
will live in the code.*
