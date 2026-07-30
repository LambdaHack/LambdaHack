# The leader-desync migration plan

*LambdaHack · UI client · the work list for two designs — written to be
deleted*

> **This document is temporary by design.** The permanent records are
> `docs/leader-desync-bug.md` (the crash, the analysis, the live-read
> design) and `docs/promptgetkey-hygiene.md` (the abort-split). This file
> holds only what stops being true when the work lands: the ordering, the
> conversion inventory, the artifacts still to write, and the state of the
> test battery. Nothing here is worth keeping afterwards — the records
> carry the reasoning, the code carries the result — but deleting it is
> not the one-line act it reads as, eight files naming it — seven to be
> reworded, the eighth being `tools/leader-census.py`, which the first
> bullet below disposes of — as
> `git grep -n leader-desync-migration -- . ':!docs/leader-desync-migration.md'`
> lists in full. When §04's last step is done:
>
> - delete this file, and `tools/leader-census.py` with it — except that
>   the post-mortem's §10.3 rests its census-rather-than-floor claim on
>   the tool by name, so either that sentence is edited in the same commit
>   or the tool stays;
> - note the landing in `CHANGELOG.md` (the lines are drafted in §02), and
>   add to each record the outcome line it reserves;
> - reword the inbound references, which no mechanical pass can see once
>   the target is gone: the backticked `docs/leader-desync-migration.md`
>   paths that command lists, and the pointers by *name* it does not —
>   `git grep -nE 'migration (document|plan)'` over `CLAUDE.md`, both
>   records and the wasm plan, whitespace-tolerant because the phrase wraps
>   across line breaks and a line-oriented grep answers 1 where a wrapping
>   one answers 11. Counting those by hand is what went wrong here before;
>   the second list is read, never driven to zero, since §11 goes on saying
>   "the migration" of the *work* after this file is gone;
> - edit `CLAUDE.md` twice: the sentence in "Where to look next" that names
>   this file, and the pointman gotcha's "Until the live-read design lands"
>   clause, which the landing is what resolves;
> - and the four sites outside the documents, which are the ones a reader
>   forgets: `docs/wasm-frontend-unified-plan.md` names this file three
>   times, one of them the tag-ownership sentence, which keeps its claim and
>   drops only the citation; `test/HandleHumanLocalMUnitTests.hs:193` and
>   `test/InventoryMUnitTests.hs:72` cite it from test comments; and
>   `tools/check-doc-examples.py:57` names it in its own non-vacuity recipe;
> - then re-run `python3 tools/check-doc-refs.py` over `CLAUDE.md`, both
>   records and the wasm plan — the pass that catches a backticked path left
>   behind — and re-run the `git grep -n` above, which must come back empty
>   bar whatever branch the first bullet took. Exit 2 from `check-doc-refs`
>   means the run did not happen, `../lambdahack.github.io` being unmounted,
>   not that nothing is left; that is the ordinary sandboxed case and the
>   one way this bullet passes vacuously.
>
> File:line citations were verified against the tree at commit
> `4b92b291a` (2026-07-30) — the newest commit touching any file they
> cite; re-run `python3 tools/check-plan-citations.py
> docs/leader-desync-migration.md --restamp` after the reading pass, and
> re-verify the only/every/never claims by repo-wide grep. Verify the
> post-mortem's §§10–11 in the same pass: they are the live half of that
> record — the design this plan executes — so they drift with this file
> rather than ageing with its frozen sections.

## 00 · Status, size and per-step checks

Keep this table current as the work proceeds; it is the reason this
document lives in the repository rather than in someone's head. Steps 2 to
5 of §02 belong in **one** commit, though they keep two rows here, the
work being too large for one cell: a characterization and the code it
characterizes must not be committed apart, and seven of the ten flips step
4 performs are earned by conversions step 5 does — LR3 to LR6 and the
bridge tests turn on `pointmanCycleLevel` and `pointmanCycle` reading
live, `alterDir` on `pickPoint`, and all four convert in step 5(a). So
neither of those two rows is green alone. Every other row is a commit of
its own, each leaving the suite green, which is what makes the rows the
unit of rollback §01 relies on.

| step | touches | check when done | state |
|---|---|---|---|
| §02.0 spike | `MonadClientUI` plus the three frames of `PointmanCycleLevel`, and the five test call sites that break with them (`HandleHelperMUnitTests.hs:121`, `:141`, `:176`, `FrameMUnitTests.hs:343`, `:377`) | the library compiles and the witness reads tolerably at a real call site; then, once those five take a witness, the suite compiles and LR1/LR2/LR5 are green while LR3/LR4 and the two bridge tests are red and LR6 unrepresentable — the spike working, not failing | pending |
| §02.1 witness, accessors | `MonadClientUI` only, ~30 lines | `cabal build`; the contract series green and its count unmoved; nothing else changes, this step having no callers yet | pending |
| §02.2–4 dialog chain, assertions, flips | `InventoryM` (7 functions), `HandleHumanLocalM` (`chooseItemDialogMode`, the three `chooseItem*Human` wrappers, and `psuitReq`, which loses its own `ActorId`), `HandleHumanGlobalM` (`itemMenuHuman`, `chooseItemMenuHuman`, and `psuitReq`'s second call site in `projectItem`), `HandleHumanM` (their boundary cases and the `CmdLeader` field type), `HandleHelperM` (the one assertion `4a6eca154` disabled), test edits across all five test modules, nearer 20 than the 14 first estimated | the contract series green *unchanged* and its count unmoved; the flip series green *with the flipped values* and its count down one as step 4 deletes LR6 (11 → 10), each flip verified first against the candidate as step 4 spells out; `stylish-haskell -i` leaves every touched file alone | pending |
| §02.5 sweep | the remainder of §03's read-live set (fourteen functions, judgment calls), then the fifteen convert-half of §03's tail with the sixteen boundary cases dispatching them, across 4 modules | `cabal build`; both series green with counts unmoved; `hlint .` says `No hints`; `stylish-haskell -i` leaves every touched file alone; no `CmdLeader` case passes an `ActorId`, read off `cmdSemanticsLeader` alone | pending |
| §02.6 verification | nothing; it is the gate | full suite; `make test-short`, `make test-medium`; the manual timeline session, a fling-dialog switch and an apply-dialog switch (§03's sibling (c), pinned by PR 0 but checked here in the real frontend) | pending |
| §04.1 extract `macroStep` | one pure function, plus the eight-row table in §04; the two AS cases it depends on land earlier, in §01's PR 0 | the table passes; the whole AS series untouched and green, the two new cases included — they were written against the unsplit primitive and must survive the split unedited | pending |
| §04.2 name `abortMacroPlayback` | `FrameM`, ~10 lines | AS4–AS6 green *without edits* | pending |
| §04.3 audit the residual writes | `FrameM` only; the drafted haddock in §04 | the haddock lists every write the body performs | pending |
| §04.4 AS series unchanged | nothing; it is the gate | the whole AS series — PR 0's two additions included, which is what the gate is for — and X1/X2 pass with no edits to them | pending |
| §05 battery | — | landed on master: the series and its harness in `3453b1777` through `8b5703e87`, then sibling (d)'s pin and the `permittedProjectClient` retag in `643337f51` | **done** |

### Running this plan

**Who runs it.** A session, autonomously, not a person working from
memory of the campaign — the same executor
`docs/wasm-frontend-unified-plan.md` writes for, and the reason that plan
spells out per item what a session cannot do. Nothing below is addressed
to someone who already knows which check opens a window or which count
was true last week. Two consequences bind every row: a session does
exactly what is written and nothing that is merely implied, so an
unstated step is an unperformed one; and it cannot tell a check it is
expected to skip from one it has failed to run, so a row whose acceptance
it cannot complete has to say so rather than leave the gap to judgment.
That this had to be stated is itself the evidence — the intent was read
off the prose and got read wrong.

**The gates, once.** Every "check when done" above is one of these, run from
the repo root. Read the counts, not the exit status alone — a suite that
silently loses a test still passes:

```
cabal build                                      # library, executable, suite
cabal test                                       # 154 tests today
cabal test --test-options='-p "/contract/"'      # 26 today; moves once
cabal test --test-options='-p "/LR-flip/"'       # 10 today; moves twice
hlint .                                          # must print: No hints
stylish-haskell -i <each .hs the step touched>   # must leave them unchanged
python3 tools/leader-census.py                   # before step 2 only; see §03
```

Those three counts move at exactly two points and nowhere else. PR 0 of
§01 takes them to 157, 28 and 11 — two AS cases and one flip pin — and §02
step 4 then deletes LR6, taking them to 156, 28 and 10. A "count unmoved"
in the table above is against whichever of the three baselines its row
follows, and this is the only place the sequence is stated, so a row that
disagrees with it is wrong there rather than here. A count that shifts
otherwise is the finding, not a nuisance: both patterns select on the test
*name*, so a renamed test leaves its series silently. Builds take minutes —
set the timeout rather than reading one as a hang — and the flag set stays
fixed for the campaign, `+with_expensive_assertions` included, since
changing it rebuilds every local package.

**What may be fanned out, and what may not.** The conversion is a
type-directed cascade inside *one* library and *one* test-suite component:
change a signature and the compiler names the next site, so the tree is red
until the frontier closes. The edits are therefore serial, in one working
tree — two agents converting two modules in parallel produce two partial
states, neither of which compiles, and each pays a full four-library rebuild,
`dist-newstyle` being per worktree where only the package store is shared.
What does parallelize is everything that reads rather than writes: the
per-function placement analyses behind §03's table, the flip verifications
of step 4, the authoring of PR 0's tests, and an adversarial pass after each
commit asking whether any converted function still holds an identity across
a wait. Fan out to decide, converge to edit, fan out to refute.

**Stop and ask.** Three outcomes are not the implementer's to settle: step 0
finding that the witness reads badly at real call sites, which reopens
witness versus witness-free (the post-mortem's §10.4); any `[contract]`
test whose *outcome* will not come back green — a call site that merely
needs a witness to compile is not one of those, and §02 step 4 names the
seven such sites; and any `hlint` hint that cannot go without contorting
code, since a new `.hlint.yaml` exception is the author's call.

**Never**, each having cost someone a round trip already: don't convert
`projectItem` or `meleeAid`, which the post-mortem's §10.6 pins; don't
convert the nineteen keep-param entries of §03's tail; don't fix the apply
closure the way `psuitReq`'s is fixed, by moving the call, since there it is
the *actor* that must go live; and don't bind a `getLeaderUI` result before
a `promptGetKey` and use it after, which is this whole document in one
sentence.

**Owed, and not started**, so that the confidence of this document is not
mistaken for coverage. PR 0 owes three tests: the two AS cases of §04 step
1, of which the special-event one needs something the harness does not
have — a `ChanFrontend` that *records* `FrontResetKeys` rather than
printing it (`UnitTestHelpers.hs:135`), that request being the branch's
only effect — and a characterization of sibling (c) in the apply dialog,
which no test enters at all. Sibling (d)'s pin landed ahead of them, in
`test/InventoryMUnitTests.hs`.

**Claims here that no pass can re-run.** Four assertions in these three
documents rest on experiments whose artifact was never recorded: the two
placement verifications in the post-mortem's §10.3, the rank-2 spike in
its §10.4, the verdict table of its §08 and the baseline of its §11.
Everything else here a reader or a checker can settle; these can be
settled only by redoing the experiment. Re-establish one before leaning on
it in a decision, and when a step redoes it, record the command and the
output beside the claim, the way the scripts under `tools/` record their
non-vacuity recipes.

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
  (`dm /= ColorFull`) is entered by no AS case**, every one of them reading
  with `ColorFull` — and it is the block the split relocates, so step 1 now
  requires the missing case before the extraction. §11's
  menu-navigation microbenchmark is ruled out rather than left open.
- 2026-07-30 · a multi-angle review of all three documents. Bookkeeping it
  corrected here: step 0's gate was unmeetable, since converting
  `pointmanCycleLevel` breaks five test call sites and LR3/LR4 then go red
  — the spike working, not failing; step 5 had dropped the read-live
  remainder §00's own row assigns it, and double-counted the boundary cases
  against step 2; §04 step 1's bullet accounting named one checklist bullet
  twice and dropped another, and the decision table's caption claimed a
  bijection it never had; the drafted haddock filed `recordHistory` and the
  common cleanup as unconditional, though the voicing arm returns before
  both, and omitted `spointer`; the tail's base is 65, not 64; and
  `make test-gha` cannot witness a surviving desync, being playtests only.
  The finding that is not bookkeeping: **the apply dialog is a second
  closure case, and a third live sibling of the post-mortem's §09** — the
  placement rule as stated would leave it standing, its call being inside
  the closure already, so it is the *actor* that must go live rather than
  the call that must move. It is now in §03 beside `psuitReq`, step 2
  converts it and step 6 checks it by hand, no test entering that dialog at
  all. Two counts that looked wrong and were not: the 29 boundary cases
  (14/12/3) and the census's 72 both re-derive exactly.
- 2026-07-30 · prepared for execution rather than for reading, which moved
  two things. The two AS cases §04.1 asked for characterize the *unsplit*
  primitive and depend on neither design, so they land first, in a coverage
  PR together with the apply-dialog pin, and §04.1 inherits them green.
  And `tools/leader-census.py` turns out to be a before-check only: its
  rule is "binds a parameter named `leader…`", so every converted function
  leaves the 72 and stops being checked in either direction — the run
  stays green while certifying less and less, which is worse than going
  red, and the printed counts are the only signal it keeps.
- 2026-07-30 · §03's table now records a per-function pass — every
  read-live body read, then re-read independently — which moved three
  things and found a fourth. `psuitReq` is not a closure case: its body
  waits nowhere, so it reads at the top like anything else and the capture
  belongs to its call sites, making the four `chooseItemProjectHuman`,
  `chooseItemApplyHuman`, `runDefSkills` and `runDefInventory`, with
  `transition`'s three `defAction`s and `chooseItemDialogMode`'s
  `renderOneItem` beside them. Nine of the twenty-eight need no identity at
  all, where the count here said four. `pickNumber` is a wait, so the move
  family spans two. And the class no section had named: a value *derived*
  from the identity before a wait is exactly as stale as the identity, and
  live-read alone fixes none of them — `getFull`'s bag, whose lookup is
  `EM.!`, so the failure mode is a partial map rather than incoherence;
  `moveOrSelectItem`'s `calmE`/`overStash`/`eqpFree`; `itemMenuHuman`'s
  body. The post-mortem's §10 placement rule now carries the general form.
  One thing left open deliberately: whether a mid-dialog switch can really
  reach `getFull`'s missing key is untested, and is worth settling before
  the conversion rather than after.
- 2026-07-30 · settled: it can. A scripted Tab and Return through the real
  dialog make `getFull` return C's item with a quantity looked up in A's
  bag, and forcing it dies with `IntMap.!: key 117 is not an element of
  the map`. So the derived-value class has a crash in it, not only
  incoherence, and the post-mortem's §09 gains sibling (d) — pinned now,
  in `test/InventoryMUnitTests.hs`, non-vacuity proved both ways. What
  kept it out of sight: the two single-item callers drop the quantity
  unforced, and the move family's single-item path already guards this
  exact case with `EM.lookup` and a comment naming it, so the author had
  met the class at one site and defended it there. In the same pass the
  `permittedProjectClient` pin became `[contract]`: it had nothing to
  flip, and now guards §03's Keep ruling instead by switching the pointman
  between two rounds of calls and asserting that neither verdict moves.
  The battery is 154 tests, 26 contract and 10 flip; the flip set changed
  membership without changing size.
- 2026-07-30 · one question the crash raises is recorded rather than
  answered, in §01: whether sibling (d) being a *crash* rather than
  incoherence reorders the three PRs. Both directions are argued there.
  Log-worthy because it is a design question reopened by a finding, and
  because an unanswered question that lives only in a chat is the thing
  this section exists to prevent.

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

**It ships as three pull requests, not one and not many.** One would mix a
behavioural fix with a hygiene refactor and earn a single CI verdict for
three independent risks; many is not available, since no split finer than
these leaves the tree green in between.

| PR | contents | why it stands alone |
|---|---|---|
| 0 · coverage | the two AS cases §04.1 asks for, plus a characterization of §09's sibling (c) in the apply dialog | all three pin *today's* behaviour, land green, and are the safety net the next PR runs on — which is what the battery of §05 already did, landing ahead of the fix. Authoring them parallelizes; nothing here touches the engine |
| 1 · live-read | §02 steps 1–5, in two commits: step 1, then steps 2 to 5 together | the flips, the new apply one included, land with the engine change that earns them |
| 2 · abort-split | §04 steps 1–3 | a different design, strictly after, with its own `CHANGELOG.md` line |

Pushing any of them, and opening any of them, needs the author's explicit
go-ahead each time; the campaign ends at "branch with commits", never at
"pushed".

> **? Open, and the author's to close: does sibling (d) change this
> order?** The table was written while (d) was a hazard on paper. It is
> now a proven crash — a partial-map failure in `getFull`, not the quiet
> incoherence the rest of the family produces — and that cuts both ways,
> which is why it is recorded here rather than acted on. *For* reordering:
> a crash a player can reach argues for landing live-read first, since
> PR 0 fixes nothing and PR 1 is what closes it. *Against*: PR 0 is what
> makes PR 1 checkable, sibling (c) has no test at all, and the campaign
> has already been wrong twice in ways only a test caught — reordering
> saves little and spends the safety net exactly where this work has needed
> it. Weighing against hurry too: forcing the bad thunk needs more than one
> item selected, or the ground store, since every other path drops it
> unforced or guards it (§03), which is why the years this code has stood
> have produced no report. Until this is answered the table above stands.

Every commit below leaves the tree buildable, green and shippable, so
there is no rollback procedure to write beyond reverting it. Steps 2 to 5
are the one place where a *step* does not have that property — step 2
alone leaves the LR series red, and step 4 cannot flip what step 5 has not
yet converted — which is why the four are a single commit. The only step
that *looks* irreversible is the flip of the characterizations (step 4),
and it reverts together with
the engine change it accompanies, for the same reason.

## 02 · Live-read: migration order and verification

Steps 2–5 are one logical change and belong in one commit, the flips of
step 4 included: a characterization and the code it characterizes must not
be committed apart, or the suite is red in between and the flip loses its
evidence — and step 4's flips are not all earned by step 2, seven of the
ten turning on conversions step 5 performs. Step 1 lands before them, its
own commit and green on its own, the accessors having no callers yet. The
abort-split (§04) is a separate change on top; the test battery (§05) is a
separate one below, and has already landed.

Blast radius: six modules. `MonadClientUI` gains the accessors and the
witness, `HandleHumanM` the boundary, and the four modules §03 lists supply
the handlers — `HandleHelperM`, `InventoryM`, `HandleHumanLocalM`,
`HandleHumanGlobalM`. Nothing outside the UI client changes: not the AI
client, not the server, not the frontends, and no type crossing the
client-server boundary (the post-mortem's §10). `promptGetKey` keeps its
type, so `Client/UI.hs` and `SlideshowM` merely recompile. The work is
type-directed — change a signature, follow the errors, apply the partition
to each site the compiler names — which is the backstop over §03's
inventory: its *surface* is a census, kept exact in both directions by
`tools/leader-census.py`, but bucket membership is not bucket correctness,
and that is what the compiler settles site by site.

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
2. **Convert the dialog chain first**, taking each function's placement
   from §03's table rather than from this sentence, which only gives the
   order: InventoryM's `transition`, `getItem`/`getFull`/`getGroupItem`/
   `getStoreItem` and `runDefSkills`/`runDefInventory`; then
   `itemMenuHuman`, `chooseItemMenuHuman`, `chooseItemDialogMode` and the
   `chooseItem*Human` wrappers; and `psuitReq` itself, which loses its
   `ActorId` here rather than in step 5 — the compiler will not ask for it,
   converting its callers not being enough. Four of them are closure cases,
   where the read belongs inside the callback the menu loop re-invokes —
   `chooseItemProjectHuman`, which gains a `psuitReq` call inside `psuit`
   and *keeps* its entry call at `HandleHumanLocalM.hs:367`, that one
   feeding the invalid-aim failure at `:370` and the `sitemSel` fast path
   at `:381`, neither of which can live inside the closure;
   `chooseItemApplyHuman`, where the *actor* moves inside a `psuit` that is
   already in the right place, and which no test would catch; and the
   `runDef*` pair's right-pane callbacks. Two more carry a derived value
   down with the read rather than the read alone — `getFull`'s bag and
   `itemMenuHuman`'s body — as does `moveOrSelectItem` when step 5 reaches
   it. Delete the two manual re-reads — `recCall`'s and the
   post-`getStoreItem` one — as each becomes dead; the `7e74698af` test
   permissiveness goes with them, so there is no third site to hunt for
   (the post-mortem's §10 says why).
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
   expect a surviving desync to crash rather than to no-op quietly — in the
   unit suite and in step 6's manual session, which is where the dialogs
   are driven. Not in `make test-gha`: it is `test` plus four playtests
   (`Makefile:146-148`), every one of them `--automateAll`, so no human
   command is dispatched and no dialog opened, and a green run there says
   nothing about this assertion either way.
4. **Flip the whole [LR-flip] series**, verifying each flip by temporarily
   applying the candidate fix first, as `test/CLAUDE.md` requires. That
   verification is a loop per test, and its order is the whole point: run
   the test unmodified and record the value it pins; apply the candidate —
   the one-liner of the post-mortem's §08, or for a closure pin the live
   read inside the closure itself; run the same test and record the new
   value; revert. Only then edit the expectation, to the value the run
   produced rather than the value the comment predicts. Open
   `test/CLAUDE.md` before starting, not after a fixture surprises you: it
   holds the harness facts this needs and loads by itself only for a
   session already working under `test/`. The set is exactly what `cabal
   test --test-options='-p "/LR-flip/"'` runs — ten tests, eleven once
   PR 0's apply-dialog pin lands — and each states its target value inline
   (the set's membership changed on 2026-07-30 without its size doing so,
   which is why the count is worth re-reading rather than trusting):

   - LR3–LR6 in `test/HandleHelperMUnitTests.hs`, the post-mortem's §07
     reproducer among them (already verified to pass under the live-read
     fix); LR5 changes *shape* rather than value, since it catches a live
     assertion via `Control.Exception.try` and after the fix the assertion
     no longer fires;
   - the final cycling outcome of the bridge tests X1 and X2
     (`test/FrameMUnitTests.hs`); their `promptGetKey` half is [contract]
     and must not move;
   - the `psuitReq` verdict pin in `test/HandleHumanLocalMUnitTests.hs`,
     whose two per-actor calls become one call before and one after a
     pointman switch, `psuitReq` having lost the argument that told them
     apart. Its former companion, the `permittedProjectClient` pin, is not
     in this set any more: it became a `[contract]` test on 2026-07-30,
     having nothing to flip;
   - the two end-to-end tests of §05 (the fling dialog, `alterDir`), each
     to the value recorded in its comment;
   - the `getFull` quantity pin in `test/InventoryMUnitTests.hs`, sibling
     (d), where the flip is not a changed value but a vanished failure:
     once the bag moves down with the read, forcing the quantity returns
     C's own `(1, [])` and there is nothing left to catch;
   - and, once PR 0 has landed it, the apply-dialog pin of sibling (c) —
     the eleventh member, and the one this list would otherwise leave
     unflipped while claiming to flip the whole series.

   Two rulings the flip needs, neither of which is an expected value.
   **LR6 is deleted, not flipped**: it pins that a dangling stale `ActorId`
   yields an arbitrary pick, and after the conversion there is no argument
   to dangle, so there is nothing to edit it to. Its going takes the flip
   series 11 → 10 and the suite 157 → 156, the second of the two movements
   §00's count sequence permits; say so in the commit, since an
   unexplained count drop is exactly what §00 tells a reader to treat as a
   finding. **And a [contract] test that needs a signature edit is not a
   contract test that moved**: `getFull`'s three plain cases
   (`test/InventoryMUnitTests.hs:29`, `:39`, `:49`), the two
   `chooseItemHuman` contracts (`test/HandleHumanLocalMUnitTests.hs:324`,
   `:351`) and the `projectHuman` pair inside the sibling-(b) contract
   (`test/HandleHumanGlobalMUnitTests.hs:71`, `:73`) all pass an `ActorId`
   that the conversion removes. Giving them a witness is mechanical and
   proceeds; §00's stop-and-ask means an *outcome* that will not come back
   green, not a call site that will not compile.

   Mechanical fallout of the same step, and why it is a restructure rather
   than an edit of expected values: the LR series calls the converted
   functions directly, so LR3–LR6 must obtain a witness (`mintHasPointman`,
   whose export exists for exactly this) before they can call
   `pointmanCycle`/`pointmanCycleLevel` at all, and `psuitReq` losing its
   `ActorId` updates its four branch tests. The stub fixtures deliberately
   leave `sleader` unset — `partyCliState`'s own haddock says so, and LR13
   pins the boundary's refusal for exactly that state — so a test that
   needs a pointman designates one with `updateClientLeader`. Audit every
   test that today calls dialog code with none designated, rather than
   assuming the post-mortem's §10 prediction that the expectations are
   unaffected. Nothing in the [contract] series may move; that is what
   makes it a contract.
5. **Sweep what step 2 left**, in two halves, because only one of them is
   mechanical. (a) The read-live functions outside the dialog chain: the
   `pointmanCycle`/`pointmanCycleLevel`/`pickLeaderWithPointer` trio with
   their three `*Human` wrappers, `projectHuman`, `applyHuman`,
   `alterDirHuman`, `closeDirHuman`, `pickPoint`, `moveItemHuman`,
   `moveOrSelectItem` and `selectItemsToMove` — judgment calls, every one
   ruled on by the post-mortem's §10 but none of them by position alone:
   `pickPoint`'s read must sit between its `getConfirms` and its last use,
   `alterDirHuman`/`closeDirHuman` hold across that wait, and the four with
   no wait of their own become pure witness-passing. (b) The mechanical
   half: the fifteen convert-half of §03's tail and the sixteen boundary
   cases that dispatch them, `MoveDir` and `RunDir` sharing `moveRunHuman`.
   The other thirteen of the 29 cases move with their handlers rather than
   here — five in step 2, eight with (a) above — which is why the boundary
   is swept in three places and checked in one. Leave the "some actor"
   parameters and §03's other nineteen alone — the post-mortem's §10 rules
   on both, and there the ruling really does decide each site. The step
   ends on an invariant worth checking by reading `cmdSemanticsLeader`
   alone: no case passes an `ActorId`.
6. **Verification**: the full unit suite (154 today, 157 once PR 0 has
   landed and 156 after step 4 deletes LR6, all green before the change),
   with `-p "/contract/"` kept green at *every* step of the migration
   rather than only at its end — that series is the safety net
   the conversion runs on; `hlint .`; `make test-short` / `test-medium`
   playtests (AI-driven — they exercise the client loop, not the dialogs —
   and minutes each, so budget for them rather than reading one as a hang);
   a manual session replaying the post-mortem's §04 timeline (multi-hero
   run inside a recorded macro that opens the item menu, then
   `A-Tab`/`C-Tab`) — X1 of §05 already drives that window through the
   real `promptGetKey`, so what the session adds is everything the mock
   supplies instead: a real frontend, a macro recorded by actual
   keypresses rather than a `smacroFrame` seeded in the fixture, and the
   sample game's own bindings and party — evidence that a player can reach
   the window, not only that a fixture can. Plus a pointman switch inside
   the fling dialog *and* inside the apply dialog, to confirm the
   post-mortem's §09 siblings are gone — the apply one by hand because
   it is the real frontend and PR 0's pin is not; and `make
   frontendCrawl` for a visual pass over menus. The last three are a
   human's, not a run's: the session and the two switches are played by
   hand, and `frontendCrawl` opens an SDL2 window, so a headless session
   cannot do it at all. Performance needs no gate — the post-mortem's §11:
   no benchmark reaches this layer.

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
- Read the pointman live rather than threading it through the UI, fixing the TAB-during-item-manipulation crash and three item dialog siblings
- Split promptGetKey's interrupted-macro cleanup into a pure decision and a named abort action
```

The second lands with §04, not with this section; both go in together only
if the two changes ship in one release.

**The commit titles**, one per commit the campaign makes — PR 0's two
first, then one per code-carrying row of §00 bar the spike, which either
reverts or becomes step 1 — so the history reads as the plan does and no
step is tempted to bundle. The bodies are
written from what the step actually did — that part cannot be drafted in
advance — but the titles can, and they fix the commit boundaries:

```
Pin the two promptGetKey branches no test enters          (PR 0)
Pin the apply dialog's stale suitability closure          (PR 0)
Add the pointman witness and the live-read accessors      (§02.1)
Read the pointman live in the item dialogs                (§02.2-4)
Stop threading the pointman through the command boundary  (§02.5)
Extract the macro interrupt decision as a pure function   (§04.1)
Name promptGetKey's interrupted-macro cleanup             (§04.2)
Enumerate promptGetKey's writes in its haddock            (§04.3)
```

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
names is a failure, and so is a bucket entry the tree no longer has.

**It is a before-check, and it quietly stops checking as the work
proceeds — it does not go red.** Its rule is "binds a parameter named
`leader…`", so every converted function leaves the 72 and the tree→doc
direction stops considering it; the doc→tree direction only asks whether a
bucketed *name* is still in the tree, and a converted function keeps its
name. So the run stays green while covering less and less, and by the end
of step 5 it certifies almost nothing. Read the printed counts, then —
"72 bind a parameter named `leader…`" falling toward zero is the progress
bar, and the exit status is not. Run it green before step 2, read it for
the counts during, run it after any change to the *buckets*, which is what
it is for, and delete it with this document at the end.

One function it cannot see, and prints for hand-classification instead:
`pickLeaderWithPointerHuman`, point-free and so binding nothing. The rule's
other blind spot is wider and quieter: the 22 functions of `94 − 72` bind a
bare `ActorId` under some other name, so the cross-check reaches them in
neither direction. Seventeen are the `aid`/`source`/`target` families the
Keep bullet names by module; `pickLeader` and `pickLeaderWithPointerHuman`
it names outright; the remaining three — `skillsOverlay`
(`HandleHelperM.hs:368`), `partActorLeader` (`MonadClientUI.hs:455`) and
`partPronounLeader` (`:469`) — are hand-classified Keep here, and the first
two are instructive: `skillsOverlay` is in the post-mortem's own Keep row
and had been dropped from this list, while `partActorLeader` already reads
`sleader` live to decide whether to say "you", i.e. it is the design's own
idiom, arrived at years earlier.

The tool exists because two grep proxies stand behind this inventory and
each has failed once, neither failure showing up as a wrong count. The
*extraction* proxy read only the first bound parameter, so `runDefSkills`
and `runDefInventory`, which bind `leader` third, were dropped along with
`msgAddDone` and DrawM's four; its 64 was a floor and said so — a floor
even against its own rule, which the tool's breakdown puts at 65
first-bound, the other seven binding `leader` later in the head — where the
tool's 72 is a census. The *classification* proxy — which bucket a
function then belongs in — saw a wait only in a function's own body, so
`closeDirHuman`, whose wait sits inside the `pickPoint` it calls, was
filed as harmless until re-read. The tool replaces the first proxy and
cannot replace the second: it checks that every function is in *a*
bucket, never that it is in the right one, which is what its clean run
says out loud. The type-checker is still the final word, this being a
type-directed refactor in which the compiler enumerates what is left.

**Read live** — the identity must stop being threaded through each. The
table is the work list, one row per function, and its last column is the
only decision that fixes anything: a read placed above the wait is the
stale copy under a new name. Of the twenty-eight, nine never need the
identity again — they hand it on, and only the witness travels; seven read
at the top, nothing intervening; and the remaining twelve are the work,
seven reading below a wait and five inside a callback the menu loop
re-invokes. Line citations are to the function's own module unless the cell
names another. Derived by `tools/leader-census.py`'s surface plus a reading
of every body, each row then re-derived independently; the last column is
the part no tool checks.

| function | module | mints? | the wait it spans | last use after it | the read goes |
|---|---|---|---|---|---|
| `transition` | `InventoryM` | inherit | the menu loop it drives | `:379`, `:398`, `:431` — three `defAction`s | inside each `defAction`, not at the top |
| `getItem` | `InventoryM` | inherit | — | — | nowhere; witness only |
| `getFull` | `InventoryM` | inherit | `InventoryM.hs:285` `getItem` | `:290` `bagAll`, closed over the entry body | after the wait — and the *bag* moves with it |
| `getGroupItem` | `InventoryM` | inherit | in `getFull` | — | nowhere; witness only |
| `getStoreItem` | `InventoryM` | inherit | in `getItem` | — | nowhere; witness only |
| `runDefSkills` | `InventoryM` | inherit | `InventoryM.hs:518` `displayChoiceScreenWithDefItemKey` | `:519` `skillsInRightPane leader` | inside the right-pane callback; `:516` keeps a top read |
| `runDefInventory` | `InventoryM` | inherit | `InventoryM.hs:645` the same call | `:646` the `meleeSkill` the callback closes over | inside the callback, with `getActorMaxSkills` |
| `pointmanCycle` | `HandleHelperM` | inherit | — | — | at the top |
| `pointmanCycleLevel` | `HandleHelperM` | inherit | — | — | at the top |
| `pickLeaderWithPointer` | `HandleHelperM` | inherit | — | — | nowhere; witness only |
| `chooseItemHuman` | `HandleHumanLocalM` | mint | — | — | nowhere; witness only |
| `chooseItemDialogMode` | `HandleHumanLocalM` | inherit | `HandleHumanLocalM.hs:177` `getStoreItem` | `:346` the recursive call | after the wait, replacing the manual re-read at `:180-182`; the `renderOneItem` callbacks capture too |
| `chooseItemProjectHuman` | `HandleHumanLocalM` | mint | in `getGroupItem`, called at `:398` | `:393` `psuitReqFun` inside `psuit` | inside `psuit`; the `:381` branch needs its own call |
| `chooseItemApplyHuman` | `HandleHumanLocalM` | mint | in `getGroupItem`, called at `:586` | `:581` `permittedApplyClient` | inside `psuit`; sibling (c), and nothing tests it |
| `psuitReq` | `HandleHumanLocalM` | inherit | — | — | at the top; its *caller* is the closure case |
| `pointmanCycleHuman` | `HandleHumanLocalM` | mint | — | — | nowhere; witness only |
| `pointmanCycleLevelHuman` | `HandleHumanLocalM` | mint | — | — | nowhere; witness only |
| `pickLeaderWithPointerHuman` | `HandleHumanLocalM` | mint | — | — | nowhere; witness only |
| `itemMenuHuman` | `HandleHumanGlobalM` | mint | `HandleHumanGlobalM.hs:1603` `displayChoiceScreen` | `:1614` `blid b`, the entry body | after the wait — re-fetch the body, not only the id |
| `chooseItemMenuHuman` | `HandleHumanGlobalM` | mint | — | — | nowhere; witness only |
| `projectHuman` | `HandleHumanGlobalM` | mint | — | — | at the top; `projectItem` keeps its pinned id |
| `applyHuman` | `HandleHumanGlobalM` | mint | — | — | at the top |
| `alterDirHuman` | `HandleHumanGlobalM` | mint | in `pickPoint`, called at `:1060` | `:1061` `alterTileAtPos` | after the wait; no pre-wait use to keep |
| `closeDirHuman` | `HandleHumanGlobalM` | mint | in `pickPoint`, called at `:1291` | `:1293` `closeTileAtPos` | after the wait; `:1284-1287` keeps a top read |
| `pickPoint` | `HandleHumanGlobalM` | inherit | `HandleHumanGlobalM.hs:1356` `getConfirms` | `:1362` `shift (bpos b)` | after the wait; move the `:1348` body read down |
| `moveItemHuman` | `HandleHumanGlobalM` | mint | — | — | at the top |
| `moveOrSelectItem` | `HandleHumanGlobalM` | inherit | `HandleHumanGlobalM.hs:787` `pickNumber`, `:796` `selectItemsToMove` | `:806` `moveItems` | after each wait — with `calmE`, `overStash`, `eqpFree` |
| `selectItemsToMove` | `HandleHumanGlobalM` | inherit | in `getFull`, called at `:870` | — | at the top; its `psuit` captures no actor |

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
rule of the post-mortem's §10 therefore binds here as it does at
`chooseItemProjectHuman`: the read goes inside the callback, not at the top
of the body. `psuitReq` itself is not one of these — its own body
(`HandleHumanLocalM.hs:510-527`) waits nowhere, so it reads at the top like
any other function; what it is, is the *value* a caller captures, and the
four closure cases are therefore call sites: `chooseItemProjectHuman`,
`chooseItemApplyHuman`, `runDefSkills` and `runDefInventory`, with
`transition`'s three `defAction`s and `chooseItemDialogMode`'s
`renderOneItem` alongside them in the table.

A fourth site belongs with them and is worse, being a live bug rather than
a classification slip: `chooseItemApplyHuman`'s own `psuit`
(`HandleHumanLocalM.hs:579-585`) calls `permittedApplyClient leader` inside
the action it hands to `getGroupItem leader psuit` (`:586`), which
`transition` re-runs per keypress (`InventoryM.hs:443`) while the store
permits switching — so the apply dialog judges items for whoever opened it.
That is the post-mortem's §09 sibling (c), and it differs from (a) in the
only way that matters to this step: (a) gains a `psuitReq` call inside the
closure while keeping its entry one, this one is inside already and needs
its *actor* read live. A conversion that puts `getLeaderUI` at the top of
the body satisfies the compiler and leaves the bug. PR 0 pins it; §02
step 6 also switches it by hand, that being the real frontend.

**And the last column found a class the design had not named: it is not
only the identity that goes stale, but what a body derives from it before
the wait.** `getFull` binds the entry actor's body and then a bag accessor
closed over it (`InventoryM.hs:264-265`), runs the whole dialog
(`:285`), and looks the chosen items up in *that* bag (`:290`) — with
`EM.!`, so an item the new pointman has and the old one lacks is not
incoherence but a partial-map failure. That is no longer a hazard on
paper: a scripted Tab and Return through the real dialog reach it, and
forcing the returned quantity dies with `IntMap.!: key 117 is not an
element of the map`, which `test/InventoryMUnitTests.hs` now pins as the
post-mortem's sibling (d). `moveOrSelectItem` computes `calmE`,
`overStash` and `eqpFree` from
the entry body and applies them after `pickNumber`'s wait
(`HandleHumanGlobalM.hs:783-787`); `itemMenuHuman` compares `blid b`
against the switched-to actor at `:1614` using the body it read before
`:1603`. Reading `sleader` live in these bodies fixes none of them on its
own — the *derivation* has to move below the wait too, which is why the
table's last column speaks of the bag and the flags, not only of the read.
The general form, which the post-mortem's §10 now carries: wherever an
identity is used before a wait to derive a value used after it, the
derivation moves with the read; "move the read" is the special case where
the derived value is the identity itself. A third wait surfaces with them,
named nowhere before: `pickNumber` (`HandleHelperM.hs:594`) displays a
choice screen, so the move family spans two waits rather than one.

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
pivot), `skillsOverlay`, `skillCloseUp` and `skillsInRightPane` (the
described subject), `accessModeBag` (pure), `partActorLeader` and
`partPronounLeader` (which read the pointman themselves, to decide whether
the actor they are given is "you"), and every `ActorId` bound as `aid`/
`source`/`target` in `RunM`, `SessionUI`, `WatchCommonM`,
`WatchSfxAtomicM` and `WatchUpdAtomicM`.

Five more are named `leader` and mean it, but are one-step callees whose
caller has just read the pointman, so they keep the parameter for the
reason the tail's second half does: `msgAddDone`
(`HandleHumanGlobalM.hs:1328`, reached from the tile-altering and
door-closing paths once each has the identity it will act for) and DrawM's
`drawLeaderDamage`, `checkWarningHP`, `checkWarningCalm` and
`checkWarnings`, which render one frame from a leader `drawHudFrame` read
for that frame. Three of the four are also called for actors that are not
the pointman at all, so a live read would be wrong outright there:
`checkWarningHP` and `checkWarningCalm` from `WatchUpdAtomicM` with an
`aid` (`:268`, `:271`, `:312`, `:315`), and `checkWarnings` per drawn actor
inside `drawFrameActor` (`DrawM.hs:332`); only `drawLeaderDamage` is
leader-only, its single caller (`DrawM.hs:556`) passing the very pointman
the frame was read for. They are listed here rather than in the tail
because they bind `leader` later in the head and so fall outside the 65
first-bound the tail is counted against.

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
here can go stale (the read-live table lists 28, one more than the census
sees — the point-free `pickLeaderWithPointerHuman`; and the Keep bullet
adds to its 11 the functions that bind no `leader…` parameter at all:
`pickLeader`, `skillsOverlay`, `partActorLeader`, `partPronounLeader` and
the `aid`/`source`/`target` families). The
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

1. Extract `macroStep` into whichever home the preamble's condition
   settles — `InputDecision` only if the wasm plan's Phases 0/2 have landed
   by then, `FrameM`'s own pure section otherwise, and the abort-split
   record's §01 warns that the first home also puts the module downstream
   of `Client.UI.Frontend` — with the *decision* half of that record's §01
   branch-exactness checklist as its test table. Four of
   that checklist's eight bullets are about the decision and become pure
   cases — the interrupt-inputs bullet, which is two cases rather than one
   (not queried; a disturbing report); the F1-help exemption surviving that
   same report; the legal-key guard, which aborts playback even when *not*
   interrupted; and the no-macro branch that must leave the macro stack
   alone — five rows in all, the interrupt bullet earning two. To them the
   table below adds three baseline paths any decision function needs: a
   voiced key with its remaining macro, a legal key voiced against a
   non-empty key set, and an empty macro under an interrupt, which must
   still be `NoMacro`. The other four bullets pin the *shell* rather than
   the decision — the common cleanup, the read-before-clear ordering, the
   special-event logic and the `addToMacro` recording — so a pure table
   cannot express them and none is missing from it. Three of those four are
   pinned already, by AS4, AS5 and AS8. **The special-event one is pinned
   by nothing**: every AS case calls `promptGetKey` with
   `ColorFull`, so the `dm /= ColorFull` branch and its `unless (gunderAI
   fact)` guard are entered by no AS case at all — and that is exactly the
   block the split relocates into `specialEventKeyReset`. (The integration
   test does enter it, down the shutdown path the record's §01 now names,
   and observes nothing there, so it pins nothing either.) An AS case for
   it has to exist *before* this step, or the refactor moves untested code
   and step 4's "must pass without edits" gate has a hole precisely where
   it is being relied on. A second one belongs with it: the no-macro
   branch's "no `resetPlayBack`" invariant is entered by AS4 and AS7 but
   observed by neither, both running on fixtures whose macro stack is
   already empty, so it too would survive being broken. Both characterize
   the *unsplit* primitive and are independent of either design, so §01
   lands them in PR 0, well ahead of this step; if that PR was skipped,
   they are simply this step's first commit. The pure cases are pure, so
   they sit beside the AS series or in a module of their own, wherever the
   function lands; `test/SessionUIMock.hs` already simulates macro-frame
   transitions, so no new harness machinery is needed — though the
   special-event case does need one thing the stub lacks, a frontend that
   records `FrontResetKeys` rather than discarding it. These are additions,
   not replacements:
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
   behaviour, and not something to edit away. Re-run the bridge tests X1/X2
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
-- * on every real-key read, i.e. on both non-voicing branches and never
--   on the voicing one: @recordHistory@, the cleanup (@srunning@,
--   @sxhairGoTo@, @sdisplayNeeded@, @sturnDisplayed@) and, inside
--   @connFrontendFrontKey@, @spointer@;
-- * voicing branch: the macro-frame advance (@smacroFrame@) and the
--   @MsgMacroOperation@ message;
-- * abort branch, all inside @abortMacroPlayback@: @resetPlayBack@,
--   @restoreLeaderFromRun@ -- which READS @srunning@, hence runs before
--   the common cleanup clears it -- and @resetPressedKeys@;
-- * no-macro branch: @resetPressedKeys@, when the colour mode is not
--   @ColorFull@ and the faction is not under AI;
-- * at the very end, reached from every branch and gated only on
--   @sreqQueried@ -- the one write here that is unconditional in the
--   control flow: @addToMacro@, recording the key into an in-game macro
--   being defined.
```

### The decision table that step 1 asks for, drafted

Five rows for the four decision bullets of the record's branch-exactness
checklist, the interrupt-inputs bullet earning one row per input, and three
for the baseline paths no bullet asks for; the four shell bullets can have
none, per step 1. The inputs are the four `macroStep` takes — queried,
disturbing report, keys legal for the frame, pending macro — and the
expected output is a `MacroStep`:

| queried | disturbs | legal keys | pending | expected | what it pins |
|---|---|---|---|---|---|
| yes | no | none | x, y | voice x, leaving y | baseline: the ordinary playback path |
| yes | no | x | x, y | voice x, leaving y | baseline: a legal key is voiced |
| yes | no | z | x, y | abort | an illegal macro key aborts even when not interrupted |
| no | no | none | x, y | abort | the not-queried interrupt input |
| yes | yes | none | x, y | abort | the disturbing-report interrupt input |
| yes | yes | none | F1 | voice F1 | the help exemption survives the same report |
| yes | no | none | empty | no macro | the empty-macro branch, which must not reset playback |
| no | yes | none | empty | no macro | baseline: no macro pending, so no interrupt to speak of |

Each row is one call of a pure function, so the table transcribes
directly into a test list; the AS series keeps driving the same decisions
through the real `promptGetKey`, which is what makes step 4 meaningful.

## 05 · The test battery as it stands

The design is encoded in a test suite already on master (all green on the
unmodified engine; 154 tests total, 39 of them new). Every test that pins
a design decision carries a `[contract]` or `[LR-flip]` tag — 36 do, 26
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
| permittedProjectClient judges its argument, not the pointman | ✅ contract | Sibling bug (a)'s premise, and the guard on §03's Keep ruling: the verdict is actor-dependent (`Right True` vs `Left ProjectUnskilled` on the same item), which is what makes a captured closure wrong after a switch — and it is about the actor *given*, not the pointman, so switching the pointman between two rounds of calls moves neither verdict. It carried an `[LR-flip]` tag until 2026-07-30, when the review found it had nothing to flip: §03 keeps this function's `ActorId`, so live-read must leave both answers alone, and a conversion that made it read `sleader` fails here. |
| psuitReq verdict differs per actor | ❌ LR-flip | sibling bug (a) at the exact captured value: `psuitReq` — what `chooseItemProjectHuman` bakes into the dialog's `psuit` — gives a different failure per actor with the xhair on C's own position ("aiming obstructed by terrain" for A, the degenerate "aiming blocked at the first step" for C), through the real aiming pipeline, no walkable tiles needed |
| Project executed by a different actor than the item selection | ✅ contract | Sibling bug (b), both halves of the seam: with `sitemSel` left by A's choose dialog, `projectHuman` run for A gets past the store lookup (control), run for C fails with "no item to fling" for the item just approved. Deliberately [contract]: the execute-half pinned here is correct in isolation and survives the live-read design — what it fixes is the *choose* half, whose live re-reads make the dialog re-validate for C before the selection is confirmed, closing the seam where the incoherent approval arises. |
| fling dialog: a mid-dialog switch keeps A's closure | ❌ LR-flip | Sibling bug (a) end to end, on the walkable board: a scripted `C-Tab` switches the pointman to C inside the real fling dialog, whose captured A-closure still calls the item suitable, so `Return` selects it and `sitemSel` is set — for an item the unskilled C cannot fling. Post-live-read the closure judges for C, nothing is suitable and the dialog exits "never mind" with `sitemSel` unset (flip verified by temporarily re-reading the pointman in the dialog's `psuit`). |
| alterDir: the held leader picks the square to modify | ❌ LR-flip | The remaining site of the post-mortem's §09, `alterDirHuman`/`pickPoint`, driven through the real crash window (the post-mortem's §04): a macro dies inside the wait, `promptGetKey` restores the pointman to A, and the command modifies from the actor it was *handed* — the run holding A targets C's floor, the one holding C the wall past it, and the two failures name the two tiles. Post-live-read both read the restored A and both name the floor (flip verified the same way). |
| chooseItemHuman: ESC exits the real store dialog | ✅ contract | that a whole dialog is drivable under the mock: `chooseItemDialogMode` → `getStoreItem` → `displayChoiceScreen` to the "never mind" exit, reaching `promptGetKey` through its `SlideshowM` call site — the path the end-to-end fling row above runs on |
| chooseItemHuman: scripted Tab switches pointman mid-dialog | ✅ contract | the dialog's own cycling handler and `recCall`'s re-entry — the re-sync of the post-mortem's §02, from commit `8608d6f9c`, previously untested — on the equipment store, which needs no aiming |
| getFull looks the chosen item up in the entry actor's bag | ❌ LR-flip | Sibling (d), in `test/InventoryMUnitTests.hs` beside the other `getFull` cases: a scripted Tab switches the pointman inside the real dialog and `Return` picks C's item, whose quantity `getFull` then looks up in A's bag — so the identity comes back right and forcing the quantity dies with `IntMap.!: key 117 is not an element of the map`, which the test asserts on rather than merely catching. Post-live-read the bag is re-derived below the wait with the read, and the whole pair returns: `Right (CEqp, [(testItemId2, (1, []))])`. Non-vacuity proved both ways, recorded at the test. |

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

What the battery does *not* cover, said out loud because the rest of this
section reads as coverage: the apply dialog. Sibling (c) of the
post-mortem's §09 — `chooseItemApplyHuman`'s `psuit`, §03's second closure
case — has no row above and no test anywhere; `git grep
chooseItemApplyHuman -- test/` is empty. Its analogue for the fling dialog
took a walkable board to write, so the cheapest cover is the same board
with `permittedApplyClient` in place of `psuitReq`; until someone writes
it — §01's PR 0 is where it belongs, alongside the two AS cases — §02 step
6's by-hand switch is the only check that (c) closed.

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
