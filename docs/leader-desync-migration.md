# The leader-desync migration plan

*LambdaHack -- UI client -- the work list for two designs --- written
to be deleted*

> **This document is temporary by design.** The permanent records
> are `docs/leader-desync-bug.md` (the crash, the analysis, the live-read
> design) and `docs/promptgetkey-hygiene.md` (the abort-split). This file holds
> only what stops being true when the work lands: the ordering, the conversion
> inventory, the artifacts still to write, and the state of the test battery.
> The Log is frozen entry by entry as each is dated, and so is every dated
> measurement and landed-row record outside it --- C0's row states what
> the spike met on 2026-08-07 on a parked branch, a measurement that ages rather
> than drifting, so its `153` is not a count to correct; everything else ---
> the ledger, the gates, the inventory and this ritual --- is live, so a drifted
> claim there is an error to fix rather than a record that has aged. Nothing
> here is worth keeping afterwards --- the records carry the reasoning, the code
> carries the result --- which is also why this file is deleted where
> `docs/wasm-frontend-unified-plan.md` matures into one: that campaign has
> no permanent record behind it and would lose its reasoning with the file,
> where this one has two records that already hold every sentence worth
> outliving the work. But deleting it is not the one-line act it reads as:
> `git grep -n leader-desync-migration -- . ':!docs/leader-desync-migration.md'`
> lists every file naming it, most of them wanting a reword and the rest
> something else that the bullets below spell out. One wants nothing at all:
> `tools/check-plan-citations.py:110-113` names this file inside a dated
> measurement, which ages rather than drifting. When sec. 04's last step is done
> --- which sec. 00's ledger says and the code does not, 04.4's **Split** (1)
> being the commit that records the gate's result and flips that row,
> so a session that cannot point at the flipped row has not reached this ritual:
>
> - delete this file, and `tools/leader-census.py` with it; the post-mortem's
>   sec. 10.3 rests its census-rather-than-floor claim on the tool by name,
>   so the same commit recasts that sentence to the past tense --- and drops
>   the backticked path with the tool rather than carrying it into the recast,
>   since `python3 tools/check-doc-refs.py docs/leader-desync-bug.md` resolves
>   that path today (`ok path tools/leader-census.py`) and fails on
>   it the moment the file is gone, which is the last bullet's own run,
>   and `tools/doc-refs-allow.txt` is for an artifact not yet built rather
>   than for one that will never exist again. Ruled 2026-08-07 over keeping
>   the tool, which after the migration is a checker that stays green while
>   certifying nothing, per sec. 03. Two things elsewhere rest on the tool
>   merely existing and break with it, neither reachable from the grep above,
>   since neither spells this file's name: `tools/check-doc-refs.py` names
>   it as a *passing* control in its self-test document
>   (`tools/check-doc-refs.py:216`) and in `SELF_TEST_OK`
>   (`tools/check-doc-refs.py:251`), so `--self-test` fails once the file
>   is gone --- swap that control to another tracked script under `tools/` ---
>   and `tools/defects.json` carries the record `leader-census-01` against it,
>   retired in the same commit;
> - note the landing in `CHANGELOG.md` (the lines are drafted in sec. 02),
>   and add to each record the outcome line it reserves --- bar the two PR 0
>   appends when it lands. **Ruled 2026-08-07: PR 0 appends its two.** It pins
>   the apply dialog and lands the AS cases, which is what closes
>   the post-mortem's "the apply-dialog one by nothing" and the abort-split
>   record's "two are unpinned", and it appends those two lines itself rather
>   than leaving both records under-claiming their own coverage for two PRs.
>   The cost, a second commit into a frozen record, is permitted --- an outcome
>   line per resolved claim is exactly the upkeep those records reserve,
>   the post-mortem's sec. 09 line saying outright to put the fixing commit
>   there when a sibling closes, and not the forbidden updating of a post-mortem
>   to match a later tree;
> - reword the inbound references, which no mechanical pass can see once
>   the target is gone: the backticked `docs/leader-desync-migration.md` paths
>   that command lists, and the pointers by *name* it does not ---
>   `grep -nE 'migration (document|plan)'` over `wrap80 --unwrap` output
>   for `CLAUDE.md`, both records and the wasm plan, and over that form only:
>   `git grep` is line-oriented, so no pattern of its crosses a break
>   and whitespace tolerance buys nothing there, while the phrase does wrap ---
>   the post-mortem loses one of its occurrences to a break today. Counting
>   those by hand is what went wrong here before; the second list is read, never
>   driven to zero, since sec. 11 goes on saying "the migration" of the *work*
>   after this file is gone;
> - edit `CLAUDE.md` three times: the sentence in "Where to look next"
>   that names this file; the pointman gotcha's "Until the live-read design
>   lands" clause, which the landing is what resolves; and the standing-checks
>   bullet that hands `tools/check-plan-crossrefs.py` "the two campaign plans,
>   `docs/wasm-frontend-unified-plan.md` and `docs/leader-desync-migration.md`",
>   which wants the sentence rewritten rather than the path excised, excision
>   leaving one document named as two;
> - and the sites outside the pointman records, which are the ones a reader
>   forgets. `docs/wasm-frontend-unified-plan.md` names this file
>   at the tag-ownership sentence, which keeps its claim and drops only
>   the citation, and at 3.3's **Split** and **Owns** and 2.4's **Owns**, where
>   the verb is delete rather than reword: both hold this document
>   *conditionally*, so the condition goes with the path.
>   `test/HandleHumanLocalMUnitTests.hs:193`
>   and `test/InventoryMUnitTests.hs:72` cite it from test comments.
>   `tools/check-doc-examples.py:72-75` names it among four live controls,
>   and its 0 is the one proving comment stripping still works --- drop
>   the entry, and say that the control goes with it. `tools/checks.py:64`
>   excuses `tools/leader-census.py` from coverage by naming this file,
>   so that entry goes with the tool in the same commit. Two are code rather
>   than prose and a reword will not serve. `tools/check-plan-crossrefs.py`
>   configures this document in `GRAMMARS`
>   (`tools/check-plan-crossrefs.py:161,171`), in its self-test fixtures
>   (`tools/check-plan-crossrefs.py:447,449`) and in its module docstring
>   (`tools/check-plan-crossrefs.py:13`); and `tools/checks.py` runs that script
>   with no argument, so once the file is gone it raises `Blocked: no such file`
>   and exits 2, which is never a pass here. **Ruled 2026-09-07 by Mikolaj:
>   the entry stays, as dead configuration.** Removing it would remove
>   the second grammar itself --- `SELF_TEST_DOC_2` is a copy under another name
>   and fits by that grammar's openers --- and would take with
>   it the self-test's work-list-alone, joint and shared-id sub-checks,
>   the `tools/mutants.py` block headed "the second grammar and the joint run",
>   and `tools/defects.json`'s `check-plan-crossrefs-04`, which replays both
>   documents out of `6f564870c` and so survives the deletion but
>   not the entry's, grammar selection being by fit rather than by name
>   (`tools/check-plan-crossrefs.py:610-612`). Keeping it costs one guard
>   and one reword, both tooling edits owned there and named here: the script
>   skips a configured document that is absent instead of blocking on it,
>   so `checks.py` stays green, and the docstring's "Two carry that grammar"
>   and its account of what the joint run alone can see are recast to say one
>   of the two has left the tree and survives in the fixtures. Two defect
>   records still want disposing, neither under `--audit`, all three
>   `check-plan-crossrefs` records being `"kind": "control"` and so skipped
>   in the bug direction: `-03` invokes this path outright and asserts
>   `expect_text` that a `BLOCKED` run cannot print, so it fails loudly,
>   and `-05` invokes it with `expect_absent` alone, so every absent string
>   is duly absent and the case passes **vacuously** --- the silent shape
>   this repository's conventions exist to prevent. Retire or re-plant those two
>   in the same commit; `-04` needs nothing once the entry stays;
> - and the sites no grep above reaches, because they name this campaign without
>   naming this file. One is spelled out here, its repair being neither a reword
>   nor a deletion: `test/CLAUDE.md` never spells this file's name, yet
>   its "Characterization tags" section states the vocabulary in the unlanded
>   tense --- "characterizations of known-buggy behaviour", tests that "must
>   survive both planned designs (live-read, then abort-split)", "flip
>   it together with the engine change it documents",
>   and the `# flip with the fix` comment on the `LR-flip` command in the fence
>   below them --- every clause of which the landing falsifies, since
>   by then both designs are the tree and the flip has happened. The rest
>   are a class rather than a list: `tools/checks.py`'s comment above
>   its `plan crossrefs` step, on "Both campaign plans", which spells no path;
>   and every **Owns** in the wasm plan that names an item of this campaign
>   by id (`04.4`, `C3`, `PR 0`) rather than by path. Read for them
>   over `wrap80 --unwrap` output and with more than one phrasing: one
>   line-oriented pattern over the wrapped form returned a count a second
>   pattern contradicted, which is why this is a class and not a list;
> - then re-run `python3 tools/check-doc-refs.py` over `CLAUDE.md`, both records
>   and the wasm plan --- the pass that catches a backticked path left behind
>   --- and re-run the `git grep -n` above, which is read rather than driven
>   to zero. What survives it by design: the exemption this callout opens on,
>   and `tools/defects.json`'s frozen `notes` fields and history-reading
>   `plant_cmd`s, which record what was true when they were written and
>   are not edited to match a later tree. A survivor that is neither is a site
>   the bullets above missed.
>
> File:line citations were verified against the tree at commit `f15820209`
> (2026-09-07) --- the newest commit touching any file they cite. This stamp
> sits below the ritual and is no step of it: it is the document's own
> maintenance, for as long as there is a document to stamp. Re-run
> `python3 tools/check-plan-citations.py docs/leader-desync-migration.md --restamp`
> after the reading pass, and re-verify the only/every/never claims by repo-wide
> grep. Verify the post-mortem's secs. 10--11 in the same pass: they
> are the live half of that record --- the design this plan executes --- so they
> drift with this file rather than ageing with its frozen sections.

## 00 -- Status, size and per-step checks

Keep this ledger current as the work proceeds; it is the reason this document
lives in the repository rather than in someone's head. Steps 1 to 5 of sec. 02
are **six commits**, C1 to C6 below, and every row of the ledger is green
on its own --- as is every other row here, which is what makes the rows the unit
of rollback sec. 01 relies on. The abort-split rows keep sec. 04's step numbers
as their names and the live-read ones do not, which is deliberate rather
than half-finished: over there each numbered step *is* one commit, so one name
serves both, and here it stopped being true.

That is a 2026-08-07 repartition, and the reason it is worth recording
is that the shape it replaced had an argument behind it. Steps 2 to 5 were one
commit because a characterization and the code it characterizes must
not be committed apart, and the steps as numbered scattered the ten flips away
from the conversions earning them. What forced the lump underneath
that was narrower and went unnamed: step 2 converts the dialog chain, whose
entry points *are* `CmdLeader` cases, so it changes the boundary's field type,
and that was taken to drag all 29 cases with it. The sec. 02.0 spike showed
it does not. A shim in `addLeader` and `weaveLeader` --- read the identity
from the witness and hand it on --- carries every unconverted handler across
the type change, 17 of the 29 through those two helpers alone and the twelve
direct cases through a written-out lambda each. So the boundary flips
in a commit of its own, handlers convert in groups afterwards, and each group
lands with exactly the flips it earns. The principle is unchanged and better
served; only the claim that no finer split stays green was wrong, and
it was wrong because nobody had tried it.

Every row is an item --- the C-rows and PR 0 below "Running this plan", sec.
04's rows after the steps that specify them and before the drafts, sec. 05's
at the end of that section --- opening on a heading that carries the row's name
and closing with the execution block `docs/wasm-frontend-unified-plan.md`
defines under "Handing an item to a session" --- **Split** where the item
is several commits, then **Owns**, **Done**, **Hands back** and **Decide
first**, each written out even as `nothing` --- which is read there
and not restated here. `tools/check-plan-crossrefs.py` reads this document
against itself and, run with no argument, both documents against each other,
so a file both campaigns write is named on both sides by item id, outright
or through the claimant list one item holds for it. Three things
are this document's own. The gates **Done** names are defined once,
under "Running this plan", and a gate that selects a series states the count
it expects, since the count is what is read. The lock does not serialize
on this document, as it does not on that plan: a ledger flip, and a Log line
where the Log's rule earns one, are the item's own lines. And the states
are the ones the pointman records lent that plan --- `landed` in a named commit,
`open` for code that is wrong today, `not applied` for a design nothing has
built --- with that plan's rule on the one row whose remaining evidence
is a human's: a green **Done** does not flip C7.

| row | delivers | size | depends on | state |
|---|---|---|---|---|
| C0 | the spike: witness and accessors on one path (step 0) | half a day | --- | landed -- parked on `spike-pointman-witness` (`af673d1f4`), not merged |
| PR 0 | coverage: AS14, AS15 and the apply-dialog pin | small | --- | not applied |
| C1 | witness and accessors (step 1) | tiny | C0; PR 0 | not applied |
| C2 | boundary and shim (from steps 2 and 5) | small | C1 | not applied |
| C3 | dialog chain (step 2, with the flips step 4 earns it) | medium | C2 | not applied |
| C4 | the cycle pair, and the assertion (step 3, with its flips from step 4) | small | C3 | not applied |
| C5 | the waiting three (step 5, with alterDir's flip from step 4) | small | C4 | not applied |
| C6 | the tail: sec. 03's remainder, and the shim retired (step 5) | medium | C5 | not applied |
| C7 | verification (step 6) | the gate, and a human's session | C6 | not applied |
| 04.1 | extract `macroStep` | small | C7 | not applied |
| 04.2 | name `abortMacroPlayback` | tiny | 04.1 | not applied |
| 04.3 | audit the residual writes | tiny | 04.2 | not applied |
| 04.4 | AS series unchanged, then the landing | the gate, then the deletion | 04.3 | not applied |
| 05 | the test battery | --- | --- | landed on master (`3453b1777` through `8b5703e87`, then `4b92b291a`) |

### Running this plan

**Who runs it.** A session, autonomously, not a person working from memory
of the campaign --- the same executor `docs/wasm-frontend-unified-plan.md`
writes for, and the reason that plan spells out per item what a session cannot
do. Nothing below is addressed to someone who already knows which check opens
a window or which count was true last week. Two consequences bind every row:
a session does exactly what is written and nothing that is merely implied,
so an unstated step is an unperformed one; and it cannot tell a check
it is expected to skip from one it has failed to run, so a row whose acceptance
it cannot complete has to say so rather than leave the gap to judgment.
That this had to be stated is itself the evidence --- the intent was read off
the prose and got read wrong.

**The gates, once.** Every **Done** below is built from these, named rather
than spelled out, run from the repo root. Read the counts and the printed
snippets, not the exit status alone --- a suite that silently loses a test still
passes, and a citation that slid still resolves:

```
native     cabal build && cabal test && hlint .        # 154 tests today
           && stylish-haskell -i <every .hs the        # hlint must print:
                                  item's Owns names>   #   No hints -- and a
           && git diff --exit-code <those paths>       #   hint that needs a
                                                       #   new .hlint.yaml
                                                       #   exception stops and
                                                       #   asks; stylish must
                                                       #   leave them alone
contract   cabal test --test-options='-p "/contract/"'    # 26 today; moves once
LR-flip    cabal test --test-options='-p "/LR-flip/"'    # 10 today; moves twice
AS         cabal test --test-options='-p "contract AS"'   # 13 today; moves once
census     python3 tools/leader-census.py             # green before C3; sec. 03
cite       python3 tools/check-plan-citations.py <this file, the post-mortem,
           the wasm plan>, the printed snippets re-read, never the exit alone
docs       the repo's standing document passes over this document, which
           CLAUDE.md lists; the reading pass is what `--restamp` asserts
playtests  make test-short && make test-medium       # minutes each
```

Ruled campaign 3, 2026-09-07: that block stays stated in full here even though
`docs/wasm-frontend-unified-plan.md` states a near-identical `native` chain,
and the same holds of the Log intro's log-worthy rule. A session executing one
campaign must not have to open the other campaign's document to learn what
a gate name means, and the price decides it: nearly every **Done** cell here
names `native`, so a pointer would cost a cross-campaign read at each of them,
against a chain that has moved once.

Those three counts move at exactly three points and nowhere else, the 2026-08-07
repartition adding none of them: PR 0 takes them to 157, 28 and 11 --- two
AS cases and one flip pin, the AS series itself going from thirteen to fifteen
--- C4 then deletes LR6, taking them to 156, 28 and 10; and 04.1 adds the eight
pure cases of its decision table, which carry neither marker, so the suite total
alone moves, to 164. Every other commit of PR 1 flips expectations without
moving a count, which is why "count unmoved" is a real check on each of them
rather than a formality. The three counts were re-measured on the unmodified
tree on 2026-08-07 and are 154, 26 and 10 as stated. A "count unmoved"
in a **Done** below is against whichever of the three baselines its row follows,
and this is the only place the sequence is stated, so a row that disagrees
with it is wrong there rather than here. A count that shifts otherwise
is the finding, not a nuisance: both patterns select on the test *name*,
so a renamed test leaves its series silently, and, matching by containment,
a marker merely *containing* `contract` or `LR-flip` joins it ---
`docs/wasm-frontend-unified-plan.md` keeps the other campaign's markers outside
both, twice having had to. Builds take minutes --- set the timeout rather
than reading one as a hang --- and the flag set stays fixed for the campaign,
`+with_expensive_assertions` included, since changing it rebuilds every local
package. The citation pass is owed after every code step and not only
at the end: steps 2 to 5 rewrite the very lines sec. 03 and the post-mortem's
sec. 10 cite, and the wasm plan's Appendix C cites `HandleHumanGlobalM.hs`,
`HandleHelperM.hs` and `HandleHumanLocalM.hs` lines the same conversions slide
--- cited there, not written, so no **Owns** covers them and a frozen appendix
restamps green while pointing at the wrong lines. What to do when the pass finds
one there is *record it and leave it*: R3's ruling keeps the frozen appendices
reading against the tree the investigation stamped, so the detection belongs
in this document's Log and not in an edit to that appendix. `--restamp`
then follows the reading pass, as the header says.

**What may be fanned out, and what may not.** The conversion is a type-directed
cascade inside *one* library and *one* test-suite component: change a signature
and the compiler names the next site, so the tree is red until the frontier
closes. The shim buys green *commits*, not green intermediate states --- within
C3 or C4 the frontier is open exactly as before, and only at the commit boundary
is it closed. The edits are therefore serial, in one working tree --- two agents
converting two modules in parallel produce two partial states, neither of which
compiles, and each pays a full four-library rebuild, `dist-newstyle` being per
worktree where only the package store is shared. What does parallelize
is everything that reads rather than writes: the per-function placement analyses
behind sec. 03's table, the flip verifications of step 4, the authoring of PR
0's tests, and an adversarial pass after each commit asking whether any
converted function still holds an identity across a wait. Fan out to decide,
converge to edit, fan out to refute.

**Stop and ask.** Two outcomes are not the implementer's to settle: step 0
finding that the witness reads badly at real call sites, which reopens witness
versus witness-free (the post-mortem's sec. 10.4); and any `[contract]` test
whose *outcome* will not come back green --- a call site that merely needs
a witness to compile is not one of those, and sec. 02 step 4 names the seven
such sites. A third reservation is stated where it fires rather than here ---
the `native` block's `hlint` comment above --- because this paragraph is read
once at pickup and that block at every row.

**Never**, each having cost someone a round trip already: don't convert
`projectItem`, `meleeAid` or `processTileActions`, the three sites
the post-mortem's sec. 10.6 pins; don't convert the eighteen keep-param entries
of sec. 03's tail; don't fix the apply closure the way `psuitReq`'s is fixed,
by moving the call, since there it is the *actor* that must go live; and don't
bind a `getLeaderUI` result before a `promptGetKey` and use it after, which
is this whole document in one sentence.

**Open `test/CLAUDE.md` before touching anything under `test/`**, not after
a fixture surprises you. It holds the harness facts every test here rests on,
and it loads by itself only for a session already working there --- which
is after the fixture has been chosen, and too late. That binds every row whose
**Owns** carries a `test/` path, and hardest the two that author tests
from nothing: PR 0's three and 04.1's pure cases.

**Owed, and not started**, so that the confidence of this document
is not mistaken for coverage. PR 0 owes three tests: the two AS cases of sec. 04
step 1, of which the special-event one needs something the harness does not have
--- a `ChanFrontend` that *records* `FrontResetKeys` rather than printing
it (`UnitTestHelpers.hs:135`), that request being the branch's only effect ---
and a characterization of sibling (c) in the apply dialog, which no test enters
at all. Sibling (d)'s pin landed ahead of them,
in `test/InventoryMUnitTests.hs`. Two things the 2026-08-07 probes add
to that list, both cheap and both invisible until someone starts writing:
the special-event case must run on a *party* fixture, `stubCliState`'s faction
being under AI and so silencing the very effect it observes; and the sibling (c)
pin needs `permittedApplyClient` moved out of `HandleHumanLocalM`'s
`EXPOSE_INTERNAL` block into the tests-visible group beside
`permittedProjectClient`, but needs no walkable board, its verdicts differing
per skill on the plain one.

**Claims here that no pass can re-run.** Four assertions in these three
documents rest on experiments whose artifact was never recorded: the two
placement verifications in the post-mortem's sec. 10.3, the rank-2 spike
in its sec. 10.4, the verdict table of its sec. 08 and the baseline of its
sec. 11. Everything else here a reader or a checker can settle; these can
be settled only by redoing the experiment. Re-establish one before leaning
on it in a decision, and when a step redoes it, record the command
and the output beside the claim, the way the scripts under `tools/` record their
non-vacuity recipes. **Ruled 2026-08-07: they stay flagged rather than being
re-run eagerly** --- each is redone by the step that leans on it, step 4's flip
loop covering the verdict table and both placement verifications
by construction, the sec. 02.0 spike superseding the rank-2 one against the real
tree, and sec. 11's interleaved A/B protocol needing no fresh absolutes ---
and an eager artifact would be taken against lines steps 2 to 5 rewrite.

### C0 --- spike (step 0)

`MonadClientUI` plus the three frames of `PointmanCycleLevel`, and the five test
call sites that break with them (`HandleHelperMUnitTests.hs:121`, `:141`,
`:176`, `FrameMUnitTests.hs:343`, `:377`). Its acceptance: the library compiles
and the witness reads tolerably at a real call site; then, once those five take
a witness, the suite compiles and LR1/LR2/LR5 are green while LR3/LR4
and the two bridge tests are red and LR6 unrepresentable --- the spike working,
not failing. Met exactly on 2026-08-07, on branch `spike-pointman-witness`
(`af673d1f4`), parked not merged: 4 of 153 failing and those four LR3, LR4, X1,
X2, with all 26 contract tests green and `hlint`/stylish clean. The design
stands, so the witness-free variant stays passed over and C1 proceeds. Five
findings for the items below are in the Log.

**Owns** --- nothing on master: the spike is parked on its branch and either
reverts or becomes C1's first draft, and the files it touched there are C1's,
C3's and C4's to write.

**Done** --- landed, on the branch: `cabal build`, `contract` green whole,
`LR-flip` red at exactly the four named above and nowhere else, `hlint .`
and stylish clean.

**Hands back** --- nothing.

**Decide first** --- nothing; the one question it carried, witness versus
witness-free (the post-mortem's sec. 10.4), it answered.

### PR 0 --- coverage, ahead of the engine change

The three tests sec. 01's table row names and "Owed, and not started" above
describes: AS14 and AS15, characterizing the *unsplit* `promptGetKey` ---
the special-event branch, which needs a `ChanFrontend` that *records*
`FrontResetKeys` rather than printing it (`UnitTestHelpers.hs:135`) and must run
on a party fixture, `stubCliState`'s faction being under AI
(`UnitTestHelpers.hs:350`) and so silencing the effect;
and the no-`resetPlayBack` invariant, observed on a non-empty macro stack behind
an empty pending frame --- and the apply-dialog pin of the post-mortem's sibling
(c), written the way the `psuitReq` one was, on per-actor failure verdicts
(`ApplyNoEffects`, `ApplyFood`, `ApplyUnskilled` for skills 2, 1 and 0), which
needs `permittedApplyClient` moved out of `HandleHumanLocalM`'s
`EXPOSE_INTERNAL` block into the group beside `permittedProjectClient`
and no walkable board. All three pin today's behaviour and land green; nothing
here touches the engine beyond that export move. It takes the three counts
to 157, 28 and 11, the first of the three movements "Running this plan" permits,
and the AS series to fifteen. **Append, do not insert**, on C1's model
and for C1's reason: `test/UnitTestHelpers.hs` is the most-cited file
this campaign writes --- `test/CLAUDE.md`, a seeded rules file, carries ten
citations into it, the wasm plan four and this document two of its own, every
one of them further down the file than the stub at `:135` --- so the recording
`ChanFrontend` goes at the end of the file, where it slides nothing,
and the widened `cite` in **Done** is the fallback for inserting beside the stub
rather than the plan.

**Split** --- two commits, and a third only if (1) put the recording stub beside
the existing one instead of appending it: the citation repair
over `test/CLAUDE.md` and the wasm plan that the slide then forces. The titles
of the two are drafted in sec. 02. (1) the two AS cases, named `contract AS14`
and `contract AS15` as the series is, since the `AS` gate selects on that prefix
and its count of fifteen rests on it, with the recording `ChanFrontend`
in `test/UnitTestHelpers.hs`, and the abort-split record's outcome line, whose
"two are unpinned" this closes. (2) the apply pin with its export move,
and the post-mortem's outcome line, whose "the apply-dialog one by nothing"
this closes --- the two appends the head callout rules PR 0 makes. (2) carries
the ledger flip.

**Owns** --- `test/FrameMUnitTests.hs`, `test/HandleHumanLocalMUnitTests.hs`,
`test/UnitTestHelpers.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs` (its export list
only), `docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md` (one
outcome line each, the head callout's ruling), and this document; plus,
in a citation-repair commit and only if the append above was not taken,
`test/CLAUDE.md` and `docs/wasm-frontend-unified-plan.md`, whose citations
into `test/UnitTestHelpers.hs` an insertion beside the stub would slide ---
the repair being nobody's otherwise, since neither document's own campaign
writes that file here. The claimant list for `HandleHumanLocalM.hs` and
for the post-mortem is at C3, for `test/FrameMUnitTests.hs` at 04.1; C3 also
writes `test/HandleHumanLocalMUnitTests.hs`; the wasm plan's determinism goldens
and sum-typed selection both write `test/UnitTestHelpers.hs`; 04.4 writes
`test/CLAUDE.md` and the wasm plan at this campaign's landing, and that plan's
own lock does not serialize on itself; and 04.4 appends to both records after
this item.

**Done** --- `native`, `contract` at 28, `LR-flip` at 11, `AS` at 15, `docs`
over this document and both records --- and over `test/CLAUDE.md` and the wasm
plan too if the repair commit happens --- the outcome lines going into them
here; `cite` widened to `test/CLAUDE.md` beside its usual three,
with the printed snippets re-read, unless the recording stub was appended
at the end of `test/UnitTestHelpers.hs`, in which case nothing there has moved;
and, for each of the two AS cases, the non-vacuity the harness cannot supply.
For the special-event one: run it once against `testFaction` in place
of the party fixture and watch it fail, that being the fixture that silences
the effect. For AS15: apply the break to the engine and revert it, as step 4's
flip loop does --- call `resetPlayBack` in `promptGetKey`'s no-macro branch
(`FrameM.hs`), run the case, watch it fail, revert. AS15 is written precisely
because AS4 and AS7 enter that branch and observe nothing, so a vacuous AS15
passes 04.1's "must pass without edits" gate exactly as a real one does
and reproduces the hole it was written to close. A probe applied and reverted
inside one row is outside **Owns**, which locks what a commit carries,
so `FrameM.hs` stays 04.1's.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-08-07: the two AS cases are new tests
rather than an assertion inside AS4 (sec. 01's callout), and PR 0 appends
the two outcome lines itself (the head callout).

### C1 --- witness and accessors (step 1)

`MonadClientUI` only, ~30 lines: the abstract `HasPointman`, the checking
`mintHasPointman`, `getLeaderUI` (witness required) and the `Maybe` variant, per
sec. 02 step 1, with the pinned-parameter note sec. 02 drafts sitting
on `getLeaderUI` from the start, since the sites C6 writes point at it. A file
two campaigns write, and the sharper of the two:
`docs/wasm-frontend-unified-plan.md` cites into it nine times
(`MonadClientUI.hs:166` once, `MonadClientUI.hs:329` six times --- all
`getFontSetup`, which its 2.4 rewrites and which may land at any time ---
and `MonadClientUI.hs:455` and `MonadClientUI.hs:469` once each, this plan's own
citations mirrored back by that plan's 2.4). The bodies are disjoint
and an export-list clash is loud, so the hazard is neither --- it is that ~30
lines inserted above `:166` slide all nine onto other lines *while they still
resolve*, leaving `tools/check-plan-citations.py` green over a document that has
started to lie. It is not the only such file, and the test that excluded
the others was the wrong one: a citation slides when the line count *above*
it changes, not when the cited function converts. So `HandleHumanLocalM.hs`
joins --- sec. 02 converts `chooseItemDialogMode` and the `chooseItem*Human`
wrappers well above the wasm rip-out's cited `HandleHumanLocalM.hs:815` ---
and so does `test/UnitTestHelpers.hs`, which PR 0 writes and two of that plan's
items own; of the other two both campaigns name, sec. 03's DrawM ruling
is a decision *not* to write and `SessionUIMock.hs` is read rather than written.
Files that plan only *cites* are a wider set and a different hazard, met
by the `cite` gate rather than here. The second and third files the ruling below
reserved have therefore arrived, and the reopened question was ruled
on 2026-08-07: no mechanism --- machinery that must be maintained or become
a lie, for the little both campaigns have left to run --- so each side goes
on warning by hand, this item and that plan's 2.4 and capability-constants
blocks, with this item's snippet re-reading as the check a green run cannot
replace; reopen again only if the shared set grows. **Append, do not insert**:
the spike put the whole block below `MonadClientUI.hs:469` and slid nothing,
so the hazard this item describes is avoidable rather than merely detectable,
and the snippet re-reading in **Done** is the fallback for inserting high,
not the plan.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/MonadClientUI.hs`
and this document. The wasm plan's 2.4 and its sum-typed selection practice
write the same file, both at `MonadClientUI.hs:329`; the bodies are disjoint
and the hazard is the slide above, so serialize and append.

**Done** --- `native`, `contract` unmoved, `LR-flip` unmoved, `docs`; nothing
else changes, this step having no callers yet; and, if the block goes
in anywhere but the end of the file, `cite` over the wasm plan with the four
printed snippets re-read, which stand for those nine sites, the checker printing
one per distinct line --- a green run is not sufficient there.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-08-07: the insertion point is the end
of the file, and the shared-file hazard stays hand-warned on both sides rather
than mechanized (both in the Log).

### C2 --- boundary and shim (from steps 2 and 5)

`HandleHumanM` only: the `CmdLeader` field becomes `HasPointman -> ...`,
`cmdSemantics` mints instead of reading `sleader`, and the shim goes
into `addLeader` and `weaveLeader`, which take a `MonadClientUI` constraint
where they had `Monad`; the twelve direct cases get a written-out lambda each.
`Game.LambdaHack.Client.State` goes redundant here and its import comes out.
No handler converts. That import is `HandleHumanM.hs:17`, so every line below
it moves and this is a citation-sliding step like C3 to C6: the post-mortem's
live sec. 10.4 cites `:217-218`, `:220-225`, `:170`, `:208` and `:209`, and sec.
03 below cites `:129`, all of which go on resolving after the shift.

**Split** --- two commits, (1) the boundary change and (2) the citation repair,
exactly as C3's; (2) carries the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs`,
`docs/leader-desync-bug.md` (secs. 10--11's citations, in the repair commit
only; the claimant list is at C3), and this document. **The claimant list
for `HandleHumanM.hs` lives here**: C2, then C3, C4, C5 and C6, each taking
the boundary cases of the handlers it converts, one holder at a time
in that order.

**Done** --- `native`, `contract` unmoved, `LR-flip` unmoved, `cite`
over this document and the post-mortem with the printed snippets re-read
and the ranges re-cut, then `--restamp` for both, and `docs`: the conversion
commit changes no behaviour at all, which is the whole of its claim,
and a single moved expectation means a handler was converted by accident.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-08-07: the shim in two helpers is what
the spike settled (the Log).

### C3 --- dialog chain (step 2, with the flips step 4 earns it)

`InventoryM` (7 functions), `HandleHumanLocalM` (`chooseItemDialogMode`,
the three `chooseItem*Human` wrappers, and `psuitReq`, which loses its own
`ActorId`), `HandleHumanGlobalM` (`itemMenuHuman`, `chooseItemMenuHuman`,
and `psuitReq`'s second call site in `projectItem`), their boundary cases losing
the shim, and the test edits these earn: the four flips, and the witness
the [contract] cases step 4 names need in order to compile. It rewrites lines
this document, the post-mortem's sec. 10 and the wasm plan's Appendix C cite.

**Split** --- two commits. (1) the conversion with its flips, titled as sec. 02
drafts. (2) the citation-repair commit "Running this plan" owes after every code
step: `cite` over this document and the post-mortem, the printed snippets
re-read and the ranges re-cut, then `--restamp` for both; what the pass finds
in the wasm plan's frozen Appendix C is recorded in the Log and left there. (2)
carries the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/InventoryM.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanGlobalM.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs` (its boundary cases;
the claimant list is at C2), `test/HandleHumanLocalMUnitTests.hs`,
`test/InventoryMUnitTests.hs`, `docs/leader-desync-bug.md` (secs. 10--11's
citations, in the repair commit only), and this document. **The claimant list
for `HandleHumanLocalM.hs` lives here**: PR 0, C3, C4 and C6 on this side, one
holder at a time in that order, and the wasm plan's R3
and its capability-constants practice, both citing `HandleHumanLocalM.hs:815`,
which PR 0, C3 and C4 edit above and so slide, C6's tail alone reaching below
it. **So does `InventoryM.hs`'s**: C3 and C4. **And `HandleHumanGlobalM.hs`'s**:
C3, C5 and C6. **And the post-mortem's**: PR 0 and 04.4 for the outcome lines
it reserves, C2, C3, C4, C5, C6 and C7 for the citation repairs and restamps
of its live secs. 10--11. PR 0 also writes `test/HandleHumanLocalMUnitTests.hs`;
`test/InventoryMUnitTests.hs` has one writer until 04.4's deletion commit.

**Done** --- `native`, `contract` unchanged at 28, `LR-flip` at 11 with four
flipped --- the `psuitReq` verdict pin, the fling dialog, `getFull`, and PR 0's
apply pin --- each verified first against the candidate as step 4 spells out,
`census` read for its counts, `cite` over this document, the post-mortem
and the wasm plan with the printed snippets re-read rather than the exit status,
`docs`, and --- read rather than run --- sec. 03's transitive callee walk
over the read-live rows this item converts whose wait column reads `---`.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-07-30 (the Log): the apply closure has
its *actor* read live inside `psuit` and its call stays put, and `psuitReq`
reads at the top, its callers being the closure cases (sec. 03).

### C4 --- the cycle pair, and the assertion (step 3, with its flips from step 4)

`HandleHelperM`'s `pointmanCycle` and `pointmanCycleLevel` with their two
`*Human` wrappers in `HandleHumanLocalM`, the dialog's own calls
at `InventoryM.hs:398` and `:431`, the assertion `4a6eca154` disabled re-enabled
beside them, those boundary cases losing the shim, and the LR call sites:
LR3--LR6 obtain a witness through `mintHasPointman`, LR6 is deleted, and X1/X2's
cycling outcome flips. Know what re-enabling switches on, per step 3: both
assertions are live in every build from here on.

**Split** --- two commits, (1) the conversion with its flips and (2)
the citation repair, exactly as C3's; (2) carries the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/HandleHelperM.hs`,
`engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs` (the two wrappers;
the claimant list is at C3),
`engine-src/Game/LambdaHack/Client/UI/InventoryM.hs` (the two call sites; list
at C3), `engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs` (list at C2),
`test/HandleHelperMUnitTests.hs`, `test/FrameMUnitTests.hs` (X1 and X2's cycling
outcome; list at 04.1), `docs/leader-desync-bug.md` in the repair commit (list
at C3), and this document. **The claimant list for `HandleHelperM.hs` lives
here**: C4 and C6. `test/HandleHelperMUnitTests.hs` has one writer until 04.4's
deletion commit.

**Done** --- `native`, `contract` unchanged at 28, `LR-flip` at 10 with LR3,
LR4, X1 and X2 flipped and LR5 changed in shape, LR6 deleted, so the flip count
falls by one (11 -> 10) and the suite to 156 --- say so in the commit,
an unexplained count drop being what sec. 00 tells a reader to treat
as a finding; `census` read for its counts; `cite` over this document,
the post-mortem and the wasm plan, its printed snippets re-read rather
than its exit status; `docs`; and, read rather than run, sec. 03's transitive
callee walk over the read-live rows this item converts whose wait column reads
`---`.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-08-07: LR6 is deleted rather
than flipped, confirmed unrepresentable by the spike (the Log).

### C5 --- the waiting three (step 5, with alterDir's flip from step 4)

`pickPoint` with `alterDirHuman` and `closeDirHuman`, the first of step 5's two
halves: `pickPoint`'s read sits between its `getConfirms` and its last use,
the two `*DirHuman` hold across that wait, and `closeDirHuman` keeps its top
read (sec. 03's table). It flips `alterDir`; their boundary cases lose the shim.

**Split** --- two commits, (1) the conversion with its flip and (2) the citation
repair, exactly as C3's; (2) carries the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/HandleHumanGlobalM.hs` (list
at C3), `engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs` (list at C2),
`test/HandleHumanGlobalMUnitTests.hs`, `docs/leader-desync-bug.md` in the repair
commit (list at C3), and this document. **The claimant list
for `test/HandleHumanGlobalMUnitTests.hs` lives here**: C5 for the `alterDir`
flip, then C6 for the `projectHuman` pair's witness.

**Done** --- `native`, `contract` unmoved, `LR-flip` unmoved at 10
with `alterDir` on its flipped value, verified first against the candidate
as step 4 spells out; `cite` as C3's; `docs`; and, read rather than run, sec.
03's transitive callee walk over the read-live rows this item converts whose
wait column reads `---`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### C6 --- the tail (step 5's remainder)

The remainder of sec. 03's read-live set after C3 to C5 ---
`pickLeaderWithPointer` with its wrapper, `projectHuman`, `applyHuman`,
`moveItemHuman`, `moveOrSelectItem` (recomputing `calmE`, `overStash`
and `stores` below each wait and carrying the chosen bag entry down
with the read, but leaving `eqpFree` where it is --- sec. 03 says why)
and `selectItemsToMove` --- then the fifteen convert-half of sec. 03's tail
with the sixteen boundary cases dispatching them, and the shim retired
from `addLeader`/`weaveLeader`. It flips nothing and ends on the invariant read
off `cmdSemanticsLeader` alone: no case passes an `ActorId` and no shim
survives. The three pinned-site notes drafted after sec. 02 go in here ---
at `projectItem`, `meleeAid` and `processTileActions`, all three in this item's
`HandleHumanGlobalM` --- with `projectItem` receiving its witness
from `projectHuman` in the same commit; the reason they point at is the note C1
has already put at `getLeaderUI`. Three is what sec. 10.6's rule produced
at the last reading of the tree rather than a closed list: any further site
the callee walk in **Done** turns up takes the same tiny note, pointing
at the same reason.

**Split** --- two commits, (1) the conversion and (2) the citation repair,
exactly as C3's; (2) carries the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/HandleHelperM.hs` (list
at C4), `engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs` (list
at C3), `engine-src/Game/LambdaHack/Client/UI/HandleHumanGlobalM.hs` (list
at C3), `engine-src/Game/LambdaHack/Client/UI/HandleHumanM.hs` (list at C2),
`test/HandleHumanGlobalMUnitTests.hs` (list at C5), `docs/leader-desync-bug.md`
in the repair commit (list at C3), and this document.

**Done** --- `native`, `contract` and `LR-flip` both unmoved, `census` read
a last time, `cite` as C3's, `docs`; the invariant above, read rather than run;
and, read likewise, the transitive callee walk sec. 03 owes: over both halves
of its tail --- the eighteen it keeps and the fifteen this item converts ---
and over every read-live row whose wait column reads `---` that C3 to C5 did
not already walk, reported site by site rather than as a verdict, since
it is the only step that looks past a function's own body.

**Hands back** --- nothing.

**Decide first** --- nothing: the eighteen keep-param entries and the "some
actor" parameters stay, the post-mortem's sec. 10 ruling on each ("Never",
above).

### C7 --- verification (step 6)

Nothing to write but the ledger and the Log; step 6 says what it runs and what
stays a human's, and the sec. 04 window is what no run has yet reached.

**Owns** --- this document, and `docs/leader-desync-bug.md` for the `--restamp`
its **Done** ends on (list at C3).

**Done** --- `native` at 156 tests, `contract` at 28, `LR-flip` at 10,
`playtests`, `cite` over this document, the post-mortem and the wasm plan a last
time with `--restamp` for the first two after the reading, and `docs`
with the reading pass run whole.

**Hands back** --- *judgement*: the timeline session of the post-mortem's sec.
04 in the real frontend --- a recorded macro driving a go-to-xhair run,
then `C-Tab` inside the item menu, the post-mortem's sec. 04 callout having both
mechanisms --- a fling-dialog switch and an apply-dialog switch,
and `make frontendCrawl` for a visual pass over menus, played by hand
and so reported unrun rather than green, which is what keeps this row
`not applied` after a green **Done**. The substitute gates in **Done**: X1
drives the window through the real `promptGetKey`, and the `LR-flip` series
flipped whole pins the two dialog siblings, the apply one through PR 0's pin.

**Decide first** --- nothing.

### Log

One line per surprise or re-plan, newest last, so that resuming needs
this section rather than a re-read. Log-worthy: a step that turned out larger
or smaller than its row says, a design question reopened, a classification here
found wrong, an ordering constraint discovered, a **Decide first** ruled ---
and a count found wrong only where the correction carries a lesson, since
a superseded number forecloses nothing and the entries here are frozen the day
they are dated. That binds entries written from now on; the ones above
are records and stay as they are. Not log-worthy: doing a step as written,
or editing this file before the work starts --- an entry that records only
that the plan was written is one the next reader has to skip.

- 2026-07-29 -- plan split out of the two records; the battery is on master;
  nothing of sec. 02 or sec. 04 started. The open design question is the one
  sec. 02.0 exists to answer.
- 2026-07-30 -- verification pass found two classification errors in sec. 03,
  both leaving the read-live set at 28 and the 13/15 witness split intact,
  by coincidence rather than by cancellation. A third blind spot
  in the extraction rule --- a `leader` bound later in the head --- had silently
  dropped `runDefSkills` and `runDefInventory`, which hold the entry leader
  in a right-pane callback the menu loop re-invokes; they are now
  in the read-live table, and sec. 02 step 2 converts them with the dialog
  chain. And `projectItem`/`meleeAid` were listed read-live against
  the post-mortem's sec. 10.6, which rules them pinned; they move to Keep. Read
  the pairing as the standing warning it is: an inventory built from a grep
  proxy inherits the proxy's blind spots, and a bucket assigned from the "holds
  it across a wait" test inherits nothing about whether the interaction chooses
  or confirms.
- 2026-07-30 -- sec. 03's census is now `tools/leader-census.py`, which derives
  the surface from the tree and checks it against the buckets both ways;
  the hand rule's 64 becomes a census of 72, and the five DrawM and `msgAddDone`
  entries the old rule dropped get an explicit Keep ruling. Mapping sec. 04 step
  1's pure cases onto the abort-split checklist turned up a hole that is
  not a documentation one: **the special-event branch (`dm /= ColorFull`)
  is entered by no AS case**, every one of them reading with `ColorFull` ---
  and it is the block the split relocates, so step 1 now requires the missing
  case before the extraction. sec. 11's menu-navigation microbenchmark is ruled
  out rather than left open.
- 2026-07-30 -- a multi-angle review of all three documents. Bookkeeping
  it corrected here: step 0's gate was unmeetable, since converting
  `pointmanCycleLevel` breaks five test call sites and LR3/LR4 then go red ---
  the spike working, not failing; step 5 had dropped the read-live remainder
  sec. 00's own row assigns it, and double-counted the boundary cases against
  step 2; sec. 04 step 1's bullet accounting named one checklist bullet twice
  and dropped another, and the decision table's caption claimed a bijection
  it never had; the drafted haddock filed `recordHistory` and the common cleanup
  as unconditional, though the voicing arm returns before both, and omitted
  `spointer`; the tail's base is 65, not 64; and `make test-gha` cannot witness
  a surviving desync, being playtests only. The finding that is not bookkeeping:
  **the apply dialog is a second closure case, and a third live sibling
  of the post-mortem's sec. 09** --- the placement rule as stated would leave
  it standing, its call being inside the closure already, so it is the *actor*
  that must go live rather than the call that must move. It is now in sec. 03
  beside `psuitReq`, step 2 converts it and step 6 checks it by hand, no test
  entering that dialog at all. Two counts that looked wrong and were not: the 29
  boundary cases (14/12/3) and the census's 72 both re-derive exactly.
- 2026-07-30 -- prepared for execution rather than for reading, which moved two
  things. The two AS cases sec. 04.1 asked for characterize the *unsplit*
  primitive and depend on neither design, so they land first, in a coverage PR
  together with the apply-dialog pin, and sec. 04.1 inherits them green.
  And `tools/leader-census.py` turns out to be a before-check only: its rule
  is "binds a parameter named `leader...`", so every converted function leaves
  the 72 and stops being checked in either direction --- the run stays green
  while certifying less and less, which is worse than going red, and the printed
  counts are the only signal it keeps.
- 2026-07-30 -- sec. 03's table now records a per-function pass --- every
  read-live body read, then re-read independently --- which moved three things
  and found a fourth. `psuitReq` is not a closure case: its body waits nowhere,
  so it reads at the top like anything else and the capture belongs to its call
  sites, making the four `chooseItemProjectHuman`, `chooseItemApplyHuman`,
  `runDefSkills` and `runDefInventory`, with `transition`'s three `defAction`s
  and `chooseItemDialogMode`'s `renderOneItem` beside them. Nine
  of the twenty-eight need no identity at all, where the count here said four.
  `pickNumber` is a wait, so the move family spans two. And the class no section
  had named: a value *derived* from the identity before a wait is exactly
  as stale as the identity, and live-read alone fixes none of them ---
  `getFull`'s bag, whose lookup is `EM.!`, so the failure mode is a partial map
  rather than incoherence; `moveOrSelectItem`'s `calmE`/`overStash`/`eqpFree`;
  `itemMenuHuman`'s body. The post-mortem's sec. 10 placement rule now carries
  the general form. One thing left open deliberately: whether a mid-dialog
  switch can really reach `getFull`'s missing key is untested, and is worth
  settling before the conversion rather than after.
- 2026-07-30 -- settled: it can. A scripted Tab and Return through the real
  dialog make `getFull` return C's item with a quantity looked up in A's bag,
  and forcing it dies with `IntMap.!: key 117 is not an element of the map`.
  So the derived-value class has a crash in it, not only incoherence,
  and the post-mortem's sec. 09 gains sibling (d) --- pinned now,
  in `test/InventoryMUnitTests.hs`, non-vacuity proved both ways. What kept
  it out of sight: the two single-item callers drop the quantity unforced,
  and the move family's single-item path already guards this exact case
  with `EM.lookup` and a comment naming it, so the author had met the class
  at one site and defended it there. In the same pass
  the `permittedProjectClient` pin became `[contract]`: it had nothing to flip,
  and now guards sec. 03's Keep ruling instead by switching the pointman between
  two rounds of calls and asserting that neither verdict moves. The battery
  is 154 tests, 26 contract and 10 flip; the flip set changed membership without
  changing size.
- 2026-07-30 -- one question the crash raises is recorded rather than answered,
  in sec. 01: whether sibling (d) being a *crash* rather than incoherence
  reorders the three PRs. Both directions are argued there. Log-worthy because
  it is a design question reopened by a finding, and because an unanswered
  question that lives only in a chat is the thing this section exists
  to prevent.
- 2026-07-31 -- a field-by-field comparison against
  `docs/wasm-frontend-unified-plan.md`, which moved things both ways. Three
  classifications here were wrong. sec. 02.1's "the only such file" asked
  whether the cited function converts, where what slides a citation is a changed
  line count *above* it --- so `HandleHumanLocalM.hs` joins, and the cell's own
  second-file trigger has fired, which is a mechanism question reopened rather
  than bookkeeping. Step 6's reason for keeping the visual pass a human's
  stopped being true when xvfb arrived, though the conclusion survives
  on another. And the deletion ritual's count of that plan's mentions was three
  and is four. Added, from the same comparison: the citation pass and a stylish
  verdict to sec. 00's gates, which had neither.
- 2026-07-31 -- that branch was then reviewed blind, and it had falsified
  a count of its own: adding `MonadClientUI.hs:455` and `:469` to the wasm plan
  took that plan from seven citations into the file to nine, and sec. 02.1 went
  on saying seven --- in the very commit that chased the analogous count
  from three to four, three lines away. Two more came with it: the deletion
  ritual's own grep lists nine files, not eight, and `test/UnitTestHelpers.hs`
  is a third file both campaigns write. The lesson is worth having paid for:
  fixing one instance of a count is not fixing the count.
- 2026-08-07 -- the sec. 02.0 spike ran, met its acceptance to the letter
  and is parked on `spike-pointman-witness` (`af673d1f4`). Five things
  it settled that the steps below inherit. **The insertion point is a choice,
  and the right one deletes sec. 02.1's hazard**: appending the ~30 lines
  *below* `MonadClientUI.hs:469` rather than inserting above `:166` slides none
  of the wasm plan's nine citations, so that row's "a green run
  is not sufficient" acceptance applies only if step 1 inserts high, and step 1
  should not. **The two boundary helpers need a new constraint**: `addLeader`
  and `weaveLeader` were `Monad m` and the witness makes them `MonadClientUI m`,
  which is what lets the unconverted handlers keep working through one shim
  in two places rather than 26. **`HandleHumanM` loses an import**: the boundary
  stops reading `sleader` itself, so `Game.LambdaHack.Client.State` goes
  redundant there and GHC says so. **The test monad has no `MonadFail`**,
  so `Just witness <- mintHasPointman` does not typecheck under `CliMock`
  and every minting test site wants `fromMaybe (error ...) <$> mintHasPointman`
  --- or a helper in `test/UnitTestHelpers.hs`, worth adding once step 4 has ten
  such sites rather than four. And **LR6 confirmed unrepresentable**
  by construction: it was the one call site that could not be given a witness
  and keep its meaning, which is the deletion sec. 02 step 4 already rules.
- 2026-08-07 -- a reachability spike in the real SDL frontend under Xvfb,
  driving the game with `xdotool`, settled step 6's unsettled clause and found
  more than it went looking for. **`A-Tab` does not act inside an item dialog;
  `C-Tab` does** --- the dialog binds its cycle key through `revCmd`
  (`InventoryM.hs:392`), which reads `brevMap`, and that map is built only
  from bindings with non-empty categories, where the `bcmdMap` beside it takes
  every one (`engine-src/Game/LambdaHack/Client/UI/Content/Input.hs:86-93`).
  `A-Tab` has none (`GameDefinition/game-src/Client/UI/Content/Input.hs:60`)
  and `C-Tab` has `CmdMove` (`:62`), so only `C-Tab` is reachable inside
  a dialog, while `A-Tab` still works at top level, through `bcmdMap`. Verified
  twice over, by reading and by pressing. So step 6's session should press
  `C-Tab` inside the menu --- pressing `A-Tab` there looks exactly like a window
  that was never reached. **And the multi-actor run and the injected macro come
  from opposite branches of one `if`**: `moveRunHuman`'s `runMembers`
  is `[leader]` when `runAhead`, the full selection otherwise, and `macroRun25`
  is injected only `when runAhead` (`HandleHumanGlobalM.hs:322-333`) ---
  so shift+direction (`RunDir`, which passes `runAhead = True`,
  `HandleHumanM.hs:111-112`) runs one actor and rotates no pointman, while
  the rotating multi-actor run comes from the go-to-xhair family (`:659`,
  `:671`, `:677`, all passing `False`) and injects no macro. The sec. 04 window
  therefore needs a *recorded* macro driving a go-to-xhair run,
  not shift+direction, which is why an afternoon of the latter never opened it.
  What the spike did *not* reach is the window itself; the machinery is all
  proven --- recording, replay, dialogs, mid-dialog switching, all scriptable
  headlessly --- so what step 6 keeps is the judgement, not the typing. Both
  mechanisms now live in the post-mortem's sec. 04 as a callout, since they
  outlive this file; that callout is the copy to correct if either is ever found
  wrong, and it is what took the record's frozen half its second kind of upkeep.
- 2026-08-07 -- two probes for PR 0, both answering yes with a caveat.
  `permittedApply` returns *distinct* verdicts per apply skill through the stub
  content --- `ApplyNoEffects`, `ApplyFood`, `ApplyUnskilled` for skills 2, 1
  and 0 --- so sibling (c)'s pin can be written the way the `psuitReq` one was,
  on per-actor failure verdicts, and needs no walkable board, which sec. 05's
  "cheapest cover is the same board" sentence assumed it would. The caveat
  is an export: `permittedApplyClient` sits inside `HandleHumanLocalM`'s
  `EXPOSE_INTERNAL` block, so PR 0 must first move it to the "Operations both
  internal and used in unit tests" group where `permittedProjectClient` already
  sits. And the special-event AS case is clear on the fixture that would have
  silenced it: `partyFaction` sets `gunderAI = False`
  (`UnitTestHelpers.hs:467`), so the branch's `unless (gunderAI fact)` lets
  the effect fire --- but `testFaction`, which `stubCliState` uses, sets
  it `True` (`:350`), so that case must be written on a party fixture.
  The recording `ChanFrontend` remains the one thing the harness lacks.
- 2026-08-07 -- PR 1 is repartitioned from two commits into six, each green,
  after the spike showed the boundary's field type can change while every
  handler still takes an `ActorId`. Log-worthy twice over: it is a re-plan,
  and it retires an argument this document made twice --- that no split finer
  than the lump stays green --- which was true of the steps as numbered
  and false of the work. What had been missed is that the forcing came from one
  narrow place, the dialog entry points being `CmdLeader` cases, and that a shim
  in two helpers dissolves it. The counts were re-measured in the same pass
  and are unmoved at 154, 26 and 10, and `tools/leader-census.py` runs green,
  so the inventory is exact as sec. 02 finds it.
- 2026-08-07 -- every question this document held for the author is ruled, each
  recorded where it stood: sibling (d) does not reorder the PRs (sec. 01); PR 0
  appends the two outcome lines it resolves (the header ritual); its owed
  AS cases are new tests, AS14 and AS15, the assertion-in-AS4 branch refuted
  by its fixture's empty macro stack (sec. 01); the unrecorded-experiment claims
  stay flagged, each redone by the step that leans on it (sec. 00);
  the shared-file hazard stays hand-warned on both sides (sec. 02.1's row);
  and at deletion the census tool goes, sec. 10.3's sentence recast in the same
  commit (the header). One count found wrong in the same pass: sec. 02's drafted
  `CHANGELOG.md` line said three item dialog siblings, written before (d) made
  them four. Nothing outside this file moves today --- the records' edits
  are the scheduled ones, PR 0's two outcome lines and the deletion commit's
  recast.
- 2026-09-04 -- a blind review of this branch, three readers over the pinned
  documents with the mechanical passes run first, found six things here and one
  that reaches the records. The shim count was wrong: 17 of the 29 leader cases
  reach the two helpers, not 26 --- 14 `weaveLeader` and 3 `addLeader` against
  12 direct `CmdLeader` cases in `cmdSemanticsLeader` --- which the C2 row had
  right all along, so the repartition's justifying sentence contradicted
  the table it justified. The deletion ritual's census is ten files, not nine,
  `tools/checks.py:52` being the tenth; and two of its `tools/*.py` citations
  had slid when those tools were synced with horde-ad,
  `check-doc-examples.py:56-57` to `:78-86` and `check-plan-citations.py:71`
  to `:117` --- both resolving, both naming lines that do not carry the name,
  which is the failure this document's own gate sentence warns of, landing
  inside the bullet that added it. sec. 00 and sec. 02 both called steps 2 to 5
  six commits where C1 is step 1 and the six span steps 1 to 5. The citation
  pass the gates paragraph owes after every code step was named in no row's
  acceptance cell, so at C3 to C6 --- the commits that slide the citations ---
  nothing fired; it is in those cells now. The row column was headed `commit`
  while C0 is a parked spike and C7 a gate. And sec. 04 still sent the executor
  to `InputDecision` conditionally, after the wasm plan's own 0.1 ruling, made
  on this branch, closed that home as an import cycle: both this document
  and the abort-split record now name `FrameM`'s pure section outright,
  and the abort-split is gated on the live-read design alone.
- 2026-09-04 -- the ledger rewritten into the execution-block grammar
  of `docs/wasm-frontend-unified-plan.md`, which the two campaigns had shared
  in lifecycle machinery and not in how an item is handed out: a `touches` cell
  named modules and a "check when done" cell named gates by description,
  so a session executing a row had no exhaustive lock set, no named gate
  and no statement of what to hand back, and the checker that reads that plan
  against itself exited 2 on this document. Every row is an item now,
  with **Owns**, **Done**, **Hands back** and **Decide first**
  and a `depends on` cell, and the states are the shared vocabulary. Three
  things moved with it. PR 0 has a row, having been the one unstarted piece
  of work without one --- the two outcome lines it appends to the records had
  no **Owns** to sit in. The `C5, C6` row is two rows, C6 waiting on C5.
  And the deletion ritual of the head callout is 04.4's second commit,
  so that its files have an **Owns**. `tools/check-plan-crossrefs.py` reads
  this document now, alone and jointly with the wasm plan; the joint run is what
  sees a file both campaigns write, and its first run was loud
  on `MonadClientUI.hs`, `HandleHumanLocalM.hs`, `UnitTestHelpers.hs`
  and `CLAUDE.md`, every finding on that plan's side, its items not yet naming
  this document's by id --- fixed there, in a commit of its own.

## 01 -- Sequencing: two designs, one campaign

Live-read first and in full; the abort-split strictly after it and assuming it.
The abort-split record's own normative callout says why: against
the pre-live-read tree the leader restore still invalidates threaded copies,
so naming it would decorate the bug rather than remove it.

The test battery (sec. 05) is already in the tree, on master, and stays green
throughout. It is the safety net the conversion runs on, not an output of it:
`-p "/contract/"` must pass at every step, and `-p "/LR-flip/"` is the set
that flips once, with the engine change that earns it.

**It ships as three pull requests, not one and not many.** One would mix
a behavioural fix with a hygiene refactor and earn a single CI verdict for three
independent risks; more than three would cut across the three designs rather
than along them, since each PR is one design's worth of risk and there
is no fourth. That is the argument, and it is about the *PRs*: inside PR 1
the commits are six and each is green, the 2026-08-07 repartition sec. 00
records.

| PR | contents | why it stands alone |
|---|---|---|
| 0 -- coverage | the two AS cases sec. 04.1 asks for, plus a characterization of sec. 09's sibling (c) in the apply dialog | all three pin *today's* behaviour, land green, and are the safety net the next PR runs on --- which is what the battery of sec. 05 already did, landing ahead of the fix. Authoring them parallelizes; nothing here touches the engine |
| 1 -- live-read | sec. 02 steps 1--5, as sec. 00's C1 to C6 | every flip, the new apply one included, lands in the same commit as the conversion that earns it |
| 2 -- abort-split | sec. 04 steps 1--3 | a different design, strictly after, with its own `CHANGELOG.md` line |

Pushing any of them, and opening any of them, needs the author's explicit
go-ahead each time; the campaign ends at "branch with commits", never
at "pushed".

> **Ruled 2026-08-07: sibling (d) does not change this order.** The table
> was written while (d) was a hazard on paper; it is now a proven crash ---
> a partial-map failure in `getFull`, not the quiet incoherence the rest
> of the family produces. The case for reordering --- a crash a player can reach
> argues for landing live-read first, since PR 0 fixes nothing and PR 1 is what
> closes it --- lost on three grounds: (d)'s promotion changed the campaign's
> evidence, not any PR's contents, the crash being pinned by a test already; PR
> 0 is what makes PR 1 checkable, sibling (c) has no test at all,
> and the campaign has already been wrong twice in ways only a test caught ---
> reordering saves little and spends the safety net exactly where this work has
> needed it; and forcing the bad thunk needs more than one item selected,
> or the ground store, since every other path drops it unforced or guards
> it (sec. 03), which is why the years this code has stood have produced
> no report. The table above stands.

> **Ruled 2026-08-07: PR 0's two owed AS cases are new tests**, the series
> becoming AS1--AS15 and the counts moving as sec. 00 states. One of the two
> strengthens the no-`resetPlayBack` invariant that AS4 already *enters*
> and neither AS4 nor AS7 observes (`docs/promptgetkey-hygiene.md`,
> its checklist), so an assertion inside AS4 was the live alternative ---
> refuted by the fixture: AS4 runs on a macro stack that is empty already,
> so an unchanged-stack assertion there passes vacuously, and observing
> the invariant takes a non-empty stack behind an empty pending frame ---
> a different setup, hence a case of its own. That also leaves AS4, a [contract]
> case, unedited ahead of the refactor it guards, and matches the series'
> one-invariant-per-case shape (the interrupt inputs of AS5/AS6/AS9, the guards
> of AS11--AS13).

Every commit below leaves the tree buildable, green and shippable, so there
is no rollback procedure to write beyond reverting it, with no exception.
The commits that *look* irreversible are the ones flipping characterizations, C3
and C4, and each reverts together with the engine change it accompanies, which
is the point of pairing them.

## 02 -- Live-read: migration order and verification

Steps 1--5 are one logical change in six commits, partitioned so
that a characterization always lands with the code it characterizes --- never
apart, or the suite is red in between and the flip loses its evidence.
The numbered steps below are the *work*; sec. 00's `C`-rows are the commits.
They do not stand one to one --- step 4's ten flips are earned by conversions
spread across steps 2, 3 and 5, and the commits follow the earning rather
than the numbering --- which is why the rows are called `C`N rather
than `sec. 02.N`: a row and a step of the same number would be different things,
and this document is executed by a session that reads a number literally. Read
a step for what to do and sec. 00 for what to commit together. Step 1 lands
first, green on its own, the accessors having no callers yet; the boundary
follows it, green because the shim leaves every handler as it was.
The abort-split (sec. 04) is a separate change on top; the test battery
(sec. 05) is a separate one below, and has already landed.

Blast radius: six modules. `MonadClientUI` gains the accessors and the witness,
`HandleHumanM` the boundary, and the four modules sec. 03 lists supply
the handlers --- `HandleHelperM`, `InventoryM`, `HandleHumanLocalM`,
`HandleHumanGlobalM`. Nothing outside the UI client changes: not the AI client,
not the server, not the frontends, and no type crossing the client-server
boundary (the post-mortem's sec. 10). `promptGetKey` keeps its type,
so `Client/UI.hs` and `SlideshowM` merely recompile. The work is type-directed
--- change a signature, follow the errors, apply the partition to each site
the compiler names --- which is the backstop over sec. 03's inventory:
its *surface* is a census, kept exact in both directions
by `tools/leader-census.py`, but bucket membership is not bucket correctness,
and that is what the compiler settles site by site.

**Step 0: spike one path before converting twenty-eight.** Half a day,
and the only step whose outcome can still change the design. Add `HasPointman`,
`mintHasPointman` and `getLeaderUI` to `MonadClientUI`, then convert a single
path end to end. `PointmanCycleLevel` is the one to pick: the battery already
pins it on both sides of the flip (LR1/LR2 as contract, LR3
as the characterization), and it is three frames deep ---
`pointmanCycleLevelHuman`, `pointmanCycleLevel`, and the dialog's own call
at `InventoryM.hs:398` --- so it exercises the boundary, an intermediate
and a caller that holds the value across a wait, which is the whole shape
of the change in miniature. What it settles: whether the token reads tolerably
at a real call site --- the one question the post-mortem's sec. 10 leaves open,
since it defers the capability monad to "if witness threading proves too noisy"
--- and whether the boundary sketch survives contact with `weaveLeader`'s
point-free reality. What would change the plan: noise at three sites will
be noise at twenty-nine, and the witness-free variant (the post-mortem's sec.
10, considered and passed over) becomes the live choice *before* the remaining
functions are touched, rather than a regret afterwards. The spike either reverts
or becomes the beginning of step 1.

1. **Add the witness and accessors** to `MonadClientUI`: the abstract
   `HasPointman`, the checking `mintHasPointman`, `getLeaderUI` (witness
   required) and the `Maybe` variant, which the post-mortem's own count leaves
   with future-proofing as its whole role, no converted function needing it.
2. **Convert the dialog chain first**, taking each function's placement
   from sec. 03's table rather than from this sentence, which only gives
   the order: InventoryM's `transition`, `getItem`/`getFull`/`getGroupItem`/
   `getStoreItem` and `runDefSkills`/`runDefInventory`; then `itemMenuHuman`,
   `chooseItemMenuHuman`, `chooseItemDialogMode` and the `chooseItem*Human`
   wrappers; and `psuitReq` itself, which loses its `ActorId` here rather
   than in step 5 --- the compiler will not ask for it, converting its callers
   not being enough. Four of them are closure cases, where the read belongs
   inside the callback the menu loop re-invokes --- `chooseItemProjectHuman`,
   which gains a `psuitReq` call inside `psuit` and *keeps* its entry call
   at `HandleHumanLocalM.hs:367`, that one feeding the invalid-aim failure
   at `:370` and the `sitemSel` fast path at `:381`, neither of which can live
   inside the closure; `chooseItemApplyHuman`, where the *actor* moves inside
   a `psuit` that is already in the right place, and which no test would catch;
   and the `runDef*` pair's right-pane callbacks. Two more carry a derived value
   down with the read rather than the read alone --- `getFull`'s bag
   and `itemMenuHuman`'s body --- as does `moveOrSelectItem` when step 5 reaches
   it. Delete the two manual re-reads --- `recCall`'s
   and the post-`getStoreItem` one --- as each becomes dead; the `7e74698af`
   test permissiveness goes with them, so there is no third site to hunt
   for (the post-mortem's sec. 10 says why).
3. **Re-enable the disabled "same leader" assertion** --- the one `4a6eca154`
   commented out in `pointmanCycleLevel` (`HandleHelperM.hs:129-130`); its twin
   in `pointmanCycle` (`HandleHelperM.hs:149-150`) is live already, which
   is what LR5 catches. Both then become theorems about a single variable rather
   than hopes about two. Know what re-enabling switches on: both are plain
   `assert`s under no `WITH_EXPENSIVE_ASSERTIONS` guard,
   and `-fno-ignore-asserts` sits unconditionally in the cabal `options` stanza
   (`LambdaHack.cabal:155`), so from this step onwards they are live in *every*
   build --- the CI playtests, which run with expensive assertions off,
   and release binaries alike, which is how the original crash reached
   a release-binary player at all (the post-mortem's sec. 06). Hence
   the ordering: re-enable after the conversion, never before, and expect
   a surviving desync to crash rather than to no-op quietly --- in the unit
   suite and in step 6's manual session, which is where the dialogs are driven.
   Not in `make test-gha`: it is `test` plus four playtests
   (`Makefile:146-148`), every one of them `--automateAll`, so no human command
   is dispatched and no dialog opened, and a green run there says nothing about
   this assertion either way.
4. **Flip the whole [LR-flip] series**, verifying each flip by temporarily
   applying the candidate fix first, as `test/CLAUDE.md` requires.
   That verification is a loop per test, and its order is the whole point: run
   the test unmodified and record the value it pins; apply the candidate ---
   the one-liner of the post-mortem's sec. 08, or for a closure pin the live
   read inside the closure itself; run the same test and record the new value;
   revert. Only then edit the expectation, to the value the run produced rather
   than the value the comment predicts. The set is exactly what
   `cabal test --test-options='-p "/LR-flip/"'` runs --- ten tests, eleven once
   PR 0's apply-dialog pin lands --- and each states its target value inline
   (the set's membership changed on 2026-07-30 without its size doing so, which
   is why the count is worth re-reading rather than trusting):

   - LR3--LR6 in `test/HandleHelperMUnitTests.hs`, the post-mortem's sec. 07
     reproducer among them (already verified to pass under the live-read fix);
     LR5 changes *shape* rather than value, since it catches a live assertion
     via `Control.Exception.try` and after the fix the assertion no longer
     fires;
   - the final cycling outcome of the bridge tests X1 and X2
     (`test/FrameMUnitTests.hs`); their `promptGetKey` half is [contract]
     and must not move;
   - the `psuitReq` verdict pin in `test/HandleHumanLocalMUnitTests.hs`, whose
     two per-actor calls become one call before and one after a pointman switch,
     `psuitReq` having lost the argument that told them apart. Its former
     companion, the `permittedProjectClient` pin, is not in this set any more:
     it became a `[contract]` test on 2026-07-30, having nothing to flip;
   - the two end-to-end tests of sec. 05 (the fling dialog, `alterDir`), each
     to the value recorded in its comment;
   - the `getFull` quantity pin in `test/InventoryMUnitTests.hs`, sibling (d),
     where the flip is not a changed value but a vanished failure: once the bag
     moves down with the read, forcing the quantity returns C's own `(1, [])`
     and there is nothing left to catch;
   - and, once PR 0 has landed it, the apply-dialog pin of sibling (c) ---
     the eleventh member, and the one this list would otherwise leave unflipped
     while claiming to flip the whole series.

   Two rulings the flip needs, neither of which is an expected value. **LR6
   is deleted, not flipped**: it pins that a dangling stale `ActorId` yields
   an arbitrary pick, and after the conversion there is no argument to dangle,
   so there is nothing to edit it to. Its going takes the flip series 11 -> 10
   and the suite 157 -> 156, the second of the three movements sec. 00's count
   sequence permits; say so in the commit, since an unexplained count drop
   is exactly what sec. 00 tells a reader to treat as a finding.
   **And a [contract] test that needs a signature edit is not a contract test
   that moved**: `getFull`'s three plain cases
   (`test/InventoryMUnitTests.hs:29`, `:39`, `:49`), the two `chooseItemHuman`
   contracts (`test/HandleHumanLocalMUnitTests.hs:324`, `:351`)
   and the `projectHuman` pair inside the sibling-(b) contract
   (`test/HandleHumanGlobalMUnitTests.hs:71`, `:73`) all pass an `ActorId`
   that the conversion removes. Giving them a witness is mechanical
   and proceeds; sec. 00's stop-and-ask means an *outcome* that will not come
   back green, not a call site that will not compile.

   Mechanical fallout of the same step, and why it is a restructure rather
   than an edit of expected values: the LR series calls the converted functions
   directly, so LR3--LR6 must obtain a witness (`mintHasPointman`, whose export
   exists for exactly this) before they can call
   `pointmanCycle`/`pointmanCycleLevel` at all, and `psuitReq` losing
   its `ActorId` updates its four branch tests. The stub fixtures deliberately
   leave `sleader` unset --- `partyCliState`'s own haddock says so, and LR13
   pins the boundary's refusal for exactly that state --- so a test that needs
   a pointman designates one with `updateClientLeader`. Audit every test
   that today calls dialog code with none designated, rather than assuming
   the post-mortem's sec. 10 prediction that the expectations are unaffected.
   Nothing in the [contract] series may move; that is what makes it a contract.
5. **Sweep what step 2 left**, in two halves, because only one of them
   is mechanical. (a) The read-live functions outside the dialog chain:
   the `pointmanCycle`/`pointmanCycleLevel`/`pickLeaderWithPointer` trio
   with their three `*Human` wrappers, `projectHuman`, `applyHuman`,
   `alterDirHuman`, `closeDirHuman`, `pickPoint`, `moveItemHuman`,
   `moveOrSelectItem` and `selectItemsToMove` --- judgment calls, every one
   ruled on by the post-mortem's sec. 10 but none of them by position alone:
   `pickPoint`'s read must sit between its `getConfirms` and its last use,
   `alterDirHuman`/`closeDirHuman` hold across that wait, and the four
   with no wait of their own become pure witness-passing. (b) The mechanical
   half: the fifteen convert-half of sec. 03's tail and the sixteen boundary
   cases that dispatch them, `MoveDir` and `RunDir` sharing `moveRunHuman`.
   The other thirteen of the 29 cases move with their handlers rather than here
   --- five in step 2, eight with (a) above --- which is why the boundary
   is swept in three places and checked in one. Leave the "some actor"
   parameters and sec. 03's other eighteen alone --- the post-mortem's sec. 10
   rules on both, and there the ruling really does decide each site. The step
   ends on an invariant worth checking by reading `cmdSemanticsLeader` alone:
   no case passes an `ActorId`.
6. **Verification**: the full unit suite (at whatever count sec. 00's sequence
   has reached by then, all green before the change), with `-p "/contract/"`
   kept green at *every* step of the migration rather than only at its end ---
   that series is the safety net the conversion runs on; `hlint .`;
   `make test-short` / `test-medium` playtests (AI-driven --- they exercise
   the client loop, not the dialogs --- and minutes each, so budget for them
   rather than reading one as a hang); a manual session replaying
   the post-mortem's sec. 04 timeline (multi-hero run inside a recorded macro
   that opens the item menu, then `C-Tab` --- not `A-Tab`, which the dialog does
   not bind, and not shift+direction for the run, which rotates no pointman;
   the post-mortem's sec. 04 callout has both mechanisms and outlives this file)
   --- X1 of sec. 05 already drives that window through the real `promptGetKey`,
   so what the session adds is everything the mock supplies instead: a real
   frontend, a macro recorded by actual keypresses rather than a `smacroFrame`
   seeded in the fixture, and the sample game's own bindings and party ---
   evidence that a player can reach the window, not only that a fixture can.
   Plus a pointman switch inside the fling dialog *and* inside the apply dialog,
   to confirm the post-mortem's sec. 09 siblings are gone --- the apply one
   by hand because it is the real frontend and PR 0's pin is not;
   and `make frontendCrawl` for a visual pass over menus. The last three
   are a human's, not a run's: the session and the two switches are played
   by hand, and `frontendCrawl` runs `--automateAll`, so reaching a menu means
   mashing keys to regain control (`UI.hs:92-96`). Keys *can* be pressed here
   --- `xdotool`, per CLAUDE.md's sandboxing notes --- so what stays a human's
   is the judgement and the sequencing, not the typing. The 2026-08-07 spike
   drove all of that headlessly, a mid-dialog pointman switch included, so what
   stays unproven is only the sec. 04 window itself, which no run has yet
   reached. Performance needs no gate --- the post-mortem's sec. 11:
   no benchmark reaches this layer.

### The artifacts sec. 02 asks for, drafted

Two steps above end in text that is already known, so here it is, on the model
of sec. 04's drafts: transcription rather than rediscovery.

**The pinned-site notes**, which step 5 owes the post-mortem's sec. 10.6 ---
it requires a note wherever a parameter is pinned across a wait, "because
an unexplained `ActorId` is what produced this document". The reason is written
once and the sites point at it, per the same `CLAUDE.md` comments rule sec. 10.3
already invokes for the eighteen keep-param sites: a substantial note
that siblings would repeat with only a noun changed belongs at its canonical
occurrence, and only tiny notes are repeated identically. So the reason goes
at `getLeaderUI`, beside the convention sec. 10.3 parks there, and lands
with C1; each pinned site takes one sentence, differing in what it confirms and,
at `processTileActions`, in the value pinned beside the identity. All four
are written out rather than described, since a described comment is one
the session has to invent. Each block below is headed by its site's name, which
is not part of the comment:

```haskell
-- getLeaderUI, under the keep-parameter convention of the post-mortem's
-- sec. 10.3
-- Where a caller pins an @ActorId@ *across* an interactive wait, it says
-- so at the site and points here, the reason being the same at each: such
-- a site confirms an action the player already chose for that actor, and
-- the only thing that can swap the pointman under a yes/no prompt, which
-- offers no switch key, is the macro-abort restore inside @promptGetKey@.
-- Acting for the swapped-in pointman would honour the keystroke and not
-- the intent, so reading @sleader@ live at such a site would be the bug,
-- not the fix. A parameter merely handed to a helper within one atomic
-- step needs no note: nothing can intervene and leave it stale.

-- projectItem
-- Pinned across the confirmation, deliberately: this confirms a fling
-- already chosen for @leader@ (getLeaderUI says why).

-- meleeAid
-- Pinned across the confirmation, deliberately: this confirms an attack
-- already chosen for @leader@ (getLeaderUI says why).

-- processTileActions
-- Pinned across the confirmations in @verifyEscape@ and
-- @verifyToolEffect@, deliberately: both confirm an action already chosen
-- for @leader@ at this tile -- the tool transformation, and leaving the
-- dungeon through the embed (getLeaderUI says why). @sb@ is pinned with
-- the identity, for the same reason and across the same waits.
```

**The `CHANGELOG.md` lines** the header callout asks for on landing. The hack
being replaced has its own line under `v0.11.0.1` ("Hack around a crash when
TABbing during item manipulation") and no issue documents either it or
this (the post-mortem's sec. 06), so neither entry carries a link:

```
- Read the pointman live rather than threading it through the UI, fixing the TAB-during-item-manipulation crash and four item dialog siblings
- Split promptGetKey's interrupted-macro cleanup into a pure decision and a named abort action
```

The second lands with sec. 04, not with this section; both go in together only
if the two changes ship in one release. Which release heading they go
under is the author's and not the session's: this campaign ends at "branch
with commits", and the heading the lines belong under may not exist when they
are written.

**The commit titles**, one per *substantive* commit --- PR 0's two first,
then C1 to C6 of sec. 00, the spike excepted, which either reverts or becomes
step 1 --- so the history reads as the plan does and no step is tempted
to bundle. The campaign makes more commits than are listed here,
and deliberately: every **Split** from C2 on names a citation-repair commit
beside its conversion, and 04.4 names the deletion and the restamp behind it.
Those carry no drafted title, having no step to be named after; each is titled
for what it repairs or records. The bodies are written from what the step
actually did --- that part cannot be drafted in advance --- but the titles below
can, and they fix the commit boundaries:

```
Pin the two promptGetKey branches no test enters          (PR 0)
Pin the apply dialog's stale suitability closure          (PR 0)
Add the pointman witness and the live-read accessors      (C1)
Take the witness at the command boundary                  (C2)
Read the pointman live in the item dialogs                (C3)
Read the pointman live when cycling, and assert it again  (C4)
Read the pointman live after the point-picking wait       (C5)
Stop threading the pointman through the command boundary  (C6)
Extract the macro interrupt decision as a pure function   (sec. 04.1)
Name promptGetKey's interrupted-macro cleanup             (sec. 04.2)
Enumerate promptGetKey's writes in its haddock            (sec. 04.3)
```

## 03 -- The conversion inventory

The surface the post-mortem's sec. 10 partition has to be applied to, listed
rather than estimated, so that resuming this work needs no re-derivation.

**How the list was made** --- by `python3 tools/leader-census.py`, which is also
how it is re-checked. The tool walks `engine-src/Game/LambdaHack/Client/UI`,
finds the 94 top-level functions taking a bare `ActorId` parameter, and reports
the 72 that bind one named `leader...` (`HandleHumanGlobalM` 34,
`HandleHumanLocalM` 20, `InventoryM` 10, `HandleHelperM` 4, `DrawM` 4) ---
then cross-checks them against the buckets below in both directions: a function
the tree holds and no bucket names is a failure, and so is a bucket entry
the tree no longer has.

**It is a before-check, and it quietly stops checking as the work proceeds ---
it does not go red.** Its rule is "binds a parameter named `leader...`",
so every converted function leaves the 72 and the tree->doc direction stops
considering it; the doc->tree direction only asks whether a bucketed *name*
is still in the tree, and a converted function keeps its name. So the run stays
green while covering less and less, and by the end of step 5 it certifies almost
nothing. Read the printed counts, then --- "72 bind a parameter named
`leader...`" falling toward zero is the progress bar, and the exit status
is not. Run it green before step 2, read it for the counts during, run it after
any change to the *buckets*, which is what it is for, and delete it
with this document at the end.

One function it cannot see, and prints for hand-classification instead:
`pickLeaderWithPointerHuman`, point-free and so binding nothing. The rule's
other blind spot is wider and quieter: the 22 functions of `94 - 72` bind a bare
`ActorId` under some other name, so the cross-check reaches them in neither
direction. Seventeen are the `aid`/`source`/`target` families the Keep bullet
names by module; `pickLeader` and `pickLeaderWithPointerHuman` it names
outright; the remaining three --- `skillsOverlay` (`HandleHelperM.hs:368`),
`partActorLeader` (`MonadClientUI.hs:455`) and `partPronounLeader` (`:469`) ---
are hand-classified Keep here, and the first two are instructive:
`skillsOverlay` is in the post-mortem's own Keep row and had been dropped
from this list, while `partActorLeader` already reads `sleader` live to decide
whether to say "you", i.e. it is the design's own idiom, arrived at years
earlier.

The tool exists because two grep proxies stand behind this inventory and each
has failed once, neither failure showing up as a wrong count. The *extraction*
proxy read only the first bound parameter, so `runDefSkills`
and `runDefInventory`, which bind `leader` third, were dropped along
with `msgAddDone` and DrawM's four; its 64 was a floor and said so --- a floor
even against its own rule, which the tool's breakdown puts at 65 first-bound,
the other seven binding `leader` later in the head --- where the tool's 72
is a census. The *classification* proxy --- which bucket a function then belongs
in --- saw a wait only in a function's own body, so `closeDirHuman`, whose wait
sits inside the `pickPoint` it calls, was filed as harmless until re-read.
The tool replaces the first proxy and cannot replace the second: it checks
that every function is in *a* bucket, never that it is in the right one, which
is what its clean run says out loud. The type-checker is still the final word,
this being a type-directed refactor in which the compiler enumerates what
is left.

**What closes the second proxy is a callee walk, and it is transitive rather
than one level deep**: from each bucketed function, follow the callees until
a wait or until the identity's last use, whichever comes first. The tail's
classification is body-local by its own statement, and the read-live rows whose
wait column reads `---` rest on a reading that could have missed a callee's wait
the same way, so the walk is owed over the tail's Keep half, over its Convert
half, and over those rows. It is owed *before* the conversion that acts
on the entry, not after, so each converting item carries its own share
in **Done** --- C3, C4 and C5 for the read-live rows each converts, C6
for the rest of them and for both halves of the tail --- reported site by site
rather than as a verdict. One level is not enough, and the two sites walked
so far are both the argument for the depth and what the walk has returned.
`alterCommon` (`HandleHumanGlobalM.hs:494`) hands its `leader` to `verifyAlters`
(`:549`), whose own body waits nowhere and tail-calls the already-pinned
`processTileActions` (`:1110`), whose inner loop is what reaches `verifyEscape`
and its `displayYesNo`: one level reports clean, while the caller in fact holds
the identity (used at `:555`) and a value derived from it (`spos`, `:502`, used
at `:553`) across that confirmation. And `moveRunHuman`
(`HandleHumanGlobalM.hs:312`), on the Convert list, builds `runParams`
from `leader` (`:325-329`), enters the same chain higher still through
`moveSearchAlter` (`:346`), and writes `srunning` from those pre-wait params
on return (`:332`, through the `initRunning` bound at `:330`). Neither moves
bucket. `alterCommon` is a link of a chain whose bottom is pinned already,
and threading the identity down it is what the Keep-the-parameter ruling asks
--- a re-read at any link would let one multi-step operation act for two actors
--- so the comment the post-mortem's sec. 10.6 requires stays at the pinned site
the chain ends in and not at each link, per `CLAUDE.md`'s comments rule.
`moveRunHuman` is that chain's head and converts as its row says: it reads live
at the boundary, and everything below it, `runParams` included, is that one read
threaded down. What the walk buys is that both answers are now read rather
than assumed, which one level's could not be.

**The walk was then performed whole, 2026-09-07, over the tail's thirty-three
entries: twelve hold, twenty-one are clean.** It is not twelve findings. All
twelve reach one of two waits --- `verifyEscape`'s and `verifyToolEffect`'s
`displayYesNo` inside `processTileActions`, and `meleeAid`'s own --- down
a single chain: `moveOnceToXhairHuman` and `continueToXhairHuman` through
`goToXhair`, `goToXhairExplorationMode` and `goToXhairGoTo` into `moveRunHuman`,
and `alterWithPointerHuman` through `alterTileAtPos`, each reaching
`alterCommon` and `verifyAlters` below it. So the reasoning above covers them:
they are links of a chain whose bottom is pinned already, the identity is one
boundary read threaded down, and the comment sec. 10.6 requires stays
at the pinned end rather than at each link. **No entry moves bucket
on this evidence**, and none should be moved without asking sec. 10.6's
choose-versus-confirm question at the site --- both waits here *confirm*, one
leaving the dungeon and one an attack already aimed, which is why the chain ends
where it does. Three exact results are worth keeping. `applyItem`'s documented
near-miss is confirmed clean: it asks `displayYesNo`
(`HandleHumanGlobalM.hs:1044`, `:1047`) and afterwards uses only `go`, `iid`
and `fromCStore`, none of them derived from the leader. `runOnceToXhairHuman`
reaches the chain and is guarded out of it, `HandleHumanGlobalM.hs:723` fixing
its `run` argument `True`, which takes `moveSearchAlter`'s `:481` arm and leaves
`meleeAid` behind its own `not run` --- clean by path rather than by structure,
so it is the one entry a later constant could flip. And three clean entries
are handed an identity already stale by a caller the read-live table covers
rather than this tail: `moveItems` from `moveOrSelectItem`, `closeTileAtPos`
from `closeDirHuman`, `permittedApplyClient` from inside `psuit`. One caution
the walk earned about its own citations, binding on anyone re-running it: a use
written inside a `let`-bound action is cited where it is *written*, not where
it *runs*, so `meleeAid`'s `updateTarget leader` reads
as `HandleHumanGlobalM.hs:399`, ahead of the waits at `:408` and `:412`,
and in fact executes after both, through the `returnCmd` calls at `:410`
and `:414`. A line number alone says the opposite of the truth there.

**Read live** --- the identity must stop being threaded through each. The table
is the work list, one row per function, and its last column is the only decision
that fixes anything: a read placed above the wait is the stale copy under a new
name. Of the twenty-eight, nine never need the identity again --- they hand
it on, and only the witness travels; seven read at the top, nothing intervening;
and the remaining twelve are the work, seven reading below a wait and five
inside a callback the menu loop re-invokes. Line citations are to the function's
own module unless the cell names another. Derived by `tools/leader-census.py`'s
surface plus a reading of every body, each row then re-derived independently;
the last column is the part no tool checks.

| function | module | mints? | the wait it spans | last use after it | the read goes |
|---|---|---|---|---|---|
| `transition` | `InventoryM` | inherit | the menu loop it drives | `:379`, `:398`, `:431` --- three `defAction`s | inside each `defAction`, not at the top |
| `getItem` | `InventoryM` | inherit | --- | --- | nowhere; witness only |
| `getFull` | `InventoryM` | inherit | `InventoryM.hs:285` `getItem` | `:290` `bagAll`, closed over the entry body | after the wait --- and the *bag* moves with it |
| `getGroupItem` | `InventoryM` | inherit | in `getFull` | --- | nowhere; witness only |
| `getStoreItem` | `InventoryM` | inherit | in `getItem` | --- | nowhere; witness only |
| `runDefSkills` | `InventoryM` | inherit | `InventoryM.hs:518` `displayChoiceScreenWithDefItemKey` | `:519` `skillsInRightPane leader` | inside the right-pane callback; `:516` keeps a top read |
| `runDefInventory` | `InventoryM` | inherit | `InventoryM.hs:645` the same call | `:646` the `meleeSkill` the callback closes over | inside the callback, with `getActorMaxSkills` |
| `pointmanCycle` | `HandleHelperM` | inherit | --- | --- | at the top |
| `pointmanCycleLevel` | `HandleHelperM` | inherit | --- | --- | at the top |
| `pickLeaderWithPointer` | `HandleHelperM` | inherit | --- | --- | nowhere; witness only |
| `chooseItemHuman` | `HandleHumanLocalM` | mint | --- | --- | nowhere; witness only |
| `chooseItemDialogMode` | `HandleHumanLocalM` | inherit | `HandleHumanLocalM.hs:177` `getStoreItem` | `:346` the recursive call | after the wait, replacing the manual re-read at `:180-182`; the `renderOneItem` callbacks capture too |
| `chooseItemProjectHuman` | `HandleHumanLocalM` | mint | in `getGroupItem`, called at `:398` | `:393` `psuitReqFun` inside `psuit` | inside `psuit`; the `:381` branch needs its own call |
| `chooseItemApplyHuman` | `HandleHumanLocalM` | mint | in `getGroupItem`, called at `:586` | `:581` `permittedApplyClient` | inside `psuit`; sibling (c), and nothing tests it |
| `psuitReq` | `HandleHumanLocalM` | inherit | --- | --- | at the top; its *caller* is the closure case |
| `pointmanCycleHuman` | `HandleHumanLocalM` | mint | --- | --- | nowhere; witness only |
| `pointmanCycleLevelHuman` | `HandleHumanLocalM` | mint | --- | --- | nowhere; witness only |
| `pickLeaderWithPointerHuman` | `HandleHumanLocalM` | mint | --- | --- | nowhere; witness only |
| `itemMenuHuman` | `HandleHumanGlobalM` | mint | `HandleHumanGlobalM.hs:1603` `displayChoiceScreen` | `:1614` `blid b`, the entry body | after the wait --- re-fetch the body, not only the id |
| `chooseItemMenuHuman` | `HandleHumanGlobalM` | mint | --- | --- | nowhere; witness only |
| `projectHuman` | `HandleHumanGlobalM` | mint | --- | --- | at the top; `projectItem` keeps its pinned id |
| `applyHuman` | `HandleHumanGlobalM` | mint | --- | --- | at the top |
| `alterDirHuman` | `HandleHumanGlobalM` | mint | in `pickPoint`, called at `:1060` | `:1061` `alterTileAtPos` | after the wait; no pre-wait use to keep |
| `closeDirHuman` | `HandleHumanGlobalM` | mint | in `pickPoint`, called at `:1291` | `:1293` `closeTileAtPos` | after the wait; `:1284-1287` keeps a top read |
| `pickPoint` | `HandleHumanGlobalM` | inherit | `HandleHumanGlobalM.hs:1356` `getConfirms` | `:1362` `shift (bpos b)` | after the wait; move the `:1348` body read down |
| `moveItemHuman` | `HandleHumanGlobalM` | mint | --- | --- | at the top |
| `moveOrSelectItem` | `HandleHumanGlobalM` | inherit | `HandleHumanGlobalM.hs:787` `pickNumber`, `:796` `selectItemsToMove` | `:806` `moveItems` | after each wait; `calmE`, `overStash` and the `stores` they build are used on both sides of `selectItemsToMove`, and `stores` of `pickNumber` too, so they are *recomputed* below rather than moved below, and the chosen bag entry (`:779`, used at `:793`) moves down with the read --- but not `eqpFree` (`:780`), which has no post-wait use at all |
| `selectItemsToMove` | `HandleHumanGlobalM` | inherit | in `getFull`, called at `:870` | --- | at the top; its `psuit` captures no actor |

Three entries above are additions this inventory found, none of them
in the post-mortem's sec. 09 list of symptoms, and each was missed
by a different blind spot. `closeDirHuman` (`HandleHumanGlobalM.hs:1291`)
is `alterDirHuman`'s twin --- same `pickPoint` wait, same use of the held leader
afterwards (`closeTileAtPos leader p`); it was classified late because
the extraction rule sees a wait only in a function's own body, and this one's
wait is inside the `pickPoint` it calls. `runDefSkills` (`InventoryM.hs:511`)
and `runDefInventory` (`InventoryM.hs:623`) were missed by the third blind spot,
and they hold the leader the way sibling bug (a) does rather than in a plain
binding: each hands `displayChoiceScreenWithDefItemKey` a right-pane callback
closed over the entry leader --- `skillsInRightPane leader` in one,
a `meleeSkill` derived from it in the other --- and that argument is re-invoked
inside the menu loop, so a mid-dialog restore leaves the pane describing
an actor who is no longer pointman. The skills dialog permits the switch
outright (`maySwitchLeader MSkills = True`, `InventoryM.hs:419`). The placement
rule of the post-mortem's sec. 10 therefore binds here as it does
at `chooseItemProjectHuman`: the read goes inside the callback, not at the top
of the body. `psuitReq` itself is not one of these --- its own body
(`HandleHumanLocalM.hs:510-527`) waits nowhere, so it reads at the top like any
other function; what it is, is the *value* a caller captures, and the four
closure cases are therefore call sites: `chooseItemProjectHuman`,
`chooseItemApplyHuman`, `runDefSkills` and `runDefInventory`,
with `transition`'s three `defAction`s and `chooseItemDialogMode`'s
`renderOneItem` alongside them in the table.

A fourth site belongs with them and is worse, being a live bug rather
than a classification slip: `chooseItemApplyHuman`'s own `psuit`
(`HandleHumanLocalM.hs:579-585`) calls `permittedApplyClient leader` inside
the action it hands to `getGroupItem leader psuit` (`:586`), which `transition`
re-runs per keypress (`InventoryM.hs:443`) while the store permits switching ---
so the apply dialog judges items for whoever opened it. That
is the post-mortem's sec. 09 sibling (c), and it differs from (a) in the only
way that matters to this step: (a) gains a `psuitReq` call inside the closure
while keeping its entry one, this one is inside already and needs its *actor*
read live. A conversion that puts `getLeaderUI` at the top of the body satisfies
the compiler and leaves the bug. PR 0 pins it; sec. 02 step 6 also switches
it by hand, that being the real frontend.

**And the last column found a class the design had not named: it is not only
the identity that goes stale, but what a body derives from it before the wait.**
`getFull` binds the entry actor's body and then a bag accessor closed
over it (`InventoryM.hs:264-265`), runs the whole dialog (`:285`), and looks
the chosen items up in *that* bag (`:290`) --- with `EM.!`, so an item the new
pointman has and the old one lacks is not incoherence but a partial-map failure.
That is no longer a hazard on paper: a scripted Tab and Return through the real
dialog reach it, and forcing the returned quantity dies
with `IntMap.!: key 117 is not an element of the map`, which
`test/InventoryMUnitTests.hs` now pins as the post-mortem's sibling (d).
`moveOrSelectItem` is the same class with a twist, and the twist is what
the word "move" gets wrong here. It derives `calmE` and `overStash`
from the entry body (`HandleHumanGlobalM.hs:754-755`) and the `stores` they
build (`:756-759`), then uses all three on both sides of the `selectItemsToMove`
wait --- at `:763`, `:767`, `:771` and `:783-784` before, and at `:804-806`
after it returns --- and `stores` on both sides of `pickNumber` as well (`:774`,
then `:790` and `:794`), the other two having no use at all between that wait
and the end of its branch. A value needed on both sides cannot be moved below
the wait; it is *recomputed* there, which is a different edit. What does move
down whole is the chosen bag entry, read from the entry body's bag at `:775-779`
and consumed at `:793` after `pickNumber`. And `eqpFree` (`:780`) does neither:
it feeds only `kToPick` (`:781-782`), which is `pickNumber`'s own argument
at `:787`, so moving it below the wait would move the computation of the wait's
argument below the call that takes it. `itemMenuHuman` compares `blid b` against
the switched-to actor at `:1614` using the body it read before `:1603`. Reading
`sleader` live in these bodies fixes none of them on its own ---
the *derivation* has to move below the wait too, which is why the table's last
column speaks of the bag and the flags, not only of the read. The general form,
which the post-mortem's sec. 10 now carries: wherever an identity is used before
a wait to derive a value used after it, the derivation moves with the read;
"move the read" is the special case where the derived value is the identity
itself. A third wait surfaces with them, named nowhere before: `pickNumber`
(`HandleHelperM.hs:594`) displays a choice screen, so the move family spans two
waits rather than one.

Where the witnesses come from, counted: thirteen of the read-live functions have
a case in `cmdSemanticsLeader` and so receive a freshly minted witness
at the boundary --- `chooseItemHuman`, `chooseItemProjectHuman`,
`chooseItemApplyHuman`, `pointmanCycleHuman`, `pointmanCycleLevelHuman`,
`pickLeaderWithPointerHuman`, `itemMenuHuman`, `chooseItemMenuHuman`,
`projectHuman`, `applyHuman`, `alterDirHuman`, `closeDirHuman`, `moveItemHuman`.
The other fifteen are internal to the set and inherit it --- all but one
from a caller inside the set: `psuitReq` is called by `chooseItemProjectHuman`
(`HandleHumanLocalM.hs:367`) but also by `projectItem`
(`HandleHumanGlobalM.hs:976`), which the Keep group below holds; `projectItem`'s
only caller is the entry point `projectHuman`, so a witness reaches it
and it can pass one on. That is the shape to expect wherever a pinned function
calls a read-live one: the witness travels even where the identity does not.
`closeDirHuman` is on the list for the same reason it appears above at all ---
it is `alterDirHuman`'s twin, entry point included, its only caller being
the boundary case at `HandleHumanM.hs:129`. It is also the reason to re-derive
this count whenever the read-live set changes: a late addition there is a late
addition here, and nothing but a re-count catches it. One call site deserves
a second look during the sweep and turns out to confirm the design:
`HandleHumanGlobalM.hs:1622` calls `itemMenuHuman newAid` right after
`pickLeader False newAid`, i.e. it hands on the actor it has just made pointman
--- exactly what a live read of `sleader` returns, so the argument was carrying
what `sleader` now carries, provided the `pickLeader` stays ahead of the call.

**Keep** --- the parameter means "some actor", not "the pointman now":
`pickLeader` (the switch target), `partyAfterLeader` (the rotation pivot),
`skillsOverlay`, `skillCloseUp` and `skillsInRightPane` (the described subject),
`accessModeBag` (pure), `partActorLeader` and `partPronounLeader` (which read
the pointman themselves, to decide whether the actor they are given is "you"),
and every `ActorId` bound as `aid`/ `source`/`target` in `RunM`, `SessionUI`,
`WatchCommonM`, `WatchSfxAtomicM` and `WatchUpdAtomicM`.

Five more are named `leader` and mean it, but are one-step callees whose caller
has just read the pointman, so they keep the parameter for the reason the tail's
second half does: `msgAddDone` (`HandleHumanGlobalM.hs:1328`, reached
from the tile-altering and door-closing paths once each has the identity it will
act for) and DrawM's `drawLeaderDamage`, `checkWarningHP`, `checkWarningCalm`
and `checkWarnings`, which render one frame from a leader `drawHudFrame` read
for that frame. Three of the four are also called for actors that are
not the pointman at all, so a live read would be wrong outright there:
`checkWarningHP` and `checkWarningCalm` from `WatchUpdAtomicM` with an `aid`
(`:268`, `:271`, `:312`, `:315`), and `checkWarnings` per drawn actor inside
`drawFrameActor` (`DrawM.hs:332`); only `drawLeaderDamage` is leader-only,
its single caller (`DrawM.hs:556`) passing the very pointman the frame was read
for. They are listed here rather than in the tail because they bind `leader`
later in the head and so fall outside the 65 first-bound the tail is counted
against.

Three more are kept *across a wait*, and are the one place in this inventory
where the post-mortem's sec. 10.6 overrides the "holds the pointman across
a wait" test rather than being served by it. `projectItem`
(`HandleHumanGlobalM.hs:967`) and `meleeAid` (`HandleHumanGlobalM.hs:386`) each
ask `displayYesNo` and *then* call `updateTarget leader`; but they *confirm*
an action already chosen for a particular actor rather than *choose* one,
and only the macro-abort restore can swap the pointman under such a prompt,
since a yes/no offers no switch key. `processTileActions`
(`HandleHumanGlobalM.hs:1115`) is the third, and it sat in the tail below until
campaign 3 re-read it. Its own body waits nowhere, which is the whole of what
the tail's rule tested; but the inner `processTA` it drives calls `verifyEscape`
(`:1157`), which waits at `:1237`, and `verifyToolEffect` (`:1186`), which waits
at `:1258`, and it recurses past the first of those at `:1160` into a branch
reading `kitAssocs leader` (`:1172-1173`). So it holds the identity across
a wait by exactly the blind spot this section's opening records, a wait seen
only in a function's own body, and it holds a value derived from it too ---
the body `sb` read at `:1118` and read again at `:1199` after the loop, the way
sibling (d)'s bag is. Both of its prompts are yes/no and both confirm an action
already chosen for that actor at this tile --- the tool transformation
in `verifyToolEffect`, and, in `verifyEscape`, leaving the dungeon through
an embed whose effects pass `IK.isEffEscape` --- so sec. 10.6 pins it rather
than sending it live. Acting for the new pointman would honour the keystroke
and not the intent, so all three keep an explicit `ActorId` meaning "the actor
this confirmation is about" --- the "some actor" column --- and each site
carries the comment sec. 10.6 requires wherever a parameter is pinned across
a wait. `projectItem` here means the UI's; the AI's `Client/AI/PickActionM.hs`
has a homonym with a different signature, which is not a call into this layer.

**The mechanical tail** --- the 33 that remain after the two groups above,
so that the census partitions its 72 as 27 read live, 12 kept and these 33. What
was tested of them is narrower than the conclusion once drawn from it: no wait
sits between the identity's arrival and its last use *within each function's own
body*. That is the classification proxy, and it cannot see a wait inside
a helper the function calls --- which is how the third of the Keep group's
pinned sites above stayed here until it was read a level down. Both halves
of this tail owe the transitive callee walk the proxy paragraph at the head
of this section specifies, before C6 converts the one and leaves the other; C6's
**Done** carries it. (The read-live table lists 28, one more than the census
sees --- the point-free `pickLeaderWithPointerHuman`; and the Keep bullet adds
to its 12 the functions that bind no `leader...` parameter at all: `pickLeader`,
`skillsOverlay`, `partActorLeader`, `partPronounLeader`
and the `aid`/`source`/`target` families.) The post-mortem's sec. 10 rules
on them by position relative to the boundary, and the two halves get opposite
answers.

**Convert** (fifteen) --- dispatched *at* the boundary, so once they are done
no `CmdLeader` case passes an `ActorId` at all:

- `HandleHumanGlobalM`: `alterWithPointerHuman`, `continueToXhairHuman`,
  `moveOnceToXhairHuman`, `moveRunHuman`, `runOnceAheadHuman`,
  `runOnceToXhairHuman`, `waitHuman`, `waitHuman10`, `yellHuman`
- `HandleHumanLocalM`: `acceptHuman`, `clearTargetIfItemClearHuman`,
  `selectActorHuman`, `xhairItemHuman`, `xhairStairHuman`, `xhairUnknownHuman`

**Keep the parameter** (eighteen) --- reached *below* the boundary, the caller
passing the identity it has just read, so a re-read in the callee would buy
nothing and could let one multi-step operation act for two actors. The sweep
confirms them as the compiler names them, and all eighteen owe the transitive
callee walk stated above --- the walk that has already moved one entry
from this list into the pinned group, and that C6's **Done** carries.
`goToXhair` is the clean case: its only callers are three of the fifteen above.

- `HandleHumanGlobalM`: `alterCommon`, `alterTileAtPos`, `applyItem`,
  `closeTileAtPos`, `displaceAid`, `goToXhair`, `goToXhairExplorationMode`,
  `goToXhairGoTo`, `moveItems`, `moveSearchAlter`, `verifyAlters`
- `HandleHumanLocalM`: `endAiming`, `endAimingMsg`, `permittedApplyClient`,
  `permittedProjectClient`, `posFromXhair`, `projectCheck`, `xhairLegalEps`

`applyItem` earns a place in this tail at all, rather than in the read-live
table above, by a hair: it also asks `displayYesNo`, but makes no use
of the leader afterwards.

## 04 -- Abort-split: migration step and verification

The abort-split is the final step of the joint migration --- strictly after sec.
02, assuming the live-read design is complete and its verification step passed,
and with `macroStep`'s home already settled: `FrameM`'s own pure section, per
the abort-split record's sec. 01 and the wasm plan's 0.1 ruling that closed
the alternative as an import cycle. It is gated on the live-read design alone:

1. Extract `macroStep` into a pure section of `FrameM`, beside
   `dropEmptyMacroFrames`, whatever the wasm plan's item 0.1 has done
   by then --- that item's own 2026-08-07 ruling makes `InputDecision` an import
   cycle for this function, and the abort-split record's sec. 01 carries
   the reason --- with the *decision* half of that record's sec. 01
   branch-exactness checklist as its test table. Four of that checklist's eight
   bullets are about the decision and become pure cases --- the interrupt-inputs
   bullet, which is two cases rather than one (not queried; a disturbing
   report); the F1-help exemption surviving that same report; the legal-key
   guard, which aborts playback even when *not* interrupted; and the no-macro
   branch that must leave the macro stack alone --- five rows in all,
   the interrupt bullet earning two. To them the table below adds three baseline
   paths any decision function needs: a voiced key with its remaining macro,
   a legal key voiced against a non-empty key set, and an empty macro
   under an interrupt, which must still be `NoMacro`. The other four bullets pin
   the *shell* rather than the decision --- the common cleanup,
   the read-before-clear ordering, the special-event logic and the `addToMacro`
   recording --- so a pure table cannot express them and none is missing
   from it. Three of those four are pinned already, by AS4, AS5 and AS8.
   **The special-event one is pinned by nothing**: every AS case calls
   `promptGetKey` with `ColorFull`, so the `dm /= ColorFull` branch
   and its `unless (gunderAI fact)` guard are entered by no AS case at all ---
   and that is exactly the block the split relocates
   into `specialEventKeyReset`. (The integration test does enter it, down
   the shutdown path the record's sec. 01 now names, and observes nothing there,
   so it pins nothing either.) An AS case for it has to exist *before*
   this step, or the refactor moves untested code and step 4's "must pass
   without edits" gate has a hole precisely where it is being relied on.
   A second one belongs with it: the no-macro branch's "no `resetPlayBack`"
   invariant is entered by AS4 and AS7 but observed by neither, both running
   on fixtures whose macro stack is already empty, so it too would survive being
   broken. Both characterize the *unsplit* primitive and are independent
   of either design, so sec. 01 lands them in PR 0, well ahead of this step;
   if that PR was skipped, they are simply this step's first commit. The pure
   cases are pure, so they sit beside the AS series or in a module of their own,
   wherever the function lands; `test/SessionUIMock.hs` already simulates
   macro-frame transitions, so no new harness machinery is needed --- though
   the special-event case does need one thing the stub lacks, a frontend
   that records `FrontResetKeys` rather than discarding it. These are additions,
   not replacements: the AS cases keep driving the same decisions through
   the real `promptGetKey`, which is what makes step 4 meaningful.
2. Name `abortMacroPlayback`; keep the common cleanup in the shell,
   with the read-before-clear ordering noted in the record's sec. 01.
3. Audit the shell's residual writes: `promptGetKey` stays mutating by design
   (the record's sec. 01), so walk its body and, for every remaining effect ---
   the voicing branch's macro-frame advance and `MsgMacroOperation` message,
   the special-event `resetPressedKeys`, `recordHistory`, the common cleanup
   block, the `sreqQueried`-gated `addToMacro` recording --- either hoist
   it to the callers (only where all of them want it and the sec. 05 outcomes
   survive) or keep it with a comment stating why a key-read primitive
   is its natural home. The recording already carries the model comment
   ("recorded here, not in @UI.humanCommand@, to also capture choice of items
   from menus"). End state: `promptGetKey`'s haddock exhaustively lists
   the state it may write, so nothing about the primitive is hidden again.
4. The FrameM contract tests (the AS series of sec. 05) must pass *without
   edits* across this step --- any diff in their outcomes is the finding,
   evidence that the refactor altered `promptGetKey`'s observable behaviour,
   and not something to edit away. Re-run the bridge tests X1/X2 too: their
   `promptGetKey` observations are [contract] as well, and the one part of them
   that does change, the cycling outcome, has already changed by then ---
   in the live-read step (sec. 02, step 4).

### 04.1 --- extract `macroStep`

One pure function in `FrameM`'s pure section beside `dropEmptyMacroFrames`,
with the decision table drafted below as its test list; step 1 above has
the rest. The two AS cases it depends on are PR 0's and land earlier.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/FrameM.hs`,
`test/FrameMUnitTests.hs` (the pure cases beside the AS series --- or a module
of their own, per **Decide first**) and this document. **The claimant list
for `FrameM.hs` lives here**: 04.1, 04.2 and 04.3, one commit each
in that order. **So does `test/FrameMUnitTests.hs`'s**: PR 0, C4 and 04.1.

**Done** --- `native`, at 164 tests: the eight pure cases of the table below
are green and are the third and last count movement "Running this plan" permits,
`AS` staying at 15 and untouched --- PR 0's two cases included, written against
the unsplit primitive and surviving the split unedited --- and `contract` at 28,
neither marker being on a pure case; and `docs`.

**Hands back** --- nothing.

**Decide first** --- where the pure cases live, step 1 allowing either: (a)
beside the AS series in `test/FrameMUnitTests.hs`, which adds no module
and touches no file another campaign holds; (b) a module of their own, which
adds it to `test/Spec.hs` and to the test-suite `other-modules`
in `LambdaHack.cabal`, both contended in the wasm plan with their claimant lists
at its 0.1. (a) is the recommendation; either way the AS series keeps driving
the same decisions through the real `promptGetKey`.

### 04.2 --- name `abortMacroPlayback`

`FrameM`, ~10 lines: the abort action named, the common cleanup kept
in the shell with the read-before-clear ordering noted in the record's sec. 01
(step 2).

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/FrameM.hs` (list at 04.1)
and this document.

**Done** --- `native`, `AS` at 15 with AS4--AS6 green *without edits*,
`contract` at 28, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 04.3 --- audit the residual writes

`FrameM` only: the walk step 3 describes, ending in the haddock drafted below.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/FrameM.hs` (list at 04.1)
and this document.

**Done** --- `native`, `AS` at 15, `contract` at 28, `docs`; and the haddock
lists every write the body performs, checked by reading the body against it.

**Hands back** --- nothing.

**Decide first** --- nothing: hoisting an effect to the callers is allowed only
where all of them want it and the sec. 05 outcomes survive, per step 3, which
decides each site.

### 04.4 --- AS series unchanged, then the landing

The gate of step 4, then the deletion ritual of the head callout, which
is this campaign's landing and is spelled out there rather than here.

**Split** --- three commits. (1) nothing but this document: the gate's result
in the Log and the ledger flip. (2) the deletion commit the head callout spells
out, bullet by bullet, which carries the two records' remaining outcome lines
and the `CHANGELOG.md` lines drafted in sec. 02. (3) the two records' restamps,
which cannot ride (2): that commit touches `.py` and `.hs` as well as `.md`,
and `CLAUDE.md`'s restamp rule wants a follow-up touching only `.md` files,
no document here citing one by line.

**Owns** --- this document, deleted in (2) with `tools/leader-census.py`;
`tools/checks.py` (its `UNCOVERED` entry); `CHANGELOG.md` (both drafted lines);
`docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md` (the outcome
lines the head callout reserves, and sec. 10.3's recast; the post-mortem's
claimant list is at C3, and PR 0 writes the abort-split record before this);
`CLAUDE.md` (its three sentences; the wasm plan's 0.1 and R3 name this item,
and its 0.2 and 2.1 reach it through 0.1's claimant list); `test/CLAUDE.md`
(its tag section's unlanded tense, per the head callout's bullet on it, and PR 0
writes it before this if the recording stub was not appended);
`docs/wasm-frontend-unified-plan.md` (the sites the head callout enumerates,
under that plan's rule that the lock does not serialize on it; PR 0 is the other
claimant here); `test/HandleHumanLocalMUnitTests.hs`
and `test/InventoryMUnitTests.hs` (one comment each, after PR 0 and C3);
`tools/check-doc-examples.py` (its live-control entry, and the sentence saying
the control went with it); `tools/check-doc-refs.py` (its self-test document
and `SELF_TEST_OK`, whose passing control was this campaign's tool);
`tools/check-plan-crossrefs.py` (its docstring, its `GRAMMARS` entry
and its self-test fixtures) with `tools/mutants.py` beside it, the second
grammar going with that entry and taking the self-test and mutant rows
that exercise it; and `tools/defects.json` (`leader-census-01` and the three
`check-plan-crossrefs` records the head callout names). The last four are here
because the ritual's work in them is not a rewording but a repair,
and an **Owns** that omitted them would forbid the very commit that has to make
it.

**Done** --- `native` at 164, since (2) edits two test files; `AS` at 15
with no edits to it; `contract` at 28; and `LR-flip` at 10 with X1 and X2
passing unedited, that being the gate that reaches them --- both are named
`LR-flip X1` and `LR-flip X2`, so no path of theirs contains `contract`
and a green `contract` run is silent about them, the containment trap sec. 00
warns of. Then, for (2), `docs` over `CLAUDE.md`, both records and the wasm
plan, the `check-doc-refs.py` run and the `git grep` the head callout ends on,
read as that bullet says, and `check-all tools` whole --- self-tests, mutants
and the defect records in both directions --- since `tools/checks.py`, two
checkers, a live control and three defect records all move with the deletion.

**Hands back** --- nothing.

**Decide first** --- nothing, ruled 2026-08-07: the census tool goes
with this document, sec. 10.3's sentence recast in the same commit (the head
callout).

### The haddock that step 3 asks for, drafted

Step 3 ends with "`promptGetKey`'s haddock exhaustively lists the state it may
write". That list is already known, so here it is, making the step transcription
rather than rediscovery:

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
for the baseline paths no bullet asks for; the four shell bullets can have none,
per step 1. The inputs are the four `macroStep` takes --- queried, disturbing
report, keys legal for the frame, pending macro --- and the expected output
is a `MacroStep`:

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

Each row is one call of a pure function, so the table transcribes directly
into a test list; the AS series keeps driving the same decisions through
the real `promptGetKey`, which is what makes step 4 meaningful.

## 05 -- The test battery as it stands

The design is encoded in a test suite already on master, all green
on the unmodified engine at the baseline counts sec. 00 states, in its gate
block and its count sequence and not here, so that one battery change falsifies
fewer figures. Every test that pins a design decision carries a `[contract]`
or `[LR-flip]` tag; the four `psuitReq` branch tests are plain coverage
and carry neither. The meaning of the two tags, the tasty patterns that run each
series and the discipline for flipping one are defined once in `test/CLAUDE.md`,
not restated here. What matters below is which test carries which tag,
and that flipping the `[LR-flip]` set is step 4 of sec. 02.

### The live-read series --- `test/HandleHelperMUnitTests.hs` (extensive)

| test | class | pins |
|---|---|---|
| LR1, LR2 | contract | the target invariant: in-sync cycling advances Forward/Backward correctly |
| LR3 | LR-flip | the 4a6eca154 reproducer: stale leader -> cycling silently no-ops |
| LR4 | LR-flip | three-member party: stale leader -> the *wrong* member is picked |
| LR5 | LR-flip | the changelog crash itself: stale leader fires `pointmanCycle`'s live "same leader" assertion (caught via `try`) |
| LR6 | LR-flip | a dangling stale `ActorId` is silently tolerated and yields an arbitrary pick (unrepresentable post-live-read) |
| LR7, LR8 | contract | `partyAfterLeader` pivot rotation, incl. the unknown-pivot edge that enables `np == sleader` (its parameter survives live-read per the post-mortem's sec. 10 partition) |
| LR9 | contract | the `pickLeader` primitive: no-op on current, switch otherwise |
| LR10, LR11 | contract | banned factions: dungeon-wide cycling refused, same-level cycling still allowed --- the partition subtlety a live-read rewrite must not change |
| LR12 | contract | the dungeon-wide twin's non-banned success path: in-sync `pointmanCycle` advances (the same function whose live assertion the desync crashes in LR5) |
| LR13 | contract | the `CmdLeader` boundary itself: with no pointman designated, dispatch refuses with the friendly failure --- the one place that turns `Maybe ActorId` into an `MError`, kept by the post-mortem's sec. 10 |

LR1, LR2 and LR10--LR13 drive the command through the real key-loop entry point
(`cmdSemInCxtOfKM`, with the key looked up in the sample game's bindings ---
the `dispatchCmd` helper), so the leader the handler cycles from is read
from `sleader` at dispatch time, in sync by construction, as for any top-level
keystroke. LR3--LR5 instead call `pointmanCycleLevel`/`pointmanCycle` the way
the item dialogs do (`InventoryM.hs:398` and `InventoryM.hs:431`), with a held
leader --- the desync's entry point --- after the real `restoreLeaderFromRun`
has moved the pointman under them.

### The sibling bugs and dialog contracts --- the `HandleHuman*MUnitTests` pair

The sibling bugs of the post-mortem's sec. 09, and the two contracts pinning
the dialog path they run through, in `test/HandleHumanLocalMUnitTests.hs`
and `test/HandleHumanGlobalMUnitTests.hs`:

| test | class | pins |
|---|---|---|
| permittedProjectClient judges its argument, not the pointman | contract | Sibling bug (a)'s premise, and the guard on sec. 03's Keep ruling: the verdict is actor-dependent (`Right True` vs `Left ProjectUnskilled` on the same item), which is what makes a captured closure wrong after a switch --- and it is about the actor *given*, not the pointman, so switching the pointman between two rounds of calls moves neither verdict. It carried an `[LR-flip]` tag until 2026-07-30, when the review found it had nothing to flip: sec. 03 keeps this function's `ActorId`, so live-read must leave both answers alone, and a conversion that made it read `sleader` fails here. |
| psuitReq verdict differs per actor | LR-flip | sibling bug (a) at the exact captured value: `psuitReq` --- what `chooseItemProjectHuman` bakes into the dialog's `psuit` --- gives a different failure per actor with the xhair on C's own position ("aiming obstructed by terrain" for A, the degenerate "aiming blocked at the first step" for C), through the real aiming pipeline, no walkable tiles needed |
| Project executed by a different actor than the item selection | contract | Sibling bug (b), both halves of the seam: with `sitemSel` left by A's choose dialog, `projectHuman` run for A gets past the store lookup (control), run for C fails with "no item to fling" for the item just approved. Deliberately [contract]: the execute-half pinned here is correct in isolation and survives the live-read design --- what it fixes is the *choose* half, whose live re-reads make the dialog re-validate for C before the selection is confirmed, closing the seam where the incoherent approval arises. |
| fling dialog: a mid-dialog switch keeps A's closure | LR-flip | Sibling bug (a) end to end, on the walkable board: a scripted `C-Tab` switches the pointman to C inside the real fling dialog, whose captured A-closure still calls the item suitable, so `Return` selects it and `sitemSel` is set --- for an item the unskilled C cannot fling. Post-live-read the closure judges for C, nothing is suitable and the dialog exits "never mind" with `sitemSel` unset (flip verified by temporarily re-reading the pointman in the dialog's `psuit`). |
| alterDir: the held leader picks the square to modify | LR-flip | The remaining site of the post-mortem's sec. 09, `alterDirHuman`/`pickPoint`, driven through the real crash window (the post-mortem's sec. 04): a macro dies inside the wait, `promptGetKey` restores the pointman to A, and the command modifies from the actor it was *handed* --- the run holding A targets C's floor, the one holding C the wall past it, and the two failures name the two tiles. Post-live-read both read the restored A and both name the floor (flip verified the same way). |
| chooseItemHuman: ESC exits the real store dialog | contract | that a whole dialog is drivable under the mock: `chooseItemDialogMode` -> `getStoreItem` -> `displayChoiceScreen` to the "never mind" exit, reaching `promptGetKey` through its `SlideshowM` call site --- the path the end-to-end fling row above runs on |
| chooseItemHuman: scripted Tab switches pointman mid-dialog | contract | the dialog's own cycling handler and `recCall`'s re-entry --- the re-sync of the post-mortem's sec. 02, from commit `8608d6f9c`, previously untested --- on the equipment store, which needs no aiming |
| getFull looks the chosen item up in the entry actor's bag | LR-flip | Sibling (d), in `test/InventoryMUnitTests.hs` beside the other `getFull` cases: a scripted Tab switches the pointman inside the real dialog and `Return` picks C's item, whose quantity `getFull` then looks up in A's bag --- so the identity comes back right and forcing the quantity dies with `IntMap.!: key 117 is not an element of the map`, which the test asserts on rather than merely catching. Post-live-read the bag is re-derived below the wait with the read, and the whole pair returns: `Right (CEqp, [(testItemId2, (1, []))])`. Non-vacuity proved both ways, recorded at the test. |

Two constraints shaped the three pins written first --- the two (a) verdict rows
and the (b) seam row --- both verified against the stub harness: the full
`psuitReq` pipeline fails deterministically on the default unknown-tile board
(`"aiming obstructed by terrain"`), so bug (a) was first pinned through
per-actor failure verdicts rather than through a whole dialog;
and `projectHuman`'s store lookup precedes all aiming, so bug (b) is drivable
with no walkable tiles at all. The walkable board lifted both limitations,
and the two end-to-end rows --- the fling dialog and `alterDir` --- run on it.
Its construction, and every other harness fact these tests rest on down to why
`emptyUnknownTile` is exported the way it is, is documented in `test/CLAUDE.md`,
which is maintained against the code; none of it is restated here.

What the battery does *not* cover, said out loud because the rest
of this section reads as coverage: the apply dialog. Sibling (c)
of the post-mortem's sec. 09 --- `chooseItemApplyHuman`'s `psuit`, sec. 03's
second closure case --- has no row above and no test anywhere;
`git grep chooseItemApplyHuman -- test/` is empty, and PR 0 is what falsifies
that, being the item that writes the first such test. Its analogue for the fling
dialog took a walkable board to write, and the cover assumed to need the same
board turns out not to: the 2026-08-07 probe found `permittedApply`'s verdicts
differing per skill on the plain board, so PR 0's pin is written on per-actor
failure verdicts as the `psuitReq` one was. Until it lands --- sec. 01's PR 0
is where it belongs, alongside the two AS cases --- sec. 02 step 6's by-hand
switch is the only check that (c) closed.

### The abort-split's own part, and what the tags mean jointly

The series above pin the live-read design; the two below pin the abort-split.
Stated jointly, the classifications mean:

- **[contract]** --- behaviour that must survive the live-read
  *and* the abort-split designs unchanged. The whole AS series is deliberately
  in this class: pinned against `promptGetKey`'s unchanged type, required
  to pass before live-read, after it, and after the abort-split --- the safety
  net under the abort-split refactor, never flipped.
- **[LR-flip]** --- characterizations of the current desync-prone behaviour,
  each with the post-live-read expectation stated inline; they flip when
  the live-read design lands, never as part of the abort-split.

### The abort-split series --- `test/FrameMUnitTests.hs` (all [contract])

| test | pins |
|---|---|
| AS1 | `addToMacro`: records bound keys only, never `Record`, no-op when idle |
| AS2 | `dropEmptyMacroFrames`: GCs empty frames, always keeps the last |
| AS3 | voicing: a legal, uninterrupted macro key is consumed --- and the run *survives* (the enabler of the crash window, post-mortem sec. 04) |
| AS4 | natural end: no macro -> run cancelled, pointman *not* restored (sec. 01 branch-exactness) |
| AS5 | abort via illegal macro key: macro wiped, run cancelled, pointman *restored* to the run leader --- the hidden write, pinned through the real `promptGetKey` |
| AS6 | abort via `sreqQueried = False`: same outcome through the other interrupt input that `macroStep` must reproduce |
| AS7 | rendered (non-blank) frames work under the mock: `drawHudFrame` over the stub board --- pinned because the end-to-end fling test above depends on it |
| AS8 | keys voiced from a macro are recorded into an in-game macro being defined --- the "recorded here, not in @UI.humanCommand@" semantics that sec. 04's audit step keeps inside the primitive |
| AS9 | abort via a disturbing report --- the third interrupt input, driven by the real `stopPlayBack`: same outcome as AS5/AS6 |
| AS10 | the F1 exemption: a help-displaying macro survives the same disturbing report --- voiced, run intact, pointman untouched |
| AS11--AS13 | `restoreLeaderFromRun`'s guards, one each --- no-op without a run, no-op for a `noRunWithMulti` faction, no-op when the run leader is gone from the level --- pinned because `abortMacroPlayback` relocates the function verbatim |

### Bridge --- touching both designs

**X1** replays the whole crash window (post-mortem sec. 04) end-to-end
with the leader restore performed by the *real* `promptGetKey`
(the post-mortem's sec. 07 test, kept as LR3 for its simplicity, calls
`restoreLeaderFromRun` directly instead of through `promptGetKey`): run rotates
the pointman, dialog captures it, macro dies inside the dialog, `promptGetKey`
restores the run leader, stale cycling no-ops. Its `promptGetKey` observations
are [contract]; only its final cycling outcome is [LR-flip].

**X2** repeats the window with the post-abort keypress arriving as a *real* key
from the scripted frontend stub: `promptGetKey` aborts the macro, restores
the pointman and returns a literal `C-Tab`, which the test resolves through
the fixture CCUI's real bindings and feeds to the dialog's cycling call
with the stale captured leader, as `InventoryM`'s `cycleLevelKeyDef` would. Same
[contract]/[LR-flip] split as X1.

> **What building the abort-split series caught.** The AS5/AS6/X1 outcomes
> confirm empirically that the restore-on-abort pathway runs under the stock
> unit-test mock with a blank frame (`onBlank = True`) --- no real frontend
> needed --- which is what makes the AS contract series cheap enough to keep
> green across both refactors.

### 05 --- the battery, landed

The series and its harness in `3453b1777` through `8b5703e87`, then sibling
(d)'s pin and the `permittedProjectClient` retag in `4b92b291a`. Its counts
are the baseline "Running this plan" states.

**Owns** --- nothing; landed.

**Done** --- landed: `contract`, `LR-flip` and `AS` at the baseline counts sec.
00 states.

**Hands back** --- nothing.

**Decide first** --- nothing.

---

*LambdaHack -- the migration plan for the pointman-desync work -- temporary
by design: delete it when sec. 04's last step lands. The reasoning lives
in `docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md`; the result
will live in the code.*
