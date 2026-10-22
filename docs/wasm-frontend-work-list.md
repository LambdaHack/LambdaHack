# WASM Frontend Work List

> **This document is temporary by design.** The permanent record
> is `docs/wasm-frontend-unified-plan.md`: the goals, the designs and their
> rulings, the decisions against, the audit and the Log. This file holds only
> what stops being true when the work lands: how an item reaches master,
> the ledger with every item's state, the gates, and each item's execution block
> --- **Split**, **Owns**, **Done**, **Hands back**, **Decide first** ---
> under the record's own heading for the item, whose design and rulings are read
> there. It is deleted when its last row lands or retires,
> as `docs/leader-desync-migration.md` will be, and the record's head callout
> then says the specification landed in full. One item is in flight at a time,
> in one checkout. The state vocabulary is `docs/leader-desync-bug.md`'s, stated
> in the record's head; a row here carries it.
>
> **Landing an item** does two things and nothing else: it appends an outcome
> line to the record, and it flips that item's row below from `open`
> or `not applied` to `landed -- <hash>`. The outcome line goes **immediately
> after the item's opening paragraph** in the record, before any subsection,
> so that opening the item at its head answers the question; its shape
> is the doctest bullet's under the record's Ground rules, and the outcome lines
> of 0.0 and 1.3 there. The hash in both places names the commit that did
> the work. No commit can name its own, so the record is a `.md`-only commit
> written after that one --- which is what a **Split** means when it says a code
> commit "carries the outcome line": the record follows that commit and names
> it. Several **Split**s number the record separately instead, and those say
> outright which commit's hash goes in. A claim the landing falsified
> is corrected separately, so that review can tell a relocation
> from a correction (Handing an item to a session).
>
> **Landing part of an item takes no outcome line**, and the row stays live:
> a partial landing is recorded in the item's body as specification for what
> remains, which is what R2 does today. **A standing item never lands** --- 0.3,
> 1.4 and R2 are ongoing or recurring by their own size cells, explicit widths
> and functional core review rules by theirs --- so it retires when the campaign
> does, rather than acquiring a hash.
>
> An item that proposes an artifact may take its outcome line **only once
> its entries in `tools/doc-refs-allow.txt` can be deleted**; that file's
> comment already asks for exactly that deletion, and gating on it makes
> "landed" a checked claim rather than a declared one. Which items the gate
> reaches is read off `tools/doc-refs-allow.txt` itself, which the checker
> already parses, rather than listed here: a list of items and a count
> of entries here would be a second copy of that file, and it went wrong
> the first time an entry moved between items. Every other item is ungated,
> its outcome line resting on the same reading as any other claim here;
> and an item whose artifact turns out unneeded clears the gate by deleting
> the entry together with the sentence that named it.

## How this lands

The ledger says what the items are and the blocks say how to execute one;
this says how they reach master, because the review is the scarce input
and no session can decide it.

**One review unit per ledger item, not per agent and not one for the campaign.**
A single pull request carrying Phase 0 through 2 would be unreadable, and one
per agent would make the bottleneck worse rather than better. The unit
is therefore a branch off whichever ref carries the newest copy of this document
--- resolved at execution time,
`git log -1 --all --format=%H -- docs/wasm-frontend-work-list.md`, and never
read off a name, because branching off an older copy hands the session
a different version of the item it was given. The `--all` is load-bearing rather
than tidy: without it the command is relative to `HEAD`, which is itself a name
--- and a name the same paragraph mints per item below --- so from any branch
predating the newest copy it returns exactly the older one this rule exists
to avoid. A name will not serve: this paragraph named `unify-campaign-docs`,
which has since merged into `master` and been overtaken there by further commits
touching this file, so the name came to point at exactly the older copy
it was written to steer a session away from. Read the unit
with `git log -p <that commit>..<branch>`; a GitHub PR exists only after
an authorized push, so the review that gates an item is the local one.
**A review branch is named for its item** --- `item-0.1`, `item-2.1`, `item-R3`
--- so that `git branch --list 'item-*'` maps onto the ledger.

**3.1 was to go first, as a shakedown**, the smallest item whose correctness
a machine can settle end to end; 0.0, 1.1's keyword half and 1.3 went first
instead on 2026-09-14 and rehearsed the ritual --- the gates, the outcome line
and the ledger flip --- so what 3.1 keeps is its ordering role: 1.2's per-frame
case waits on it. It rehearses no allowlist deletion, proposing no artifact
and so having no entry to delete; the items that build one carry that step.

**One item is reviewed entirely alone: the `Sdl.hs` drawing loop** (2.1's
refactor together with its `CellStyle` adoption, which rewrite abutting regions
of one hot loop). It rewrites shipped native rendering; SDL2 is exercised in CI
only by the init-and-quit backdoor and by `make smokeSdl`'s few self-played
frames, which require frames to arrive and not what they show; expensive
assertions are off in every workflow; and its real gate is display-bound.
Its branch carries `Sdl.hs` and nothing else; most of this document's `Sdl.hs`
anchors sit in the region it rewrites, so the pass will name every one
the refactor breaks, and the re-anchoring is a separate commit.

**2.4 splits**, so that each half gets the review it needs: the capability
mechanism lands early, provably behaviour-preserving
with `Wasm.supportsMultiFont = False`, and the one-boolean flip is reviewed
alone as a product decision. Unsplit it would arrive as mechanism plus risk
in one diff.

**Never autonomously:** push, open or force-update a pull request, or take
an outcome line on a partial landing.

## Ledger --- every item's state


The row is the unit of rollback: each item is one commit, or the **Split** run
of commits its block names, every one of them leaving the suite green --- which
is what makes a row something that can be reverted rather than unpicked. Which
sections of the record are frozen and which live is its head callout's to say.

| sec. | delivers | size | depends on | state |
|---|---|---|---|---|
| 0.0 | AltGraph fixes (keys, mouse/wheel); highlight-outline rule | tiny | --- | landed -- 403aafe13 |
| 0.1 | `InputDecision` shared module; sync `lhKey` | small | 0.0; 0.3 baseline | landed -- 83b10c8a5 |
| 0.2 | `CellStyle` + generated TS tables and fixtures | medium | --- | landed -- 53f508d1a |
| 0.3 | FFI-coverage battery (baseline before 0.1, then per-commit) | ongoing | --- | standing |
| 0.4 | canvas painter in place of the DOM grid (B.10) | medium | 0.3 for the reactor driver | not applied |
| 1.1 | crosshair cursor (CSS keyword; then generated SVG cursor) | trivial | 0.2 for the final form | landed -- 0a72de79c |
| 1.2 | working `Ctrl+P` and `--printEachScreen` screenshots | small | 0.4 for the canvas; 0.3 for the reactor driver; 3.1 for the per-frame case | open |
| 1.3 | fullscreen toggle with scaling | small | --- | landed -- d211caa38 |
| 1.4 | banner/title truthfulness | trivial, recurring | feature landings | open -- standing |
| 1.5 | `allFontsScale` honored in browser | small | 0.4 for the painter; 0.3 for the reactor driver; R4 for player control | open |
| 1.6 | game-end page lifecycle: a finished or crashed game says so | small | --- | open |
| 2.1 | `OverlayLayout` extraction + `Sdl.hs` on it and on `CellStyle` | medium--large | 0.2 for `CellStyle`; determinism goldens (native harness) | not applied |
| 2.2 | browser canvas overlay renderer + font wiring | medium | 2.1, 0.2, 0.4; 1.5 for the startup call | not applied |
| 2.3 | overlay transport over JSFFI | medium | 2.1, 2.2; 0.3 for the end-to-end battery | not applied |
| 2.4 | multi-font capability flip | tiny diff, big review | 2.1--2.3; determinism goldens | not applied |
| 2.5 | post-flip QA | small | 2.4; 1.2 for `Ctrl+P` | not applied |
| 3.1 | `lhStart` reads WASI argv | small | --- | not applied |
| 3.2 | Node driver for the game reactor | small | 3.1, 0.3 | not applied |
| 3.3 | `nodeBench*`/`nodeDeployedBench` targets | small | 3.2 | not applied |
| R1 | save robustness: staging keys plus a generation pointer | medium | 3.2, for the lag re-measurement | open |
| R2 | browser-and-frontend CI | ongoing | grows per phase | standing |
| R3 | retire GHCJS support | one commit, plus repair and record | 2.5 (parity) | not applied |
| R4 | URL-parameter options | small | 3.1 | not applied |
| R5 | performance pass | exploratory | 2.5, 3.3; R4 + 3.1 for the browser-side instrument | not applied |
| R6 | screenshot/overlay coherence | small | 1.2, 2.2 | retired -- B.10 |
| R7 | pixel-perfect bitmap map fonts on the canvas, scaled whole by integers, the scalable font only off `allFontsScale` 1.0 | medium | 0.4; 1.5 for the scale; R4 for a player-chosen fontset | not applied |
| capability constants | named constants in place of behavioral CPP | --- | --- | not applied |
| sum-typed selection | one frontend field replacing four `Bool`s | mechanical, moderate churn | --- | not applied |
| RawFrontend contract | haddock contract, tasty harness, add-a-frontend checklist | medium | haddock and checklist: nothing; native harness after 0.1, which creates its module; wasm case with 0.3 | not applied |
| determinism goldens | fixed-seed frame-and-overlay and final-state digests | medium | --- | not applied |
| frontend CI smokes | xvfb SDL, pty ANSI, a short nodeBench run | small each | 3.3 for the wasm one | not applied |
| explicit widths | `punindex`, never the `Enum` instance, in frontend code | review rule; its one live violation is 2.1's, and the fix adds a `tools/checks.py` step | --- | open -- standing |
| functional core | the standing review bar for frontend modules | review rule; nothing to execute | --- | standing |

## Handing an item to a session

Every item carries, under Items below, the same execution block: four labels
in a fixed order --- **Owns**, **Done**, **Hands back**, **Decide first** ---
each written out even when it has nothing to say, as `nothing`, so
that a missing label is a defect rather than a shrug. A fifth, **Split**, comes
first where it applies and is omitted where it does not; that is the one
deliberate asymmetry, and its absence says the item is a single commit.

- **Split** --- the landing order for an item worked as several commits,
  and which of them carries the outcome line and the `tools/doc-refs-allow.txt`
  deletions --- the record commit being the one that waits on **Hands back**.
- **Owns** --- the files the item writes, exhaustively, so that the `docs` gate
  knows which documents it runs over and a reader knows what a landing touches.
  One item is in flight at a time, in one checkout, so no two items ever write
  a file together and nothing here serializes them; where another item writes
  the same lines, **Owns** says so, since whichever lands first shapes
  the other's diff.
- **Done** --- the gates below that apply, named rather than spelled out, plus
  whatever is this item's own. It is the definition of finished for everything
  a session *can* verify.
- **Hands back** --- the acceptance no session can perform, opening
  with the word `display`, `browser` or `judgement`, followed by the substitute
  gate that *is* in **Done**. An item whose **Done** covers its whole acceptance
  says "hands back nothing", and the label exists so that the rest cannot
  be mistaken for one of those. The outcome line waits on what this label hands
  back: a session runs **Done**, reports the handed-back check as outstanding,
  and stops. A green **Done** is evidence about one commit rather
  than the judgement that an item is finished: it does not flip a row whose
  remaining evidence is a human's, any more than it does on a partial landing
  --- and where **Split** applies, the line waits for the commit **Split** names
  as well.
- **Decide first** --- the questions an executing session must not answer
  for itself, with their branches. `nothing` means the item is ready to hand out
  as written.

**The gates, once.** Every **Done** is built from these, run from the repo root.
They are spelled out here and nowhere else: thirty-odd items repeating one
command chain is the second definition G1 forbids, and the chain that drifts
is the one nobody re-reads.

```
native   cabal build && cabal test && hlint .
         && git add <every .hs the item's Owns names>   # new files diff too
         && stylish-haskell -i <those paths>
         && git diff --exit-code <those paths>     # stylish left them alone
ts       make test-ts        # npm run typecheck runs inside the target
wasm     make build-wasm && make test-wasm  # UNSANDBOXED after 0.3's npm ci
deploy   make build-ts        # UNSANDBOXED: writes into the pages checkout
page     make page-gate       # UNSANDBOXED: runs npm ci; the page as
         deployed, in headless Firefox: boots, paints, repaints on a key;
         PAGE_GATE_OVERLAYS=1 from 2.4 on
docs     the repo's standing document passes over every .md document
         the item's Owns names, the record and this file always among them; CLAUDE.md
         lists the passes and this document does not; what they cost is
         the paragraph below
```

**What `docs` costs, and when.** The mechanical passes are seconds, and they run
over each document the item's **Owns** names and not only over this one, since
a hand-reflowed paragraph or a broken anchor in another document is invisible
to every pass over this file. The one that is not seconds is `CLAUDE.md`'s
reading pass, and it is owed over the record and this file, end to end, once per
landing, after the item's last edit to this file; the commits before it run
the mechanical passes. The ledger's size cells size the code, so this cost
is on top of every one of them, and it is the largest single thing a `small` row
asks for.

**The page gate**, ruled 2026-09-14 by Mikolaj: local only, run before every
browser landing, and never a CI job --- CI stays blind to the page
by that choice, R2's ruling against a `build-ts` job standing. `make page-gate`
builds the page as `build-ts` would deploy it into a scratch directory,
and `ts-src/page-gate.mjs` serves it, loads it in headless Firefox
over WebDriver BiDi and asserts that frames arrive after `lhStart`, that a `?`
changes the screen and an Escape changes it again once it is quiet, and, once
2.4 lands, that an overlay is submitted. It observes the page by wrapping
the `globalThis` hooks the loader installs and by hashing `#screen`, a canvas's
data URL where one exists, so 0.4 owes it no change. Its two controls, run
the day it was written: a site missing `LambdaHack.wasm` fails both frame
assertions, and `PAGE_GATE_OVERLAYS=1` fails before 2.4; and, run 2026-10-21,
an `lhKey` without its scheduler run fails the quiet-screen assertion. It
is the campaign's acceptance test: what a human still walks at 2.5 is the look,
not whether the page runs.

**Read the counts, not the exit status alone** --- `CLAUDE.md`'s rule, binding
here too: a suite that silently loses a test still passes, and a listing is what
settles a count claim. Never pipe one, or a **Done** line: the trap bullet below
has why.

**Tasty tags are a repo-global namespace, shared with the pointman campaign.**
`-p` matches by containment with no anchor available; `~/slop/CLAUDE.md`'s tasty
bullet carries both halves, the anchor's two failure modes and containment,
and `test/CLAUDE.md` shows containment on `LR1`, which selects LR1 together
with LR10 through LR13. A tag is therefore claimed by containment,
not by equality, and a marker that merely *contains* another campaign's is
not a fresh claim but a silent join. `[contract]` and `[LR-flip]`
are the pointman campaign's, and `docs/leader-desync-migration.md` pins the size
of each series step by step, so a marker of ours falling inside either filter
moves a count that document treats as a finding. This campaign claims two
series: `[fe-invariant]` for the frontend contract cases, and `[fe-golden]`
for the determinism goldens, whose bare `golden` would be an ordinary English
word inviting exactly the containment this paragraph is about. A new series here
claims a marker containing no other campaign's, and the minting item owes two
proofs, one per direction, because the listing settles only one of them.
Freshness: `cabal test --test-options='--list-tests -p "/<marker>/"'` must
select nothing before the series exists, which is the same command that reports
a count for a marker that does exist and so is not a vacuous check. Containment:
the marker must not *contain* `contract` or `LR-flip`, which no listing can
show, since it reports what existing names contain the marker and never what
the marker contains --- a literal substring test of the candidate against
those two, named in the item's **Done** beside the listing. `[fe-contract]`
is why both are stated: it listed clean before its series existed and sat inside
`/contract/` all the same, which the 2026-07-30 Log entry records. `[frontend]`
is not available, incidentally: it is already a substring of test names
the suite carries. Reusing --- or containing --- a tag breaks two things at once
and neither loudly: the other campaign's cardinality invariant,
and this campaign's gate, which would otherwise pass by selecting tests it did
not write.

The logistics live at the item and nowhere else. A central table of owned files,
gates and open questions was proposed and is ruled out: it would duplicate every
item's name and half its body; it would drift from the bodies the first time
an item was edited without it, in the direction that reads as authoritative;
and, decisively, it could not freeze. The maturation rule is per item ---
a landed item's logistics are frozen history like the rest of it --- whereas
a table's rows would go on sitting beside live ones, current-looking long after
the commits that consumed them, which is exactly the aged record the freeze
callout at the top of this document exists to prevent. Only that last reason
turns on this document rather than on tables, so a table is right only where
the whole plan is deleted when the work lands and no row outlives its body ---
which is the pointman campaign's sec. 00 and not this file. The other two
reasons bind there too.

**Owns** is a checked field, not prose. `../tools/check-doc-refs.py` resolves
the backticked paths in this document, so naming an artifact the item has
not built yet fails the run until the file exists; such a name carries an entry
in `tools/doc-refs-allow.txt`, with its reason, alongside the entries the item
bodies already need. The existing landing gate closes the loop: an item
that proposes an artifact takes its outcome line only once its allowlist entries
can be deleted, and **Owns** is what makes them findable from the commit
that lands it. Every such item therefore writes `tools/doc-refs-allow.txt`
and carries it in **Owns** by that rule rather than by enumeration --- most
items that build anything write it, and the one-holder-at-a-time discipline
binds it as it binds `Sdl.hs`. Deleting the entry is the executing session's act
and not something a checker will prompt: `../tools/check-doc-refs.py` tries
`resolves` before `is_allowed`, so the moment the artifact exists its token
prints `ok path` and the stale entry is never read again --- which is why
the allowlist's own header asks for the deletion by hand, and why a survivor
means a proposed artifact was never built rather than a check that failed.

Two traps have each already produced a wrong result in this repo
or this campaign, so a session is told both rather than trusted to know them.

- A **Done** line piped into `tail` or `head` reports the *pipe's* exit status,
  so a failed build reads as success --- which is how this campaign recorded one
  false positive already. **Done** lines are `&&`-chains, never pipelines; long
  output is scrolled, not trimmed.
- A green `../tools/check-plan-citations.py` proves that every anchor is found
  on exactly one line, not that the line still says what the claim needs,
  so the snippets it prints are re-read after any commit that rewrites a cited
  line, and **an item whose commit rewrites such a line owes a re-anchoring,
  a `.md`-only commit after the landing**, named by the pass rather than
  by the item. Until 2026-09-14 the citations were line numbers, which a commit
  above them slid onto other text with the pass still green, and every item owed
  a repair commit for it. A correction of a claim the landing falsified stays
  *separate* from the landing, as the re-anchoring does, so review can tell
  a relocation from a correction: commit `a7e825c2d` could not, having rewritten
  R2 from "Add a job to..." into "Partly landed, in..." and bent a factual claim
  in the same diff. That reaches frozen sections and `landed` items too:
  an anchor a rewrite broke is re-anchored and the prose around it left
  as it stands.

**One item is in flight at a time, in one checkout.** The hot files are small
--- a hundred to a couple of hundred lines each, `GameDefinition/index.html`
with no automated gate on it at all --- and nearly every phase writes them,
several items rewriting the same four-line `startup` body in `Wasm.hs`, so two
items in one of them are two items in the same paragraph, and the locks
and claimant lists this plan carried until 2026-09-14 could not have made them
concurrent. What the campaign did parallelize was upstream of the code,
on the **Decide first** questions, each blocking an item that was otherwise
ready to hand out.

Worktree isolation helps exactly where a collision would be loud: a new module
no one else has a copy of (0.1's `InputDecision`, 2.1's `OverlayLayout`),
and A/B builds wanting two trees and two binaries at once. It is actively
harmful on this plan's central risk. Git never reports a conflict between
`Sdl.hs` and `Wasm.hs`, or between either and `terminal.ts` --- they
are different files --- so two isolated sessions each fixing the same behavior
in its own frontend, differently, merge clean and produce precisely
the near-duplicate drift this plan exists to eliminate. Isolation also removes
the one mechanism that catches it: diffing analogous positions in a single tree.
Parity work stays in one checkout.

---

## Items

Each item's design and rulings are in the record, under the same heading; what
is here is how it lands.

## Phase 0 --- shared foundations

### 0.0 Immediate fixes (ship before anything else)

**Split** --- the jsdom harness first (`ts-src/src/terminal-input.test.ts`,
`npm i -D jsdom`, a per-file `// @vitest-environment jsdom` docblock), which
also deletes its own `tools/doc-refs-allow.txt` entry as it creates the file,
then the two AltGraph fixes as **one** commit, with the highlight fix
concurrent. The three fixes are one ledger row; the outcome line and the ledger
flip ride whichever of the two fix commits lands last, after a citation-repair
commit of its own: the AltGraph fix rewrites
`terminal-core.ts:export function altModifier`, cited here, so the sentence
citing it is reworded rather than re-ranged, and the restamp rides that commit;
the highlight fix rewrites one entry of `terminal-core.ts`'s
`HIGHLIGHT_TO_COLOR` table, which this document does not cite ---
its `terminal-core.ts:Cell background-fill` is `bgColor`'s fill, which the fix
leaves standing --- so that half owes no repair.

**Owns** --- `ts-src/src/terminal.ts`, `ts-src/src/terminal-input.test.ts`,
`ts-src/src/terminal-core.ts` and its test, `ts-src/package.json`
and `package-lock.json`, `docs/wasm-frontend-unified-plan.md` --- the AltGraph
commit rewrites `terminal-core.ts:export function altModifier`, cited here,
so this item owes the citation-repair commit and its restamp ---
and `tools/doc-refs-allow.txt`, whose `ts-src/src/terminal-input.test.ts` entry
the harness commit deletes as it creates the file.

**Done** --- `ts`, `docs`.

**Hands back** --- *browser*: one AltGr chord on a European layout and one
`A-MiddleButtonRelease` with AltGr held, since jsdom synthesises
`getModifierState` and so proves forwarding, not that the browser reports it.
The highlight rule hands back nothing: the vitest case pins it.

**Decide first** --- nothing.

### 0.1 `InputDecision`: one Haskell brain for keyboard/mouse decisions

**Split** --- a re-spike, then five commits. (0) the sync-export re-spike:
a throwaway probe against the current toolchain proving the sync
`foreign export javascript` compiles and coexists with the reactor's
`-optl-Wl,--export=` wiring and the command-linked test suite --- the earlier
validation left no record of its syntax, which is what this step writes
into this item's body; no code lands. If it fails, land the first two commits,
leave `lhKey` async with `preventDefault` staying in TS, and record the ruling.
Performed 2026-08-07: the record sits in the record's `Wasm.hs` changes bullet,
and it held, so the fallback is dead. Then `InputDecision` and its tasty tests
with no consumer wired; then `Sdl.hs`'s verbatim squash/Esc block
(`Sdl.hs:decideKey (modTranslate ksm) key`, the mouse read between them left
alone) replaced by a `decideKey` call that ignores `kdPreventBrowser` ---
the smallest consumer, native, no FFI; then the jsdom harness,
`ts-src/package.json` and its lock gaining jsdom
and `ts-src/src/terminal-input.test.ts` pinning today's forwarding,
its `tools/doc-refs-allow.txt` entry deleted as the file appears;
then the input-side contract cases, written to pass on today's code so
that the rewrite must keep them green, creating
`test/FrontendContractUnitTests.hs` with its registration, minting
`[fe-invariant]` and deleting the module's `tools/doc-refs-allow.txt` entry
as the file appears; then the `lhKey` rewrite with `terminal.ts`, `loader.ts`,
the forwarding tests' new expectations and `ts-src/run-wasm-game.mjs`'s `lhKey`
injection with its coverage case in the same commit and nothing else in it,
as the ground rules require. The `Sdl.hs` commit and the rewrite are disjoint;
they are ordered so that the signature change arrives alone. The outcome line,
and the deletion of `Client/UI/Frontend/InputDecision.hs` and of this item's new
`test/InputDecisionUnitTests.hs` entry from `tools/doc-refs-allow.txt`, ride
the rewrite.

**Owns** --- new
`engine-src/Game/LambdaHack/Client/UI/Frontend/InputDecision.hs`
and `test/InputDecisionUnitTests.hs`; `LambdaHack.cabal` and `test/Spec.hs`
for their registration; `engine-src/Game/LambdaHack/Client/UI/Frontend/`'s
`Sdl.hs` and `Wasm.hs`; `engine-src/Game/LambdaHack/Client/UI/Frontend.hs`,
its export list only, carrying the re-export the record's ruling (2) requires;
`engine-src/Game/LambdaHack/Client/UI/Key.hs`, one clause in `keyTranslateWeb`,
which no other item writes, `Key.hs:keyTranslateWeb ::` naming the function's
head; `ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`ts-src/src/terminal-input.test.ts`, and `ts-src/package.json`
with `package-lock.json` for jsdom; `ts-src/run-wasm-game.mjs`, 0.3's game
driver, for the `lhKey` injection and its coverage case;
`tools/doc-refs-allow.txt`; `docs/wasm-frontend-unified-plan.md`, the `Sdl.hs`
commit rewriting the block cited here and so owing a re-anchoring; and,
in a commit of its own after the landing, `CLAUDE.md`'s frontend bullet, whose
first module name this item delivers. It owns nothing
in `docs/promptgetkey-hygiene.md`: that record's sec. 01 was rewritten
onto `FrameM`'s pure section on 2026-09-04, when this item's own ruling (2),
in the record, made `InputDecision` an import cycle for `macroStep`, so there
is no callout left for a landing to retire. Nothing else. The input-side
`RawFrontend` contract cases the practice assigns here create the new
`test/FrontendContractUnitTests.hs`, the contract's one module (its ruling (2)
in the record), in the commit of their own the Split places; the contract's
harness extends it. 0.0 lands first or is dropped, since the `lhKey` commit
rewrites the very listener it fixes. Four items edit one sentence
of `CLAUDE.md`: 0.1, 0.2 and 2.1 for the frontend bullet's three module names,
2.1 again for the Gotchas bullet's "awaits its scheduled fix", which its fix
of `Sdl.hs:let Point{..} = toEnum i` falsifies, R3 for the GHCJS mentions,
and the pointman campaign's 04.4 for the sentences that campaign's landing
falsifies, enumerated in its head callout. Where the edit rides is each item's
**Split** to say, and they differ on purpose: 2.1 folds it into the re-anchoring
commit that follows its `Sdl.hs` rewrite, R3 into the rip-out whose deletions
the reworded sentences describe, and 04.4 into the deletion commit that resolves
the clause.

**Done** --- `native`, `ts`, `wasm`, `page`, `docs`, plus a run
of `ts-src/run-wasm-game.mjs` against the fresh reactor, injecting `lhKey`
with its new signature, and the `[fe-invariant]` marker's two proofs,
as the contract's Done states them, for the module this item creates.

**Hands back** --- *browser*: `preventDefault` from Haskell is the point
of the change and no headless gate reaches it --- Tab not moving focus,
`C-+`/`C--`/`C-0` still zooming the page, Esc, AltGraph chords, right-click
and wheel, after an unsandboxed `make build-ts` and a background
`make serve-wasm`. The substitute in Done is the game-driver run carrying 0.3's
export coverage, which is the stated reason 0.3's baseline is sequenced before
this item, plus the jsdom forwarding cases. The `Sdl.hs` commit hands back
a *display* look as well (`make frontendCrawl`): `cabal test`'s SDL cases
exercise init-and-quit and font decoding, never the event loop where `decideKey`
now sits.

**Decide first** --- nothing; the rulings are the record's.

### 0.2 `CellStyle` + generated TS tables and fixtures

**Split** --- three commits. `CellStyle` plus its tasty tests, consumed
by nobody; then the freshness case, the committed `ts-src/src/generated/` files
with their `extra-source-files` lines, and `terminal-core.ts` consuming
the generated palette and asserting `styledCell` against every fixture;
then the crosshair commit the record's ruling (4) requires --- the ASCII-art
alpha/BW pair out of `Sdl.hs` into an SDL-free pure module of its own
under `engine-src/Game/LambdaHack/Client/UI/Frontend/`, `cursorXhair` reading
it, and `cursor.ts` generated from it, which is also 1.1's final form.
`Sdl.hs`'s drawing loop adopts `CellStyle` in 2.1, on the branch How this lands
reviews alone. The outcome line, and the deletion of this item's
`tools/doc-refs-allow.txt` entries --- `CellStyle.hs`, `palette.ts`,
`fixtures.json`, `cursor.ts`, `ts-src/src/generated/`,
`test/CellStyleUnitTests.hs`, and the crosshair module's, added under whatever
name the executing session mints for it and before that name is written anywhere
--- ride the crosshair commit, the last of the three.

**Owns** --- new `engine-src/Game/LambdaHack/Client/UI/Frontend/CellStyle.hs`,
the crosshair-art module beside it that the record's ruling (4) requires,
`test/CellStyleUnitTests.hs`, `ts-src/src/generated/palette.ts`,
`ts-src/src/generated/fixtures.json` and `ts-src/src/generated/cursor.ts`;
modified `LambdaHack.cabal`, `test/Spec.hs`, `ts-src/tsconfig.json`
(`resolveJsonModule`, without which importing `fixtures.json` fails
`tsc --noEmit`), `ts-src/src/terminal-core.ts` and its test,
`ts-src/src/terminal.ts`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`, which the crosshair
commit alone writes, `engine-src/Game/LambdaHack/Client/UI/Frontend.hs` (export
list only, re-exporting both new modules per 0.1's ruling (2) in the record),
`tools/doc-refs-allow.txt`, `docs/wasm-frontend-unified-plan.md`,
for the outcome line and whatever re-anchoring the pass names after
the crosshair commit rewrites `cursorXhair`, and, in a commit of its own after
the landing, `CLAUDE.md`'s frontend bullet, whose second module name this item
delivers. The generated files are **committed**, not ignored: they are what
the freshness case compares against and what `make build-ts` bundles. The three
commits are strictly serial: each consumes what the previous adds.

**Done** --- `native`, `ts`, `wasm`, `docs`, plus the freshness case's two
proofs --- one byte changed in a committed copy fails `cabal test`
and the regeneration switch restores it, and `tar tzf` over `cabal sdist`'s
tarball lists the three copies.

**Hands back** --- *browser*, for `cursor.ts`: pixel parity against the native
pointer is the claim and only an eye settles it. Done's substitute
is the fixture equality between `CellStyle.styleCell` and `styledCell`, which
is the whole correctness claim.

**Decide first** --- nothing; the rulings are the record's.

### 0.3 FFI-coverage test (baseline first, then per-commit)

**Split** --- the import half first: the declarations the suite reaches
in `Wasm.hs`, `WasmFile.hs` and `GameDefinition/Main.hs` --- `WasmFile.hs`'s
six, reachable only through `Common/File.hs`'s seven exported functions since
`WasmFile` is a hidden other-module, and `Wasm.hs`'s `js_submitFrame`, reached
by `Frontend.Wasm.startup` plus `fdisplay` on a constructed `SingleFrame` ---
together with the JS stubs (`globalThis.localStorage`, `globalThis.LZString`,
`globalThis.lhSubmitFrame`) that `run-wasm-test.mjs` must install before
`wasi.start`, `LZString` being the real library, a devDependency since the page
gate and never the sibling checkout's `lz-string*.js`, a deployed artifact,
rather than an identity stub, so that the save path runs the compression
it ships with. The same commit adds the `npm ci` that installs it
to the `test-wasm` recipe and to CI's `test-wasm` job, so `make test-wasm` wants
an unsandboxed call from then on. Then the export half, its mechanism ruled
2026-08-07: a thin `ts-src/run-wasm-game.mjs` --- 3.2's seed, created here
and grown there --- that instantiates the real reactor binary
from `make build-wasm` with the same stubs, `wasi.initialize`s it, calls
`lhStart` and injects `lhKey`/`lhWheel`/`lhMouseUp`, asserting frames arrive;
no argv is needed, defaults suffice for coverage. That covers the reactor
linkage and `lhStart` itself, which no branch inside the command-linked test
suite ever can, and the commit deletes the `run-wasm-game.mjs` entry
from `tools/doc-refs-allow.txt` as the file appears. The standing half --- every
new FFI declaration lands with its coverage case --- is a review rule,
not a commit; by its own size cell 0.3 retires with the campaign rather
than acquiring a hash, so it takes no outcome line. Its two
`tools/doc-refs-allow.txt` entries therefore leave with the commits that create
their files rather than with a landing --- `test/FfiCoverageUnitTests.hs`
with the import half, `run-wasm-game.mjs` with the export half --- and they
are the two artifact deletions in this plan that no outcome line gates.

**Owns** --- `ts-src/run-wasm-test.mjs`, the new `ts-src/run-wasm-game.mjs`
(created here as 3.2's seed; 3.2 grows it into the benchmark driver), new
`test/FfiCoverageUnitTests.hs`, `test/Spec.hs`, `LambdaHack.cabal`,
`tools/doc-refs-allow.txt` (the `test/FfiCoverageUnitTests.hs`
and `run-wasm-game.mjs` entries), the `Makefile`'s `test-wasm` recipe
and the `test-wasm` job of `.github/workflows/lint-and-test-suites.yml`,
for the import half's `npm ci`, `docs/wasm-frontend-unified-plan.md`, the import
half rewriting the driver's wiring cited here, and, in a commit of its own after
the import half, `CLAUDE.md`'s session fact on which wasm targets need
an unsandboxed call. Nothing else. The two commits are serial: the export half's
driver imports the setup and stubs the import half installs --- defined once,
shared by both drivers rather than copied. Later items add to the game driver,
0.1, 0.4, 1.2, 1.5 and 3.2 each their own coverage case, and to the test driver,
2.3 its case and 1.5 its startup hook's stub.

**Done** --- `native`, `wasm`, `docs`, plus, on the export half,
`make build-wasm` and a run of the seeded driver against the fresh reactor,
nonzero on a missing export or an absent frame.

**Hands back** --- *judgement*, and only for the standing half: a reviewer
confirms each FFI-touching commit carries its case. The substitute in Done
is `make test-wasm`, which fails when a covered declaration breaks but never
when an uncovered one is added. The cheap mechanization --- a checker asserting
that the count of `foreign import/export javascript` declarations equals
the battery's case count --- is **refused** rather than merely unproposed,
and refused on what it would actually prove: two equal counts pin cardinality
and not correspondence, so a declaration added in the same commit as a case
deleted passes it; and the case count has to be declared somewhere
for the checker to read, which puts a hand-maintained number in place
of the coverage it claims to assert. Revisit if a trigger fires: an uncovered
FFI declaration reaches master; the declarations outgrow what a reviewer checks
in one reading; or the case count becomes derivable from the battery itself
rather than declared beside it, which retires the second objection. The two
executable halves hand back nothing.

**Decide first** --- nothing; the rulings are the record's.

### 0.4 Canvas painter: one renderer for the browser

**Split** --- (0) the measurement B.10 could not make from the armchair:
the painter as a throwaway on this item's branch, driven under the protocol
at Phase 2's intro, p95 against the recorded 3.00ms in the same Xvfb setup, plus
one `xwd` frame beside the DOM build's for the glyphs; the number
and the probe's canvas diff go into B.10 before (1) merges, and are the baseline
2.5 re-measures against. Then (1) the painter, `index.html`'s `#screen` rule
for the canvas, the op-list vitest cases, `fullscreen.ts` unchanged
in interface, and the half-cell mouse column through `loader.ts`
into `Wasm.hs`'s `lhWheel` and `lhMouseUp`, their types unchanged and their
first argument's meaning changed, with the game driver's coverage case pinning
it; and (2) the outcome line and the ledger flip.

**Owns** --- `ts-src/src/terminal.ts` and its forwarding tests
in `ts-src/src/terminal-input.test.ts`, `ts-src/src/terminal-core.ts`
and its test, `ts-src/src/fullscreen.ts` (its transform applies to the canvas
now), `GameDefinition/index.html` (the `#screen` rule, the banner untouched),
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs` (`lhWheel`
and `lhMouseUp`), `ts-src/src/loader.ts`, `ts-src/run-wasm-game.mjs`
for the coverage case, and `docs/wasm-frontend-unified-plan.md` --- Split (0)'s
number and probe in B.10, the outcome line, the row, and the Repo facts bullet
on how the frontend works today, which this landing re-cuts.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`, `docs`, plus Split (0)'s
number standing in B.10 before (1) merges, and a run
of `ts-src/run-wasm-game.mjs` against a fresh reactor injecting `lhWheel`
and `lhMouseUp` at an odd half-cell column.

**Hands back** --- *display*: that the map on the canvas reads as the DOM
build's at scale 1.0, the `xwd` pair of Split (0) being a session's to take
and a human's to judge, and one AltGr click at half-cell precision.
The substitute gates in Done are the op-list cases and the page load.

**Decide first** --- nothing; the rulings are the record's.

## Phase 1 --- small parity wins

### 1.1 Crosshair cursor over the map (trivial)

**Split** --- two commits, in this order: the `crosshair` keyword line first,
then the generated SVG data-URI form once 0.2 has emitted `cursor.ts`.
The keyword commit takes **no** outcome line and leaves the row live, per
the partial-landing rule; the SVG commit carries the outcome line and flips
the ledger row. Neither deletes a `tools/doc-refs-allow.txt` entry ---
`cursor.ts` and `ts-src/src/generated/` were 0.2's, and 1.1 is ungated.
The keyword commit landed in `91d75ca5e` and the SVG commit in `0a72de79c`.

**Owns** --- `ts-src/src/terminal.ts`, and, in the landing commit only,
`docs/wasm-frontend-unified-plan.md`. `../lambdahack.github.io` is redeployed
by **Done**, not owned.

**Done** --- `ts`, `wasm`, `deploy`, `page`, `docs`, plus
`grep -qF 'data:image/svg+xml' ../lambdahack.github.io/bundle.js`.

**Hands back** --- *display*: that the pointer reads as a crosshair
over the map, and, for the SVG form, that the browser accepted the data URI
rather than falling back to the `crosshair` keyword --- a failure
with no console error, no failing test and no trace in the bundle.
The substitute gate in **Done** is the `grep` over the deployed `bundle.js`,
which proves the string shipped and nothing about how it painted.

**Decide first** --- nothing.

### 1.2 Screenshots: make `Ctrl+P` real (small)

**Split** --- two commits. (1) The filename helper plus its tasty test,
with `Sdl.hs` switched onto it: native only, no TS and no wasm in the diff. (2)
The wiring: `Wasm.hs`'s `js_printScreen` import and its `fprintScreen` override,
`lhPrintScreen` in `loader.ts`, the capture in `terminal.ts` --- 0.4's canvas
by `toBlob`, after the pending frame's paint when one is pending ---
with its `<a download>` click, and 0.3's coverage case
in `ts-src/run-wasm-game.mjs` --- the reactor driver installs
a `globalThis.lhPrintScreen` stub, injects `Ctrl+P` through `lhKey` and asserts
one call with a filename of the expected shape, then runs again
under `--printEachScreen`, the driver passing its extra arguments through
as WASI argv, and asserts one call per frame. (2) needs (1), 0.4, 0.3
for the reactor driver and 3.1 for the `--printEachScreen` run's argv,
and carries the outcome line and the ledger flip.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`
and `Wasm.hs` beside it, the helper's home module
under `engine-src/Game/LambdaHack/Client/UI/Frontend/`,
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs` for the re-export that module
reaches its consumers through (0.1's ruling (2) in the record, which binds every
new module here), a new tasty module registered in `test/Spec.hs`
and `LambdaHack.cabal`, `ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`ts-src/run-wasm-game.mjs`, whose argv pass-through is this item's unless 3.2
lands first, and `docs/wasm-frontend-unified-plan.md`. 1.5 writes the same lines
--- `startup`'s body and the `foreign import javascript` block in `Wasm.hs`,
`loader.ts`'s `declare global` and its `globalThis.lh*` wiring, `terminal.ts`'s
`Terminal` interface and returned object, and the driver --- so whichever
of the two lands first shapes the other's diff. `../lambdahack.github.io`
is redeployed by **Done**, not owned.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`, `docs`, plus both runs
of `ts-src/run-wasm-game.mjs` against a fresh reactor, as 1.5's Done says
of its own: `make test-wasm` drives the other driver, where nothing calls
`fprintScreen`.

**Hands back** --- *display*: press `Ctrl+P` in a browser and see a PNG download
under the helper's name, showing the screen. The PNG is the painter's own
backing store, and whether the fonts loaded is 0.4's to check, its painter
awaiting `document.fonts.load` before the first paint. The substitute gates
in **Done** are the tasty test pinning the filename against a fixed time
and the two reactor-driver runs, which are what executes `Wasm.hs`;
`make test-wasm` adds nothing here: nothing it runs calls `fprintScreen`,
so an undefined `globalThis.lhPrintScreen` would go unnoticed there.

**Decide first** --- nothing; the rulings are the record's.

### 1.3 Fullscreen toggle with scaling (small, no Haskell)

**Split** --- two commits. The button, the scaling helper and its vitest,
with `loader.ts`'s one import and call; then the banner commit this item's
landing owes under 1.4's rule, the "fullscreen" clause coming off
`GameDefinition/index.html` with only that phrase in the alternation. The second
carries the outcome line and flips the ledger row; the two
`tools/doc-refs-allow.txt` entries leave with the first, which creates the files
they name.

**Owns** --- `GameDefinition/index.html`, `ts-src/src/fullscreen-core.ts`
and `ts-src/src/fullscreen-core.test.ts`, the one import and one call they need
in `ts-src/src/loader.ts`, `tools/doc-refs-allow.txt` (its two entries leave
with the commit that creates the files),
and `docs/wasm-frontend-unified-plan.md` --- the import shifts
`loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`, cited twice,
so this item is not citation-safe either. `../lambdahack.github.io`
is redeployed by **Done**, not owned.

**Done** --- `ts`, `wasm`, `deploy`, `docs`, plus
`grep -qF Fullscreen ../lambdahack.github.io/index.html` &&
`grep -qF fullscreenchange ../lambdahack.github.io/bundle.js` and, on the banner
commit, `! grep -qE 'fullscreen' ../lambdahack.github.io/index.html` --- which
bites only if the button's markup spells the word nowhere in lower case,
so the label is capitalised and no id, class or attribute of the button may
be `fullscreen`; a session that wants one renames it rather than weakening
the grep.

**Hands back** --- *display*: that the page really enters fullscreen,
that the scaled grid is centred and aspect-correct rather than clipped,
and that bitmap blur at non-integer scales is acceptable. Doubly so ---
`requestFullscreen()` demands a *trusted* user gesture, so no page script
and no `element.click()` can raise it, and even a browser-automation channel
would need a synthetic trusted click. `#screen` is laid out
`width: max-content; margin: 0 auto`, so a `transform: scale()` grows
the painted box while the layout box stays where it was; whether that reads
as centred is exactly the part no gate sees. The substitute gates in **Done**
are the vitest assertion on the pure scale arithmetic and the two greps
over the deployed page and bundle --- and note that `make build-ts` merely `cp`s
`index.html`, so malformed HTML deploys with everything green.

**Decide first** --- nothing; the rulings are the record's.

### 1.4 Banner truth maintenance (trivial, recurring)

**Split** --- recurring by design: one commit per capability landing, each
deleting the clause its feature falsified *and repairing the sentence around
it* --- 1.3 for "fullscreen", off since `d211caa38`, 2.4 for "proportional
fonts" and for the `#status` div's and the README's "limited to the square
font", R1 for the savefile-corruption clause
(`[Ss]avefiles are prone to corruption`, the bracket for the README's lower
case), R5 for "runs rather slowly" once its bars are cleared. One commit belongs
to no landing and can be taken today: "and persistent save games" is already
false --- persistence works, via `WasmFile.hs`, and Out of scope says so ---
so no feature will ever falsify it and nothing else in this Split reaches it.
Each runs the **Done** line below with only its own phrase in the alternation.

Delete-only is not enough, and the repair belongs here rather than to three
sessions inventing it separately: every clause above sits inside a carrier
sentence that stops parsing without it. Both carriers, with their end states.
The banner's (`GameDefinition/index.html:proportional fonts`) is a list
of reasons to prefer the native binary, so it shortens as items go --- three
with an Oxford comma, then two joined by "and", then one with neither ---
and when the last goes the whole "For ... use the native binary instead."
sentence goes with it, its `<a>` to the releases page included, while
"This is the experimental wasm build." and everything from "If the game window
is too large" onward stay untouched throughout. The quoted README paragraph
(`GameDefinition/index.html:The game runs rather slowly`, and in `README.md`
the paragraph under the heading "Installation of the sample game from binary
archives") is three claims supporting one conclusion, "Hence, after trying out
the game, you may prefer to use a native binary". Its first two share one
sentence in both files --- "The game runs rather slowly in the browser (fastest
on Chrome) and you are limited to the square font ..." --- so R5's clause
and 2.4's are not independently deletable: whichever lands first deletes its own
clause together with the "and you" that joins them and re-opens the survivor
as a sentence of its own, "The game runs rather slowly in the browser (fastest
on Chrome)." or "You are limited to the square font for both map and texts."
on the page and "You are limited to the square font for all purposes, though
it's scalable." in `README.md`, and whichever lands second deletes that sentence
whole. The third, R1's, hangs off an "Also," in the README, and the rule
that the word moves to whichever claim then stands second never fires on it:
it is last of the three and stays last while either earlier claim survives,
so what the one case that reaches it wants --- both earlier claims gone,
this one standing first --- is the word deleted rather than moved. When the last
claim goes the conclusion goes with it, taking the div's whole "from README:"
block and leaving the README's section to open on "Pre-compiled game binaries
are available through the release page". No commit carries an outcome line
and the ledger row never flips: 1.4 is a standing item and retires
with the campaign. It deletes no `tools/doc-refs-allow.txt` entry.

**Owns** --- `GameDefinition/index.html`, `README.md` (the browser paragraph
the `#status` div quotes, and nothing else in that file),
and `docs/wasm-frontend-unified-plan.md` wherever this item's own body quotes
the banner: 1.4 is `open -- standing`, so a quote that no longer matches
the page is an error to fix, not drift to leave alone. Every item that writes
`index.html` writes the banner or the `#screen` rule beside it --- 0.4, 1.3, R4,
R5 and the banner commits of 2.4 and R1 --- and the four that touch the quoted
paragraph write `README.md` too: this item, 2.4, R1 and R5.
`../lambdahack.github.io` is redeployed by **Done**, not owned.

**Done** --- `wasm`, `deploy`, `page`, `docs`, plus
`! grep -qE 'proportional fonts|limited to the square font' ../lambdahack.github.io/index.html`
&& `U=$(mktemp)` && `wrap80 --unwrap README.md > "$U"` &&
`! grep -qE 'limited to the square font' "$U"`, each page's own phrases --- both
on the deployed one, so a forgotten `make build-ts` fails the first,
and the square-font one on the source `README.md`, which no build deploys.
The README half greps an unwrapped copy rather than the file: `wrap80` keeps
that file wrapped, so a deletion earlier in the paragraph reflows what remains
and a later landing's line-oriented grep passes over a clause that is still
there, split across two lines --- and that silence reads exactly like success.
An `&&` chain and never a pipeline, per the trap bullet below: piped,
the negation would report the grep's status whatever `wrap80` did. The `[Ss]`
bracket R1's **Split** explains guards the other axis, case; a phrase can
be missed for either reason and each guard covers one.

**Hands back** --- *judgement*: whether the replacement sentence is true
of the shipped build. No display is needed and no command decides it --- a grep
proves a stale phrase is gone, never that what replaced it is accurate,
and the `#status` div carries two further claims of the same kind whose truth
is owned by 2.4 and R1 rather than by whoever edits the page. The substitute
gate in **Done** is exactly that absence grep, run over the deployed
`index.html` rather than the source, so a forgotten `make build-ts` fails it ---
and over `README.md`, which is source because no build deploys it.

**Decide first** --- nothing; the rulings are the record's.

### 1.5 `allFontsScale` in the browser, alongside browser zoom

**Split** --- two commits. (1) the startup channel and the scale it carries:
`Wasm.hs` binding the `ClientOptions` its `startup` discards today, `loader.ts`
wiring, `terminal.ts` handing the scale to 0.4's painter, which re-derives
its cell from the font loaded at that size, so the scale survives every
dimension change, and 0.3's coverage case in `ts-src/run-wasm-game.mjs`,
the reactor driver, asserting the payload arrives, with a stub for the startup
hook among the stubs `ts-src/run-wasm-test.mjs` installs, since 0.3's battery
there runs `Frontend.Wasm.startup` and would trap on an undefined hook. (2)
the outcome line naming (1)'s hash and the ledger flip: 1.5's player-facing half
belongs to R4, so withholding it is not a partial landing of this item. Neither
commit deletes a `tools/doc-refs-allow.txt` entry.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`ts-src/src/loader.ts`, `ts-src/src/terminal.ts`, `ts-src/run-wasm-game.mjs` ---
the reactor driver 0.3's export half seeds and 3.2 grows ---
`ts-src/run-wasm-test.mjs` for the startup hook's stub,
and `docs/wasm-frontend-unified-plan.md`. It does not write
`GameDefinition/index.html`, whose `#screen` rule 0.4 rewrote for the canvas.
1.2 writes the same lines in the four shared files, so whichever lands first
shapes the other's diff. No ordering against 1.3 survives its ruling:
that helper measures the live screen and assumes no cell size.
`../lambdahack.github.io` is redeployed by **Done**, not owned.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`, `docs`, plus a run
of `ts-src/run-wasm-game.mjs` against a fresh reactor, the one thing
in this list that executes the coverage case the Split adds: `make test-wasm`
drives the other driver, where the startup hook is only stubbed.

**Hands back** --- *display*: that the map renders larger. The item body says
how to check nothing --- alone among 1.1, 1.2 and 1.3, whose bodies each name
a check, labelled `Verify:` at 1.2 and in plain prose at the other two ---
and the gap is not one a browser channel would close, because a session cannot
even *produce* the interesting case: `allFontsScale = 1.0` sits
in `config.ui.default`, which is embedded at compile time via `rcfgUIDefault`,
and the browser has no argv and no config file until R4, so exercising any other
scale means a throwaway edit there plus a full wasm rebuild --- an edit
that must not be committed, since it changes native behavior too. The substitute
gates in **Done** are `make test-wasm`, which link-checks the new import string,
and the reactor-driver run, which proves the startup payload arrives carrying
this item's field.

**Decide first** --- nothing; the rulings are the record's.

### 1.6 Game-end page lifecycle (small)

**Split** --- two commits. (1) the code: `loader.ts` keeping `lhStart`'s promise
and marking the page finished or crashed by how it settles. (2) the landing:
the outcome line naming (1)'s hash and the ledger flip. No allowlist entry:
this item proposes no file.

**Owns** --- `ts-src/src/loader.ts` and `docs/wasm-frontend-unified-plan.md`.
It does not write `engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`shutdown` staying `return ()` by the ruling, nor `ts-src/src/terminal.ts`,
the grid needing no change, keys into a finished reactor going nowhere,
nor `GameDefinition/index.html`, the loader creating the element it needs
as the load-failure path reuses the one it finds. R4 writes the same `lhStart`
call, its rejected-parameter message being a branch of this handler,
so whichever lands first shapes the other's diff.

**Done** --- `ts`, `wasm`, `deploy`, `page`, `docs`, plus, in-session,
the deployed page under the browser-under-Xvfb setup `CLAUDE.md` describes,
driven to the game's own exit through its menu with `xdotool` and the finished
state read back over WebDriver BiDi, which R4's block records as reaching
the deployed page. That run is the first to see a real save-and-exit settle
`lhStart`'s promise, the ruling's premise, so a page left without the finished
state stops the item and reopens the ruling.

**Hands back** --- *judgement*: whether the finished and crashed pages read
as such to a player, the wording being nobody's gate. The substitute gate
in Done is the in-session DOM read after a real exit, which proves the page
shows something on it; the crashed branch has none, no in-session route crashing
the deployed game, so it is read in review.

**Decide first** --- nothing; the rulings are the record's.

## Phase 2 --- multi-font, as extraction rather than re-implementation

### 2.1 Extract `OverlayLayout`: the pure half of `Sdl.hs`'s overlay drawing

**Split** --- six commits. (1) the module and its tests with no consumer:
`engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs` with the freshness case that writes 2.2's
branch-complete `propCutoff`/`propFit` table into `ts-src/src/generated/`, both
of `LambdaHack.cabal`'s module lists and the table's `extra-source-files` line,
`Frontend.hs`'s re-export of it, and `test/Spec.hs`; the module goes
in the library's *unconditional* `exposed-modules`, not the `else` branch
that carries `Sdl.hs`, so `make test-wasm` compiles it too and the wasm consumer
inherits a module already proved to build there. (2) `Sdl.hs` adopting
`CellStyle`, its backlight fill read from `backlightRGB` in place of `greyRGBA`,
`colorToRGBA` derived from `Color.colorToRGB`,
and the `Sdl.hs:let Point{..} = toEnum i` `punindex` fix. (3) the `Sdl.hs`
refactor onto `OverlayLayout`. (4) the explicit-widths step
in `tools/checks.py`, with its mutant in `tools/mutants.py`. (5)
the re-anchoring of this plan's and the repo-root `CLAUDE.md`'s `Sdl.hs`
citations against the shrunken file, most of which the pass will name broken,
plus that file's frontend bullet, whose last module name this item delivers,
retiring the clause, and the Gotchas bullet's "awaits its scheduled fix" clause,
which (2)'s fix falsifies --- mandatory and separate, per the freeze callout.
(6) the outcome line naming (2)'s and (3)'s hashes, the ledger flip,
and the deletion of this item's two `tools/doc-refs-allow.txt` entries. (2)
and (3) are the branch How this lands reviews alone, carrying `Sdl.hs`
and nothing else, so (1) lands ahead of it, a partial landing, and (4) to (6)
after it; (5) before (6), so a correction is never in the same diff
as the landing record.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs`, `test/Spec.hs`, `LambdaHack.cabal`,
`ts-src/src/generated/` for the branch table,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs` (export list only,
re-exporting the new module per 0.1's ruling (2) in the record),
`docs/wasm-frontend-unified-plan.md`, `CLAUDE.md`, `tools/doc-refs-allow.txt`,
and `tools/checks.py` and `tools/mutants.py` for the explicit-widths step
and its mutant. It does not write the wire encoder: that lands in the same
module file under 2.3.

**Done** --- `native`, `wasm`, `docs`, plus `make test-medium`, the `make bench`
pair under `xvfb-run` on each of (2) and (3), and the freshness case's proof
as 0.2's Done states it.

**Hands back** --- hands back nothing on display, as of 2026-07-31. `make bench`
(`Makefile:bench: benchBattle benchAnimBattle`) includes
`benchFrontendBattle`/`benchFrontendCrawl` (`Makefile:benchFrontendBattle:`,
`Makefile:benchFrontendCrawl:`), which pass neither `--frontendNull`
nor `--frontendLazy` and so drive the real SDL renderer --- exactly the per-cell
hot path (2) and (3) rewrite, and the reason the headless
`benchBattle`/`benchCrawl` cannot substitute: they never enter that path at all.
`xvfb-run` is installed now, so that pair runs here, and so does
the `make frontendCrawl` look, frame by frame through `xwd` --- subject
to the two conditions a bare install does not supply, which are CLAUDE.md's
to state. One limit is not lifted: the headless renderer is llvmpipe,
so a before/after pair must be measured wholly under it and never against
a number from the real display, which is what the before/after scripts of (2)
and (3) are still for.

**Decide first** --- nothing; the rulings are the record's.

### 2.2 Browser overlay renderer, in isolation

**Split** --- three code commits and the record commit. (0) no capture any
longer: the baseline 2.5 compares against is 0.4's Split (0) number in B.10,
standing before this item starts, 0.4 being in its `depends on`; the DOM-grid
number this step took on 2026-08-07, re-taken 2026-09-04, stays at Phase 2's
intro as history. (1) the font wiring: the font fields this item adds
to the startup call 1.5 creates --- the payload specified in the body ---
with their handling in `loader.ts`, the `FontFace` declarations
in `terminal.ts`, and the `Makefile` copy of the referenced
`GameDefinition/fonts/*.ttf.woff` into the pages checkout. It depends on nothing
else in Phase 2 --- `sfonts` and `schosenFontset` are already populated
in the browser build. (2) `ts-src/src/overlay-core.ts`'s `propCutoff`/`propFit`
twin and its vitest table; sec. 2.1 gives both functions as complete Haskell
bodies, so it can be *written* before 2.1 lands, but it may only *land* once
2.1's freshness case writes the branch-complete table, hand-written fixtures
being forbidden here. (3) the overlay renderer: all three of 2.1's sections
drawn through 0.4's draw-op core onto its canvas, each fixed-pitch cell's glyph
clipped to its box by that core, as the pitfalls say. (4) the outcome line
naming (3)'s hash, the ledger flip, and the deletion of `overlay-core.ts`
and `overlay-core.test.ts` from `tools/doc-refs-allow.txt`.

**Owns** --- `ts-src/src/overlay-core.ts`, `ts-src/src/overlay-core.test.ts`,
`ts-src/src/terminal.ts`, `ts-src/src/loader.ts`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`, `Makefile`,
`docs/wasm-frontend-unified-plan.md` and `tools/doc-refs-allow.txt`. 2.1's
freshness case writes (2)'s branch table; this item edits neither the case
nor the table.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`, `docs`, plus
`test -f ../lambdahack.github.io/DejaVuLGCSans-Bold.ttf.woff` &&
`test -f ../lambdahack.github.io/Hack-Bold.ttf.woff`, plus 0.4's baseline number
standing in B.10 (Split (0)). The two file tests are the deployment gate
and are non-vacuous today --- neither font is in that checkout, which
is the whole font deployment gap this item closes.

**Hands back** --- *browser*: that the declared `FontFace`s actually load. sec.
2.2's own "live game pixel-identical to before" needs no human ---
`getFontSetup` still gates on `"sdl"`
(`MonadClientUI.hs:= Frontend.frontendName`), so the overlays are provably `[]`
and the vitest case asserting zero draw calls on an empty payload decides it,
which is why the substitute gate in Done is `make test-ts` plus the two
font-deployment tests rather than a look. The HiDPI and browser-zoom checks
are deferred to 2.5 by the plan itself, not by this block.

**Decide first** --- nothing; the rulings are the record's.

### 2.3 Transport: overlays across the JSFFI boundary

**Split** --- three commits. (1) the pure encoder and its tasty round trip, both
inside 2.1's module and test files, with the freshness case there writing
the encode fixtures into `ts-src/src/generated/` and their `extra-source-files`
line --- no FFI, no TS, so the wire layout is frozen and reviewable on its own.
(2) the transport: the additive `js_submitOverlays`, `Wasm.hs`'s `display`
filling the buffer, the TS decoder in `ts-src/src/overlay-core.ts`,
its `globalThis` hook in `loader.ts`, the hand-off in `terminal.ts`,
and the `run-wasm-test.mjs` coverage case --- one commit, because 0.3's rule
is that a new `foreign import/export` lands with its coverage case in the same
diff. (3) the outcome line naming (2)'s hash and the ledger flip; this item has
no `tools/doc-refs-allow.txt` entries, 2.1's and 2.2's having gone with their
own items.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs`,
`test/OverlayLayoutUnitTests.hs`, `LambdaHack.cabal` and `ts-src/src/generated/`
for the encode fixtures,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`ts-src/src/overlay-core.ts`, `ts-src/src/overlay-core.test.ts`,
`ts-src/src/terminal.ts`, `ts-src/src/loader.ts`, `ts-src/run-wasm-test.mjs`
and `docs/wasm-frontend-unified-plan.md`.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`, `docs`, plus
the freshness case's proof as 0.2's Done states it.

**Hands back** --- hands back nothing. sec. 2.3's "verified by playing" argues
its own case away in the next sentence: `getFontSetup` still gates on `"sdl"`
(`MonadClientUI.hs:= Frontend.frontendName`), the overlays are provably `[]`,
and playing proves nothing the zero-draw-call vitest case
and the `run-wasm-test.mjs` end-to-end case do not. Phase 2's browser budget
is spent at 2.4 instead.

**Decide first** --- nothing; the rulings are the record's.

### 2.4 Flip the switch --- as a capability, not a name list

**Split** --- five commits, and the split is the point. (1) the capability
mechanism, behaviour-preserving: `supportsMultiFont` exported by `Sdl.hs`
(`True`), `Wasm.hs` (`False`, for now), `ANSI.hs` and `Teletype.hs` (`False`),
the `Frontend.hs` dispatch mirroring `frontendName`'s CPP-guarded guard chain
(`Frontend.hs:frontendName :: ClientOptions`), `getFontSetup` reading
it (`MonadClientUI.hs:= Frontend.frontendName`), and the new
`MonadClientUIUnitTests.hs` case pinning it. This is provably a no-op: today's
gate is `frontendName soptions == "sdl"`, already `False` for wasm, ANSI,
Teletype, null and lazy alike, so a dispatch true only for `Sdl.hs` reproduces
it exactly --- which is why (1) may land at any time, even before 2.1. (2)
`Wasm.supportsMultiFont = True`, one line and nothing else. (3) the banner
commit this item's **Owns** carries under 1.4's rule: the "proportional fonts"
and "limited to the square font" clauses come off `GameDefinition/index.html`
and the second off `README.md` too, run with 1.4's **Done** and only
those phrases in the alternation. (4) the re-anchoring (1) owes: it rewrites
the `multiFont` gate in `MonadClientUI.hs`, which every citation of that line
anchors on. (5) the outcome line naming (2)'s hash and the ledger flip;
this item has no `tools/doc-refs-allow.txt` entries.

**Owns** --- `engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/ANSI.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend/Teletype.hs`,
`engine-src/Game/LambdaHack/Client/UI/Frontend.hs`,
`engine-src/Game/LambdaHack/Client/UI/MonadClientUI.hs`,
`test/MonadClientUIUnitTests.hs`, `GameDefinition/index.html`, `README.md` ---
the "proportional fonts" and "limited to the square font" clauses its flip
falsifies, the second of them in both files since the `#status` div quotes
the README, deleted in this item's landing sequence under 1.4's rule, per
that item's 2026-08-07 ruling --- and `docs/wasm-frontend-unified-plan.md` ---
(1) rewrites the `multiFont` gate the citations anchor on, so this item owes
the re-anchoring. `Dom.hs` is deliberately not here: a dead example file
no configuration compiles, R3. The sum-typed selection practice also rewrites
the `multiFont` gate in `MonadClientUI.hs`, so whichever lands first shapes
the other's diff; the pointman campaign's C1 appends its witness and accessors
to the same file below `partPronounLeader`, per that campaign's own ruling,
so it rewrites nothing cited here.

**Done** --- `native`, `ts`, `wasm`, `deploy`, `page`
with `PAGE_GATE_OVERLAYS=1`, `docs`, plus `make test-medium`.

**Hands back** --- *browser*: commit (2) is the moment players see something
different, and it is the whole human-review budget of Phase 2 concentrated
into a one-line diff --- menus, message log, help and item descriptions
rendering in the proportional fonts, and the mono-overwrites-prop overrun
behaving. That concentration is what the split buys: the mechanism is reviewed
in (1) with no behaviour change, the risk in (2) with no mechanism left
to re-read. In Done instead: `cabal test` pins the capability itself through
the new `MonadClientUIUnitTests.hs` case --- which is what stops the stub's
`sfrontendNull = True` from making the flip merely not-broken rather
than covered --- and `make test-medium` is the native playtest battery sec. 2.4
asks for.

**Decide first** --- nothing.

### 2.5 Post-flip QA checklist

**Split** --- one commit, after the human checklist is signed off: this item's
outcome line and the ledger flip. No banner commit: 1.4's 2026-08-07 ruling made
the banner a post-condition of every capability item, so the "proportional
fonts" clause comes off in 2.4's landing sequence, and 2.4 sits before this item
in the chain. R6's commit stood here as (1) until 2026-09-14 and retired with R6
(B.10).

**Owns** --- `docs/wasm-frontend-unified-plan.md` and nothing else:
`GameDefinition/index.html` left this list with the banner commit, which
is 2.4's.

**Done** --- `ts`, `wasm`, `deploy`, `page`, `docs`. No banner grep here: 2.4
deletes that clause and carries the check, and a grep run after 2.4 has landed
would pass having tested nothing.

**Hands back** --- *judgement*: every checklist item is a human's.
The side-by-side visual comparison with SDL2 (menus, message log, help, item
descriptions, and the mono-overwrites-prop overrun), the HiDPI and browser-zoom
spot checks, and the frame-timing re-measurement's verdict --- the runs
themselves are a session's now, capture and re-measurement under one Xvfb setup
with the left-hand side 0.4's Split (0) number (B.10 has the record).
The screenshot line is a glance too: one renderer draws the PNG and the screen,
so it cannot regress to map-only on its own (B.10).

**Decide first** --- nothing; the rulings are the record's.

## Phase 3 --- port the Node benchmark targets from GHCJS to WASM

### 3.1 Argv into the reactor: `lhStart` reads WASI args

**Split** --- two commits. First `GameDefinition/Main.hs`: `getArgs` in place
of the literal `[]`, the comment rewritten to the new contract,
and `import System.Environment (getArgs)` --- a new import, since `getArgs`
occurs in no tracked `.hs` file today and `Core/Prelude.hs` re-exports
no `System.Environment`. Imports precede declarations, so it cannot ride
the `#ifdef USE_WASM` block that carries its only consumer: it goes
in the file's import block under an `#ifdef USE_WASM` of its own, beside
the `#ifndef USE_BROWSER` one already there. Unguarded it is an unused import
on every native build, which is a warning and leaves `native` green, so no gate
this item names would catch it. Second the landing --- outcome line plus
the ledger row. (The "not yet validated" retirement a third commit once carried
was overtaken by the recorded spike of 2026-08-07 --- the body's Spike paragraph
holds the artifact.)

**Owns** --- `GameDefinition/Main.hs`, which R3 also writes, rewording its GHCJS
comment; and `docs/wasm-frontend-unified-plan.md`. Deliberately not `CLAUDE.md`:
its "there is no argv and no config file on disk" stays true after the change,
since the loader still passes the one-element argv `["LambdaHack"]`
(`loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`), and touching
it would widen this item for nothing.

**Done** --- `native`, `wasm` --- of whose two halves only `make build-wasm`
bears on this item, `make test-wasm` linking `Main.hs` in no configuration ---
`page` and `docs`, the page gate being the page load this line used to spell
out.

**Hands back** --- hands back nothing, ruled 2026-09-04. The page load this item
used to hand back is a session's to run: `firefox-beta` under Xvfb with matchbox
is what 2.2's Split (0) drove to take the frame-timing baseline recorded
at Phase 2's intro, and what R5's rendering half uses, so "no in-session gate
reaches it" stopped being true when that browser arrived. It moves into **Done**
above. What survives here is why `wasm`'s halves are unequal: `make build-wasm`
is the load-bearing one, because `lhStart` sits inside `#ifdef USE_WASM`,
so `cabal build` and `cabal test` compile none of it and a native-only
done-check would verify nothing. Nor does `hlint .` see it: the live
`arguments:` passes `-XNoStarIsType` alone
(`.hlint.yaml:arguments: [-XNoStarIsType]`), the `--cpp-simple` beside it being
a commented-out template (`.hlint.yaml:arguments: [--color,`) that CLAUDE.md
forbids restoring --- measured by planting a redundant bracket on each side
of the `#ifdef`, of which only the one outside it was reported.
So `make build-wasm` compiles this item's only reader. The argv claim needed
no human either: the spike ran in a session, and 3.2's driver turns it
into a standing check.

**Decide first** --- nothing; the rulings are the record's.

### 3.2 A Node driver for the game reactor

**Split** --- two commits; the exit-propagation correction the old first commit
scheduled landed with the 2026-08-07 ruling fold, so no session can implement
the wrong bullet. The driver growth; then the landing: outcome line and ledger
row --- the `run-wasm-game.mjs` deletion from `tools/doc-refs-allow.txt` rides
0.3's export half, where the file now appears.

**Owns** --- `ts-src/run-wasm-game.mjs` (0.3's seed, grown here; its argv
pass-through is 1.2's if 1.2 lands first)
and `docs/wasm-frontend-unified-plan.md`. The allowlist line and every stub
are 0.3's per its ruling and this item's (c), and lz-string has been
in the package and lock files since the page gate.

**Done** --- `ts`, `wasm`, `docs`, plus `. ~/.ghc-wasm/env` && `T=$(mktemp -d)`
&& `W=$(wasm32-wasi-cabal list-bin exe:LambdaHack)` &&
`~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$W" --output "$T/ghc_wasm_jsffi.mjs"`
&&
`node ts-src/run-wasm-game.mjs "$W" "$T/ghc_wasm_jsffi.mjs" --newGame 1 --gameMode crawl --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 200 --automateAll --keepAutomated`
&&
`! node ts-src/run-wasm-game.mjs "$W" "$T/ghc_wasm_jsffi.mjs" --stopAfterFrames notanumber`.

**Hands back** --- hands back nothing. Every integration point in the item
is an exit status, the negated second run being the non-vacuity control
that proves the driver's success rule can still fail. *Judgement* survives only
in the third bullet --- whether the reported peak `WebAssembly.Memory`
and `setItem` timings measure what R1 wants --- and that is R1's gate,
not this one's; 3.2 owes the numbers, not their reading.

**Decide first** --- nothing; the rulings are the record's.

### 3.3 Repurpose the Makefile targets

**Split** --- three commits. The `Makefile` rewrite first: both `nodeBench*`
targets onto the 3.2 driver, and `nodeMinifiedBench` renamed. Then, separate
because the rewrite falsified them, the claims elsewhere that these targets
are dead --- the `nodeBench*` sentence in `.claude/skills/playtests/SKILL.md`.
Last the landing: outcome line, ledger row, and the `make nodeDeployedBench`
deletion from `tools/doc-refs-allow.txt`.

**Owns** --- `Makefile`, `.claude/skills/playtests/SKILL.md`,
`tools/doc-refs-allow.txt` and `docs/wasm-frontend-unified-plan.md`.

**Done** --- `wasm`, `deploy`, `docs`, plus `make nodeBench` &&
`make nodeDeployedBench` &&
`grep -q nodeDeployedBench .claude/skills/playtests/SKILL.md` &&
`! grep -q nodeMinifiedBench Makefile`. The `deploy` gate is what makes
the second benchmark mean anything: without it `nodeDeployedBench` times
whatever `../lambdahack.github.io/LambdaHack.wasm` was left there last, which
is the artifact this item is supposed to be validating.

**Hands back** --- *judgement*: "plausible frame counts" and the wasm-vs-native
ratio are readings, not exit statuses, and the two halves are harvested
asymmetrically --- the native binary takes the stdout redirect `CLAUDE.md`
describes, while the wasm run is exempt --- that redirect sits inside
`Main.hs`'s `#ifndef USE_BROWSER`, which no browser build compiles ---
and prints to the terminal, so a session comparing the two reads one from a file
and one from stdout, or silently compares nothing. Done gates the half
that is mechanical: both benchmarks run to completion, and a crash or hang fails
the target.

**Decide first** --- nothing; the rulings are the record's.

## Related goals

**R1 --- Save robustness.** Design and rulings in the record.

**Split** --- four commits. R1a re-measures the browser save lag under wasm
from 0.3's instrumented stub and records the number
in `docs/wasm-frontend-unified-plan.md`, editing it alone. R1b
is the staging-key/generation-pointer change in `WasmFile.hs` plus
its interrupted-cycle test, and carries the `test/WasmFileUnitTests.hs` deletion
from `tools/doc-refs-allow.txt`. R1c acts on R1a's number against the ruled
100ms bar --- at or under, periodic autosave re-enabled; above, a save driven
from `pagehide`/`visibilitychange`. R1d is the banner commit this item's
**Owns** carries under 1.4's rule --- the savefile-corruption clause R1b
falsifies comes off `GameDefinition/index.html` and off `README.md`, run
with 1.4's **Done** and only `[Ss]avefiles are prone to corruption`
in the alternation. The bracket is load-bearing, 1.4's **Done** grepping
case-sensitively: the page capitalises the sentence and the README does not,
so either literal alone passes over one of the two files and the negated grep
would come back green having tested nothing there. R1b and R1c each rewrite
lines cited here, R1c the two `#if` sites and R1b the `WasmFile.hs` layout 0.3's
ruling cites into, so each owes the re-anchoring the pass will name. R1d carries
the outcome line.

**Owns** --- `engine-src/Game/LambdaHack/Common/WasmFile.hs`, including
the header's "Mirrors JSFile.hs ... exactly in storage format" claim, which
R1b's layout change falsifies and which R3 does not touch;
`engine-src/Game/LambdaHack/Common/File.hs`, whose interface gains
the cycle-commit operation, `engine-src/Game/LambdaHack/Common/HSFile.hs`, which
implements it as a no-op, and `engine-src/Game/LambdaHack/Server/CommonM.hs`,
whose `writeSaveAll` calls it --- the three files this item's own ruling names,
two of which the capability-constants practice also rewrites; the new
`test/WasmFileUnitTests.hs` (CPP-guarded to an empty group natively, the way
`test/Spec.hs` already guards its SDL cases), `test/Spec.hs`, and the test-suite
`other-modules` list in `LambdaHack.cabal`;
`Server/LoopM.hs:!defined(USE_JSFILE) &&`
and `WatchUpdAtomicM.hs:UpdKillExit{} -> do`; and `GameDefinition/index.html`
with `README.md` beside it --- the savefile-corruption clause R1b falsifies
stands in both, capitalised in one and not the other, the `#status` div quoting
the README's paragraph rather than copying it, and R1d deletes it from both
in one commit under 1.4's rule, per that item's 2026-08-07 ruling;
`tools/doc-refs-allow.txt`, whose `test/WasmFileUnitTests.hs` entry R1b's commit
deletes; and `docs/wasm-frontend-unified-plan.md`, which R1a edits alone
to record its number and R1b's and R1c's re-anchorings edit. It does not write
`ts-src/run-wasm-test.mjs` --- the localStorage stub is 0.3's, per that item's
own ruling, and a stub behaviour the test needs lands there first, as a 0.3
change. R1c, R3 and the capability constants rewrite the same two `#if` sites,
so whichever lands first shapes the others' diffs.

**Done** --- `native`, `wasm`, `page`, `docs`.

**Hands back** --- *judgement*: a real tab kill mid-cycle, which the Node stub
can only simulate. R1a's number no longer waits on judgement --- the 100ms bar
is ruled, so R1c's branch is decided by where the measurement lands.
The substitute gate in Done is the interrupted-cycle case under `make test-wasm`
--- staging keys written, pointer not flipped, the previous generation still
served intact --- plus 0.3's `encodeEOF`/`strictDecodeEOF` round trip, which
already runs in the wasm environment and stays green through the pointer.

**Decide first** --- nothing; the rulings are the record's.

**R2 --- Browser-and-frontend CI.** Design and rulings in the record.

**Split** --- one commit per job, and the row never flips: R2 is a standing
item, so no commit carries an outcome line and none has
`tools/doc-refs-allow.txt` entries to delete. Two jobs are ready today
and ordered, ruled 2026-08-07: the cache commit first --- `~/.ghc-wasm`
and the wasm cabal store, the toolchain bootstrap being the expensive part
of every wasm job including the next one --- then a `make build-wasm` job, which
feeds 0.3's export-half coverage, the game driver run against the reactor
it builds. One non-job commit rode the same ruling and landed on master
(`2b4b7d71f`): `build-ts` chains with `&&` now, so the deploy-carrying Done
gates across this plan no longer trust an exit status that cannot fail. The rest
land with the artifact each runs, as the practice's `xvfb` SDL and pty ANSI
smokes did: a short `nodeBench` smoke after 3.3.

**Owns** --- `.github/workflows/lint-and-test-suites.yml`, plus the `Makefile`
where a job needs a target of its own ---
and `docs/wasm-frontend-unified-plan.md`, for the re-anchoring a job owes when
it rewrites a line cited here in either file. Never
`.github/workflows/haskell-ci.yml`, which `haskell-ci regenerate` owns.

**Done** --- `wasm`, `docs`, plus
`python3 -c "import yaml; yaml.safe_load(open('.github/workflows/lint-and-test-suites.yml'))"`
--- the parse, plus the job's own payload run locally, the second half
substituted per commit.

**Hands back** --- *judgement*: whether the job is green on a GHA runner,
and whether the toolchain cache pays for itself, which takes two authorized
pushes to see --- one cold, one warm --- and which a session may not arrange
for itself. There is no substitute gate, a workflow edit having no local exit
status beyond parsing; Done runs the payload and nothing more, and CI status
is read afterwards, as the standing checks require.

**Decide first** --- nothing; the rulings are the record's.

**R3 --- Retire GHCJS support (one browser target is enough).** Design
and rulings in the record.

**Split** --- three commits, on 2.1's pattern, because **Decide first** (c)
already makes the re-anchoring a commit of its own and the label's absence would
otherwise assert this is one commit. (1) the rip-out, everything the body lists,
in one diff. (2) the `.md`-only re-anchoring (c) specifies, the frozen
appendices' citations included. (3) the outcome line naming (1)'s hash
and the ledger flip. (2) before (3), so a correction is never in the same diff
as the landing record. Nothing to delete from `tools/doc-refs-allow.txt`:
this item proposes no artifact.

**Owns** --- `LambdaHack.cabal` (the `supportNodeJS` flag, the `impl(ghcjs)`
`cpp-options` stanza and the comment above it, `-DREMOVE_TELETYPE`,
the `ghcjs-options` knob block, and the two `impl(ghcjs)` module/dependency
branches), `cabal.project.local.js` (deleted), `Frontend.hs` (the `USE_GHCJS`
import branch and both `#ifndef REMOVE_TELETYPE` guards), `File.hs`'s
`USE_JSFILE` branch, `TieKnot.hs` (both sites), the `USE_JSFILE` halves
at `Server/LoopM.hs:!defined(USE_JSFILE) &&`,
`HandleHumanLocalM.hs:!defined(USE_JSFILE) &&`
and `WatchUpdAtomicM.hs:#if defined(USE_JSFILE)`, header comments only
in `Dom.hs` and `JSFile.hs`, the two orphan GHCJS comments nothing else claims
--- `GameDefinition/Main.hs:This test is faulty in the browser`, reworded
in wasm/browser terms since the fact survives the compiler it names,
and `Point.hs:not supported yet by GHCJS (not verified)`'s "not supported yet
by GHCJS" clause, updated or dropped --- and `CLAUDE.md`
and `docs/wasm-frontend-unified-plan.md`. Every item adding `exposed-modules`
to `LambdaHack.cabal` writes into the very conditional nest R3 collapses,
the orphan comment
at `GameDefinition/Main.hs:This test is faulty in the browser` sits in a file
3.1 also writes, and the three engine `#if` sites are also R1's
and the capability-constants practice's, so whichever lands first shapes R3's
diff. The pointman campaign writes two of these too: its PR 0, C3, C4 and C6
write `HandleHumanLocalM.hs`, the first three above the `#if` site this item
cites, and its 04.4 writes `CLAUDE.md` at that campaign's landing.

**Done** --- `native`, with stylish widened past the gate's Owns-derived list
to `$(git ls-files '*.hs')` --- the one item that widens it, and repo-wide here
because the rip-out's Haskell files are scattered across the tree
and its **Owns** names them all --- `wasm`, `docs`, plus `make test-gha` &&
`git diff --exit-code` &&
`! git grep -q 'USE_GHCJS\|USE_JSFILE\|REMOVE_TELETYPE\|supportNodeJS\|ghcjs-options' -- ':!*.md' ':!LambdaHack.cabal.bkp' ':!LambdaHack.cabal.flattened' ':!engine-src/Game/LambdaHack/Client/UI/Frontend/Dom.hs' ':!engine-src/Game/LambdaHack/Common/JSFile.hs'`.
The grep half is non-vacuous: run against today's tree it finds matches
and fails the line, so it can only pass after the rip-out.

**Hands back** --- *browser*: one page load after `make build-ts`
and `make serve-wasm`, per the ground rules --- a formality here, since every
branch deleted is compiled by no supported configuration, so native and wasm
object code are unchanged by construction. The substitute gate in Done
is `make test-gha`, which drives `--frontendTeletype` through whole games
and so covers the only removal with live source consumers, `-DREMOVE_TELETYPE`.
The `CLAUDE.md` rewording is authored text and gets a human read rather
than a mechanical edit.

**Decide first** --- nothing; the rulings are the record's.

**R4 --- URL-parameter options.** Design and rulings in the record.

**Split** --- three commits, on 2.1's pattern, since the label's absence would
otherwise assert this is one commit. (1) the change, which deletes the two
allowlist entries as it creates the files. (2) the re-anchoring (1) owes:
it rewrites the `new WASI` line cited here. (3) the outcome line naming (1)'s
hash and the ledger flip.

**Owns** --- the new `ts-src/src/url-options-core.ts`
and `ts-src/src/url-options-core.test.ts`, `ts-src/src/loader.ts`
(the `new WASI(["LambdaHack"], [], fds)` argv construction
at `loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`),
`GameDefinition/index.html` if the knobs get on-page documentation,
`tools/doc-refs-allow.txt`, whose two `url-options-core` entries commit (1)
deletes as it creates the files, and `docs/wasm-frontend-unified-plan.md`,
the re-anchoring commit included. R5's `?benchmark` mode is one
of this allowlist's own entries and edits the same `loader.ts` call,
so whichever lands first shapes the other's diff. So does 1.6, whose handler
on `lhStart`'s settlement this item's rejected-parameter message is a branch of.

**Done** --- `ts`, `page`, `docs`.

**Hands back** --- *browser*: that a parameter typed into the address bar
actually reaches the engine. 3.2's Node driver cannot answer it --- it hands
argv to the WASI shim directly and never loads `loader.ts` --- but a headless
browser can, contrary to what this block claimed until the browser arrived:
WebDriver BiDi navigates the deployed page and `location.search` reads back
intact, so once this item lands its confirmation is scriptable rather than one
served page per parameter. The substitute gate in Done is the vitest case
over `url-options-core.ts`: query string in, args array out, which is everything
between the URL and `loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`.

**Decide first** --- nothing; the rulings are the record's.

**R5 --- Performance pass (exploratory, after Phases 2 and 3).** Design
and rulings in the record.

**Split** --- R5a is the instrument: the `?benchmark` URL mode plus the rAF
frame-timing collector, its pure half in a `*-core.ts` under vitest,
and it carries the deletion of its two `tools/doc-refs-allow.txt` entries. R5b
is the measurement pass the item exists for --- R5a's browser rendering numbers
beside the `make nativeBench`-vs-`make nodeBench` ratio --- each landing
as a recorded number in `docs/wasm-frontend-unified-plan.md` and nothing else,
and it carries the outcome line. R5c exists only if a ruled bar is crossed ---
the game-logic ratio beyond 3x, or `submitFrame`-to-paint p95 beyond one 60Hz
frame, 16.7ms, an exceedance confirmed on the real display before R5c opens ---
and is otherwise not schedulable, with no commit here; below both bars, "runs
rather slowly" is stale and R5 deletes that clause itself, from the page
and from `README.md`, under 1.4's rule and with both files in its **Owns**
for the purpose.

**Owns** --- the new `ts-src/src/frame-timing-core.ts`
and `ts-src/src/frame-timing-core.test.ts`, `ts-src/src/terminal.ts`
(the `submitFrame`-to-paint instrumentation), `ts-src/src/loader.ts`
(the `?benchmark` entry), `GameDefinition/index.html` and `README.md` (the "runs
rather slowly" clause, which stands in both, deleted under 1.4's rule when both
bars are cleared), `tools/doc-refs-allow.txt` (R5a's two `frame-timing-core`
entries), and `docs/wasm-frontend-unified-plan.md`; R5b writes it alone.
`?benchmark=` sits in R4's parameter allowlist, so whichever of the two lands
first shapes the other's `loader.ts` diff.

**Done** --- `ts`, `page`, `docs`, which decides R5a only.

**Hands back** --- *judgement*: what the numbers are worth. The rendering half
is a session's to run now: the pass is `make build-ts` (unsandboxed) plus
`make serve-wasm` and the page driven in-session under the Xvfb setup, llvmpipe
numbers comparing only within that setup, with a real-display pair only
if a verdict is challenged. Phase 3's `nodeBench*` targets run `--frontendNull`,
so `Wasm.hs`'s frontend never starts and no frame is ever painted --- R5a has
no substitute anywhere in this plan. What Done does gate is the collector's
arithmetic --- count/mean/p95 over a fixed timestamp list --- and beside it sits
the game-logic half, `make nativeBench` against `make nodeBench` on one machine
with one seed set, which a session can run but not judge, and which must
not be parallelized across agents.

**Decide first** --- nothing; the rulings are the record's.

**R6 --- Screenshot/overlay coherence.** Design and rulings in the record;
retired by B.10, so nothing here is executed.

**Owns** --- nothing. With one renderer there is nothing to compare, and neither
1.2 nor 2.2 carries a shared entry point for it.

**Done** --- nothing.

**Hands back** --- nothing.

**Decide first** --- nothing; the rulings are the record's.

**R7 --- Pixel-perfect bitmap map fonts.** Design and rulings in the record.

**Split** --- (1) the BDF decoder and atlas generator with its tests
and the committed atlas, consumed by nobody; (2) the painter drawing
from the atlas at 1.0 and integer scales and from the outline font otherwise,
`make build-ts` copying the atlas beside the woff; (3) the outcome line
and the ledger flip, for R7a. R7b, ruled in the record, takes no commit here:
it follows R7a's landing as a change of the font choice alone.

**Owns** --- `ts-src/src/terminal.ts`, `ts-src/src/terminal-core.ts`
and its test, `ts-src/src/fullscreen.ts`, `Makefile` (the atlas copy
into the pages checkout), `GameDefinition/fonts/` (the converted BDFs),
the decoder module and the atlas under names the executing session mints, their
`tools/doc-refs-allow.txt` entries added before the names are written anywhere,
and `docs/wasm-frontend-unified-plan.md`.

**Done** --- `ts`, `wasm`, `deploy`, `page`, `docs`, plus `test -f`
on the deployed atlas.

**Hands back** --- *display*: that a 1.0 map on the canvas is the `.bdf` pixel
for pixel, an `xwd` of each side diffed, and that the switch to the outline font
when `allFontsScale` leaves 1.0 reads as SDL2's behaviour and not
as a regression. The substitute gate in Done is the decoder's tests: a glyph's
pixels out equal to the BDF's bitmap in.

**Decide first** --- nothing; the rulings are the record's.

## Multi-frontend practices (adopted)

**Capability constants instead of behavioral CPP.** Design and rulings
in the record.

**Split** --- two commits and their repair, exports before uses: first
the File-layer modules gain the constants (`Common/HSFile.hs`,
`Common/WasmFile.hs`, `Common/JSFile.hs`, re-exported through `Common/File.hs`,
whose selection `#ifdef` at `File.hs:#ifdef USE_JSFILE` stays untouched),
then the three engine consumers branch on values and lose their `#if`s;
then the re-anchoring the second owes, the three `#if` sites being lines cited
here that it rewrites. The outcome line names the second commit's hash
and follows the third; the item sits in no `tools/doc-refs-allow.txt` gate,
so it deletes nothing there.

**Owns** --- `Common/File.hs`, `Common/HSFile.hs`, `Common/WasmFile.hs`,
`Common/JSFile.hs`, `Server/LoopM.hs`, `Client/UI/HandleHumanLocalM.hs`,
`Client/UI/Watch/WatchUpdAtomicM.hs`, and `docs/wasm-frontend-unified-plan.md`,
for the re-anchoring the second commit owes. The two code commits are strictly
ordered --- the second does not compile without the first. R1's whole point
is flipping two of the constants (`Server/LoopM.hs:!defined(USE_JSFILE) &&`,
`WatchUpdAtomicM.hs:#if defined(USE_JSFILE)`) and R3 deletes their `USE_JSFILE`
halves, so whichever of the three lands first shapes the others' diffs: R3's
**Owns** and body both say "whichever lands first", Sequencing schedules
this item any time, and neither `depends on` cell names it. The pointman
campaign writes `HandleHumanLocalM.hs` too --- its PR 0, C3, C4 and C6 ---
converting several functions above
the `HandleHumanLocalM.hs:!defined(USE_JSFILE) &&` this item and R3 both cite,
so re-read those two snippets if it has landed.

**Done** --- `native`, `wasm`, `docs`, plus
`! git grep -lE 'USE_JSFILE|USE_WASMFILE' -- engine-src/Game/LambdaHack/Server/LoopM.hs engine-src/Game/LambdaHack/Client`.
That `git grep` names three files today, which is what makes its later silence
evidence rather than a vacuous search.

**Hands back** --- *browser*: a session does now enter the save-and-exit path
--- driven through the real page, `localStorage` gains the five save keys
and a reload restores them --- and killing the browser 0.2s, 1.0s and 2.5s after
the save loses them where 5.0s and a graceful close keep them (n=1 each).
So the 2s would not survive an abrupt death and is not what a graceful quit
needs, which is R1's number to act on rather than this item's. What stays
unverified is the case the constant is named for: a real tab close
with the browser surviving it. The substitute gate in Done is `make test-wasm`,
which proves only that the wasm build compiles, links and runs its suite
with the constants in place; the number itself is re-examined by R1, not
by this item.

**Decide first** --- nothing; the rulings are the record's.

**Sum-typed frontend selection.** Design and rulings in the record.

**Split** --- three commits, on 2.1's pattern, since the label's absence would
otherwise assert this is one commit. (1) the change, one diff because the type
change is what forces every site to move together. (2) the re-anchoring (1)
owes: every file (1) rewrites is cited here, and the pass names what broke. (3)
the outcome line naming (1)'s hash and the ledger flip.

**Owns** --- `Common/ClientOptions.hs` (the field), `Server/Commandline.hs`
(the four parsers at `Commandline.hs:frontendANSIP :: Parser Bool` and their
bindings at `Commandline.hs:<- frontendANSIP`), `Client/UI/Frontend.hs` (both
guard chains), `Client/UI/DrawM.hs` and `Client/UI/MonadClientUI.hs` --- the two
`frontendName soptions ==` string compares the sum type subsumes
(`DrawM.hs:unless (frontendName soptions`,
`MonadClientUI.hs:= Frontend.frontendName`), which this item's churn list omits
and which `git grep 'frontendName soptions =='` shows are the only two
in the tree --- `test/UnitTestHelpers.hs`, whose `sfrontendNull = True`
is the sole fixture setting an `sfrontend*` field,
and `docs/wasm-frontend-unified-plan.md`, for the re-anchoring commit. 2.4
rewrites `MonadClientUI.hs:= Frontend.frontendName` and dispatches through
`Frontend.hs:frontendName :: ClientOptions`, and R3 deletes
the `#ifndef REMOVE_TELETYPE` guards at `Frontend.hs:= Teletype.startup`
and `Frontend.hs:-> Teletype.frontendName` --- the exact lines this item turns
into cases --- so whichever lands first shapes the others' diffs.
The determinism goldens also write `test/UnitTestHelpers.hs`, for their
fixtures, and the pointman campaign writes both files too: its C1 appends
to `MonadClientUI.hs` below `partPronounLeader`, and its PR 0 adds a recording
`ChanFrontend` to `test/UnitTestHelpers.hs`.

<!-- probe: git -C .. grep -h 'frontendName soptions ==' -- '*.hs' | wc -l :: 2 -->

**Done** --- `native`, `wasm`, `docs`, plus `make test-short` &&
`! git grep -q 'frontendName soptions ==' -- '*.hs'` &&
`LH=$(cabal list-bin exe:LambdaHack)` &&
`! "$LH" --frontendNull --frontendTeletype --newGame 1 --gameMode dig --benchmark --stopAfterFrames 1 --automateAll`.
Both negated checks fail *today* and so are non-vacuous: the grep finds two
sites, and the double `--frontend` invocation exits 0 because guard order
silently picks null. `make test-short` plays whole games --- minutes,
not seconds.

**Hands back** --- *judgement*: `--help` text and error wording change when four
independent `switch`es become alternatives of `flag'`, and nothing reads either.
The substitute gates in Done are the nonzero exit
on `--frontendNull --frontendTeletype` and `make test-short`, which drives
`--frontendTeletype` end to end through the rewritten parser.

**Decide first** --- nothing; the rulings are the record's.

**The RawFrontend contract, written down and tested.** Design and rulings
in the record.

**Split** --- three commits. The haddock contract on `RawFrontend`'s fields
(`Frontend/Common.hs:data RawFrontend = RawFrontend`) and the "how to add
a frontend" checklist first, since the contract is what the harness then asserts
against, and free to land alone as a partial landing taking no outcome line
(the record's ruling (4)) --- carrying the hoist of `nullStartup`,
`lazyStartup`, `display` and `frameTimeoutThread` into `Frontend.hs`'s
unconditional export list under the "internal and used in unit tests" group
(ruled 2026-08-07), so exports precede the harness that uses them; the native
harness second, once 0.1 has created its module, driving
`nullStartup`/`lazyStartup`/`Teletype.startup`; the wasm case third,
on the `globalThis.lhSubmitFrame` stub 0.3's baseline battery installs
in `ts-src/run-wasm-test.mjs`
(`run-wasm-test.mjs:globalThis.lhSubmitFrame = (addr, w, h) => {`), without
which the first foreign call out of `Wasm.hs` traps. The third commit carries
the outcome line.

**Owns** --- `Client/UI/Frontend/Common.hs` (haddock), `Client/UI/Frontend.hs`
(export list only), `test/FrontendContractUnitTests.hs`, which 0.1 creates,
`test/Spec.hs`, the `test-suite test` stanza of `LambdaHack.cabal`
(`LambdaHack.cabal:test-suite test`), and `docs/wasm-frontend-unified-plan.md`.
The input-side cases land inside 0.1, which creates the test module and mints
`[fe-invariant]`, and this item's harness extends it; 2.3 and 1.5 are the items
extending `ts-src/run-wasm-test.mjs`; this one and 3.2 do not.

**Done** --- `native`, `wasm`, `docs`, plus
`cabal test --test-options='-p "/fe-invariant/"'` --- the series marker 0.1's
commit minted under the tasty-tag rule above: it selected nothing when listed
before the series existed, and `fe-invariant` contains neither `contract`
nor `LR-flip`, the other half of that rule's proof, which the listing cannot
show. That used to carry a second build,
`cabal build --builddir=dist-norelease --flags=-release test`, as the only thing
catching a harness leaning on the `EXPOSE_INTERNAL` block; `release` now
defaults `False`, so the ordinary build is that build and the clause would only
re-state the default.

**Hands back** --- *display*: SDL2 is the frontend whose folklore the contract
most needs to bind --- the bound main thread via
`startupBound`/`workaroundOnMainThreadMVar`, the frame-queue handshake ---
and no in-session *harness* reaches it --- `xvfb-run` does now play whole games
through that loop and `xdotool` keys do reach it (CLAUDE.md), but that exercises
the frontend rather than isolating the `fshowNow` handshake, so SDL is covered
by the haddock and by review alone. The substitute gate in Done is the harness
itself, run against null, lazy and Teletype natively and against the real
`Wasm.hs` under `make test-wasm`.

**Decide first** --- nothing; the rulings are the record's.

**Determinism goldens --- native first, then cross-backend.** Design and rulings
in the record.

**Split** --- two commits, as the item's own staging says: the native harness,
which must precede 2.1, then the wasm stage, which only adds the same goldens
to `make test-wasm` and to R2's existing job. The second carries the outcome
line and the `tools/doc-refs-allow.txt` deletion.

**Owns** --- the new `test/DeterminismGoldenUnitTests.hs`, `test/Spec.hs`,
the `test-suite test` stanza of `LambdaHack.cabal`
(`LambdaHack.cabal:test-suite test`), whatever fixtures the digest needs
in `test/UnitTestHelpers.hs` and `test/SessionUIMock.hs`,
`tools/doc-refs-allow.txt` (the `test/DeterminismGoldenUnitTests.hs` entry,
deleted by the second commit), and `docs/wasm-frontend-unified-plan.md`.
The sum-typed selection writes `test/UnitTestHelpers.hs`, as does the pointman
campaign's PR 0, for its recording `ChanFrontend`. If the digest
is over committed literals rather than a file, it owns no data file; if
over a file, that file joins this list rather than living beside the test.

**Done** --- `native`, `wasm`, `docs`, plus
`cabal test --test-options='-p "/fe-golden/"'` --- a second series marker
for this campaign, minted under the tasty-tag rule above, so the first commit
lists before the series exists and shows that the marker selects nothing,
and states the other half of that rule's proof beside it: `fe-golden` contains
neither `contract` nor `LR-flip`, which the listing cannot show and only reading
the marker can.

**Hands back** --- *judgement*: whether the digest is actually sensitive to what
2.1 and 2.4 change is the entire value of the item, and no run answers it ---
a green golden that guards nothing looks exactly like a green golden that guards
everything. The substitute gate is the non-vacuity proof the standing checks
demand of any new self-checking assertion: perturb one drawing or layout input,
re-run `cabal test --test-options='-p "/fe-golden/"'` --- this item's own
marker, never the bare `golden` the tasty-tag rule forbids --- expect failure,
and record the perturbation next to the test.

**Decide first** --- nothing; the rulings are the record's.

**A CI smoke for every shipped frontend.** Design and rulings in the record.

**Split** --- three commits, ordered by which gate can run at all:
the pty-driven ANSI smoke (verifiable in-session), the `xvfb-run` SDL smoke
(likewise, since the display arrived --- see Hands back), and the `nodeBench`
smoke, which waits on 3.3. The targets sit at the `Makefile`'s end. The third
carries the outcome line; the item is in no `tools/doc-refs-allow.txt` gate
beyond the three target names this block introduces, which its own commit
deletes as it lands.

**Owns** --- `Makefile`, gaining `smokeANSI`, `smokeSdl` and `smokeNodeBench`,
`.github/workflows/lint-and-test-suites.yml`, one step each in the playtest job,
whose apt line ruling (2) gives `xvfb`, `tools/doc-refs-allow.txt`, whose
`make smokeANSI`, `make smokeSdl` and `make smokeNodeBench` entries leave
with the commits that add those targets, `docs/wasm-frontend-unified-plan.md`,
and `CLAUDE.md`, whose CI paragraphs say what the smokes cover.
Not `.github/workflows/haskell-ci.yml`, which is generated from the cabal file
and whose hand edits vanish on the next regenerate.

**Done** --- `native`, `docs`, plus `make smokeANSI` && `make smokeSdl` &&
`make smokeNodeBench --dry-run`. `--dry-run` gates the one target that cannot
execute here on existing and on expanding, without running it;
`check-doc-refs.py` resolves all three names against the makefile, which is what
makes the allowlist deletion a checked claim.

**Hands back** --- hands back nothing. *Display*: `xvfb-run` and `Xvfb`
were installed on 2026-07-31 and a whole game has since played out in the real
SDL2 frontend here, so the SDL smoke is gated in-session like the ANSI one,
rather than first executing in a CI run that would need a push and
so an explicit go-ahead. Two conditions the bare install does not supply ---
a missing `/tmp/.X11-unix` and a GLX segfault --- are CLAUDE.md's to state
and this item's to obey: the `smokeSdl` recipe carries them or the target
reproduces neither here nor in CI. One limit survives: the renderer is llvmpipe,
so an in-session frame *count* is a gate and an in-session *timing* is not.

**Decide first** --- nothing; the rulings are the record's.

**Frontends pass widths explicitly.** Design and rulings in the record.

**Owns** --- nothing --- this is a review rule, not an item to execute.
Its whole executable residue is `Sdl.hs:let Point{..} = toEnum i`, owned by 2.1,
whose `CellStyle` adoption rewrites that loop, together with the `CLAUDE.md`
Gotchas clause the fix retires and the standing check the fix adds: the grep
below as a step of `tools/checks.py` and a mutant in `tools/mutants.py`
restoring the `toEnum` form in `Sdl.hs` to prove the step bites (the record's
ruling); fixing it standalone contradicts the coupling stated just above
and collides with that commit's diff. The rule retires with the campaign rather
than taking a hash.

**Done** --- no gate of its own, only
`! git grep -nE 'Point\{\.\.\} = toEnum' -- 'engine-src/Game/LambdaHack/Client/UI/Frontend/*.hs' ':!engine-src/Game/LambdaHack/Client/UI/Frontend/Dom.hs'`.
It returns nonzero today, naming `Sdl.hs:let Point{..} = toEnum i` --- which
is what proves it a real search rather than a silent one --- and must return
zero once the fix lands, so it is an acceptance criterion on that commit's diff
and, as a `tools/checks.py` step from then on, a check whenever
`check-all tools` runs, not a task of its own. Deliberately not a bare `toEnum`
grep: every other `toEnum` in `Sdl.hs` is a legitimate conversion into an SDL
numeric type --- geometry mostly, and a log priority
at `Sdl.hs:SDL.logSetAllPriority $` --- and a gate that flags them would stop
being read.

**Hands back** --- *judgement*: the grep catches only the shape already seen,
`Point{..} = toEnum`. Whether a newly written decoding site passes the width
explicitly --- including one that spells the decode differently, or one
in a frontend not yet in the tree --- is a review question with no mechanical
substitute; the grep above is the floor, asked of any diff touching a frontend
module.

**Decide first** --- nothing.

**Functional core, imperative shell --- per frontend (the review bar).** Design
and rulings in the record.

**Owns** --- nothing --- this is a review rule, not an item to execute.
It generated 0.1, 0.2 and 2.1 and is stated so as to outlive them; handed out
as a task, it becomes an agent hoisting decisions out of `Sdl.hs`, `Wasm.hs`
and `ts-src/src/terminal.ts`, which is those three items' work done
speculatively, in their files, without their specs. It retires with the campaign
rather than taking a hash.

**Done** --- none exists, and inventing one is the error. The rule's entire
enforcement is the question asked of every diff touching a frontend module:
"would a fourth frontend have to copy this?"

**Hands back** --- *judgement*: all of it, and there is no substitute gate,
because there is no Done. The nearest mechanical neighbours are the other items'
own suites, which pass equally whether or not a decision was left sitting
in the shell.

**Decide first** --- nothing; the rulings are the record's.

## Sequencing

This orders the work.

```
0.0 immediate fixes: AltGraph on keys AND mouse/wheel; highlight-outline
    rule (fill-only kinds; four-sided outlines)
0.3-baseline --- FFI battery for the existing surface, before 0.1
    (imports via the test driver; exports via the thin reactor driver,
    3.2's seed)
frontend CI smokes, ANSI and SDL --- early, beside 0.3-baseline: they
    depend on nothing, and no other CI check runs those event loops
    (the nodeBench smoke waits on 3.3)
0.1 InputDecision (sync export re-spiked as its step (0); fixes the live
    input-bug class; brings the key-translation table tests, jsdom
    forwarding tests, and the input-side RawFrontend contract cases)
0.2 CellStyle + generated files (gives 2.2 its palette)
0.4 canvas painter (B.10) --- before 1.2, 1.5 and 2.2, which build on it;
    its Split (0) measures the frame cost against the Phase 2 baseline
1.1 / 1.2 / 1.3 / 1.4 / 1.5 / 1.6 --- any order; each
    capability's landing ends with its own banner commit, per 1.4's rule
    (1.2 and 1.5 come after 0.4, 1.5's full player-facing form after
    R4 too; 1.2's per-frame case waits on 3.1)
2.1 OverlayLayout extraction + Sdl.hs onto it and CellStyle
    (native-only, playtest- and bench-gated)
2.2 browser overlay renderer (visual no-op)
2.3 overlay transport (visual no-op, additive FFI)
2.4 capability flip (the visible change; heaviest review)
2.5 QA (R6 retired, B.10)
3.1 lhStart reads WASI argv (spike done and recorded in the item;
    independent of Phases 1-2; shared infrastructure with R4)
3.2 Node reactor driver (grows 0.3's seed driver; exit propagation)
3.3 repurpose nodeBench* targets; nodeMinifiedBench -> nodeDeployedBench
    (Phase 3 may run before or between Phases 1-2 --- it's independent, and
    doing it early unlocks the wasm-vs-native ratio and R1's save-lag
    measurement via 3.2's instrumented stub)
R2 CI jobs --- grow them per phase (0.3's game driver with build-wasm; a short
    nodeBench smoke run joins after 3.3)
0.3 FFI coverage --- after the baseline, incremental with every
    FFI-touching commit
capability constants / sum-typed selection --- standalone refactors, any
    time
RawFrontend contract --- haddock and checklist any time; the native harness
    after 0.1, whose input-side cases create its module; the wasm case
    with 0.3
determinism goldens --- native harness before 2.1; cross-backend under
    R2's test-wasm job
R1 --- independent but for its save-lag re-measurement, after 3.2
R4 --- after 3.1, which leaves it loader-only
R3 GHCJS rip-out --- one commit, after 2.5 (parity); Dom.hs/JSFile.hs stay
    as documented-dead examples
R5 --- after 2.5 and 3.3, measurement first
R7 --- after 0.4 and 1.5; R7b after R7a
```

Rationale, condensed: the input cluster goes first (mechanism already validated,
fixes live bugs, small); the generated files second because two later steps
consume them; multi-font last because it is the largest item and every earlier
phase either de-risks it (0.2 fixtures, 2.1 native-side proof) or is what
it builds on (2.3 uses 0.3's test harness, 2.2 uses 0.2's palette).

---
