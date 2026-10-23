# WASM Frontend Plan: SDL2 Parity on a Shared-Haskell Architecture

## Goals and approach

This plan supersedes `wasm-frontend-haskell-alignment-design.md`
and `sdl2-to-wasm-parity-plan.md`. It pursues two goals at once:

- **G1 --- one canonical definition.** Knowledge that has a canonical Haskell
  definition must not be hand-ported into TypeScript or duplicated between
  frontends. It is shared via common pure Haskell modules, generated tables,
  and generated behavioral fixtures.
- **G2 --- SDL2 parity.** Close the gaps between the SDL2 frontend
  and the WASM/browser build that this campaign schedules: pointer cursor,
  screenshots (`Ctrl+P` and `--printEachScreen` alike), fullscreen (including
  scaling), display scale, multi-font (proportional + mono) rendering,
  and the game-end page lifecycle (1.6) --- plus the input/rendering fidelity
  fixes in 0.0, the paragraph key's web clause in 0.1 and fixed-pitch glyph
  cropping in 0.4. That is not every real gap, and the goal no longer claims
  it is: the 2026-09-07 re-audit recorded in Appendix C found six more, of which
  four were scheduled the same day --- its (3) into 1.2, (4) as 1.6, (5)
  into 2.2 and (6) into 0.1, each with a ledger row --- and two were carried
  into item bodies without a row of their own: the half-cell mouse pitch, which
  0.4 has since closed under B.10, and the compound-modifier shift flag, which
  0.1 says outright needs a signature it has not specified, and which carries
  no ledger row and so has no state. SDL2's behaviour is not always its intent:
  What would falsify this has the rule and the case.

The ordering follows from G1: multi-font done naively would re-implement
in TypeScript the layout logic that lived in `Sdl.hs`'s
`drawPropOverlay`/`drawPropLine`/`drawPropChunk` ([Sdl.hs#L648][sdl-648]) until
2.1 --- a much larger instance of the hand-porting that already produced real
bugs (a DeadKey set ported with 11 of 13 values missing; a palette color slip).
So: **shared-Haskell foundations first (Phase 0), small parity wins on top
(Phase 1), multi-font as a shared-module extraction (Phase 2), and the Node
benchmark tooling port (Phase 3).** Related goals (R1--R7) and adopted
multi-frontend practices follow the phases.

Citations name a file and a text anchor its cited line carries, which
`../tools/check-plan-citations.py` finds and prints with the line's number;
there is no stamp and nothing restamps, and a rewritten line fails the pass
instead of sliding under it. Ruled 2026-09-08, applied 2026-09-14, when
the first three landings had each cost a repair commit for lines they merely
slid. Where text describing an earlier tree cites a line the code has since
rewritten, the citation is instead a GitHub permalink pinned to a published
commit carrying that tree, which the same checker settles against the commit.
The Log's entries name the line numbers they corrected in words, as history.
Some file basenames are duplicated among this repo's tracked `.hs` files,
so every citation here to one of them names its directory as well:
`Server/LoopM.hs` vs `Client/LoopM.hs`, the engine's vs the game's
`Content/Input.hs`, the engine's vs the game's `Client/UI/Content/Screen.hs`,
`GameDefinition`'s vs `definition-src`'s `Content/RuleKind.hs`,
`Client/State.hs` vs `Common/State.hs` and `Server/State.hs`,
and `Server/CommonM.hs` vs `Client/CommonM.hs`. Decisions *against* work,
deferrals and their rationale are collected in Appendix B; the SDL2-vs-wasm
audit record in Appendix C, its verified non-gaps and its 2026-09-07 gap list
alike; the GHCJS->JS-backend port investigation in Appendix A. Whether an item
has landed is answered by its row in the work list, and by whether its body here
carries an outcome line.

> **What this record is for, and what is frozen in it.** It is kept
> indefinitely, for two readers: the one who wonders why the web stack reads
> as it does, and the one who proposes a browser-side change and needs to know
> whether it was already ruled out. Since 2026-09-14 it is the record alone,
> on the model of the pointman records: the ledger, How this lands, Handing
> an item with its gates, every item's execution block --- **Split**, **Owns**,
> **Done**, **Hands back**, **Decide first** --- and Sequencing live
> in `docs/wasm-frontend-work-list.md`, which is deleted when its last row
> lands, as `docs/leader-desync-migration.md` will be. Wherever a body here
> names one of those, it means that file's. The rulings that closed an item's
> **Decide first** stay here, under **Rulings** at the end of the item; an open
> question is the work list's alone.
>
> **The state vocabulary is `docs/leader-desync-bug.md`'s, borrowed whole**,
> and the row that carries it is the work list's. Where the two ever disagree,
> that record is right and this one is the error: it is the older discipline,
> it is the one `CLAUDE.md` describes, and a vocabulary decision taken there
> needs no glance at this file. Three states, and a fourth this campaign needs
> and a post-mortem does not:
>
> - **landed**, with the commit that did it --- the item describes the code
>   as of that commit and is not maintained against later trees, bar one further
>   outcome line per claim that *resolves* and the re-anchoring of a citation
>   whose line a later commit rewrote, which changes no prose. Ordinary drift
>   in a landed item is left alone; rewriting one to match a later tree
>   is the mistake this rule exists to prevent.
> - **open** --- the code is wrong *now*, and the item says how. A drifted claim
>   here is an error to fix, and "fixing" it by writing the defect up as solved
>   is the one thing that must not happen.
> - **not applied** --- the design is specified and the artifact does not exist.
>   Its claims about *today's* tree are held to a plan's standard.
> - **standing** --- the item never lands, because it recurs (0.3, 1.4, R2)
>   or because it is a review rule with nothing to execute (explicit widths,
>   functional core). It retires when the campaign does. The asymmetry
>   is deliberate: the pointman records have no such case and need no such word.
>
> The distinction a reader will trip on is **open** against **not applied**,
> because the work list's ledger sorts by item number and not by it: R1 and 2.2
> are two rows of one table and mean different things --- a tab killed mid-save
> leaves saves of mixed generations, while `ts-src/src/overlay-core.ts`
> is a file nobody has written. The first is a bug report, the second
> a specification, and only the second may be edited to match a tree that moved.
> The rule fires at the item and the state lives in the ledger, so before
> editing any claim in an item's body, read that item's row in the work list:
> `landed` means leave it alone. The state *label* is deliberately not repeated
> at the item --- a second copy of the column is what drifts out of step
> with it. Two things are not that copy: the outcome line, which
> is the landing's own frozen record rather than a label, and a `standing` item
> saying so where its **Split** or **Owns** turns on it, as 1.4's and R2's do
> --- *this row never flips* is a rule about their commits, not a reading
> of the column.
>
> A row a ruling retires before it lands reads `retired -- <ruling>`, as R6's
> does since B.10, the ruling being its outcome, and a standing row whose code
> is wrong now reads `open -- standing`, as 1.4's does; the rest
> of the vocabulary is the four above. One claim in either state is exempt,
> by the same borrow: a recorded measurement ages rather than drifts, so
> it is re-measured against its stated protocol rather than corrected.
>
> **Landing an item** appends an outcome line here, immediately after the item's
> opening paragraph and before any subsection, so that opening the item
> at its head answers the question, and flips the item's row in the work list;
> the hash in both places names the commit that did the work, and the record
> is a commit touching no code, written after it. Partial landings, standing
> items, the allowlist gate and what a **Split**'s record commit is are the work
> list's head to state.
>
> **What this document takes, and no fifth kind:** an outcome line per claim
> that resolves; a dated audit note where a later reading refutes or sharpens
> a section and changes what a reader would do, as Appendix C's 2026-09-07 note
> and B.7's 2026-08-07 one, neither licensing a rewrite of the prose under it;
> a re-anchoring of a citation whose line a commit rewrote, changing no prose;
> and, in Appendix B alone, a new dated ruling. The Log takes a dated entry.
> Frozen from the start are the Log entry by entry, Ground rules, Out of scope
> and the appendices; live until the campaign ends, because the work falsifies
> them, are Goals and approach, Repo facts, Build & verification loop, What
> would falsify this and the intros that open a phase or the practices --- Phase
> 2's carrying the frame-timing baseline, whose numbers are exempt
> as measurements; everything else is per item, frozen when its row lands,
> and when the last row lands or retires this callout becomes one line saying
> the specification landed in full.

## Log

One line per surprise or re-plan, newest last, so that resuming this campaign
needs this section rather than a re-read of the whole plan. Log-worthy: an item
that turned out larger or smaller than its row says, a design question reopened,
a classification here found wrong, an ordering constraint discovered, a **Decide
first** ruled on --- and a count found wrong only where the correction carries
a lesson, a superseded number foreclosing nothing and the entries here being
frozen the day they are dated. That binds entries written from now on; the ones
below are records and stay as they are. Not log-worthy: doing an item
as written, or editing this file before the work starts --- an entry recording
only that the plan was written is one the next reader has to skip.

- 2026-07-30 -- plan restructured to mature rather than be split or deleted;
  execution blocks added to all thirty-one items. Twenty-five of them carry
  an unanswered **Decide first**, which is the campaign's real blocking front
  --- not the code. Three are hard blockers rather than preferences: 0.3b's
  exports cannot be called after `wasi.start()` returns, the RTS having exited;
  0.2c cannot reach `cursorXhair`, which `Sdl.hs` does not export; and 3.3 must
  choose between the node on PATH and the ghc-wasm-bundled one, which differ
  in `node:wasi`.
- 2026-07-30 -- the RawFrontend contract item's gate was `-p "/contract/"`,
  which is the pointman campaign's tag over the same suite: vacuous as a gate
  here, and it would have moved a count that document requires never to move.
  Retagged `[fe-contract]`, and the namespace is now stated in the gates above.
- 2026-07-30 -- that retag did not separate the namespaces. `-p` matches
  a substring of the test path --- `-p "/ontract/"` selects the same tests
  as `-p "/contract/"`, and `/^contract/` selects none --- so `[fe-contract]`
  sat inside the pointman campaign's filter, and the first frontend contract
  test to land would still have moved that campaign's count. Half the fix had
  been made and the whole one recorded. The series is `[fe-invariant]` now,
  and the namespace paragraph states containment rather than equality
  as the test.
- 2026-07-31 -- a field-by-field comparison against the three pointman
  documents. It moved lifecycle machinery in --- the stamp's anchor invariant,
  the carve-out for a recorded measurement, the inventory of claims no pass can
  re-run --- and couplings out: 2.4 and capability constants now name the files
  the other campaign cites into. Two counts it re-derived, and the entry above
  is history and cannot be edited to hold them: the ledger has **thirty** items,
  not thirty-one, and twenty-four carry an unanswered **Decide first**,
  not twenty-five. All four labels re-derive to thirty apiece, so no item lacks
  an execution block; the arithmetic was simply wrong.
- 2026-07-31 -- a blind review of that branch, and a study of whether keys can
  be injected into the SDL2 frontend. Corrected here: the determinism goldens
  item cited lines 630, 142 and 588 of `UnitTestHelpers.hs`, all of which
  resolve and none of which is the definition named --- `test/CLAUDE.md` had
  the right lines all along, so the disagreement between two documents is where
  this should have been caught. And the RawFrontend contract's "no in-session
  run enters its event loop" is false: `xvfb-run` plays whole games through it,
  and XTEST keys reach it with nothing installed.
- 2026-07-31 -- `firefox-beta` and `matchbox-window-manager` were installed,
  and testing the browser items against them found three more claims here false,
  all in `browser` hands-backs: 3.1's "`hlint .` does see it, `.hlint.yaml`
  passing `--cpp-simple`" --- it passes `-XNoStarIsType` alone, and a bracket
  planted each side of the `#ifdef` proved only the outer one is linted; R4's
  "No headless path exists"; and the capability constants' "nothing in-session
  enters the page-unload path", which now carries a measurement instead.
  The pattern is worth the entry: each was a *reason* for a hands-back, so each
  made a check look impossible that was merely unattempted.
- 2026-08-05 -- the **Decide first** fields were re-read against the rest
  of the document. Four claims found wrong, three of them R3's: its (a) posed
  a question the item's own body had answered as checked fact from the start ---
  no README clause survives, the "The game runs rather slowly in the browser"
  paragraph is R1's and R5's --- so the field is three questions now;
  its mention inventory listed the `nodeBench*` targets and their `SKILL.md`
  sentence, both claimed by 3.3's Split all along; and its renumbering count
  said nine, missing the live citation of lines 41--48 of `Frontend.hs`
  in the capability constants' intro. The fourth is 0.2's bench-gate question,
  which still handed the gate to a human run --- the reason 2.1's block retired
  when `xvfb-run` arrived; 0.2's Done and Hands back follow 2.1's now.
- 2026-08-05 -- the TS typecheck got its standing home: `make test-ts` runs
  `npx tsc --noEmit` before vitest --- inside the recipe line, so the Makefile's
  line count and every cited line stay put --- and the CI job inherits
  it by running the target. The `ts` gate shrank to `make test-ts`, and the trap
  bullet that made every TS-touching Done carry the clause retired with the gap.
  Non-vacuity measured with a planted type error: vitest alone passed over it;
  the reworked target fails on it.
- 2026-08-05 -- the frame-timing baseline ruled, closing 2.2's (a), R5's first
  two questions and 2.5's whole field: the capture is step (0) of 2.2's Split,
  gating its first merge; the probe is the throwaway `performance.now()` one,
  its diff recorded so 2.5 re-applies it; the venue is in-session under one Xvfb
  setup for capture and re-measurement alike, numbers compared only within it;
  the number lands at Phase 2's intro, dated, with its protocol. R5a stays where
  the ledger puts it, and R5's instrument keeps only R5's own pass. Twenty-three
  items now carry an unanswered **Decide first**, seven none.
- 2026-08-07 -- every remaining **Decide first** ruled, in one walk
  over the twenty-three items that still carried one, each field now recording
  its rulings dated. The headline shapes: 0.3's export half is a thin reactor
  driver, 3.2's seed pulled into Phase 0 --- the real binary, the reactor wiring
  and `lhStart` covered before 0.1, with lz-string, the lock file and every stub
  0.3's; one extensible JSON startup call serves 2.2's fonts and 1.5's scale;
  2.2 imposes integer cell metrics on the grid rather than chasing measurements;
  the banner is a post-condition of every capability item, "banner last" struck
  from Sequencing; thresholds decide where judgement would have waited --- R1's
  100ms autosave bar, R5's 3x ratio and 16.7ms p95; `Frontend.hs` re-exports
  the new pure modules, foreclosing `macroStep`'s parking in `InputDecision`;
  and every test-reachable internal name sits in an ordinary "internal and used
  in unit tests" export group, nothing behind `EXPOSE_INTERNAL`. Two live wrong
  claims were corrected in the fold itself rather than scheduled: 3.2's
  exit-propagation bullet and sec. 2.1's `EXPOSE_INTERNAL` test-route sentence.
  No item carries an unanswered **Decide first** now; the campaign's blocking
  front is execution, not rulings.
- 2026-08-07 -- the five unrecorded experiments re-run and the baseline taken,
  one spike session. A single throwaway probe build --- an `lhProbe*` block
  in `Wasm.hs` plus three `--export=` lines, reverted --- settled them all:
  the sync-export syntax is a trailing ` sync` in the export string, compiling
  under the reactor wiring and linking into the command suite, so 0.1's fallback
  died unfired; `getArgs` surfaces `node:wasi` args with `getProgName` holding
  argv[0]; every exit class rejects the export's promise as `RuntimeError`
  carrying the shown exception; a post-resolution fault escapes the promise ---
  in the probed forked-thread shape via the RTS's own stderr report rather
  than a Node `uncaughtException`; startup on an empty store never touches
  `LZString`; and the unmodified reactor pushed 79 frames through the driver
  stubs in five seconds, demonstrating the (c') mechanism before 0.3
  is dispatched. The frame-timing baseline landed at Phase 2's intro --- mean
  0.77ms, p95 3.00ms at n=2870, intro-animation protocol --- so 2.2's Split (0)
  gate is already satisfied. Two probe-shaped corrections rode along: 3.1's
  "not yet validated" sentence became its recorded artifact,
  and the claims-no-pass-can-re-run inventory now points at artifacts instead
  of folklore.
- 2026-09-04 -- a blind review of the branch that carried the last four entries,
  three readers over the pinned documents with the mechanical passes run first.
  What it found here, all of it ripples the 2026-08-07 rulings did not pay: 2.5
  still scheduled the banner deletion that 1.4's ruling had moved into 2.4's
  landing sequence, so its one machine-checked gate would have passed having
  tested nothing --- that commit, its checklist line
  and its `proportional fonts` grep are gone, and 2.4 and R1 gained the banner
  commits their **Owns** already implied; 0.3's **Split** both scheduled
  an allowlist deletion and said it had none; the artifact gate named five items
  and a count of entries, which an entry moving from 3.2 to 0.3 and a new one
  at 1.2 had already falsified, so it now reads that file instead of copying it;
  1.2's second commit promised to delete two of its own entries while adding
  a third; the determinism-goldens row advertised only the digest family its own
  ruling says cannot see what 2.1 and 2.4 change; 2.2 and 3.2 were dispatchable
  on `depends on` cells that omitted 1.2 and 0.3; R1, 3.2 and 1.5 still pointed
  at 3.2 for stubs that had become 0.3's; 1.4's single-writer list named four
  of the seven items that write `index.html`; and 2.3's ruling restated
  the chunk-header field list that 2.1 owns, disagreeing with it on a bit
  against a word. Two further corrections: 0.2's ruling (4) claimed
  the generator takes no SDL dependency, which `build-depends: lib:LambdaHack`
  gives it regardless, so the freshness job carries the apt line the other
  Haskell jobs carry; and the frame-timing baseline was re-taken because
  its anchor named a commit a replay had orphaned. The re-take confirms the old
  figure rather than replacing it --- p95 3.00ms both times, and mean 0.75ms
  at n=2905 against 0.77ms at n=2870 --- and found the protocol's real defect,
  that the published mean is cumulative and so meaningless without the n
  it is read at. The intro now states that, the versions, and no hash.
- 2026-09-04 -- a second reading of the same branch, this time with a blind walk
  of 3.1 by a session that tried to execute it, and the repairs for both folded
  into one pass. From the earlier half: `Sdl.hs`'s lines 645--669 re-ranged
  to 636--669 and `UnitTestHelpers.hs`'s line 207 to 208; 2.1's placement table
  restored to its eight rows from six; 3.3's `--dbgMsgSer --logPriority 4`
  bullet reversed, the two bench pairs sharing those flags rather than differing
  on them; and `[fe-golden]` minted for the determinism goldens. From the walk:
  3.1 still scheduled a spike its own body records as performed, promised
  a one-range citation repair where the change moves every `Main.hs` citation
  this document carries, and never mentioned the `System.Environment` import
  the change needs --- unguarded, an unused-import warning no gate of 3.1's
  would fail on. Counts and classifications found wrong: the header named four
  duplicated `.hs` basenames where the plan in fact cites six, so it states
  the property now; `Sdl.hs`'s line 105 is a log priority among the surviving
  `toEnum` sites called geometry; the `HighlightBackground` fill `#251F1F`
  is reachable from neither of 0.2's two named generator sources, the gap ruling
  (4) had already met over `cursorXhair`; the hot frontend files run a hundred
  to a couple of hundred lines rather than a couple of hundred each; and two
  superlatives died to one count --- of the thirty **Owns** fields, sixteen name
  `tools/doc-refs-allow.txt` and seventeen name this document, so neither
  it nor the workflow file is the campaign's most anything. Ownership gaps
  closed rather than argued: `Frontend.hs` joined the four items whose own
  rulings make them edit it, `README.md` got owners for the three clauses
  this campaign falsifies in it (the page's `#status` div quotes
  that paragraph), and `terminal.ts`, `loader.ts` and `ts-src/run-wasm-game.mjs`
  got one claimant list each --- at 0.0, 0.1 and 0.3, the shape 1.4 already used
  for `index.html` --- in place of a pairwise exclusion graph that had gone
  asymmetric. Three decisions written in rather than left to be re-taken:
  the review unit branches off the branch carrying this document
  and not `master`, whose copy of it is older; an outcome line waits on what
  **Hands back** names and not on a green **Done**; and the citation-repair
  obligation is stated once, in "Handing an item to a session", scoped to live
  items.
- 2026-09-04 -- two corrections to the entry above, which is history and
  is not edited to hold them, per the precedent the 2026-07-31 entry set.
  Its two **Owns** counts were true when they were measured and false
  by the time the pass closed, that same pass having added this document
  to three more items' **Owns** as it went; the property is what the sentence
  needed and all it needs, which is that `tools/doc-refs-allow.txt`
  and this document are each named by most items' **Owns**, so neither
  is the campaign's most contended anything. And 3.1 hands back nothing now,
  ruled today: its page load moved into **Done**, `firefox-beta` under Xvfb
  being what 2.2's Split (0) already drove to take the frame-timing baseline,
  so the claim that no in-session gate reached it had outlived the browser's
  arrival. That matters beyond the item --- an outcome line waits on what
  **Hands back** names, so the shakedown would otherwise have stopped
  at **Done** and proved neither the ledger flip nor the restamp it is chosen
  to rehearse.
- 2026-09-04 -- `tools/check-plan-crossrefs.py` landed (`8b5f72924`), reading
  this document against itself, and its first run over the tip found 60 places
  where an **Owns** shared a file with an item it did not name. Repaired
  on the one-list-per-file shape 0.0, 0.1, 0.3 and 1.4 already had: lists added
  at 2.1 for `Sdl.hs`, 2.2 for `Wasm.hs`, 0.3 for `ts-src/run-wasm-test.mjs`
  and 0.1 for `LambdaHack.cabal`, `test/Spec.hs` and `Frontend.hs`, and every
  other claimant pointed at its list. Three claims found wrong on the way:
  the RawFrontend contract said 3.2 extends `run-wasm-test.mjs`, which 2.3 does
  and 3.2 never touches; 0.1's ledger row omitted 0.0, whose harness commit
  creates the test file 0.1 writes and which 0.1's own **Owns** says lands
  first; and R3 said 2.2 extends the `os(wasi)` block of `LambdaHack.cabal`,
  a clause standing since 2026-07-30 that 2.2's body never backed in any
  revision --- its one Haskell change is a foreign import, which no cabal block
  sees --- so the clause is gone.
- 2026-09-07 -- the four gaps the same day's re-audit had left unscheduled
  are scheduled. Three fold into the item whose mechanism they need, a row being
  the unit of review and none of them a review's worth: (3) `--printEachScreen`
  into 1.2, on the `fprintScreen` override it installs; (5) fixed-pitch glyph
  cropping into 2.2's canvas shell, beside the cell rule its (a) ruling imposes;
  (6) the paragraph key into 0.1, a clause where its table tests were to pin
  an absence. (4), the game-end page lifecycle, bore on no item and is 1.6,
  `open`. Two ordering constraints came with them --- 1.2's per-frame case
  and 1.6's driver case reach game end only through argv, so both wait on 3.1,
  first anyway as the shakedown --- and one **Decide first** is open again,
  1.6's: which side signals the end is a design ruling, not a scheduling one.
- 2026-09-14 -- the first landings, 0.0, 1.1's keyword half and 1.3, in one
  session and three commits, after a review that measured the plan against
  the code it governs. 0.0 and 1.3 departed from their Splits, and their outcome
  lines say how: 0.0 took no jsdom harness, a pure `altModifier` being testable
  on a plain object; 1.3 folded its banner clause into the landing commit
  and put its wiring in a `fullscreen.ts` beside `loader.ts` rather than inside
  it. The citation repairs and the restamp ride this one `.md` commit for all
  three, not one per item.
- 2026-09-14 -- line citations retired for text anchors, the checker taught
  the form, the stamp gone, and the citation-repair commit with its restamp
  struck from every Split and Owns: the three landings had each owed one
  for lines they merely slid. Only an item that rewrites a cited line now owes
  a re-anchoring, and the pass names which. The Log keeps its numbers
  as history. The checker's horde-ad twin was unmounted and takes the same code
  when next mounted.
- 2026-09-14 -- the locks and claimant lists retired for one rule, one item
  in flight at a time in one checkout, the same review having measured the plan
  against the code it governs: the hot files are too small for two items
  to share, the plan said so itself, and the machinery had produced sixty
  findings about its own consistency and no landing. **Owns** stays
  the exhaustive file list, and names another item only where it writes the same
  lines, since whichever lands first shapes the other's diff;
  `tools/check-plan-crossrefs.py` drops its A2, the contention graph, and keeps
  the rest.
- 2026-09-14 -- ruled by Mikolaj: one canvas, no DOM grid (B.10), the question
  the review found unruled. 0.4 is the painter, R6 retires, 1.2 and 2.2 carry
  re-cut notes at their heads, and R7 records the work the canvas makes possible
  and the DOM never could: pixel-perfect `.bdf` map fonts from a glyph atlas,
  the whole canvas scaled by integers where SDL2's fullscreen scaling softens,
  the scalable font only when `allFontsScale` leaves 1.0 as in SDL2, and later
  a rule that keeps the bitmap wherever it looks crisper. R7's first draft
  credited SDL2 with a crisp doubled `.bdf` at scale 2.0; measured the same
  evening, it renders the outline font there, as README says. Appendix B takes
  a new dated ruling from now on, the freeze rule amended for it.
- 2026-09-14 -- the page gate landed, `make page-gate`
  and `ts-src/page-gate.mjs`, the campaign's acceptance test the review found
  missing: the deployed page in headless Firefox, frames after `lhStart`, a key
  changing the screen, an overlay after 2.4. Ruled local only and never a CI
  job; `page` joins every browser item's Done. Its first run passed on the tree
  as it stands, six frames of 80x24 and a `?` repainting; both controls failed
  as they should. lz-string came in as a devDependency with it, ahead of 0.3,
  which wants it for the same reason.
- 2026-09-14 -- a blind reader of the day's diff found 1.3's button inside
  the banner it hides, so fullscreen could be left by Escape alone; the button
  now sits in its own `#controls` element and floats over the letterbox. Found
  on the same pass: pointers in the new work list aimed at text that had moved
  to the record, and 1.1's Done lacking the page gate.
- 2026-09-14 -- the plan split, on the review's finding that one file carried
  two genres: this record keeps the designs, the rulings --- an item's closed
  **Decide first** now reads **Rulings** at its end --- the decisions against,
  the audit and this Log, frozen the way the pointman records are;
  `docs/wasm-frontend-work-list.md`, deleted when its last row lands, takes
  the ledger, How this lands, Handing an item with the gates, every execution
  block and Sequencing. Bodies here that name a Split, an Owns, the ledger
  or Sequencing mean the work list's, said once in the head callout rather
  than at each of a hundred sites. `tools/check-plan-crossrefs.py` reads
  the work list and treats naming the record as it treats naming the work list,
  and its snapshot control keeps the old allowlist phrase.
- 2026-10-20 -- 1.2 re-cut for B.10, smaller than its row said: a screenshot
  is 0.4's canvas by `toBlob`, so the rasterizer and its draw-op module went,
  with the rasterizer's `tools/doc-refs-allow.txt` entries, and the Split is two
  commits. The re-cut reopened ruling (b), now 1.2's **Decide first**: 1.3's
  fullscreen scales the canvas by a CSS transform, so a bare `toBlob` saves
  the unscaled frame where SDL's screenshot is of the scaled window.
- 2026-10-20 -- 2.2 re-cut for B.10, smaller than its body said: the canvas
  is 0.4's, so the overlay canvas over the span grid, its alignment, its imposed
  cell metrics with their startup assert, its `overflow: hidden` and its own
  backing store went, ruling (a) with them; the font wiring, the twin
  and the fixed-pitch crop, now a clip rect in the draw-op list, stand.
- 2026-10-20 -- two **Decide first** questions ruled by Mikolaj. 1.6's end
  is signalled by the loader on `lhStart`'s settlement, so the item drops
  its import, its driver case and its waits on 3.1 and 0.3, resting on a premise
  its browser run checks first. R7b keeps the bitmap at integer `allFontsScale`
  alone, the criterion for crisper being arithmetic.
- 2026-10-20 -- 1.2's reopened ruling (b) ruled by Mikolaj: a bare `toBlob`,
  so under 1.3's fullscreen the PNG is the unscaled frame where SDL's is
  of the scaled window.
- 2026-10-20 -- 1.5 re-cut for B.10, with an ordering constraint found: it hands
  its scale to 0.4's painter, so it waits on 0.4, and the `#screen` comment
  it was to repair is 0.4's, which rewrites that rule.
- 2026-10-20 -- ruled by Mikolaj (B.11): 0.2's generated files are written
  and checked by a tasty case in the existing suite, in place of a generator
  executable, its Makefile target and its CI job, giving up the deploy gate; R2
  no longer adds a freshness job.
- 2026-10-20 -- ruled by Mikolaj, on a review of how the campaign's rules
  outlive it: the RawFrontend contract's haddock and checklist may land ahead
  of its harness, the checklist carrying the explicit-widths and functional-core
  rules (that item's ruling (4)); the widths grep becomes a `tools/checks.py`
  step, added with the fix (that practice's ruling); the ANSI and SDL smokes
  move early in Sequencing; and B.4 stays rejected, no trigger having fired
  (its 2026-10-20 note).
- 2026-10-20 -- ruled by Mikolaj on eight gaps a review of the plan found. 0.3's
  import half adds the `npm ci` its `LZString` stub needs to the `test-wasm`
  recipe and CI job. 0.1 takes the jsdom harness, the game driver's `lhKey`
  injection and a driver run in Done, and creates
  `test/FrontendContractUnitTests.hs`, minting `[fe-invariant]`, the contract's
  harness extending it. 2.1 and 2.3 write their own fixtures by freshness cases,
  not 0.2. 0.4 takes `Wasm.hs`'s mouse exports for the half-cell column. 1.5
  creates the startup call and 2.2 adds its fonts. R4 no longer waits
  on sum-typed selection, its ruled parameters selecting no frontend. 2.5's
  baseline is 0.4's canvas number in B.10. With them
  `tools/check-plan-crossrefs.py` treats a landed or retired row's text
  as history, claiming no allowlist entry and writing nothing still to come,
  which is how `ts-src/src/terminal-input.test.ts` had lost its live owner
  unseen.
- 2026-10-20 -- ruled by Mikolaj: 0.1's input-side contract cases land
  in a commit of their own before the `lhKey` rewrite, written to pass
  on today's code, so the rewrite must keep them green. With it, 1.2's Split
  names its 0.3 and 3.1 dependencies, 1.5 stubs its startup hook in the test
  driver, through which 0.3's battery runs `Frontend.Wasm.startup`, and 0.4's
  Owns names 0.1's rewrite of the same mouse exports.
- 2026-10-21 -- ruled by Mikolaj: the `Sdl.hs` drawing loop is reviewed alone,
  on a branch carrying `Sdl.hs` and nothing else, as How this lands says,
  and 0.2's and 2.1's Splits, which put `tools/checks.py` on that branch, yield.
  With it, the loop's adoption of `CellStyle` moves from 0.2's last commit
  to 2.1, ahead of the refactor on that branch: kept in 0.2, it would leave
  that row open until 2.1's branch lands, 2.1's module landing inside 0.2's run
  of commits. 2.1's module lands ahead of the branch and the explicit-widths
  step after it, re-cutting that practice's ruling of 2026-10-20, which put
  the step in the commit that removes the violation.
- 2026-10-21 -- 0.1 landed in `83b10c8a5`, and a sync JSFFI export proved
  to leave the threads it wakes unscheduled: the built page repainted on keys
  only when another event entered the RTS scheduler, which the game driver
  and the page gate both missed, pressing their keys while the game played
  itself and its timers kept the scheduler running. `lhKey` now calls
  `rts_scheduleWork`, a `yield` there having not sufficed, and the driver
  and the page gate each press Escape on a quiet screen.
- 2026-10-21 -- so that no later sync export can miss it, the scheduler run
  moved out of `lhKey` into `Wasm.hs`'s `syncExport`, together
  with the exception handler, and `tools/check-sync-exports.py` fails a sync
  export not built on it. Why the `yield` had not sufficed, measured
  on a reproducer outside the tree: it runs the threads the export woke,
  but a thread one of those wakes goes to the end of the queue, behind
  the export's own, which then finishes and leaves the scheduler. The gap
  is GHC's, reported as GHC
  [#28010](https://gitlab.haskell.org/ghc/ghc/-/work_items/28010).
- 2026-10-21 -- the line form retired from every document and refused
  by the citation pass: the Log's quoted numbers are spelled in words, and two
  colon-led line numbers among the practices, which no pass had read since
  2026-09-14, became anchors.
- 2026-10-22 -- ruled by Mikolaj: 0.2's `palette.ts` carries no page-chrome
  colours, nothing 0.2 writes consuming them, and 1.1 takes the cursor's hotspot
  from `cursor.ts`, as it takes the URI, rather than writing it by hand.
- 2026-10-22 -- 1.2's code is committed, in `23dd759c3` and `3f6954acb`, its row
  waiting on the hand-back. Firefox takes `Ctrl+P`, Control with Shift-p,
  for a new private window, so there the page saves no screenshot. How a Firefox
  player takes one is open, Mikolaj's to rule.
- 2026-10-22 -- each of the game's Control, Alt and function-key bindings
  pressed into a page that prevents every keydown, on firefox-beta 159
  under Xvfb: Firefox keeps from the page `C-t`, `C-q` and `C-P`, which
  it reserves, and `C-Tab`, `C-BackTab`, `C-PgUp` and `C-PgDn`, which switch
  tabs, and passes it every other one; inside
  `requestFullscreen({keyboardLock: "browser"})`, an option since Firefox 151
  that leaves fullscreen on a held Escape, it passes all of them. In a window,
  the three reserved ones cleared in the profile's customKeys.json, the file
  Firefox 147's `about:keyboard` writes, reach the page too, while the tab keys
  stay Firefox's, with one tab open as with two.
- 2026-10-22 -- ruled by Mikolaj: his browser hand-backs are taken in Firefox
  alone, and a session runs each browser test of its own in Chromium as well,
  installed for it from the xtradeb PPA. 1.2's hand-back moves to Firefox
  with it, where `Ctrl+P` waits on that item's **Decide first**.
- 2026-10-22 -- ruled by Mikolaj: 1.2 gives the page gate its Chromium leg,
  through chromedriver from xtradeb's chromium-driver package,
  so `make page-gate` runs its assertions in headless Firefox and in headless
  Chromium.
- 2026-10-22 -- the same chords in Chromium 156, and in plain fullscreen, 1.3's
  today, in both browsers. A Chromium window keeps from the page `C-t`
  and the four tab keys, `C-q` and `C-P` reaching it; plain fullscreen passes
  Chromium every chord but `C-Esc`, which leaves fullscreen, and Firefox none
  of the seven it keeps in a window, `C-Esc` lost besides; fullscreen
  with the lock, `keyboardLock: "browser"` in Firefox
  and `navigator.keyboard.lock()` in Chromium, passes every chord in both.
- 2026-10-22 -- ruled by Mikolaj: no second key bindings for the chords
  a browser keeps in a window, the player going fullscreen for them. 1.3's
  toggle now enters fullscreen with the keyboard locked; `decideKey` keeps every
  Control chord but zoom's from the browser, in a window as in fullscreen; F11
  leaves fullscreen in both browsers, the page leaving on it where Chromium's
  lock hands it over and does nothing more; and the banner tells the player.
  Holding Escape, the other way out under the lock, sends the game a run
  of Escapes before it leaves. 1.2's **Decide first** is ruled with it:
  a Firefox player takes a screenshot in fullscreen.
- 2026-10-22 -- ruled by Mikolaj at 1.6's hand-back: the line saying how
  the game ended replaces the banner's text, a status line under the banner,
  as first built, having looked iffy. Fullscreen hides the banner, so the page
  leaves fullscreen first.
- 2026-10-23 -- ruled by Mikolaj at 2.1's landing: 2.1 lands ahead
  of the determinism goldens its row depends on, the frames their harness
  captures being the engine's, upstream of the `Sdl.hs` drawing 2.1 rewrote;
  a screenshot comparison of each of its `Sdl.hs` steps with the commit before
  stands in for them, and their native harness now precedes 2.4.
- 2026-10-23 -- ruled by Mikolaj at 2.2's landing: the mono layer is drawn
  as `Sdl.hs:setMonoChar ::` draws it, recoloured for the row and nothing more,
  and not through `cellRunOps`, as 2.2's "with `CellStyle`'s per-cell rules
  applied by the painter" reads. Those rules ring each cell, which in Hack-Bold
  at 13px in a half cell covers 4.9% of the printable ASCII glyphs' ink
  and a third of `M`'s, and swap a dim U+00B7 for U+22C5, which mono text
  carries as `drawBullet`'s menu bullets. The swap makes the map font's dim
  floor smaller, its U+22C5 being 2x1 pixels to its U+00B7's 2x2, while
  Hack-Bold's two dots are the other way round, so in mono it would make
  the bullets larger. Both are measured on a PIL rendering placed as the canvas
  places glyphs. Whether `cellRunOps` takes a face and a glyph table and draws
  both fixed-pitch layers is settled when the ring goes, the entry below.
- 2026-10-23 -- ruled by Mikolaj: the LambdaHack issue #281 workaround goes,
  in the step after 2.2 or the one after that. It is the black one-pixel ring
  `Sdl.hs` draws around every square cell without a coloured highlight,
  under "Rectangle drawing is broken in SDL 2.0.16", and its copies,
  `CellStyle`'s black border for those cells and the browser's `strokeRect`. SDL
  fixed the rectangle in 2.0.18, closing its issue 4001 on 2021-11-27,
  and this machine runs SDL 2.30.0. On a working SDL the ring falls inside
  the cell, where it covers 45 of the 4116 ink pixels of 99 map-font glyphs
  at 16px, the font keeping a margin, so the drop gives SDL 2.0.16 its trails
  back and otherwise changes almost nothing a player sees. The item is not yet
  written.

## Repo facts the plan builds on

- **How the wasm frontend works today, in five lines.** `ts-src/src/loader.ts`
  instantiates the reactor `CLAUDE.md` describes with a WASI shim and calls
  `lhStart`, the first of its four exports (`lhStart`, `lhKey`, `lhWheel`,
  `lhMouseUp`), saying in the banner how the game ended once `lhStart`'s promise
  settles. Each frame, `Wasm.hs`'s `display` passes the `Word32` cell buffer's
  address to `globalThis.lhSubmitFrame`; `terminal.ts` snapshots it and repaints
  the changed cells on one `<canvas>` (diffing against the previous frame)
  by the draw ops the pure `cellRunOps` in `terminal-core.ts` computes. Input
  flows the other way: DOM events -> `lhKey`/`lhWheel`/`lhMouseUp` exports ->
  the engine's key channel. Two hooks more: `startup` hands
  `globalThis.lhConfigure` the startup payload as JSON, and `fprintScreen` hands
  `globalThis.lhPrintScreen` the name to save the canvas under. Saves bypass all
  of this --- `WasmFile.hs` talks to `localStorage` directly over JSFFI.
- **`Dom.hs`/`JSFile.hs` (the GHCJS frontend) are dead code**, permanently
  and for the version reasons Appendix A.1 sets out. They still appear
  throughout this plan as the *canonical origin* of web-frontend logic:
  `Wasm.hs` and `terminal.ts` were ported from them, so where the web stack's
  intent is in question, `Dom.hs` is the reference. Their fate is decided (R3):
  no revival --- they stay as documented-dead example code, and every other
  trace of GHCJS is ripped out once WASM reaches parity.
- **Layout and deployment.** What `make build-ts` deploys is `CLAUDE.md`'s;
  the `ts-src/` layout is stated in neither document, and each item's **Owns**
  is where the files this plan writes are named --- `ts-src/src/serve.ts`
  and its `ts-src/src/serve-core.ts`, the local dev server `make serve-wasm`
  runs, being claimed by no item here. What this plan adds is where the deploy
  lands and why it is load-bearing here: the sibling checkout
  `../lambdahack.github.io` (`Makefile:build-ts:`) is the GitHub Pages site
  players actually load, so an artifact that does not reach it has not shipped,
  which is what several **Done** lines grep for.
- **Fonts.** `config.ui.default` sets `chosenFontset = "dejavuBold"`;
  its `[fonts]` section names the web fonts directly:
  `dejavuBold = FontProportional "DejaVuLGCSans-Bold.ttf.woff" 13 HintingHeavy`,
  `dejavuMono = FontMonospace "Hack-Bold.ttf.woff" 13 HintingHeavy`. The `.woff`
  files live in `GameDefinition/fonts/`.
- **The browser build has the fontset config but not the font bytes.**
  `config.ui.default` is embedded at compile time via Template Haskell
  (`rcfgUIDefault`, `GameDefinition/Content/RuleKind.hs:, rcfgUIDefault = $(do`;
  merged with the on-disk user config in `UIOptionsParse.hs:mkUIOptions ::`),
  so `schosenFontset`/`sfontsets`/`sfonts` are populated even with no argv ---
  the multi-font gate's `not (T.null (fontPropRegular chosenFontset))` conjunct
  already passes for the default fontset; only the frontend check blocks it.
  However, `GameDefinition/game-src/Client/UI/Content/Screen.hs` sets
  `rFontFiles = []` under `USE_BROWSER` (natively
  `$(embedDir "GameDefinition/fonts")`), deliberately keeping font bytes out
  of the browser payload. In the browser, fonts are **static web assets
  by design**: Haskell knows their names and sizes
  (`sfonts :: [(Text, FontDefinition)]`,
  `Common/ClientOptions.hs:[(Text, FontDefinition)]`; `FontDefinition` itself,
  whose constructors carry filename and size,
  at `Common/Misc.hs:data FontDefinition =`) but cannot supply bytes. Phase
  2.2's font wiring is built around exactly that split.
- **Font deployment gap.** `../lambdahack.github.io` currently carries just one
  of those fonts, `16x16xw.woff` --- committed by hand in 2019, not produced
  by any build --- beside `lz-string*.js`, used by `WasmFile.hs`'s save
  compression. `make build-ts` copies no fonts, so the other files
  in `GameDefinition/fonts/` never reach the pages repo, and the deployed
  `index.html` references none of them. Any step adding font usage must extend
  `build-ts` to copy the needed `GameDefinition/fonts/*.ttf.woff` files (prefer
  the Makefile over committing to the pages repo, so the repo of record stays
  this one).

## Ground rules

- Every step leaves `make build-wasm && make build-ts && make serve-wasm`
  producing a game that runs with no new console errors, and `make test-ts` /
  `make test-wasm` / `cabal test` green. Steps are independently shippable.
- Pure logic goes where it can be tested: shared decisions in pure Haskell
  modules with tasty tests; unavoidably-TS pure logic in `*-core.ts` with vitest
  tests; DOM/FFI wiring stays thin and is verified by running the game. One
  narrow exception to wiring-by-hand: input event *forwarding* in `terminal.ts`
  gets jsdom-driven vitest coverage (synthetic `KeyboardEvent`/`MouseEvent`s in,
  forwarded argument tuples out) --- the AltGraph class of bug lives exactly
  there, and this is the test that would have caught it. Focus/bfcache behavior
  stays manual (jsdom can't emulate it).
- Prefer additive JSFFI exports/imports over changing existing signatures. One
  deliberate exception: Phase 0.1 changes `lhKey` (sync + an event parameter),
  because `preventDefault` from Haskell requires it. That signature change lands
  first, in isolation, with `loader.ts` updated in the same commit; everything
  after it is additive again.
- Every test suite, old or new, runs in CI --- a test that only runs
  on a developer's machine is treated as not existing. Each new test surface
  this plan introduces lands in CI in the same commit that introduces it;
  the one pre-existing gap was doctests. *Landed in `a7e825c2d`: they run
  as their own job in the hand-written workflow, following CLAUDE.md's recipe.*
- Follow the module-as-interface convention: new shared modules go
  under `engine-src/Game/LambdaHack/Client/UI/Frontend/` and are reached only
  via the `Frontend` subtree's existing interfaces.

## Build & verification loop

`make build-wasm`, `make build-ts`, `make serve-wasm`, browser
at `localhost:8080` (`make run-wasm`). TS tests: `make test-ts` (`ts-src`'s own
`typecheck` script, its `format:check`, then vitest). The typecheck stays
in the target: it is the only TS type check that runs anywhere --- esbuild
strips types unchecked, and vitest passed over a planted type error that `tsc`
then caught. Native Haskell tests: `cabal test`. Wasm-compiled Haskell tests:
`make test-wasm` (drives the tasty binary through Node via
`ts-src/run-wasm-test.mjs`; the `common options` stanza applies
`USE_BROWSER`/`USE_WASM`/`USE_WASMFILE` to the test-suite under `os(wasi)`,
so the suite is *compiled and linked* in the real browser configuration ---
`WasmFile.hs` as the file backend, `Wasm.hs` as the chosen frontend --- which
catches wasm-only compile and link breakage. 0.3's FFI battery executes both
modules' code, `WasmFile.hs` through `Common/File.hs`'s exports and `Wasm.hs`
through `startup` and `fdisplay`, and 0.1's input-side contract cases enter
`Wasm.hs` through `startup`; the rest of the suite does not enter `Wasm.hs`,
the integration test passing `--frontendNull` and every fixture setting
`sfrontendNull = True`, so `nullStartup` stands in for `Chosen.startup`).

**Claims here that no pass can re-run.** Five live assertions rested
on experiments whose artifact was never recorded: 0.1's sync-export spike, 0.3's
"RTS is not initialised" measurement, 3.1's `getArgs` spike, and 3.2's
exit-propagation and `LZString` probes. All five were redone on 2026-08-07 ---
one throwaway probe build, a `lhProbe*` export block in `Wasm.hs` plus three
`--export=` lines (reverted, never committed), driven by a scratch Node script
mirroring `run-wasm-test.mjs`'s wiring --- and each claim now carries
its command shape and output beside it. The rule stands for the next such claim:
it can be settled only by redoing the experiment, so re-establish it before
leaning on it in a decision, and record the artifact beside the claim, the way
CLAUDE.md records what the headless SDL run needs.

## Phase 0 --- shared foundations

### 0.0 Immediate fixes (ship before anything else)

*Landed in `403aafe13`, as one commit rather than the Split's two plus
a harness: the AltGraph pair go through a pure `altModifier`
in `terminal-core.ts`, tested there on a plain object rather than under jsdom,
so no jsdom harness and no `ts-src/src/terminal-input.test.ts` were added ---
that file and its `tools/doc-refs-allow.txt` entry are 0.1's now, which names
both --- and the highlight rule is `highlightColor`'s three-kind branch, pinned
in `terminal-core.test.ts`. The `#251F1F` fill is untouched.*

*Its hand-back confirmed by Mikolaj on 2026-10-20 in a browser, on a keyboard
layout with AltGr: an AltGr chord arrives with `A-`, an AltGr middle click aims
without the description, as `A-MiddleButtonRelease` should, and an AltGr wheel
arrives as `A-WHEEL-UP`.*

- **AltGraph, keyboard.** `terminal.ts:(e) => onKey(e)` forwarded `e.altKey`
  only (since `403aafe13` it forwards the `alt` that `altModifier` computes
  at `terminal-core.ts:export function altModifier`); `Dom.hs:modAltG <- ask`
  ORs `getAltKey || getAltGraphKey`, so AltGraph-only chords (common on European
  layouts) are lost in the WASM build. Forward
  `e.altKey || e.getModifierState("AltGraph")`; 0.1's redesign then subsumes
  the fix.
- **AltGraph, mouse and wheel.** SDL folds AltGr (and the GUI keys) into Alt
  for mouse events too --- `modTranslate` reads `keyModifierAltGr` and applies
  to button/wheel events via `getModState` (`Sdl.hs:modTranslate ::`). The wasm
  handlers pass only `e.altKey`, so e.g. the `A-MiddleButtonRelease` binding
  (`GameDefinition/.../Content/Input.hs:, ("A-MiddleButtonRelease", mouseMMBMute)`)
  can't fire with AltGr. Same one-line fix per handler.
- **Highlight outlines follow SDL2.** The rule (also encoded in 0.2's
  `CellStyle` and pinned by its fixtures), read off `chooseAndDrawHighlight`
  ([Sdl.hs#L482][sdl-482]): *every* kind gets a four-sided `SDL.drawRect`
  over the whole cell box, but the colour differs. Kinds other
  than `HighlightNone`/`HighlightBackground`/`HighlightNoneCursor` go through
  `drawHighlight`, which sets `highlightToColor bg` and resets to `blackRGBA`
  afterwards (`Sdl.hs:drawHighlight !col !row !color = do`). Those three instead
  take `workaroundOverwriteHighlight`, drawing the same rect in the renderer's
  *current* colour --- always `blackRGBA` outside `drawHighlight` --- because
  rectangle drawing is broken in SDL 2.0.16 (LambdaHack issue #281) and a stale
  rect must be erased rather than left for the glyph to cover. So the browser
  rule is **black border**, not "no border", for the three kinds. It reads
  as no border on the black map background, but `HighlightBackground`
  is the vision backlight (`DrawM.hs:backlightVision ::`, on by default via
  `smarkVision = 1`, `SessionUI.hs:, smarkVision = 1`), and its grey wash
  is blitted *before* `chooseAndDrawHighlight` runs
  (`Sdl.hs:let tgtR = SDL.Rectangle (vp (col`) --- so SDL really shows a grey
  cell ringed in black there, and today's [terminal.ts#L133-L134][terminal-133]
  `inset 0 0 0 1px` box-shadow is wrong only in its `BrBlack` colour,
  not in drawing a ring at all. Fix in `terminal-core.ts` (border colour black
  for the three kinds) with a `terminal-core.test.ts` case. Background for why
  the wasm build behaved this way, and the ruling on GHCJS's two-edge rendering,
  is in Appendix B.

All three fixes land with tests: the two AltGraph fixes get the jsdom forwarding
tests (the ground-rule exception above), the highlight rule
its `terminal-core.test.ts` case.

### 0.1 `InputDecision`: one Haskell brain for keyboard/mouse decisions

**New module** `Game.LambdaHack.Client.UI.Frontend.InputDecision`:

```haskell
data KeyDecision = KeyDecision
  { kdModifier       :: K.Modifier  -- squashed modifier to actually send
  , kdResetChan      :: Bool        -- True for Esc
  , kdPreventBrowser :: Bool        -- True unless an allowed passthrough
  }
-- No key field: nothing here remaps a key, and both consumers translate it
-- before they ask (@Sdl.hs:SDL.keysymKeycode@, and @keyTranslateWeb@ in
-- @lhKey@), so each already holds *a* translated key. That the two are not the
-- same key under a compound modifier is left standing by this field list and
-- is not closed by this item: @Sdl.hs:shiftPressed =@ translates under the raw
-- left/right shift bits, independent of Ctrl and Alt, while @lhKey@ passes
-- @modifier == K.Shift@ (@Wasm.hs:K.keyTranslateWeb@), @False@ whenever Ctrl or
-- Alt is also down, so @C-S-\<key\>@ translates shifted natively and unshifted
-- in the browser. The flag is consumed upstream of @decideKey@, so closing that
-- divergence means deciding the shift flag here too, a signature this item has
-- not specified; Appendix C's 2026-09-07 audit note records the behaviour. Dead
-- keys reach only @kdPreventBrowser@; if a rule ever does remap, the field
-- returns and this comment goes.

decideKey :: K.Modifier -> K.Key -> KeyDecision
decideWheel :: Double -> Maybe K.Key      -- WheelNorth/WheelSouth/Nothing
decideMouseButton :: Int -> K.Key         -- DOM button code -> *ButtonRelease
```

*Landed in `83b10c8a5`, the `lhKey` rewrite, with one commit the Split had
not placed: `Wasm.hs`'s wheel and button bodies moved onto `decideWheel`
and `decideMouseButton` alone, in `f8b849af8`. The rewrite also has `lhKey` call
the RTS's `rts_scheduleWork`, as the `Wasm.hs` changes bullet below records.
The paragraph key, U+00A7, is read as backquote only unshifted, a shifted one
being another key's on German keyboards, and none of its shifted characters
is mapped. jsdom is held at 26, the newest admitting Node 18. The input-side
contract cases run natively on a frontend built as `lazyStartup` builds its own,
`nullStartup` and `lazyStartup` staying internal until the contract's harness
exports them, and on `Wasm.startup` under `make test-wasm`.*

*Its hand-backs confirmed by Mikolaj on 2026-10-21: the `Sdl.hs` look
under `make frontendCrawl`, and the browser checks, after the first
of these found the page repainting on keys only rarely, which the scheduler run
fixed.*

This subsumes four independent copies of the same decisions: `Dom.hs`'s
keydown/wheel/mouseup handlers (the canonical origin), `Wasm.hs`'s
`lhKey`/`lhWheel`/`lhMouseUp`, `terminal.ts`'s
`CTRL_PASSTHROUGH_KEYS`/`DEAD_KEYS` (the cross-language copy that produced
the 11-of-13-DeadKeys bug), and `Sdl.hs`'s verbatim copy of the
modifier-squash + Esc-reset block (`Sdl.hs:decideKey (modTranslate ksm) key`),
which is replaced by a `decideKey` call that ignores `kdPreventBrowser` ---
the module is frontend-universal, not web-specific. The two pieces are disjoint
on purpose: the keyboard handler's `p <- SDL.getAbsoluteMouseLocation` sits
between them and **stays**, `Sdl.hs:rf kdModifier` consuming it, and cutting
the block as one range leaves `p` unbound with the obvious repair deleting
exactly the mouse-position behaviour Appendix C's first bullet records
as a booby trap. The browser-zoom passthrough (`C-+`/`C--`/`C-0`) stays
allowlisted here; it coexists with 1.5's `allFontsScale`.

**`Wasm.hs` changes:**

- `lhKey` becomes a **sync** export taking the raw `KeyboardEvent` as a `JSVal`.
  Sync is required: async exports resolve on a microtask after the keydown
  dispatch has returned, too late for `preventDefault`. This was validated
  empirically against the real GHC wasm toolchain (a sync export's
  `preventDefault` demonstrably suppressed Tab's default; the browser makes
  a few shortcuts like Ctrl+T non-preventable by design, so don't test
  with those). Re-spiked 2026-08-07, per Split (0), and recorded: the syntax
  is a trailing ` sync` in the export string ---
  `foreign export javascript "lhProbeSync sync" lhProbeSync :: Int -> IO Int`
  --- which compiled under the reactor's `-no-hs-main`/`--export=` wiring,
  linked unchanged into the command-linked test suite, and returned its value
  synchronously when called from Node (`42`, not a thenable), so the async
  fallback branch died unfired. Known trade-off: sync exports don't yet
  propagate uncaught Haskell exceptions to the JS call site --- wrap the short
  body so nothing can throw past the boundary. A second, found 2026-10-21 when
  the page stopped repainting on keys while the game idled: a sync export
  returns once its own thread is done, so the threads a key wakes wait
  for the next event to enter the RTS scheduler, which an idle game never gets;
  `lhKey` therefore calls the RTS's `rts_scheduleWork`, which asks
  for a scheduler run right after the keydown handler, as an async export runs
  one before it returns.
- Modifiers and `.key` are read off the event from Haskell via `unsafe` imports
  (`"$1.key"`, `"$1.ctrlKey"`, `"$1.altKey || $1.getModifierState('AltGraph')"`,
  ...), putting the AltGraph OR in one place, next to where `Dom.hs` does
  the same.
- One new import `js_preventDefaultAndStop :: JSVal -> IO ()` (`Dom.hs` never
  calls one without the other), called when `kdPreventBrowser`.
- `lhWheel`/`lhMouseUp` keep their numeric signatures (TS already prevents
  unconditionally for wheel/contextmenu/mouseup, exactly like `Dom.hs`)
  but their bodies shrink to `decideWheel`/`decideMouseButton` calls.

**TS changes:** `CTRL_PASSTHROUGH_KEYS` and `DEAD_KEYS` are deleted; the keydown
listener shrinks to forwarding the event. `loader.ts`'s `LhExports.lhKey` type
updates (sync, takes the event).

**`Dom.hs`** is not refactored --- it stays as a documented-dead example file
(R3).

**Verify:**

- tasty tests for `decideKey`: each passthrough char, DeadKey, Esc, each squash
  case;
- table-driven tasty tests for `keyTranslateWeb` (`Key.hs:keyTranslateWeb ::`),
  the key-translation layer's web table, untested today and the home
  of the DeadKey bug class --- and for it alone, ruled 2026-08-07: reaching
  SDL's `keyTranslate` (`Sdl.hs:keyTranslate :: Bool`) costs `EXPOSE_INTERNAL`
  for tests, an sdl2 build-depend in the suite and a `#ifndef USE_BROWSER`
  guard, so it stays covered by the uniformity-diff review rule instead. One
  known hole in that table is in scope for a fix as well as for the tests,
  scheduled 2026-09-07: SDL maps keycode 167, the paragraph key, to backquote,
  or tilde under shift
  (`Sdl.hs:on some keyboards the key below ESC is paragraph`), where
  `keyTranslateWeb` has no clause. The clause mirroring SDL's goes into `Key.hs`
  in the commit that brings these tests, keyed on the sign itself, U+00A7 ---
  the shifted key reports a different, layout-dependent character on the web,
  and whether any of those is mapped is the executing session's to settle
  against the MDN table --- and the table pins the presence rather
  than the absence. Trivial, no binding using it; Appendix C's 2026-09-07 audit
  note records the gap;
- the jsdom forwarding tests for the `terminal.ts` listener (ground-rule
  exception);
- the input-side RawFrontend contract cases (key delivered while a frame
  is pending, Esc reset, `FrontPressed` --- see the contract practice), landing
  in a commit of their own before the `lhKey` rewrite and written to pass
  on today's code, since they guard exactly what it rewires;
- then run the game: Ctrl+- zoom, Tab, Esc, AltGraph chords, right-click, wheel.

**Rulings** --- all three ruled on 2026-08-07. (1) The sync-export spike
is Split (0)'s re-spike --- run the same day: syntax and linker coexistence
recorded in the body's bullet, the async fallback dead unfired. (2)
`Frontend.hs` re-exports `InputDecision` --- the module-as-interface convention
takes no exception --- and the same channel binds 0.2's and 2.1's new modules
and the test suite that consumes them. The known cost is accepted:
`docs/promptgetkey-hygiene.md`'s idea of parking `macroStep` here closes
as an import cycle (`KeyMacroFrame` sits in `SessionUI`, which imports
`Client.UI.Frontend`, `SessionUI.hs:import Game.LambdaHack.Client.UI.Frontend`),
so that record's `FrameM` fallback is the parking's only home --- and since
2026-09-04 it is that record's stated home rather than its fallback, this ruling
having been applied there and in the migration plan's sec. 04 rather than left
to an outcome line whose condition could never come true. (3) `keyTranslateWeb`
alone is in test scope, as the Verify bullet now states with the three costs
that ruled SDL's table out; the uniformity-diff review rule covers the SDL side.

### 0.2 `CellStyle` + generated TS tables and fixtures

**New module** `Game.LambdaHack.Client.UI.Frontend.CellStyle` holding the pure
per-cell decision currently in `Dom.hs`'s `setChar`
(`Dom.hs:let setChar :: Int`): decode `AttrCharW32`, even-row
`White`->`AltWhite`, glyph substitution (space->nbsp, dim floor->`'\x22C5'`),
highlight->border and background color (per 0.0's rule). `Sdl.hs`'s
`setSquareChar` (`Sdl.hs:setSquareChar :: Int`) contains the *same* AltWhite
and floor-substitution rules (with a bitmap-font variant, `'\x0007'`), while
`setMonoChar` (`Sdl.hs:setMonoChar :: Int`) shares only the AltWhite rule ---
which is also the honest statement of what each call site consumes.
So the module is written for and consumed by **both** native and browser
frontends, parameterized by the per-frontend floor-glyph choice (`'\x0007'`
for SDL bitmap fonts, `'\x22C5'` for SDL scalable and the web, `'.'` in ANSI
and Teletype --- `ANSI.hs:== floorSymbol then`,
`Teletype.hs:acCharRaw == floorSymbol`, which makes those two frontends
candidate consumers of the substitution rule too, though they use neither
AltWhite nor highlights). The even-row `White`->`AltWhite` recolour
is additionally exported as a small named function of its own --- row
and `Color` in, `Color` out, the exact name the executing session's --- which
`styleCell` consumes internally and 2.1's `chunkPropLine` imports: one
definition, ruled 2026-08-07, so neither item invents a second copy.

*Landed in `53f508d1a`, the crosshair commit, after `3840a808a` and `15c90625a`.
The frontend passes its substitute glyphs as a record, `CellGlyphs`, the space's
beside the dim floor's, since SDL draws a plain space; the recolour
is `rowColor` and the fill `backlightRGB`. The switch is a tasty option
of the suite's own, `--regenerate`. The index fixtures pin `cellPosition`, which
`terminal-core.ts` gained and `terminal.ts` now decodes frame indices with.
The crosshair module is `MouseCursor.hs`, which holds the cursor's size
and hotspot too, so `Sdl.hs`'s `createCursor` call and the freshness case
writing `cursor.ts` both read them there. `Sdl.hs` takes nothing else
from this item: the riders below, and its reading of `backlightRGB` in place
of `greyRGBA`, went with the drawing loop's adoption of `CellStyle` to 2.1,
as ruled 2026-10-21.*

*Its hand-back confirmed by Mikolaj on 2026-10-22: over a page using
`cursor.ts`, Firefox's pointer matched SDL's in shape and hotspot. On the page's
first load Firefox showed a plain cross until the window was moved, and
with a fresh profile the right pointer at once.*

Two riders on the `Sdl.hs` side of this work: derive `colorToRGBA`
(`Sdl.hs:colorToRGBA ::`, a hand-maintained palette copy by its own comment)
from `Color.colorToRGB`, closing the Haskell-vs-Haskell copy alongside
the Haskell-vs-TS ones; and since `Sdl.hs`'s per-cell drawing is a hot path,
gate its adoption of `CellStyle` on before/after `make bench` runs
(`benchFrontendBattle`/`benchFrontendCrawl` exercise exactly that path,
with fixed seeds), the two binaries built back-to-back on one flag set and run
interleaved --- `~/slop/CLAUDE.md`'s toggle-based A/B recipe, and this item's
to obey rather than to restate.

**Generated files** --- computed by a tasty case, the freshness case,
and committed under `ts-src/src/generated/`:

1. `palette.ts`: `PALETTE` / `HIGHLIGHT_TO_COLOR` from the real
   `colorToRGB`/`highlightToColor` (`Color` has no `Bounded` --- enumerate
   `[Black .. BrWhite]` explicitly; `Highlight` via `[minBound .. maxBound]`).
   One value the browser needs is reachable from neither source:
   the `HighlightBackground` cell fill `#251F1F`, which
   is [Sdl.hs#L876][sdl-876]'s `greyRGBA` --- a third of `colorToRGBA BrBlack`
   by its own comment, and carried by no `Color` constructor --- hand-copied
   at `Dom.hs:then "#251F1F"`, and in `terminal-core.ts` until `15c90625a`.
   It is a highlight-rule output like the border colour beside it,
   so `CellStyle` names it once, `Sdl.hs` reads it there in place of `greyRGBA`,
   and the freshness case writes it from that name. Same reachability problem
   ruling (4) meets over `cursorXhair`, different remedy: this one already has
   a home in `CellStyle` and the art has none.
2. `fixtures.json`: behavioral fixtures from `CellStyle.styleCell`
   over a representative input sample (floor bright/dim, space, ordinary glyph,
   even/odd row x `White`, every `Highlight`), plus `(i, w) -> (col, row)`
   index-decoding fixtures pinning the `toEnum`-vs-`punindex` invariant
   (guaranteed by the engine's `Client/UI/Content/Screen.hs`'s
   `rwidth == RK.rWidthMax` assertion --- which the fixtures make checkable
   from the TS side too; frontend code itself stops relying on it per
   the explicit-widths practice below).
3. `cursor.ts`: an SVG data-URI rendering of `Sdl.hs`'s `cursorXhair` bitmap
   (see 1.1), so the browser pointer is generated from the same definition
   the native cursor is built from.

`terminal-core.ts` imports the generated palette instead of its hand-written
one; `terminal-core.test.ts` asserts `styledCell` matches every fixture ---
a mismatch means TS drifted from `CellStyle.hs`, full stop.

**Freshness enforcement:** the freshness case fails on any difference between
a file it computes and the committed copy, and run with a regeneration switch
it rewrites the copies instead, so `cabal test` is the check, in CI on every GHC
the matrix builds. It lives in `test/CellStyleUnitTests.hs` and needs what
the suite's SDL fontset cases already have: the files listed
in `extra-source-files`, since CI tests an unpacked sdist,
and `#ifndef USE_BROWSER` around it, since the suite also runs as wasm
under `make test-wasm`. 2.1's branch table and 2.3's encode fixtures are written
by freshness cases of their own, in `test/OverlayLayoutUnitTests.hs`,
on the same terms. The switch is `tasty-golden`'s `--accept` or a hand-rolled
tasty option, the executing session's choice, `tasty-golden` being no dependency
today and the suite building under wasi too. Its name must not contain
`fe-golden`, which the determinism goldens claim, `-p` selecting by containment.
No deploy is gated: `make build-ts` bundles the committed copies as they stand,
so a stale one reaches the page unless `cabal test` ran first, which B.11
accepts.

**Rulings** --- all four ruled on 2026-08-07, (4) re-cut on 2026-10-20 for B.11.
(1) `styleCell`'s result type is one concrete record of engine `Color`s plus
the atlas/glyph fields, each frontend rendering it through its own thin step ---
SDL directly, the DOM grid and 2.2's canvas via the generated `PALETTE` hex
lookup that exists for exactly that mapping; the hot path stays monomorphic,
and 2.1 and 2.2 inherit the record. (2) The highlight-border rule
is a `CellStyle` output field: the three-kind branch ---
`chooseAndDrawHighlight`'s ([Sdl.hs#L482][sdl-482]), border versus background
versus none --- becomes structured output in `Color` terms, pinned
by the fixtures since every `Highlight` is in the sample, so a TS drift fails
vitest instead of passing it; the trap stands recorded: `HIGHLIGHT_TO_COLOR`
alone, where `HighlightBackground -> BrBlack` is correct, would silently revert
0.0's fix. (3) The bench gate's acceptance: interleaved A/B pairs per the toggle
recipe (`benchFrontendBattle` and `benchFrontendCrawl`, fixed seeds), accepted
if the `CellStyle` binary's times stay within the interleaved pairs' own spread,
failed on a consistent regression beyond it; the in-session llvmpipe pair
settles the verdict --- the cost under test is per-cell CPU work, which software
rendering exposes rather than masks --- and a real-display pair is handed back
only if that verdict is challenged. (4) The crosshair ASCII art moves
into an SDL-free pure shared module that `Sdl.hs` and the freshness case both
consume --- per-pixel art serving the SVG directly, where the packed 1-bpp
`Word8` vectors `cursorXhair` returns are an SDL encoding --- because
`cursorXhair` is exported nowhere and `Frontend.Sdl` is no module at all
under `os(wasi)`.
### 0.3 FFI-coverage test (baseline first, then per-commit)

GHC's wasm JSFFI is a trust-the-embedded-string mechanism with no arity
or behavior checking against the declared Haskell type (an ecosystem gap,
not fixable here: no typed-binding generator exists for the wasm backend,
and even one that did would still pay the per-call wasm<->JS boundary cost ---
it could not reproduce ghcjs-dom's zero-cost typed bindings, because GHCJS never
crosses a runtime boundary at all). Mitigation: extend the `make test-wasm`
battery (`run-wasm-test.mjs`) so every `foreign import/export javascript`
in the game's wasm build --- today spread over `Wasm.hs`, `WasmFile.hs`,
*and* `GameDefinition/Main.hs` (the `lhStart` export) --- is exercised at least
once with a known input/output. Sequenced deliberately: the **baseline battery
for the existing surface lands before 0.1**, so the riskiest FFI change
in the plan (0.1's `lhKey` signature rewrite) is made against tested ground.
After that, each new FFI declaration lands with its coverage case in the same
commit. Partly landed, in `test/FfiCoverageUnitTests.hs`
and `ts-src/run-wasm-game.mjs`: the baseline, covering every such declaration
--- the imports under `make test-wasm`, and the exports by the game driver run
by hand against a `make build-wasm` reactor, its checks made on the frames
the `lhSubmitFrame` stub records rather than on the frontend channel ruling (3)
names; after it, the imports 0.1 gave `lhKey`, covered by that driver's `lhKey`
case, 1.2's `js_printScreen`, by its `Ctrl+P` case, and 1.5's `js_configure`,
by its startup-payload case. Still to do: the standing half, and the game
driver's run in CI, with R2's `build-wasm` job.

<!-- probe: git -C .. grep -h 'foreign \(import\|export\) javascript' -- engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs engine-src/Game/LambdaHack/Common/WasmFile.hs GameDefinition/Main.hs | grep -vc '^-- ' :: 19 -->

**Rulings** --- all three ruled on 2026-08-07. (1) The export mechanism
is the thin reactor driver the Split now specifies --- a slice of 3.2 pulled
forward, driving the real binary. The measurement that forced the question
stands recorded: the test binary's own `lhKey`/`lhWheel`/ `lhMouseUp` die after
`wasi.start` returns ("newBoundTask: RTS is not initialised", a command's
`_start` running `hs_exit` on the way out), and `lhStart` lives
in `GameDefinition/Main.hs`, which the suite never links --- re-established
2026-08-07: tasty `--help` under the test driver's wiring exits 0,
and a subsequent `lhKey` call throws with the RTS printing exactly that message.
The mechanism itself was demonstrated the same day: the unmodified reactor
under a scratch driver --- `wasi.initialize`, the stubs, `lhStart` --- pushed 79
frames of 80x24 through the `lhSubmitFrame` stub in five seconds with the game
loop live. The refused branches, so they are not re-proposed: re-entrant calls
from the `lhSubmitFrame` stub would cover only the command-linked copies
of the export bodies, resting on an unspiked re-entry mechanism, and leave both
the reactor wiring and `lhStart` uncovered until Phase 3; a second,
reactor-linked test component would duplicate 3.2's machinery against
a synthetic twin while the real binary's export list stayed unexercised ---
the `.cabal` comment it contradicts is the smaller objection. The consequences
are taken deliberately: CI builds the wasm executable too (R2's build-wasm job,
closing the reactor-wiring gap CLAUDE.md names), and a slice of 3.2's design
is decided in Phase 0. (2) `test/FfiCoverageUnitTests.hs` dual-runs:
the file-backend table asserts backend-neutral semantics through
`Common/File.hs`'s exported functions and runs natively over `HSFile.hs` too,
in a temp directory, so `cabal test` exercises it on every CI GHC version
and drift between the twin backends is caught; only the frontend and export
cases carry the wasm-only guard. (3) "A known input/output" is the proposed
list, adopted as the acceptance criteria: an `encodeEOF`/`strictDecodeEOF` round
trip returning the original value; `doesFileExist` False before a write and True
after; `renameFile` leaving the new key holding the old bytes and the old key
absent (`WasmFile.hs:renameFile :: FilePath`); a damaged EOF marker rejected
rather than silently decoded, which is also R1's stated wasm round-trip test;
and, for `js_submitFrame`, the stub recording exactly one call per frame
with `(addr, w, h)` matching `rwidth`/`rheight` and the first cell word equal
to the fixture's. The export half's criteria follow the same shape when
it lands: a known synthetic event produces the expected key on the frontend
channel.

### 0.4 Canvas painter: one renderer for the browser

Ruled 2026-09-14 (B.10). `terminal.ts` keeps its name, its listeners, 1.1's
cursor, its focus and bfcache handling and its per-cell diff, and replaces
the span grid with one `<canvas>` inside `#screen`: a backing store
of `cols * boxSize` by `rows * boxSize` device pixels, `boxSize` the map font's
cell as `Sdl.hs` takes it from the loaded font, its CSS size that divided
by `devicePixelRatio`. Per changed cell, a `fillRect` of the background,
the glyph by `fillText` centred in the cell and clipped to it, as `Sdl.hs`
crops, in the `lambdaHackFont` face, and a `strokeRect` one pixel inside
the cell in the border colour --- the three values `styledCell` already
resolves, so `terminal-core.ts` and its tests stand, and the ring is SDL2's
`drawRect` rather than an inset box-shadow imitating it. The pointer reports
pixels, and the wheel and mouseup handlers divide them by `halfSize` as SDL2's
`pointTranslate` does, passing the half-cell column to `lhWheel`
and `lhMouseUp`, which build their `PointUI` from it directly rather
than by `squareToUI`, whose doubling made every column even; that closes
Appendix C's (1). Fonts load before the first paint, `document.fonts.load`
awaited once. The painter is the draw-op core 1.2 and 2.2 consume: a pure
function from a cell run to fill, text and stroke commands
in `terminal-core.ts`, and a thin interpreter over the 2D context, so op lists
stay testable without a canvas and 2.1's three overlay sections draw through
the same core.

*Landed in `9eb0c2c3c`, the Split's (1), with (0)'s number recorded in B.10
after it, by `91ca6700f`, so that the record names the painter it measured. One
thing beyond this body: a change of `devicePixelRatio`, as browser zoom makes,
repaints the last frame at the new ratio rather than leaving the browser
to resample the canvas.*

*Its hand-back confirmed by Mikolaj on 2026-10-22: the frames of Split (0) read
as the DOM build's, and on the deployed page a left click on a menu choice's
last half-cell activates it and an AltGr middle click on the map snaps
the crosshair without the description. The one AltGr click at half-cell
precision first asked for, tried in the menu, did nothing there, the menu
dropping a modified click as under SDL; `20193a152` split it in two.*

**Rulings** --- the shape is B.10's and the number is Split (0)'s to take.

---

## Phase 1 --- small parity wins

Independent of each other but for 1.2, whose keyboard lock extends 1.3's
`fullscreen.ts`; any order otherwise. 1.3 doesn't depend on Phase 0, nor does
1.1's first form, its final form resting on 0.2's `cursor.ts`; since B.10 1.2
waits on 0.4, whose canvas its screenshot is, and 1.5 on 0.4's painter, which
takes its scale. 1.2's per-frame case waits on 3.1.

### 1.1 Crosshair cursor over the map (trivial)

`ts-src/src/terminal.ts` `buildGrid` set `container.style.cursor = "crosshair"`
among its container styles from `91d75ca5e` until `0a72de79c` ---
the zero-dependency interim step, landed without an outcome line, the row
staying live for the final form; Mikolaj confirmed on 2026-10-20
that the pointer reads as a crosshair over the whole grid. The final form rests
on 0.2, whose freshness case writes an SVG data-URI cursor from SDL2's own
bitmap definition (the ASCII art at `MouseCursor.hs:xhairCursorPixels ::` since
`53f508d1a`, which `Sdl.hs:cursorXhair :: (VS.Vector` packs for SDL; 32x27,
hotspot (13,13) at `MouseCursor.hs:xhairCursorHotspot = (13, 13)`):
`terminal.ts` switches to `cursor: url(<data-URI>) 13 13, crosshair`, taking
the URI and the hotspot both from `cursor.ts` --- the keyword staying
as the fallback. Pixel-parity with the native pointer, sourced from the one
canonical definition. Verify by hovering, both before and after the switch.

*Landed in `0a72de79c`, as designed, the Split's second commit after
`91d75ca5e`.*

*Its hand-back confirmed by Mikolaj on 2026-10-22 on the deployed page:
the pointer over the map is SDL's crosshair, shown at once.*

### 1.2 Screenshots: make `Ctrl+P` real (small)

Today `C-P` -> `PrintScreen` -> `printScreenHuman` shows *"Screenshot printed."*
and calls `fprintScreen`, which for wasm
is the `Common.hs:fprintScreen = return` dummy. This item restores that key and,
since 2026-09-07, `--printEachScreen` with it: the flag that dumps every frame
is read at `Sdl.hs:when sprintEachScreen $ printScreen sess` and nowhere else,
after `SDL.present`, and its browser counterpart is `Wasm.hs`'s `display`
calling the same `fprintScreen` action after `js_submitFrame` when
`sprintEachScreen` is set --- which needs `startup` to keep the `ClientOptions`
it discards today, the edit 1.5 makes too, so whichever of the two lands first
makes it. Two things about it are not obvious. That call reaches TS before
the rAF paint, so the capture waits for that paint when a frame is pending
and runs at once otherwise, or every per-frame PNG shows the frame before.
And nothing in the browser can set the flag --- the loader passes no argv
and R4's allowlist admits no debug flag --- so its consumer is the reactor
driver, where Split (2)'s coverage case runs a second time
under `--printEachScreen` and asserts one call per frame; that run is why
this item waits on 3.1. Appendix C's 2026-09-07 audit note records the gap.
The fix follows `Sdl.hs:let rf = rfWithoutPrintScreen`'s own pattern (override
the field after `createRawFrontend`), with the **filename scheme living
in Haskell**, shared with SDL2:

1. Extract `Sdl.hs:printScreen :: FrontendSession`'s timestamp scheme
   (`"prtscn" <> dateText`, spaces->`_`, `:`->`.`) into a small shared pure
   helper, in a new pure sibling module under `Client/UI/Frontend/` (ruled
   2026-08-07; `Frontend.Common` keeps its IO character, and the choice fixes
   the tasty module's name and its `LambdaHack.cabal` stanza), unit-tested
   in tasty against a fixed time; `Sdl.hs` calls it too. Timezone: UTC
   in the browser --- under wasi `getTimeZone` resolves to UTC,
   and that divergence from SDL's local-time names is accepted (document
   it in the helper's haddock; a `js_tzOffsetMinutes` FFI import was considered
   and rejected as not worth the extra FFI surface).
2. `Wasm.hs`:
   `foreign import javascript unsafe "globalThis.lhPrintScreen($1)" js_printScreen :: JSString -> IO ()`;
   `startup` sets `fprintScreen` to compute the name (`.png`, not `.bmp` ---
   canvas exports PNG natively) and call it.
3. TS: `lhPrintScreen(name)` (wired in `loader.ts` next to `lhSubmitFrame`)
   takes 0.4's canvas by `toBlob` as a PNG and triggers an `<a download>` click
   with the name. There is no second renderer (B.10): the PNG is the painter's
   backing store, `cols * boxSize` by `rows * boxSize`, overlays included once
   2.4 flips, so 1.5's scale reaches it through `boxSize`. 1.3's fullscreen does
   not: it scales the canvas by a CSS transform and leaves the backing store
   as it is, so a bare `toBlob` saves the unscaled frame where SDL's
   `printScreen` reads the live `SDL.windowSize`, and that stands by ruling (b):
   the PNG is the frame at its own size with fullscreen on or off.

*Landed in `23dd759c3` and `3f6954acb`, the Split's (1) and (2),
then `225533cd1`, the page gate's Chromium leg, and `37cd2f315`, fullscreen
with the keyboard locked, which a Firefox player's `Ctrl+P` needs. Beyond
this body: `decideKey` keeps every Control chord but zoom's from the browser,
and F11 leaves fullscreen in both browsers.*

*Its hand-back confirmed by Mikolaj on 2026-10-22, in Firefox.*

Verify: tasty test for the filename helper; Split (2)'s two reactor-driver runs,
the machine criterion; run the game, press `Ctrl+P` --- a one-time human glance
that a PNG downloads under the helper's name and shows the screen. In a Firefox
window that chord opens a new private window, and the page saves nothing:
Firefox passes a page the keydowns of Control and Shift but never the `P`'s,
so no `preventDefault`, such as `lhKey` calls and `Dom.hs` called, can stop
the window, nor can `permissions.default.shortcuts` set to allow (measured
2026-10-22 on firefox-beta under Xvfb, where the same page given
`--printEachScreen` downloaded a PNG of each frame).

**Rulings** --- all four ruled on 2026-08-07, (a) and (b) re-cut on 2026-10-20
for B.10 and (b) re-ruled by Mikolaj the same day. (a) The machine criteria
are the tasty test and the driver runs: the draw-op list (a) named is 0.4's,
and the PNG is the canvas's own pixels, so the look is a one-time human glance
and no tolerance comparison is built. (b) The screenshot honors the live
rendered size under 1.5's scale, which the backing store has, and
not under 1.3's fullscreen, where SDL's `printScreen` saves the scaled window:
a bare `toBlob`, B.10's one call. Weighed and not taken: the canvas drawn
at 1.3's fit scale into an offscreen canvas first, parity with SDL at the price
of a second call and of `drawImage`'s resampling, which need not blur
as the transform does on screen. (c) The filename helper lives in a new pure
sibling module under `Client/UI/Frontend/`, as the body now states;
`Frontend.Common` is untouched. (d) 0.3's per-commit rule binds `js_printScreen`
--- the field's "nothing headless executes `Wasm.hs`" premise retired with 0.3's
reactor-driver ruling --- via the stub-and-injected-`Ctrl+P` case Split (2) now
carries.

### 1.3 Fullscreen toggle with scaling (small, no Haskell)

Page-level only (`sfullscreenMode` is a startup-time SDL choice,
`ClientOptions.hs:data FullscreenMode =`, with no in-game command even
natively). Add a "Fullscreen" button --- the word alone, no glyph before it:
the page sets `font-family: lambdaHackFont, monospace`, and that face covers
the Latin alphabet and stops well below the geometric-shape block, so an icon
codepoint would fall through to whatever generic `monospace` supplies
and no gate here asks what that draws --- to `GameDefinition/index.html` next
to the banner; `click` -> `document.documentElement.requestFullscreen()` /
`document.exitFullscreen()` toggled on `document.fullscreenElement`; keep
the label synced via `fullscreenchange` (covers browser-`Escape` exit). A button
beats telling players to press F11: several browsers leave some chrome visible
under F11, and the Fullscreen API is the closer analogue of SDL2's
`BigBorderlessWindow`.

*Landed in `d211caa38`, as one commit rather than the Split's two: the banner's
"fullscreen" clause came off in the same commit, under 1.4's rule but not
in a commit of its own. The wiring sits in a new `ts-src/src/fullscreen.ts`
beside `loader.ts`'s one import and call, on `terminal.ts`'s pattern,
and the arithmetic in `fullscreen-core.ts` under vitest. The page,
not `#screen`, goes fullscreen, the banner hidden meanwhile, and the grid
is scaled by `transform` with a `translateY` that centres it; focus returns
to the grid after the click, since the button took it.*

*Its hand-back confirmed by Mikolaj on 2026-10-20 on a 1080p screen: the page
enters fullscreen and leaves it by the button, Escape and F11, the grid centred
and aspect-correct, and the blur at a non-integer scale acceptable. A scale read
from a docked developer console is the fit to the viewport the console leaves
--- 1.19792 there, 460 px of height over the grid's 384 --- not the fullscreen
one.*

**Scaling is part of parity, not a nicety:** SDL fullscreen sets
`rendererLogicalSize` so the whole frame scales up to fill the screen
with aspect ratio preserved (`Sdl.hs:unless (fromMaybe NotFullscreen`); a bare
`requestFullscreen()` would just center the fixed 16px grid in a sea of black.
While fullscreen, scale `#screen` to fit --- e.g. a CSS
`transform: scale(min(vw/gridW, vh/gridH))` computed
on `fullscreenchange`/`resize` (transform keeps the DOM layout untouched;
bitmap-font blur at non-integer scales is the same trade-off SDL's scaler
makes). Factor any pure scale-computation helper into a `*-core.ts` for vitest;
the `requestFullscreen` call itself is a run-the-game check (jsdom can't).

**Rulings** --- both ruled on 2026-08-07. (a) `loader.ts` imports
`fullscreen-core` and carries the one wiring call --- the shape the Owns already
sketches; one bundle, one entry point, and the inline `<script>` branch stays
refused, `make build-ts` merely `cp`ing `index.html` so a syntax error there
deploys green. (b) The helper measures the live grid --- `#screen`'s
`getBoundingClientRect()`, the pure core taking grid and viewport sizes
as parameters --- and never assumes a cell size, which restores "any order,
parallel-friendly" for this pair and is correct independently of 1.5: browser
zoom through the allowlisted `C-+`/`C--` passthrough already changes
the effective cell size today.

### 1.4 Banner truth maintenance (trivial, recurring)

`GameDefinition/index.html`'s banner will go staler as features land
("For proportional fonts, use the native binary"). Update the text as each
feature ships. The savefile-corruption caveat stays until R1 addresses it.
**The page is not the only one**: the `#status` div carries "from README:"
and quotes `README.md`'s "Installation of the sample game from binary archives"
paragraph, so the square-font and savefile-corruption sentences are in two
files, and this item's rule binds both --- a clause deleted from the page
is deleted from the README in the same commit. (The GHCJS-era page's community
features are deliberately not restored, and the page `<title>` stays hardcoded
--- Appendix B.)

**Rulings** --- ruled on 2026-08-07: the banner is a post-condition of every
capability item. Each such item's landing sequence ends with its own banner
commit under this item's rule --- deleting the clause its feature falsified,
repairing the carrier sentence as the **Split** specifies, and running
this item's Done with its own phrase in the alternation --- so the page stops
lying the day the feature ships, and `index.html` joins 2.4's and R1's **Owns**
(1.3 already carries it), `README.md` alongside it wherever the clause stands
in both. Each clause deletion is disjoint, so no two landings touch the same
words. Sequencing's "banner last" reading is struck.

### 1.5 `allFontsScale` in the browser, alongside browser zoom

The browser build supports *both* mechanisms, as complementary: `allFontsScale`
(the game's own display-scale setting, as in SDL2) *and* the browser's native
zoom (`C-+`/`C--`/`C-0`, allowlisted in 0.1).

*Landed in `433743890`, the Split's (1), the hook named `lhConfigure`. Beyond
this body: the payload carries `allFontsScale` only where the client options do,
so the test suite's fixtures, which set none, hand over an empty object,
and the reactor driver checks the value an "--allFontsScale X" pair of args
gives.*

*Its hand-back confirmed by Mikolaj on 2026-10-22, in Firefox.*

Mirror SDL2's semantics (`Sdl.hs:if fromJust sallFontsScale`): at scale 1.0 use
the pixel-perfect rendering --- for the web that means the map font's `.woff`
drawn at its native 16px, as 0.4's painter draws it, until R7a puts the `.bdf`
itself there --- and at any other scale fall back to scalable rendering, i.e.
the same outline font at `16 * scale` px, accepting the browser's rasterization
the way SDL accepts FreeType's. Auxiliary (prop/mono) font sizes multiply
by the scale too, which 2.2's font wiring already specifies.

Mechanics: `sallFontsScale` is already in the embedded config/options; pass
it to TS in the one extensible startup call, which 2.2 specifies
under "The startup payload, specified here" and which this item creates
with its one named field, 2.2 adding its fonts after --- and hand it to 0.4's
painter, which loads the map font at `16 * scale` px and takes its cell
from the loaded font, as `Sdl.hs` does, rather than from a hardcoded 16px,
so cell metrics stay consistent. That is why this item waits on 0.4: on the DOM
grid the same change would be a `font-size` the painter then replaces. Until R4
lands there is no way for a *player* to change the value in the browser
(no config file, no argv), so the full player-facing feature
is `1.5 + R4 (?allFontsScale=)`; 1.5 alone makes the engine-side value honored
instead of silently ignored.

**Rulings** --- ruled on 2026-08-07: this item's scale is a named field
of the one extensible startup payload, so a scale-only `lhSetFontScale`
is refused and no later field forces the signature change the ground rules
forbid. The payload itself --- hook, encoding, extensibility --- is specified
at 2.2 and deliberately not restated here. One thing that looked like a decision
and is not: SDL's "bitmap at 1.0, scalable otherwise" branch
(`Sdl.hs:if fromJust sallFontsScale`) needs no browser counterpart, because
the `dejavuBold` fontset points both `fontMapScalable` and `fontMapBitmap`
at `16x16xwScalable`, i.e. at the same `16x16xw.woff` the page already loads ---
so `16 * scale` px on the deployed font is correct and deploys no new font.

### 1.6 Game-end page lifecycle (small)

At game end SDL frees its fonts, destroys renderer and window and quits
(`Sdl.hs:SDL.destroyRenderer srenderer`); the browser build's `shutdown`
is `return ()` and the loader discards `lhStart`'s promise, so a finished game
leaves a live-looking frozen page whose screen goes on swallowing keys,
and a crash leaves the same page. The loader fixes both by keeping `lhStart`'s
promise and acting on how it settles, under 3.2's success rule: resolved,
or rejected with the message `ExitSuccess`, marks the page finished --- a line
in place of the banner's text naming a reload as the way to play again, the page
first leaving fullscreen, where the banner is hidden --- and any other rejection
marks it crashed, the line giving the exception's message. `shutdown` stays
`return ()`, so no FFI declaration is added and 0.3's rule asks no coverage
case. Restarting in place, `lhStart` called a second time on the live instance,
is unmeasured and not proposed. A fault raised in the RTS scheduler loop
on a later tick escapes the promise, as 3.2 records, and so this handler. R4's
on-page message for a rejected parameter is a branch of the same handler,
whichever of the two items lands first creating it. 1.4's rule reaches no banner
clause here.

*Landed in `2e9908375`, the Split's (1). Beyond this body: a save-and-exit
through the main menu resolved `lhStart`'s promise, in Firefox and in Chromium,
rather than rejecting it with `ExitSuccess`, so the ruling's premise holds.*

*Its hand-back confirmed by Mikolaj on 2026-10-22.*

**Rulings** --- ruled 2026-10-20 by Mikolaj: the loader signals the end,
on `lhStart`'s settlement, and not `shutdown` through a new import in SDL's
shape. Taken for showing a crash as well as a clean end, for needing no FFI
and no wait on 3.1 and 0.3, and for building once the handler R4 needs anyway;
given up are SDL's shape, a Haskell-side moment and a headless gate, the page
being checked in a browser alone, and Appendix C's (4) now reads as a non-gap,
`shutdown` doing nothing by design. The premise is 3.2's probe on a throwaway
`lhProbeCrash` export: no real save-and-exit has been seen to settle
the promise, so Done's browser run is the first to see it, and should
it not settle, the ruling reopens. Ruled 2026-10-22 by Mikolaj,
at the hand-back: the line replaces the banner's text, a status line
under the banner having looked iffy.

---

## Phase 2 --- multi-font, as extraction rather than re-implementation

Strict chain 2.1 -> 2.2 -> 2.3 -> 2.4; each independently shippable and visually
a no-op until 2.4 flips the switch. (Until then, nothing is missing on screen:
under `singleFontSetup` the engine pre-flattens all overlay text
into the square-font `singleArray`, which is why the wasm build shows menus
and the log today, just in the map font.)

Before 2.2 lands, capture a browser frame-timing **baseline** and re-measure
at 2.5, so multi-font's rendering cost is an attributed, measured change rather
than a guess. Ruled on 2026-08-05: the capture is step (0) of 2.2's Split, uses
the throwaway `performance.now()` probe --- R5a is not pulled forward --- runs
in-session under the same Xvfb setup as 2.5's re-measurement, the two numbers
comparing only within that setup, and lands here: a dated number
with its protocol and the probe's diff, appended to this paragraph when taken.
Re-ruled 2026-10-20 by Mikolaj: B.10 put the canvas between this number and 2.2,
so the baseline 2.5 compares against is 0.4's, taken by 0.4's Split (0)
on the canvas, beside the DOM grid re-measured rather than against this one,
and recorded in B.10 with its probe; this DOM-grid number stays as history,
and the move from DOM to canvas is charged to 0.4.

**Taken 2026-09-04**, pre-2.2, on this branch's tip with the probe below applied
and nothing else: **p95 3.00ms** per frame, and **mean 0.75ms read at n=2905**,
measured as `submitFrame` entry to end of `applyFrame` inside the rAF callback
by the throwaway probe, which publishes cumulative n/mean/p95 through
`document.title`. **The mean is cumulative, so it falls with n as warm-up washes
out and is meaningless without the n it was read at**: one run gave 0.93
at n=533, 0.77 at n=1733, 0.75 at n=2905 and 0.63 at n=4531. p95 is the stable
figure --- 3.00 in every reading of that run bar two early excursions to 4.00
--- so 2.5 compares p95 against p95 and the mean only at the same n. Protocol:
`make build-wasm` and `make build-ts` with the probe applied, `make serve-wasm`,
`firefox-beta --no-remote` with a scratch profile (first-run pages suppressed)
under Xvfb `:99` at 1280x900x24 with matchbox, EGL pinned to Mesa; the intro
animation is left running with no input --- it repaints continuously at roughly
18 frames a second here, which is the whole load --- and the title is read via
`xdotool getwindowname` once p95 holds steady across successive readings.
llvmpipe numbers, comparable only within this setup. Versions, so that a later
re-measurement can tell a toolchain change from a rendering one: wasm GHC
9.12.4.20260402, firefox-beta 156.0b2, node v26.4.0 from `~/.ghc-wasm/env`.
An earlier run that also drove 150 arrow keys after the intro read mean 1.36ms,
p95 4.00ms at n=581 --- warm-up dominated, and the keyed frames add no load
the animation lacks, so the input-free protocol is the one 2.5 repeats.
The probe's diff, which 0.4's Split (0) adapts to the canvas:

```diff
--- a/ts-src/src/terminal.ts
+++ b/ts-src/src/terminal.ts
@@ -118,6 +118,10 @@ export function mountTerminal(
   let pendingFrame: Uint32Array | null = null;
   let rafHandle: number | null = null;
 
+  // THROWAWAY frame-timing probe (2026-08-07) -- never commit.
+  const lhProbeSamples: number[] = [];
+  let lhProbeT0 = 0;
+
   // Apply one already-snapshotted frame's cell diffs to the DOM. Deferred
   // to requestAnimationFrame by submitFrame() below, batching same-tick
   // calls into a single browser paint, mirroring Dom.hs's
@@ -139,6 +143,7 @@ export function mountTerminal(
   // Snapshots the framebuffer and schedules a render; does not paint
   // synchronously itself (see applyFrame, deferred below via rAF).
   function submitFrame(addr: number, w: number, h: number): void {
+    lhProbeT0 = performance.now();  // THROWAWAY probe -- never commit.
     if (w !== cols || h !== rows) buildGrid(w, h);
     // Snapshot synchronously: the wasm buffer at `addr` is only valid for
     // the duration of this call (Wasm.hs's display uses an `unsafe` FFI
@@ -152,6 +157,16 @@ export function mountTerminal(
         const frame = pendingFrame;
         pendingFrame = null;
         if (frame) applyFrame(frame);
+        // THROWAWAY frame-timing probe (2026-08-07) -- never commit.
+        if (frame) {
+          lhProbeSamples.push(performance.now() - lhProbeT0);
+          const n = lhProbeSamples.length;
+          const sorted = [...lhProbeSamples].sort((a, b) => a - b);
+          const mean = lhProbeSamples.reduce((a, b) => a + b, 0) / n;
+          const p95 = sorted[Math.max(0, Math.ceil(n * 0.95) - 1)];
+          document.title =
+            `LHPROBE n=${n} mean=${mean.toFixed(2)} p95=${p95.toFixed(2)}`;
+        }
       });
     }
   }
```

### 2.1 Extract `OverlayLayout`: the pure half of `Sdl.hs`'s overlay drawing

**New module** `Game.LambdaHack.Client.UI.Frontend.OverlayLayout`, holding every
per-line and per-chunk decision that today sits interleaved with SDL IO
in [Sdl.hs#L571][sdl-571]. It is cut along the *measurement* line, and that seam
is the design: chunking, colours, faces and the two fixed-pitch line cuts
are pure and take no metrics at all, while the proportional pen --- the one
quantity that depends on rendered width --- advances through two small pure
functions: a measurement-free cutoff test, called *before* a chunk is rendered,
and a fit function called with the width just measured. The ruler stays
in the frontend; the cutoff rule, the clamp rule and the trimmed-line marker get
one definition.

*Landed in `3fcc93a1e` and `bfd562927`, the Split's (2) and (3), after
`b42a2ebe2`, its (1), whose tests write the branch table,
`ts-src/src/generated/prop-fit.json`, through 0.2's freshness case and switch,
exported from `test/CellStyleUnitTests.hs`; `6354a14c6` is its (4). It landed
ahead of the determinism goldens, as the Log's ruling of 2026-10-23 says,
and in their place each of (2) and (3) was measured against the commit before,
under Xvfb: the screenshots of every frame drawn in the first 300
of `make bench`'s seeded battle and crawl games are identical but
for the seconds of a date the game prints, and over ten interleaved rounds
of `benchFrontendBattle` and `benchFrontendCrawl`, with the null-frontend
`benchBattle` as control, the median ratio of user-space instruction counts
is within 0.004% of 1 and that of session times within 2.2%, the control's being
the widest.*

**Metrics.** Everything device-space is parameterized, never baked to SDL's
numbers:

```haskell
-- | Cell metrics in one consumer's own device units: SDL pixels of the loaded
-- map font, or the device pixels of 0.4's canvas painter, which takes its
-- cell from the loaded font as SDL does (B.10).
data LayoutMetrics = LayoutMetrics
  { lmHalfSize :: Int  -- ^ one 'PointUI' step, and the mono pitch
                       --   (@Sdl.hs:halfSize) (row@)
  , lmWidth    :: Int  -- ^ @rwidth coscreen@, screen width in squares
  }
  deriving (Show, Eq)

-- | One square cell: the square pitch and the row height of all three
-- layers (@Sdl.hs:halfSize) (row@, @Sdl.hs:tgtR = SDL.Rectangle (vp (col@,
-- <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L686>). Derived, not stored, so that "map font
-- determines cell size for all others" (@Sdl.hs:font determines@) cannot come
-- apart in a consumer or in a test fixture --- an inconsistent record stops
-- being representable, which a hidden data constructor alone cannot deliver
-- once the tests reach this module through the "internal and used in unit
-- tests" export group.
lmBoxSize :: LayoutMetrics -> Int
lmBoxSize = (2 *) . lmHalfSize

-- | The only constructor exported, the data constructor staying inside the
-- module as 'PropCursor'\'s does. @Sdl.hs@ passes half the loaded map font's
-- size, that font having been opened at a point size already multiplied by
-- @sallFontsScale@ (@Sdl.hs:intToDouble fsize@); the browser passes its
-- painter's @halfSize@, half the cell it takes from the loaded map font
-- the same way. @width@ is the same @rwidth coscreen@ the caller passes to
-- 'layOutFrame' --- the one quantity this record shares with the metric-free
-- half, and two different values there would leave the line filter and the pen
-- clamping against different screens, so a consumer binds it once and uses that
-- binding twice.
mkLayoutMetrics :: Int -> Int -> LayoutMetrics
mkLayoutMetrics halfSize width =
  assert (halfSize > 0 && width > 1)
  $ LayoutMetrics halfSize width
```

**The metric-free half.** Its invariant is visible in the signatures: none
of these takes a `LayoutMetrics`, so none of them can emit a device-space
number. `cellPitch` is the one exception, labelled as such: it interprets
a `CellLayer` into a device advance, and is here only to sit beside the type
it interprets.

```haskell
-- | Which of the two proportional faces draws a chunk (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L678>,
-- over the two fonts loaded at @Sdl.hs:(fontPropRegular@).
--
-- Deliberately not 'Overlay.DisplayFont': that type names the three overlay
-- *layers* (@Overlay.hs:data DisplayFont@), is kept abstract on purpose so that
-- 'FontSetup' overrides cannot be bypassed (@Overlay.hs:frontend-enforced@),
-- and refuses width questions outright (@textSize PropFont@ is an @error@,
-- @Overlay.hs:textSize :: DisplayFont@) --- which is the one question this
-- module poses. It has no bold constructor either, so it cannot express this
-- choice.
--
-- Two constructors and no square or mono ones: those layers have exactly one
-- face each, so their per-item font choice does not exist.
data PropFont = PropRegular | PropBold
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | The face rule, on the already even-row-adjusted colour. @AltWhite@ sorts
-- between @White@ and @BrBlack@ (@Color.hs:data Color =@), so the even-row
-- substitution provably cannot flip a chunk from regular to bold --- which
-- is why 'chunkPropLine' may apply it before reading the colour here, as
-- <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L668> does.
propFontOf :: Color.Color -> PropFont
propFontOf fg | fg >= Color.White && fg /= Color.BrBlack = PropRegular
              | otherwise = PropBold

-- | One maximal same-colour run of a proportional line. It carries no position:
-- its x is the running sum of the *measured* widths of every chunk before it
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L674>), which is the whole reason the placement half of
-- this module exists.
data PropChunk = PropChunk
  { pcColor :: Color.Color   -- ^ even-row adjusted
                             --   (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L668>)
  , pcText  :: Text          -- ^ colours dropped (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L673>);
                             --   never empty
  }
  deriving (Show, Eq)

-- | Which face draws this chunk. Derived, not stored, so it cannot be supplied
-- out of step with 'pcColor'.
pcFont :: PropChunk -> PropFont
pcFont = propFontOf . pcColor

-- | @T.all isSpace@ of the text (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L681>), derived likewise. Not
-- a convenience: it decides whether an overrunning run may claim the last cell
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L531>) and whether the cut raises the marker
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L555>), so it is the one measurement-rule input that must
-- reach the consumer, and 2.3 carries it on the wire as a bit rather than
-- letting the far side re-derive the rule.
pcAllSpace :: PropChunk -> Bool
pcAllSpace = T.all isSpace . pcText

-- | One proportional line, still at its logical 'PointUI' start.
data PropLine = PropLine
  { plStartX :: Int  -- ^ 'PointUI' x, in half-cells; multiplied by
                     --   @lmHalfSize@ exactly once, in 'startPropLine'
                     --   (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L651>)
  , plRow    :: Int
  , plChunks :: [PropChunk]
  }
  deriving (Show, Eq)

-- | Which fixed-pitch layer a run belongs to. Total, unlike a face tag: these
-- two layers are the ones with a pitch.
data CellLayer = LayerSquare | LayerMono
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Device advance of one cell: a box for square
-- (@Sdl.hs:tgtR = SDL.Rectangle (vp (col@), a half for mono
-- (@Sdl.hs:halfSize) (row@).
cellPitch :: LayoutMetrics -> CellLayer -> Int
cellPitch lm = \case
  LayerSquare -> lmBoxSize lm
  LayerMono -> lmHalfSize lm

-- | A run of cells in one fixed-pitch layer, already cut to the screen.
--
-- The cells stay raw 'Color.AttrCharW32' words, @bg@ included, because
-- the square layer really carries highlights: @bg@ enters the atlas
-- key when it is @HighlightBackground@ (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L622>),
-- picks the grey fill (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L635>) and drives
-- @chooseAndDrawHighlight@ (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L647>, defined
-- at <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L482>). The even-row and floor-glyph
-- substitutions are *not* applied here --- they are 0.2's `CellStyle` rules,
-- applied by the painter per cell; restating them would be the second
-- definition G1 forbids.
data CellRun = CellRun
  { crLayer :: CellLayer
  , crRow   :: Int
  , crCol   :: Int  -- ^ leftmost cell, in units of 'cellPitch': square
                    --   columns after @uiToSquare@'s @div 2@
                    --   (@PointUI.hs:uiToSquare ::@, applied at
                    --   <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L606>), 'PointUI' half-cells
                    --   for mono (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L572>)
  , crCells :: AttrString  -- ^ non-empty
  }
  deriving (Show, Eq)

-- | One frame's overlays. The field order is the draw order --- prop, then
-- square, then mono last, so a mono overrun warning wins over the proportional
-- text beneath it (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L706>, and the engine says the same of the
-- layers at @Overlay.hs:UI. Overlays@) --- and 2.3's payload carries its three
-- sections in that order, so neither consumer has to know the rule to obey it.
-- @singleArray@ is not here: the map is not an overlay and is drawn by diffing
-- (@Sdl.hs:unless arraysEqual@).
data FrameLayout = FrameLayout
  { flProp   :: [PropLine]
  , flSquare :: [CellRun]
  , flMono   :: [CellRun]
  }
  deriving (Show, Eq)

-- | Lay out a whole frame. The 'Int' is @rwidth coscreen@. Applies
-- 'propLineStartFits' itself and drops rejected lines --- the test is
-- measurement-free, so the pure module can, and a consumer never sees a line
-- the cutoff refused (ruled 2026-08-07: divergence on exactly the case this
-- module unifies is impossible by construction).
layOutFrame :: Int -> SingleFrame -> FrameLayout

-- | Split a line into maximal same-colour chunks (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L657>),
-- resolving two rules that a hand-port would drift on: a *leading* space
-- run takes the colour of the next non-space character, or its own if
-- the line is all spaces (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L661>), while an *interior*
-- space run joins the run to its left, spaces matching any colour in
-- @sameAttr@ (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L666>). Even-row @White@ becomes @AltWhite@
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L668>) via `CellStyle`, not by a second copy of the
-- rule.
--
-- Asserts that the cell each chunk takes its colour from has a @bg@ of
-- @HighlightNone@ or @HighlightNoneCursor@
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L668>) --- an assertion that today can only fire
-- with SDL running and after this extraction fires under @cabal test@ too.
--
-- Cuts nothing: cutting is 'propFit''s business. That is what turns the
-- concatenation property below into an equality.
chunkPropLine :: Int          -- ^ row, for the even-row rule
              -> AttrString   -- ^ the line
              -> [PropChunk]

-- | @take (rwidth - col)@ after @uiToSquare@ (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L606>); drops
-- runs left empty.
layOutSquare :: Int -> OverlaySpace -> [CellRun]

-- | @take (2 * rwidth - x)@, in half-cells (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L574>), plus the
-- mono @bg@ assertion (@Sdl.hs:setMonoChar ::@).
layOutMono :: Int -> OverlaySpace -> [CellRun]

-- | The half of the overrun cutoff that *is* expressible in logical units:
-- may a line starting at this 'PointUI' x be drawn at all? Exactly equivalent
-- to 'propCutoff' on the line's first chunk, because 'lmBoxSize' is @2 *
-- lmHalfSize@ by definition and @lmHalfSize > 0@ by 'mkLayoutMetrics'\'s
-- assertion:
--
-- > xUI * halfSize >= (rwidth - 1) * boxSize  <=>  xUI >= 2 * (rwidth - 1)
propLineStartFits :: Int -> Int -> Bool
propLineStartFits width xUI = xUI < 2 * (width - 1)
```

**The measured half.** Two functions and an opaque pen. `PropCursor`
is constructible only by `startPropLine` and advanceable only by `propFit`,
so no consumer can present the cutoff rule with an x the module did not compute.

```haskell
-- | Pen state for one proportional line, in device units.
data PropCursor = PropCursor
  { pcurXPx :: Int  -- ^ not exported
  , pcurRow :: Int
  }
  deriving (Show, Eq)

-- | The one place a logical x becomes a device x (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L651>).
startPropLine :: LayoutMetrics -> PropLine -> PropCursor
startPropLine LayoutMetrics{lmHalfSize} PropLine{plStartX, plRow} =
  PropCursor (plStartX * lmHalfSize) plRow

-- | Has the pen reached the last column? Then this chunk and every later chunk
-- of the line is dropped --- "for KISS, reject it" (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L654>).
-- Measurement-free on purpose: @Sdl.hs@ tests it *before* rendering the chunk,
-- so a consumer that calls this first never rasterizes a chunk it then throws
-- away.
propCutoff :: LayoutMetrics -> PropCursor -> Bool
propCutoff lm PropCursor{pcurXPx} =
  pcurXPx >= (lmWidth lm - 1) * lmBoxSize lm

-- | Where a measured chunk goes, how much of it survives, and whether the cut
-- raises the trimmed-line marker.
data PropFit = PropFit
  { pfXPx     :: Int            -- ^ device x to draw at
                                --   (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L686>)
  , pfWidthPx :: Int            -- ^ device width to draw *and to clip to*:
                                --   the whole chunk when it fits, else the
                                --   cut width. @Sdl.hs@ uses it as both the
                                --   source crop
                                --   (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L526>) and
                                --   the target width
                                --   (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L687>); canvas
                                --   @fillText@ has no width argument, so
                                --   the browser needs it as a clip rect.
  , pfMarker  :: Maybe CellRun  -- ^ draw it *now*, before the next chunk
  , pfNext    :: PropCursor
  }
  deriving (Show, Eq)

-- | Resolve one measured chunk: the three-way clamp of
-- <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L529> and the marker condition of
-- <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L555>, which is exactly "the run was cut and it was not
-- blank".
--
-- The consumer supplies the natural rendered width in its own device units,
-- rounded *up*, so that "it fits" is never optimistic: @Sdl.hs@ passes the
-- rendered surface's width (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L526>, produced by
-- the @TTF.shaded@ call at @Sdl.hs:TTF.shaded (fromJust@), the browser @ceil
-- (measureText t)@.
--
-- Precondition: @not (propCutoff lm cur)@, which is also what makes the third
-- branch positive --- the guard gives @remaining > lmBoxSize@.
propFit :: LayoutMetrics -> PropCursor -> PropChunk -> Int -> PropFit
propFit lm cur chunk widthRaw =
  assert (not (propCutoff lm cur) && widthRaw >= 0)
  $ PropFit { pfXPx = pcurXPx cur
            , pfWidthPx = width
            , pfMarker = if trimmed
                         then Just (trimmedMarkerRun lm (pcurRow cur))
                         else Nothing
            , pfNext = cur {pcurXPx = pcurXPx cur + width} }
 where
  boxSize = lmBoxSize lm
  allSpace = pcAllSpace chunk
  -- Each of the next five lines mirrors the same clause of @Sdl.hs@'s
  -- @scaleSurfaceToTextureProp@, from <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L529> on,
  -- and the last the marker test at <https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L555>.
  remaining = lmWidth lm * boxSize - pcurXPx cur
  width | widthRaw <= remaining = widthRaw
        | allSpace = remaining
        | otherwise = remaining - boxSize
  trimmed = width /= widthRaw && not allSpace

-- | The @$@ stamped into the last square column of a trimmed row
-- (<https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L555>; @Color.hs:trimmedLineAttrW32 ::@ defines
-- it as a @BrBlack@ @'$'@), expressed as an ordinary square-layer run
-- so that no frontend needs marker code of its own. Distinct from the
-- engine-level @$@ that @truncateAttrLine@ splices into an over-long line
-- (@Frame.hs:[Color.trimmedLineAttrW32]@): that one counts logical cells before
-- a frontend sees the string, this one reacts to rendered width. Both use the
-- same word; do not unify them.
trimmedMarkerRun :: LayoutMetrics -> Int -> CellRun
trimmedMarkerRun LayoutMetrics{lmWidth} row =
  CellRun { crLayer = LayerSquare
          , crRow = row
          , crCol = lmWidth - 1
          , crCells = [Color.trimmedLineAttrW32] }
```

**Two invariants the arithmetic gives for free**, worth asserting rather
than commenting. Both clamping branches leave the pen at or past the cutoff:
`pcAllSpace` yields `xPx + width == lmWidth * lmBoxSize`, and the third branch
yields exactly `(lmWidth - 1) * lmBoxSize`, which the `>=` in `propCutoff`
catches. So **a clamped chunk always ends its line**, hence at most one marker
per line and "draw `pfMarker` immediately" is bit-exact with SDL. And since
the precondition gives `remaining > lmBoxSize`, the third branch's width
is always positive: no zero-width blit.

**Where logical units end.** Everything `FrameLayout` emits is logical,
and so is everything on the wire; device space exists only inside `PropCursor`,
which never travels. The pen is where it must: after the first chunk its x
is a running sum of measured widths ([Sdl.hs#L674][sdl-674]), which is why
the cutoff cannot be stated in logical units past the line start. The one
exactly-logical piece of it is `propLineStartFits` above.

**Rejected: a character-count cutoff.** Replacing the pixel rule with a column
count would make the module self-contained, and it does not work. The engine
already wraps proportional text by character count (`splitAttrString`,
`Overlay.hs:splitAttrString ::`, reached for prop text through
`indentSplitAttrString`, `Overlay.hs:indentSplitAttrString ::`) and deliberately
refuses to model proportional width at all --- `textSize PropFont` is an `error`
(`Overlay.hs:textSize :: DisplayFont`) and the wrap's own comment says the space
width "varies wildly"
(`Overlay.hs:Sadly this depends on how wide the space is`). A character-count
cutoff would therefore either duplicate that wrap and never fire, letting
a wide-glyph overrun paint past the screen edge, or fire on lines that fit
and truncate normal menu text. The pixel rule is the safety net for exactly
the case the character model cannot see; it stays, and with it the measured
half.

**Refactor `Sdl.hs` to consume the module** --- the proof the extraction
is faithful, and what de-risks everything downstream: if the native playtests
(`make test-medium`, `make frontendCrawl` for a visual look) pass with `Sdl.hs`
on the shared module, the browser consumer starts from known-correct layout
data. `drawPropOverlay`/`drawPropLine`/`drawPropChunk` ([Sdl.hs#L648][sdl-648])
and the two line cuts ([Sdl.hs#L574][sdl-574], [Sdl.hs#L607][sdl-607]) go;
`scaleSurfaceToTextureProp` ([Sdl.hs#L524][sdl-524]) loses its `x`, `row`
and `allSpace` parameters, its width arithmetic and its `setSquareChar` call,
shrinking to the crop-and-blit it is named for --- which is the rule leaving
the texture helper, the place the plan flagged as easiest to lose it in. What
stays in `Sdl.hs`: font handles, `TTF.shaded`, the vertical crop and centring
([`scaleSurfaceToTextureProp`][sdl-526] --- blit mechanics with no canvas
counterpart, declined deliberately rather than generalized), atlases,
`SDL.copy`, `chooseAndDrawHighlight`, and the per-cell glyph decisions
that are 0.2's business. Perf-gate the adoption as `CellStyle`'s is gated:
before/after `make bench` (`benchFrontendBattle`/`benchFrontendCrawl`, fixed
seeds), since `Sdl.hs`'s per-cell drawing is a hot path.

**How the marker travels.** It is a rule, not a datum, and it travels as one.
The rule is the single line `trimmed = width /= widthRaw && not pcAllSpace`
inside `propFit`, inseparable from the clamp that produced `width` --- which
is why the plan's earlier "output type: chunks" could not express it: the marker
is a function of a measurement, so it can only be decided when the measurement
arrives, and it cannot be known at encode time. On the wire, therefore, **it has
no slot**; what travels is `pcAllSpace`, one bit per chunk, and whichever
`propFit` runs re-derives the marker. In the consumer it appears
as `pfMarker :: Maybe CellRun`, a fully formed square-layer run at column
`lmWidth - 1` --- so a frontend draws it with the same routine it draws every
other `CellRun`, and a browser implementer never has to learn it exists.
Returning it from `propFit` rather than appending it to `flSquare` also
preserves the ordering subtlety: `Sdl.hs` writes it from inside the proportional
pass ([Sdl.hs#L555][sdl-555], called from [Sdl.hs#L684][sdl-684]), *before*
`drawSquareOverlay` runs at all ([Sdl.hs#L709][sdl-709]), so a genuine
square-overlay cell in the last column still overwrites it. Its content needs
no special casing anywhere: `trimmedLineAttrW32` is a `BrBlack` `'$'`
(`Color.hs:trimmedLineAttrW32 :: AttrCharW32`), so `AltWhite` cannot apply
to it and its `HighlightNone` background takes `workaroundOverwriteHighlight`
([Sdl.hs#L493][sdl-493]) like any other unhighlighted cell.

**2.3's payload, specified here.** 2.3 owns the mechanism --- an additive
`js_submitOverlays`, a packed `Word32` buffer passed by address, no string
marshalling, no serialization dependency --- and points here for the payload,
so the field list has one definition. Four properties fix it:

1. **Three sections, in draw order**, behind a header of three counts --- one
   per `FrameLayout` field. Making the sections structural is what lets
   a decoder obey the layer rule without knowing it, and it is the only shape
   that carries the two cell layers at all.
2. **Cell layers ship raw `AttrCharW32` words**: per run, row, col, count,
   then the already-cut cells verbatim. No layer tag on the wire --- the section
   carries it, as it carries the face's absence from the chunk header in item 3:
   `layOutSquare` fills one section and `layOutMono` the other, so the encoder
   would be writing a constant the decoder can read off the section it is in.
   `CellRun` keeps its `crLayer` in Haskell, the marker run escaping its section
   and the painter reading its pitch through `cellPitch`. `fontKind|colorIdx`
   plus codepoints would silently drop `bg`, which the square layer needs three
   times over (`Sdl.hs:setSquareChar ::`); an `AttrCharW32` already packs char,
   fg and bg into one word, and `terminal-core.ts` decodes exactly that word
   for the map grid today. Lossless *and* less encoder work.
3. **The prop chunk header splits into three fields**, and on the wire each
   is a word of its own, per 2.3's no-sub-word-packing ruling. Layer
   is no longer part of it, the layers being separate sections, so the face
   is a one-bit *choice* between the two proportional fonts
   ([Sdl.hs#L678][sdl-678]) carried in a whole word; the colour index is its own
   field; and `pcAllSpace` gets one too, because `propFit` consumes
   it ([Sdl.hs#L531][sdl-531], [Sdl.hs#L555][sdl-555]) and
   it is measurement-free, so the module owns it and the consumer needs it.
4. **`xStart` is logical**, and load-bearing rather than stylistic:
   it is logical precisely because the receiving consumer's `startPropLine`
   is what computes the device x. Nothing device-space is encoded.

The encoder still lives next to `OverlayLayout` and is still pure; tasty still
round-trips it on fixed cases and on QuickCheck-generated `OverlaySpace` values;
2.3's freshness case still writes decoder fixtures.

**The browser's share of the rule**, decided here because it decides what
this module exports: TS re-implements `propCutoff` and `propFit` --- two
comparisons and a three-way `min`, with no string, colour or chunking knowledge
--- pinned by branch-complete fixtures 2.1's own freshness case writes, a table
of `(halfSize, width, cursorX, allSpace, widthRaw) -> (stop | x, width, marker)`
run by vitest in CI, exactly as `CellStyle`'s TS twin is pinned. This keeps
the boundary-cost ruling this section already makes against a sync
`js_measureText` crossing per chunk per frame, rather than arguing it down.
Recorded alternative, with its trigger: batch instead --- one crossing per frame
carrying every chunk's text out and every measured width back, then run the real
`propFit` in Haskell. It costs a round trip and a frame of latency, and
it is worth taking only if the twin ever needs to grow past pure integer
arithmetic. Today it does not, and the subtle knowledge --- space inheritance,
face choice, the recolouring, the two cell cuts, the layer order --- never
crosses under either option.

**Tests.** Nothing above touches SDL or a canvas,
so `test/OverlayLayoutUnitTests.hs` (the harness's `<Module>UnitTests.hs`
naming, wired into `test/Spec.hs`'s list beside the others) needs
no `SessionUIMock`; `chunkPropLine` is exported through the ordinary list's
"internal and used in unit tests" group
(`Common/Kind.hs:Operations both internal`'s pattern --- the `EXPOSE_INTERNAL`
idiom this sentence once named would not compile in the ordinary build,
`release` defaulting `False`), and QuickCheck is already in the suite
(`test/SessionUIUnitTests.hs:import Test.Tasty.QuickCheck`).

Fixed cases, chunking: the empty line; one character; leading spaces before
a coloured run (the next-non-space inheritance); trailing spaces, which join
the run to their left; a multi-space run *between* two differently-coloured
runs, which joins the left one --- the case a hand-port gets backwards;
an all-space line, taking the `[] -> w` fallback ([Sdl.hs#L664][sdl-664]);
a colour change with no space at the boundary; a line whose spaces
are *coloured*, which pins the deliberate difference between the chunker's
`(== spaceAttrW32)` ([Sdl.hs#L658][sdl-658]; a default-attribute space,
`Color.hs:spaceAttrW32 :: AttrCharW32`) and `pcAllSpace`'s `Char.isSpace`
([Sdl.hs#L681][sdl-681]) --- unifying those two would silently change which
lines get the marker; and even against odd rows over a `White` cell, paired
with the assertion that both still choose `PropRegular`.

Fixed cases, placement --- one row per branch of [Sdl.hs#L530][sdl-530]
and [Sdl.hs#L654][sdl-654]: the pen below the limit; the pen exactly
at `(lmWidth - 1) * lmBoxSize`, which the `>=` stops; `widthRaw`
under `remaining`; `widthRaw == remaining`, fitting exactly with no marker,
the case an off-by-one in a port flips; `widthRaw` over, not all-space (clamp
to `remaining - lmBoxSize`, marker fires); the same all-space (clamp
to `remaining`, *no* marker --- the asymmetry that makes `pcAllSpace`
load-bearing); a clamped chunk followed by more chunks, asserting the tail
is dropped and the line ends; and `remaining == lmBoxSize + 1`, the tightest
case in which the third branch stays positive.

QuickCheck properties, over arbitrary `AttrString`s built from a legal fg
colour, a `HighlightNone`/`HighlightNoneCursor` bg and a printable char,
so that [Sdl.hs#L668][sdl-668]'s assertion holds by construction:

- *chunking is total*:
  `concatMap (T.unpack . pcText) (chunkPropLine row al) == attrStringToString al`
  (`Overlay.hs:attrStringToString ::`), for all rows and lines. Separating
  chunking from fitting is what strengthens the plan's earlier "concatenate back
  to the input minus the overrun cut" into a plain equality --- the cut is now
  a different function's claim.
- *chunks are single-coloured under space inheritance*: splitting the input
  by the chunk lengths, every cell of a chunk's slice satisfies
  `rowColor row (fgFromW32 ac) == pcColor c || ac == spaceAttrW32`
  ([Sdl.hs#L666][sdl-666]).
- *no chunk is empty, and the lengths sum to the input's.* This is also
  the termination argument: [Sdl.hs#L675][sdl-675] recurses on the remaining
  string, not on the pen, so a zero-advance measurement cannot loop --- worth
  a property rather than a comment, since the naive reading of that recursion
  says otherwise.
- *no property for the derived fields*: `pcFont` and `pcAllSpace` are functions
  of a two-field record, so "they agree" is a tautology and the QuickCheck
  property earlier drafts carried is deleted rather than kept green. It never
  reached the boundary it read as guarding either, being a Haskell-side property
  over a Haskell producer; what pins the far side is the branch table 2.1's
  freshness case writes, as everywhere else here.
- *placement stays on screen*, for all cursors satisfying
  `not (propCutoff lm cur)`, which is `propFit`'s own precondition and so
  is a domain condition rather than a case to exercise: `pfWidthPx <= widthRaw`;
  `pfXPx + pfWidthPx <= lmWidth * lmBoxSize`; `isJust pfMarker` implies both
  `pfWidthPx < widthRaw` and `pfXPx + pfWidthPx <= (lmWidth - 1) * lmBoxSize`,
  i.e. the marker cell is left unpainted; and the pen is monotonic.
- *scale invariance*, the formal content of "not baked to SDL's pixel values":
  for all `k > 0`, all `h > 0` and all `w > 1`, and cursors legal as above,
  `propFit (mkLayoutMetrics (k * h) w)` on a `k`-scaled cursor and a `k`-scaled
  `widthRaw` gives the `k`-scaling of `propFit (mkLayoutMetrics h w)`
  on the originals --- `k`-scaling both `lmHalfSize` and `pcurXPx` preserves
  `propCutoff`, so a legal cursor stays legal and the precondition survives
  the scaling. The metric-free half is invariant by construction, taking
  no metrics.
- *logical and device cutoffs agree*: for all `h > 0`, `w > 1` and `xUI >= 0`,
  `propLineStartFits w xUI` equals
  `not (propCutoff (mkLayoutMetrics h w) (startPropLine ...))` on that line's
  first chunk.

The domains are stated because `mkLayoutMetrics` and `propFit` assert them
(`halfSize > 0 && width > 1`; `not (propCutoff lm cur) && widthRaw >= 0`),
and a generator that steps outside one aborts inside the assertion rather
than failing the property --- which the non-vacuity proof below cannot tell
from a pass.
- *the cuts are the module's*: `layOutSquare`/`layOutMono` emit no run longer
  than `width - crCol` / `2 * width - crCol`, and emit the input unchanged when
  it fits.
- *transport round trip*: `decode . encode == id` on generated `FrameLayout`s
  (2.3).

Non-vacuity, per the repo's rule: each of the eight placement rows must be shown
to fail when the line of `propCutoff` or `propFit` it exercises is perturbed ---
drop the `- lmBoxSize`, flip the `not pcAllSpace`, weaken the `>=` to `>` ---
and the proof recorded next to the table, since a table built on a wrong
`remaining` passes several ways at once. The refactor's own proof stays
the native one: `make test-medium`, a `make frontendCrawl` look, and before
and after `make bench`.

**Rulings** --- all three ruled on 2026-08-07. (a) `layOutFrame` filters
with `propLineStartFits` and drops rejected lines, as its haddock above now
states: the start-cutoff test is measurement-free by this item's own seam
design, so the pure module can apply it, and a consumer-side `propCutoff` call
would be one copied decision per frontend --- the class being killed; `Sdl.hs`'s
in-recursion cutoff ([Sdl.hs#L654][sdl-654], re-entered per chunk
at [Sdl.hs#L675][sdl-675]) transcribes into the filter. (b) `chunkPropLine`
reaches its tests through the ordinary export list's "internal and used in unit
tests" group (`Common/Kind.hs:Operations both internal`'s pattern), the sec. 2.1
sentence that once routed it through `EXPOSE_INTERNAL` corrected above ---
`release` defaults `False` (`LambdaHack.cabal:flag release`,
`LambdaHack.cabal:if flag(release)`), so the idiom would not compile
in the ordinary build, the trap `test/CLAUDE.md`'s `emptyUnknownTile` bullet
records. (c) The even-row `White`->`AltWhite` recolour is 0.2's named export,
specified in its body: `CellStyle` defines it once, `styleCell` consumes
it internally, this module imports the same one --- the name is the executing
session's, the existence, home and signature shape are the ruling.

### 2.2 Browser overlay renderer, in isolation

The overlays draw on 0.4's canvas, the only renderer (B.10), where canvas
`fillText`+`measureText` is the direct analogue of SDL2's measure-then-blit. New
`overlay-core.ts`/`overlay-core.test.ts` for the pure parts ---
the `propCutoff`/`propFit` twin 2.1 specifies, which takes widths as plain
numbers, measurement staying in the renderer --- pinned by the branch table
2.1's freshness case writes rather than by hand-written cases. The renderer
draws all three of 2.1's sections, not only the proportional one: a `CellRun`
is drawn at its layer's `cellPitch` with `CellStyle`'s per-cell rules applied
by the painter, in the prop, square, mono order 2.1 fixes, through 0.4's draw-op
core, so the tests assert op lists --- no canvas dependency.

*Landed in `e549ef42e`, the Split's (3), after `63813f480` and `37e87218a`,
its (1) and (2); (0) had nothing to take, 0.4's baseline standing in B.10.
Beyond this body: the mono layer is drawn as `Sdl.hs:setMonoChar ::` draws it,
by the Log's ruling of 2026-10-23 at 2.2's landing; the painter repaints the map
cells the last frame's overlays covered before drawing the next frame's,
and holds a frame with proportional or mono overlays until the payload's fonts
have settled, loaded or not; `ts-src/tsconfig.json` takes the DOM.Iterable
library the `FontFace` declarations need; and `make page-gate` copies the fonts
as `make build-ts` does. The empty overlays every frame still carries draw
nothing and measure nothing, which an `overlay-core.test.ts` case asserts.*

*Its hand-back confirmed by Mikolaj on 2026-10-23, in Firefox. In the session's
own run, Firefox and Chromium reported all four faces loaded,
and with `Hack-Bold.ttf.woff` missing only the mono face failed, the page still
running.*

Text drawn on canvas is not selectable or visible to screen readers;
that trade-off is accepted for overlay text (Appendix B) and, since B.10,
for the map too, the ANSI frontend serving a screen reader natively.

Fonts, the G1 way --- following the `rFontFiles = []`-under-`USE_BROWSER` split:
the Haskell side owns *which* fonts at *what size* (from `sfonts` / the chosen
fontset, sizes multiplied by `sallFontsScale` exactly
as `Sdl.hs:loadFontFile fname fsize =` does), the web side owns the *bytes*
(static `.woff` assets). Concretely: at startup `Wasm.hs` resolves the chosen
fontset and passes file names + effective px sizes to JS in the one-shot setup
call specified below; TS declares them with the `FontFace` API
from `./<filename>` URLs. **TS hardcodes nothing about the prop and mono
fonts**, the map font being R7's; `make build-ts` is extended to copy
the referenced `GameDefinition/fonts/*.ttf.woff` into the pages checkout (only
`16x16xw.woff` is deployed today).

**The startup payload, specified here.** 1.5 creates this call and points here
for its shape, as 2.3 points at 2.1 for the wire payload's, so the field list
has one definition. One call, named for the general job and not for any one
field: a Haskell-side `unsafe` import of a `globalThis` hook --- the established
`lhSubmitFrame` direction, Haskell pushing when it owns the data --- taking
a single JSON `JSString`, `JSON.parse` on the JS side against a declared TS
interface, unknown fields ignored. Its fields are named, this item's fonts
and 1.5's `allFontsScale` being the two the plan needs; 1.5 creates the call
with its field and this item adds its own, so no signature ever changes
and a field-specific `lhSetFontScale` is refused. The boundary is stated
so the confusion dies: 2.3's string ban is about per-frame cost, and a one-shot
startup call is exactly where JSON is right.

Known browser pitfalls to handle here, not discover in 2.4:

- **Measure only after fonts load.** `measureText` before the `FontFace`
  finishes loading silently measures a fallback font; await `FontFace.load()`
  (or `document.fonts.ready`) before the first overlay draw, and don't cache
  widths measured earlier.
- **The mono layer's pitch is the pointer's since 0.4.**
  `Sdl.hs:pointTranslate (SDL.P (SDL.V2 x y)) =` divides a click's pixel x
  by `halfSize`, so SDL reports an odd `PointUI` x for a click in a cell's right
  half, and 0.4's handlers divide the pointer's pixels by `halfSize` the same
  way, where the DOM grid's had built their point with `squareToUI`
  (`PointUI.hs:squareToUI :: PointSquare`), always even; so the mono layer
  this item draws is clickable at its own pitch. Appendix C's 2026-09-07 audit
  note records the gap.
- **Fixed-pitch glyphs are cropped and centred natively.**
  `Sdl.hs:scaleSurfaceToTexture ::` crops an oversized glyph surface to the cell
  box and centres it over a black fill, for the square layer
  (`Sdl.hs:scaleSurfaceToTexture boxSize`) and the mono one
  (`Sdl.hs:scaleSurfaceToTexture halfSize`) alike. This item draws both
  fixed-pitch layers at `cellPitch`, so a glyph wider than its cell would
  overhang. 2.1 declines to generalize the *proportional* crop and says so;
  this is the other site, and 0.4's draw-op core closes it: its text op clips
  each cell's glyph to the cell's box with the glyph centred in it, as `Sdl.hs`
  crops (B.10), so both layers this item draws through that core are cropped,
  which the op-list tests pin. Appendix C's 2026-09-07 audit note records
  the gap.
- **Cell metrics from one source.** The browser's `lmHalfSize` is 0.4's
  painter's `halfSize`, its `boxSize` taken from the loaded font as `Sdl.hs`
  takes it rather than from config, so overlays and map draw at multiples of one
  integer under zoom, `allFontsScale` and any future font-size change.
  `mkLayoutMetrics`'s other argument needs no new channel and gets none:
  the browser's `lmWidth` is `submitFrame`'s `w`, the same value the painter
  sizes its canvas from, so the twin clamps against the screen `layOutFrame`
  already filtered against in Haskell. The overlay decoder asserts the two
  agree. That is the whole of the hazard 2.1's `mkLayoutMetrics` haddock names
  --- two different widths leaving the line filter and the pen clamping against
  different screens --- reaching across the FFI, where it would otherwise
  be silent: the vitest branch table supplies both halves itself, so no gate
  in this plan would see a browser measuring against a width the Haskell side
  never used.

Fixtures for the browser's `propCutoff`/`propFit` twin come from 2.1's freshness
case calling `OverlayLayout` --- no hand-written fixtures. Chunking itself never
crosses, so there is nothing there to pin.

Verify: vitest green; live game pixel-identical to before (nothing wired).

**Rulings** --- one ruled on 2026-08-05 and re-cut on 2026-10-20 ---
the baseline is 0.4's Split (0) number in B.10, taken in-session on the canvas
with the throwaway probe of Phase 2's intro --- and the other two on 2026-08-07,
(a) re-cut on 2026-10-20 for B.10. (a) The browser's argument
to `mkLayoutMetrics` takes rather than chases, per the explicit-widths practice:
it is the painter's own integer `halfSize`, so there is no grid to impose cell
dimensions on and no cell to measure back at startup, both of which went
with the span grid; a measured box that is odd or fractional, the normal case
under a chasing design, stays refused. (b) The one-shot setup call
is the extensible JSON payload the body now specifies under "The startup
payload, specified here", 1.5's scale field riding it; the ruling
is that paragraph, and restating its field list here is the second definition G1
forbids.

### 2.3 Transport: overlays across the JSFFI boundary

`Wasm.hs`'s `display` currently drops three of `SingleFrame`'s four fields
(`Wasm.hs:display ScreenContent{rwidth,`;
`OverlaySpace = [(PointUI, AttrString)]`,
`Frame.hs:type OverlaySpace = [(PointUI, AttrString)]`). Add an **additive**
`js_submitOverlays` alongside `js_submitFrame`, using the same idiom the frame
already uses: a packed `Word32` buffer passed by address --- everything
is numeric and fits `Word32`, no string marshalling, no new serialization
dependency. The payload's shape is specified once, in 2.1, under "2.3's payload,
specified here": three sections in draw order behind a header of counts, cell
layers shipping raw `AttrCharW32` words, the prop chunk header split into face,
colour index and the `pcAllSpace` bit, and `xStart` logical because
the receiving consumer's `startPropLine` is what turns it into a device x.
Restating it here is the second definition G1 forbids, and the field list
is exactly what drifts. The encoder lives next to `OverlayLayout` and is pure;
tasty round-trips it on fixed cases and on QuickCheck-generated arbitrary
`OverlaySpace` values, and this item's freshness case writes encode fixtures
the TS decoder is tested against. All three overlay kinds (prop/square/mono)
travel and draw through the same path, on the same canvas, in the 2.1 layer
order.

Because `getFontSetup` still gates on `"sdl"`, the overlays are provably `[]`
in the live game --- this step is a visual no-op, verified by playing.
The real-data path is exercised by tests instead: a tasty case constructs
a `SingleFrame` with non-empty overlays and checks the encoding; a vitest case
asserts the 2.2 renderer makes zero draw calls on an empty overlay payload
(the live game's case until 2.4); and the `run-wasm-test.mjs` battery (0.3)
drives `js_submitOverlays` end-to-end.

**Rulings** --- all three ruled on 2026-08-07. (a) The wire layout is flat words
with element counts: one scalar per `Word32`, no sub-word packing anywhere
except `AttrCharW32`'s own engine-native layout, and the header's counts count
elements. What that decides about the chunk header's own fields is stated
in 2.1's item 3, which is where the header is specified; restating the field
list here is the second definition G1 forbids, and this ruling deliberately does
not. The full field order is written once, as the haddock table on the encoder
in commit (1), and the TS side is pinned by generated fixtures, never
by a second reading of that table; overlays are menu text rather than the map,
so packing buys nothing and a whole mismatch class dies with it. (b) The Haskell
`decode` that the `decode . encode == id` property needs is a test-only helper,
exported through the same ordinary "internal and used in unit tests" group 2.1's
(b) ruled --- no `EXPOSE_INTERNAL` reopening. (c) "Next to `OverlayLayout`"
means the same module file, as this block and 2.1's Owns already assumed ---
no sibling module, no `tools/doc-refs-allow.txt` churn.

### 2.4 Flip the switch --- as a capability, not a name list

Each frontend module exports a constant `supportsMultiFont :: Bool` (`Sdl.hs`,
`Wasm.hs`: `True`; ANSI/Teletype: `False`; `Dom.hs` is a dead example file
no configuration compiles, R3), and `Frontend.hs` dispatches it exactly like
`frontendName` already dispatches per-frontend values
(`Frontend.hs:frontendName :: ClientOptions`, including
the null/lazy/teletype/ANSI option guards). A name list like
`frontendName soptions `elem` ["sdl", "wasm"]` would be a drift-prone string
list of the same shape G1 fights. Then `getFontSetup`
(`MonadClientUI.hs:= Frontend.frontendName`) becomes:

```haskell
multiFont = Frontend.supportsMultiFont soptions
            && not (T.null (fontPropRegular chosenFontset))
```

Re-check `test/MonadClientUIUnitTests.hs`'s `getFontSetup works in stub` after
the flip. It is platform-independent today, and stays so only as long
as the stub pins a frontend: `stubClientOptions` sets `sfrontendNull = True`
(`test/UnitTestHelpers.hs:, sfrontendNull = True`), which is the *first* guard
in `frontendName`, so it never reaches `Chosen.frontendName` and `multiFont`
is `False` under `cabal test` and `make test-wasm` alike. Add a case that pins
`supportsMultiFont` itself, so the flip is covered rather than merely
not broken.

Small diff, large blast radius: this is the step that changes what players see
and it touches shared engine code --- the one to review hardest, and to re-run
the native playtest battery on (`make test-medium` at minimum).

### 2.5 Post-flip QA checklist

- Side-by-side visual comparison with SDL2: menus, message log, help, item
  descriptions; mono-overwrites-prop overrun behavior.
- Screenshots after the flip show the overlay layers, which one renderer gives
  by construction (B.10); a `Ctrl+P` after the flip, the page made fullscreen
  by its Fullscreen button, confirms it.
- HiDPI and browser-zoom spot checks (0.4's device-pixel backing store
  under the overlays, exercised for real).
- Frame-timing re-measurement against 0.4's canvas baseline (B.10; re-apply
  the probe diff recorded there).

**Rulings** --- since 2026-08-05, re-cut 2026-10-20: 0.4's Split (0) captures
the baseline, on the canvas, before 0.4's painter merges, closing the question
this field carried. One instruction survives it: should the record show
the capture was skipped regardless, either strike that checklist line
with a recorded reason or re-derive a baseline from a build at the pre-2.2
commit --- never quietly measure against nothing and call the number
a comparison.

---

## Phase 3 --- port the Node benchmark targets from GHCJS to WASM

The Makefile still carries `nodeBenchCrawl`/`nodeBenchBattle`/`nodeBench`/
`nodeMinifiedBench` (`Makefile:nodeBenchCrawl:`), but they invoke the GHCJS
`.jsexe/all.js`, which the Makefile can no longer build --- they are dead
as written. What they did matters: headless AI-vs-AI benchmarks
of the browser-targeted build under Node
(`--frontendNull --benchmark --stopAfterFrames N --automateAll ...`, same flag
sets as the `nativeBench*` targets, sharing `RNGOPTS`), with `nodeMinifiedBench`
additionally benchmarking the *deployed* artifact from `../lambdahack.github.io`
rather than the build tree. This phase repurposes those targets for the wasm
build, restoring the browser half of the native-vs-browser benchmark comparison.

Independent of Phases 1--2; only 3.1 touches Haskell, and it's the same
mechanism R4 needs anyway.

### 3.1 Argv into the reactor: `lhStart` reads WASI args

`lhStart` (`GameDefinition/Main.hs:lhStart = do`) parses an empty list where
`main` parses real argv. Change it to parse `getArgs`: GHC's wasm RTS gets argv
from WASI `args_get`, which every host shim controls --- the browser loader
passes `["LambdaHack"]`
(`loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`), so `getArgs`
returns `[]` there and browser behavior is provably unchanged, while a Node
harness can pass real flags. Update the stale "there is no argv in the browser"
comment to describe the new contract (argv comes from the WASI host; the browser
host passes none). **Spike**: performed and recorded 2026-08-07 --- a throwaway
`lhProbeArgs` export in the reactor, driven under `node:wasi`
with `--newGame 1 --gameMode crawl` as WASI args, printed
`getProgName="LambdaHack.wasm" getArgs=["--newGame","1","--gameMode","crawl"]`,
so `getArgs` surfaces host-supplied args in reactor mode, with argv[0] held
separately by `getProgName` and no drop needed. This entry point is what R4
later feeds from URL parameters, so 3.1 is shared infrastructure, not bench-only
plumbing.

*Landed in `77e64a697`, through `OA.execParser`, the parse `main` makes, which
reads `getArgs` itself, so the change took no import and no `#ifdef` of its own.
Under `node:wasi`, `--help` as WASI args now prints the usage and settles
`lhStart`'s promise, where an empty argv starts the game.*

**Rulings** --- The spike is answered and, since 2026-08-07, carries a recorded
artifact --- the body's Spike paragraph holds the command shape and output:
a reactor linked with this repo's flags and driven through `node:wasi` reports
the host args from `getArgs` with `getProgName` holding argv[0] separately,
so `getArgs` needs no argv[0] drop of its own and the browser's one-element argv
yields `[]`, exactly as the item predicts.

### 3.2 A Node driver for the game reactor

`run-wasm-test.mjs` runs WASI *commands* (`wasi.start`, the test binary);
the game exe is a *reactor* (`_initialize` + exported `lhStart`). Grow
the sibling `run-wasm-game.mjs` that 0.3's export half seeds (ruled 2026-08-07;
the setup code is imported from the test driver, not copied) to: pass
`[args...]` as WASI argv, as `run-wasm-test.mjs` does and as 1.2 made the seed
do, `wasi.initialize(instance)`, then call and await `lhStart()`. Two
integration points to cover:

- **`WasmFile.hs` reaches for `globalThis.localStorage`
  and `globalThis.LZString` unconditionally** --- neither exists under Node.
  The in-memory `localStorage` stub is 0.3's and the lz-string devDependency
  the page gate's, both installed by the shared setup this driver imports (ruled
  2026-08-07; lz-string must come from npm, never the sibling checkout).
  With `--frontendNull`, `Wasm.hs`'s frontend (and thus `lhSubmitFrame` etc.)
  never starts, so the display stubs the seed installs are never called ---
  and this item adds no stub of its own. Its frame and payload checks cannot
  pass there, so the driver makes the seed's coverage checks only without
  `--frontendNull`, checking exit propagation and the measurements alone
  under it.
- **Exit propagation:** `make` must see failures. Await the `lhStart()` promise
  --- measured on this toolchain, a Haskell `error`, `exitWith (ExitFailure 3)`
  and `exitWith ExitSuccess` all surface as the same rejected promise carrying
  a `WebAssembly.RuntimeError` whose message is the shown exception,
  so the success rule is: resolved, or rejected with the message `ExitSuccess`;
  anything else sets a nonzero `process.exitCode`, which covers both ways
  `--stopAfterFrames` termination can surface. A fault raised in the RTS
  scheduler loop on a later tick escapes the awaited promise entirely,
  so install `process.on('uncaughtException')` as well --- Node exiting 1 there
  is luck, not a mechanism. Re-established 2026-08-07 with a throwaway
  `lhProbeCrash` export: `error`, `exitWith (ExitFailure 3)`
  and `exitWith ExitSuccess` rejected the call's promise
  with `RuntimeError: probe-error` / `ExitFailure 3` / `ExitSuccess`
  respectively, and a post-resolution fault escaped the promise as claimed ---
  though in the forked-thread shape probed it surfaced as the RTS's own stderr
  report rather than a Node `uncaughtException`, so the handler stays as defense
  for the scheduler-loop shape the original measurement met.
- **Measurement for free:** the driver reports peak `WebAssembly.Memory` size
  at exit --- the wasm counterpart of `benchMemoryAnim`'s `+RTS -s`, and memory
  was precisely the GHCJS era's pain point (the old `GHCJS_GC_INTERVAL` knobs
  existed for a reason) --- and 0.3's localStorage stub, instrumented here,
  times and reports `setItem` durations and payload sizes, so R1's save-lag
  re-measurement falls out of any nodeBench run.

**Rulings** --- (a) to (c) ruled on 2026-08-07. (a) A sibling file, seeded
by 0.3's export half and grown here, its setup imported from the test driver
rather than copied; the extend branch is refused --- two lifecycles in one
script --- and the display-stub sentence was reconciled rather than obeyed:
the seed installs the stubs deliberately, 0.3's coverage needing frames,
and `--frontendNull` runs simply never call them. (b) The exit-propagation
bullet's correction landed with the ruling fold itself --- the measured rule now
stands in the bullet --- so no driver session can build against the wrong claim.
(c) lz-string, a devDependency since the page gate, serves 0.3's import half,
which needs it for the round-trip cases; this item adds nothing
to `package.json`. The measurement stands recorded, and re-established
2026-08-07: `lhStart` under the scratch driver on an empty store issued
`getItem`s for the config, saves and scores keys plus one uncompressed `setItem`
of `config.ui.ini`, with the `LZString` stub untouched --- so only a run
that *saves* needs it, R1's measurement, not the benchmark, which leaves through
`ReqUIGameDropAndExit` --- and the npm-registry moment belongs to 0.3's landing,
where CI's `npm ci` covers it. (d) Ruled 2026-10-22 by Mikolaj: the seed's
coverage checks run only without `--frontendNull`, and **Done** runs the driver
both ways, so that this item's own gate exercises them too.

### 3.3 Repurpose the Makefile targets

- `nodeBenchCrawl` / `nodeBenchBattle`: mirroring
  `nativeBenchCrawl`/`nativeBenchBattle` exactly --- same flags and `RNGOPTS`,
  which the two pairs already share verbatim, `--dbgMsgSer --logPriority 4`
  included (ruled 2026-08-07: the ratio is honest only if the halves differ
  along nothing but the compared axis, so neither half drops a flag the other
  keeps) --- invoking the 3.2 driver
  on `wasm32-wasi-cabal list-bin exe:LambdaHack` plus post-linked glue
  (the `test-wasm` target shows the exact `post-link.mjs` recipe,
  `Makefile:test-wasm:`), under `~/.ghc-wasm/env`'s node, which the recipe
  sources for `list-bin` anyway and which the recipe states. `nodeBench` stays
  the aggregate of both.
- `nodeMinifiedBench` -> rename to `nodeDeployedBench` (there is no "minified"
  wasm; the honest name is "the deployed artifact"): run the same two benchmarks
  against `../lambdahack.github.io/LambdaHack.wasm` + its `ghc_wasm_jsffi.mjs`,
  exactly the role the minified `all.js` played --- and a coherence check
  that the two deployed files match each other, for free. If a `wasm-opt` step
  is ever added to `build-ts`, this target is what validates it.

**Verify:** both benchmarks run to completion under Node with plausible frame
counts; compare against `nativeBench` on the same machine for a first real
wasm-vs-native data point (feeding R5). A crash or hang must fail the target
(3.2's exit propagation). Once stable, a short `--stopAfterFrames` variant
is a natural cheap addition to R2's CI job --- it runs the whole engine headless
as the reactor, where `make test-wasm`'s integration test runs it only
in the command-linked test binary.

**Rulings** --- all three ruled on 2026-08-07. (a) The targets use
`~/.ghc-wasm/env`'s node (v26.4.0 today, against v18.19.1 on `PATH`), stated
in the recipe: they source the env for `list-bin` and `post-link.mjs` anyway,
`make test-wasm` already runs the same node on the same class of binary, CI's
toolchain install supplies it, and `node:wasi` is least broken on the newest
engine. (b) The executing session runs `make build-ts` itself, unsandboxed,
as the `deploy` gate's own annotation says --- the deployed half then validates
exactly what the deploy pipeline produced, which is this target's stated role
--- and committing or pushing the pages repo stays the author's. (c) The flags
mirror `nativeBench*` exactly, as the first bullet now states; the two
asymmetries that cannot be dropped are recorded rather than removed ---
the browser build hardwires `sdumpInitRngs = True`
(`TieKnot.hs:let serverOptionsJS`), a constant seed-dump prepend the native side
does not emit, and the development `cabal.project.local` carries
`+with_expensive_assertions`, reaching both halves symmetrically --- so numbers
are recorded with the config named, and no session re-runs the comparison
with assertions off on its own initiative.

---

## Related goals

**R1 --- Save robustness.** The banner's savefile-corruption caveat ---
"Savefiles are prone to corruption on the browser, e.g., when it's closed while
the game is still saving progress" on the page, and the same sentence
lower-cased and hung off an "Also," in the README the `#status` div quotes ---
is the last banner claim with no plan behind it. Grounding (from `WasmFile.hs`):
each file maps to one localStorage key written by a single synchronous `setItem`
of the lz-string-compressed payload with an `"OK"` EOF marker --- a *single* key
is already effectively atomic, and truncation is detected on read. The real
residual risk is **cross-key consistency**: a game save writes several files
(server + per-faction clients), and a tab killed mid-sequence leaves
a mixed-generation set of individually-valid saves. Fix shape: write each save
cycle to staging keys, then commit with a single generation-pointer key flip;
readers resolve through the pointer. Test plan (runnable under `make test-wasm`
with 0.3's localStorage stub): simulate an interrupted cycle --- staging keys
written, pointer not flipped --- and assert the reader serves the previous
generation intact; 0.3's `encodeEOF`/`strictDecodeEOF` round trip, which already
runs in the wasm environment, stays green through the pointer.

Two engine-side facts shape this goal:

- **Periodic autosave is disabled entirely in browser builds** ---
  `Server/LoopM.hs:Saving on the browser causes a huge lag` (not to be confused
  with `Client/LoopM.hs`) skips the periodic `writeSaveAll`
  under `USE_JSFILE`/`USE_WASMFILE`, with the comment *"Saving on the browser
  causes a huge lag"*. That measurement is GHCJS-era; re-measure under wasm
  as soon as 3.2 exists --- it instruments 0.3's localStorage stub, which
  then reports `setItem` durations from any nodeBench run. If it's now
  acceptable --- ruled 2026-08-07: a full `writeSaveAll` cycle's total `setItem`
  time at or under 100ms, a marginal hitch at autosave cadence --- re-enabling
  periodic saves shrinks the loss window far more than any atomicity work; above
  the bar, a save triggered from JS `pagehide`/`visibilitychange`
  is the alternative (needs investigation of what the server monad allows
  mid-turn).
- The exit path already contains a browser-specific mitigation to be preserved
  (or obsoleted knowingly): `WatchUpdAtomicM.hs:UpdKillExit{} -> do` waits 2s
  at `UpdKillExit` because *"some browsers seem to trash Local Storage when page
  reloaded or closed ... while they still internally finish the saving
  in the background"*.

No SDL2 analogue (SDL's own window-close path deliberately exits without a fresh
save --- `Sdl.hs:a forced shutdown,`), but this is the browser-build equivalent
of "your progress is safe", which is what parity is *for*.

**Rulings** --- all three ruled on 2026-08-07. The generation-pointer flip
is a File-layer cycle-commit operation: `Common/File.hs`'s interface gains it,
`writeSaveAll` (`Server/CommonM.hs:writeSaveAll :: MonadServerAtomic`) calls it,
`HSFile.hs` implements it as a no-op and `WasmFile.hs`
as the staging-keys-then-pointer-flip --- the only branch that closes the named
cross-key risk, and it goes through the existing backend abstraction; a per-file
pointer is refused, leaving as it would the mixed-generation set alive. Saves
already sitting in players' localStorage under the flat `path`-as-key layout
are read as generation zero: the reader falls back to the legacy keys when
the pointer is absent, the first committed cycle writes generation one plus
the pointer, and the legacy keys are cleaned only after a successful commit ---
abandoning progress is the failure the banner caveat describes, self-inflicted.
And R1c's branch is decided by R1a's number against the 100ms bar the body now
carries, the `pagehide` branch keeping its server-monad investigation
if it opens.

**R2 --- Browser-and-frontend CI.** None of Phase 0's drift protection fires
unless CI runs it. Partly landed,
in `.github/workflows/lint-and-test-suites.yml` (the hand-written workflow ---
do not touch the generated haskell-ci one): a `test-wasm` job that installs
ghc-wasm-meta and runs `make test-wasm`, and `make test-ts` split off as its own
Node-only job, so TS regressions fail in seconds. Still to add:
`make build-wasm`, with a run of 0.3's game driver against the reactor
it builds, the battery's import half riding the `test-wasm` job already. Still
to do: cache `~/.ghc-wasm` and the wasm cabal store, the toolchain being
the expensive part and bootstrapped from scratch on every run today.
The frontend-CI-matrix practice (below) widens this with xvfb SDL and pty ANSI
smokes, which run in the playtest job, and a `nodeBench` smoke after 3.3.

**Completeness requirement: everything runs in CI.** What runs there today
is CLAUDE.md's to state, not this document's; a third copy of that inventory
is what drifted. The doctest gap is closed the second of the two ways weighed
here --- a job following CLAUDE.md's recipe --- because that recipe is known
to work here, a run per library with `--with-repl=doctest`; haskell-ci's own
doctest support was not evaluated against it. And the standing rule
from the ground rules applies to everything this plan adds: the vitest suites
(including the jsdom forwarding tests), `make test-wasm` (including the FFI
battery and the RawFrontend contract harness), the generated-file freshness
check, the determinism goldens (native under `cabal test`, cross-backend
under `make test-wasm`), and the frontend smokes --- each in CI from the commit
that introduces it.

**Rulings** --- both ruled on 2026-08-07; R2 remains a track handing out one job
at a time, in the Split's order. The next two jobs are the cache
then `make build-wasm`, as the Split now states. And CI gets no `make build-ts`
job: its verifiable pieces are covered piecewise --- `tsc` and vitest
in `test-ts`, the wasm build in the `build-wasm` job --- and the deploy write
has no meaningful destination on a runner; nor, ruled 2026-09-14, a page job,
`make page-gate` running locally before every browser landing and CI staying
blind to the page by choice; what the target got is the `;`->`&&` hygiene fix,
landed on master (`2b4b7d71f`) with its measured proof --- a planted npm-ci
failure exited 0 under the old chains and 2 under the new --- because several
items' Done lines gate on `build-ts`'s exit status unsandboxed.

**R3 --- Retire GHCJS support (one browser target is enough).** The original
GHCJS target is unbuildable, permanently: this codebase requires GHC 9.10+
and the standalone GHCJS compiler died at GHC 8.10. A port to GHC's in-tree
JavaScript backend was investigated and found feasible at 1.5--3 weeks,
but is **not happening** (rationale in B.8; Appendix A is kept
as the resurrection manual). Instead, once WASM reaches parity (after 2.5),
in one commit:

- `Dom.hs` and `JSFile.hs` **stay in the tree as examples** of an alternative
  frontend/file-backend pair, each gaining a prominent header comment marking
  it as dead, bitrotten, GHCJS-only code that no configuration compiles. (They
  remain the historical origin of the web stack's logic, which is why this plan
  cites them.) In the tree only, ruled 2026-08-07: dropping the `impl(ghcjs)`
  stanzas removes them from every module list and they leave the sdist
  with it --- no `extra-source-files` entry; hlint and stylish keep covering
  them, both quantifying over tracked `.hs` files.
- **Everything else GHCJS goes**: the `impl(ghcjs)` cabal conditionals
  (`ghcjs-dom`/`ghcjs-base` deps and the exposed/other-module lines,
  `LambdaHack.cabal:-DUSE_BROWSER -DUSE_GHCJS -DUSE_JSFILE`
  and `LambdaHack.cabal:Game.LambdaHack.Client.UI.Frontend.Dom`),
  the `ghcjs-options` knobs (`GHCJS_GC_INTERVAL`, `GHCJS_BUSY_YIELD`, `-dedupe`,
  `GHCJS_BROWSER`, `LambdaHack.cabal:Haskell GC in GHCJS every 10s`)
  and the fifth, `-DREMOVE_TELETYPE`
  (`LambdaHack.cabal:ghcjs-options: -DREMOVE_TELETYPE`) --- which unlike
  the other four has source consumers, `Frontend.hs:= Teletype.startup`
  and `Frontend.hs:-> Teletype.frontendName`'s `#ifndef REMOVE_TELETYPE` guards,
  permanently true once no `ghcjs-options` line can define the macro,
  so those go too. Then the `supportNodeJS` flag entirely, it having
  no non-GHCJS use --- and with it the tracked `cabal.project.local.js`, whole:
  it is the local cabal configuration of the GHCJS build and nothing reads it,
  its other lines being ordinary build settings for that build, so the file goes
  with the target rather than only its `flags: -supportNodeJS` line, which
  is what **Owns** below says. Then `Frontend.hs:#elif defined(USE_GHCJS)`'s
  `USE_GHCJS` import branch, `File.hs`'s `USE_JSFILE` branch,
  `TieKnot.hs:#ifdef USE_JSFILE`'s GHC.Compact escape hatch,
  and the `USE_JSFILE` halves of the browser conditions
  in `Server/LoopM.hs`/`WatchUpdAtomicM.hs`/`HandleHumanLocalM.hs`
  and `TieKnot.hs:#if defined(USE_JSFILE)` (the `sdumpInitRngs` hardwiring plus
  main-thread workaround skip, a second `TieKnot.hs` site distinct
  from the GHC.Compact one above) --- or of their capability-constant
  successors, if that practice lands first. Update CLAUDE.md's GHCJS mentions
  in the same commit. The README needs no GHCJS edit --- it contains the string
  nowhere (checked repo-wide); what it carries is browser-era prose ---
  the "The game runs rather slowly in the browser" paragraph under "Installation
  of the sample game from binary archives", which the page's `#status` div
  quotes with small edits --- the page says "for both map and texts" where
  the README says "for all purposes, though it's scalable", opens the savefile
  sentence in capitals where the README hangs it off an "Also,", and drops
  the README's "(which takes a long time)" --- owned clause by clause by 2.4, R1
  and R5 under 1.4's rule, not by this rip-out.

Timed after parity, not before, so the rip-out doesn't tangle with Phase 0--2
diffs touching the same cabal stanzas and CPP sites.

**Rulings** --- all three ruled on 2026-08-07. (a) `Dom.hs` and `JSFile.hs`
are tree-only: they leave the sdist with the module lists,
no `extra-source-files` entry --- the examples live in the repo, as the first
bullet now states. (b) The two orphan GHCJS comments ---
`GameDefinition/Main.hs:This test is faulty in the browser`
and `Point.hs:not supported yet by GHCJS (not verified)` --- join this item's
Owns and are reworded in the rip-out commit, so the post-rip-out grep gate needs
no standing exceptions; the `impl(ghcjs)` stanzas in the two tracked cabal
archives are explicitly out of scope, kept verbatim by their own definition ---
as are `Makefile:nodeBenchCrawl:`'s `nodeBench*` targets
and `.claude/skills/playtests/SKILL.md`'s description of them, both 3.3's. (c)
The re-anchoring is a separate `.md`-only commit right after the rip-out ---
2.1's correction-never-with-the-landing pattern, generalized. The rip-out
deletes lines this document anchors on --- the `USE_GHCJS` import branch,
the two `REMOVE_TELETYPE` guards the sum-typed selection cites, the cabal
stanzas --- so the pass names them and their sentences are reworded there,
the frozen appendices' with their prose untouched, ruled 2026-09-08
as the fourth kind of upkeep the pointman post-mortem admits.

**R4 --- URL-parameter options.** Server/client options sit at defaults
in the browser for lack of argv. After 3.1, `lhStart` parses whatever WASI argv
the host supplies --- so the whole feature reduces to the *loader* translating
an allowlist of URL query parameters (e.g. `?fontset=`, `?allFontsScale=` ---
the player-facing half of 1.5 --- `?benchmark=`, debug flags) into the args
array it already passes to the WASI shim
(`loader.ts:const wasi = new WASI(["LambdaHack"], [], fds);`), reusing the real
options parser with no engine-side parsing code at all. This gives browser users
what argv gives native ones with no new UI surface --- an address-bar knob.
`?fontset=` only becomes meaningful once 2.2's font wiring exists,
and non-default fontsets need their `.woff` files deployed too --- deploy them
all once rather than special-casing the default set.

**Rulings** --- all three ruled on 2026-08-07, all player-visible. The allowlist
is the minimal named set --- `?fontset=`, `?allFontsScale=`, plus R5's
`?benchmark=` when it lands --- and no debug flags: URLs are shareable,
so a pasted link must not carry a soft-brick like `--stopAfterFrames`, and each
later addition is one allowlist line with a recorded reason. The failure policy:
unknown keys `console.warn` and are ignored --- real URLs accumulate third-party
cruft that must never kill the game --- duplicates take the last value
with a warning, and an allowlisted value the engine parser rejects aborts
*legibly*: the loader catches the failed `lhStart` promise and shows an on-page
message naming the parameter, never a dead canvas. And the loader validates
nothing: every value delegates to the real options parser --- the item's own
selling point --- with the loader only filtering to the allowlist, mapping query
keys to argv strings and presenting rejections; the vitest covers exactly
that query-to-args mapping.

**R5 --- Performance pass (exploratory, after Phases 2 and 3).** The banner says
the game "runs rather slowly in the browser". Phase 3's `nodeBench*` targets
supply the game-logic half of the measurement (headless wasm vs `nativeBench`
on the same machine, same seeds); the remaining half is browser-side rendering,
which nothing in Phase 3 touches (`--frontendNull` never starts the wasm
frontend). Its instrument: after R4 + 3.1, a `?benchmark` URL mode running
the same `--automateAll` AI-vs-AI game with the real browser frontend ---
the browser analogue of `benchFrontendCrawl` --- while `terminal.ts` collects
`submitFrame`-to-paint timings (rAF timestamps) and reports count/mean/p95
at game end. Used for R5's own pass alone: 0.4's baseline and the 2.5
re-measurement use the throwaway `performance.now()` probe, recorded at Phase
2's intro and adapted to the canvas in B.10, instead (ruled 2026-08-05),
so neither waits on R4 or 3.1. No committed scope beyond that --- measure first;
the frame path itself (one buffer address per frame) is already about as cheap
as the boundary allows.

**Rulings** --- two ruled on 2026-08-05, the third on 2026-08-07. sec. 2's
pre-2.2 baseline uses the throwaway probe --- R5a is not pulled ahead of R4
and 3.1 onto Phase 2's critical path, and R5 stays where the ledger puts it.
A session takes the measurements, in-session under one Xvfb setup with numbers
compared only within it --- "no session can" predated `firefox-beta`. And what
is worth chasing has its bars, as the Split now states: a wasm/native game-logic
ratio beyond 3x --- past the ordinary wasm tax, something pathological
on the table --- or a rendering p95 beyond one 60Hz frame, the llvmpipe venue
being conservative for that test since passing under software rendering implies
passing on hardware, with an exceedance confirmed on the real display before R5c
opens. Below both bars the banner's "runs rather slowly" is ruled stale.

**R6 --- Screenshot/overlay coherence.** Every renderer of "the current screen"
(the live grid, the 1.2 screenshot rasterizer, any future export) must consume
the same four `SingleFrame` fields after Phase 2. Enforced by putting the shared
drawing entry point in one TS function both paths call, and by a vitest case
rendering a fixture frame with overlays through both --- comparing draw-command
lists (per 1.2/2.2's functional-core structure), not pixels.

*Retired 2026-09-14 by B.10: with one renderer there is nothing to cohere.
The test file is never built, its `tools/doc-refs-allow.txt` entry standing
as long as this body names it, and the shared entry point it asked of 1.2
and 2.2 is 0.4's draw-op core.*

**Rulings** --- both ruled on 2026-08-07, before 1.2 starts as the field
required. "Both renderers reach the four `SingleFrame` fields
(`Frame.hs:data SingleFrame = SingleFrame`) through one shared TS entry point"
is handed to the 1.2 and 2.2 agents as an acceptance criterion --- the branch
that keeps R6 the small test its Owns now assumes; imposing it afterwards would
have been a deliberate rework of two landed items. And the entry point lives
in a module of its own that 1.2 creates and 2.2 imports --- sequencing decides,
`overlay-core.ts` not existing until Phase 2, and a live renderer importing
a module named for screenshots would break one-meaning-per-name; the neutral
name is the executing session's, its `tools/doc-refs-allow.txt` entry added
before it is written anywhere.

**R7 --- Pixel-perfect bitmap map fonts.** The native build draws the map
with the fontset's bitmap font whenever `allFontsScale` is 1.0 and
with the scalable one otherwise
(`Sdl.hs:if fromJust sallFontsScale == 1.0 then do`), and in fullscreen scales
the whole frame to the display through `rendererLogicalSize`
(`Sdl.hs:unless (fromMaybe NotFullscreen`), which README warns softens
the square font's edges. Measured 2026-09-14: under Xvfb, with the `dejavu`
fontset, whose bitmap slot is the `.bdf`, and `allFontsScale = 2.0` in the user
config, the native build renders the outline woff at 32px, antialiased ---
in the game's own `Ctrl+Shift+P` screenshot the lit map pixels form uniform 2x2
blocks 44 percent of the time, where a doubled bitmap gives all of them ---
and identically with `dejavuBold`, whose both slots are the woff; the crisp
doubled `.bdf` is not what SDL2 does today, only what it should, and this item
brings it to the browser first. The browser has drawn only the outline
`16x16xw.woff`, because a `@font-face` reads the sfnt formats alone, TrueType,
OpenType, WOFF and WOFF2, and neither BDF nor Windows FNT --- which is why
that woff was made in 2019 from the same bitmap. A canvas draws images,
so a bitmap font is a glyph atlas: decode the BDF, plain text, once --- at build
time into a PNG and a codepoint table, committed and freshness-checked like
0.2's generated files --- and paint each cell with `drawImage` from it, which
is what SDL2 does with the surfaces FreeType renders. Pixel-perfect under four
rules. The backing store is in device pixels and every blit lands at integer
device coordinates with image smoothing off, so a 16x16 tile occupies sixteen
device pixels a side. Integer scales only, the largest that fits, the rest
letterbox: a bitmap at 1.5x is a contradiction, and the letterbox is SDL2's own
logical-size rule. A fractional `devicePixelRatio`, browser zoom at 1.25 or 1.5,
is handled by working in device pixels throughout, the cell an integer number
of them and the canvas's CSS size whatever that divides to, recomputed on zoom
and resize, so the map sits on device pixels exactly and only the page around
it is fractional. And fullscreen re-renders at the fitting integer scale rather
than through 1.3's transform, which stays for the outline font, where resampling
is what SDL2 does too. The `.fnt` files are Windows raster fonts FreeType reads
for SDL2; Mikolaj converts them to BDF, so one decoder serves every bitmap font
under `GameDefinition/fonts/`. Two stages. R7a follows SDL2's font choice
and improves on its scaling: the bitmap whenever `allFontsScale` is 1.0
and the scalable font otherwise, and the canvas scaled as a whole by the largest
integer factor that fits, pixel for pixel, under browser zoom and fullscreen
alike, where SDL2's fullscreen scaling softens. R7b, later: keep the bitmap
beyond that at every integer `allFontsScale`, where SDL2 takes the outline font
today, and the outline font at every other factor. On the DOM route none
of this was reachable at any price, a span having no way to hold a glyph bitmap.

**Rulings** --- ruled 2026-10-20 by Mikolaj: R7b's criterion for crisper
is the integer one, applied by arithmetic, with no side-by-side owed ---
the bitmap at an integer `allFontsScale` and the outline font at any other,
where a nearest-neighbour bitmap's pixels come out unevenly wide. Weighed
the same day and not taken, so they are not re-proposed without new evidence:
a bound on that unevenness, which picks the outline font at 1.25x and 1.5x
anyway; an image metric over captures, whose weighting of blur against
unevenness nobody can set from first principles and whose instrument would have
to be built and proved; and a per-factor table from a human side-by-side, a new
look for every new factor.

## Multi-frontend practices (adopted)

G1 is one practice among several that make a codebase with N frontends cheap
to extend. These are adopted; the rejected companions are in Appendix B.

**Capability constants instead of behavioral CPP.** Module *selection* by CPP
is fine (`File.hs:#ifdef USE_JSFILE`, `Frontend.hs:#ifdef USE_WASM`); behavioral
forks scattered around the engine are not: the autosave skip
(`Server/LoopM.hs:!defined(USE_JSFILE) &&`), the history-dump key omission
(`HandleHumanLocalM.hs:!defined(USE_JSFILE) &&`), and the 2s exit flush wait
(`WatchUpdAtomicM.hs:#if defined(USE_JSFILE)`) are each an `#if` over the same
two macros, whose *reason* lives in a comment far from the backend it describes
--- and not even in the same *sense*: the first two
are `#if !defined(USE_JSFILE) && !defined(USE_WASMFILE)`, guarding the native
behaviour, while the exit wait is the positive
`#if defined(USE_JSFILE) || defined(USE_WASMFILE)`, browser 2s in the `#if`
branch and native 200ms in the `#else`. That inversion is exactly what makes
a mechanical conversion dangerous and a named constant safer. Let the storage
backend module itself export named constants (e.g. `savesCauseLag :: Bool`,
`filesAreDumpable :: Bool`, `needsExitFlushDelay :: Bool`) re-exported through
`File.hs`, and let the engine branch on values, not macros. A fourth backend
then answers named questions instead of hoping every negated macro conjunction
got updated; R1's re-measurement flips one constant instead of editing ifdefs.
(Server code consuming a File-layer constant respects the client-server split
--- it's the storage backend's property, not the frontend's.)

**Rulings** --- all three ruled on 2026-08-07. (1) The two
`GameDefinition/game-src/TieKnot.hs` sites stay out of scope, and the item says
so: `TieKnot.hs` *is* the knot --- the wiring layer where compile-time backend
choice is legitimately visible, not a fork scattered around the engine, which
is this practice's target class --- and besides, `TieKnot.hs:#ifdef USE_JSFILE`
(`GHC.Compact` absent under GHCJS) cannot become a runtime `Bool` at all; R3
removes their GHCJS halves on its own schedule. (2) `Common/JSFile.hs` mirrors
the constants: three lines in a dead file, guarded by the uniformity-diff rule
instead of the compiler --- and since `JSFile.hs` survives R3 as the documented
example of an alternative backend, the example then shows the complete interface
a fourth-backend author must export, the `USE_JSFILE` re-export arm staying
coherent in principle until R3 deletes it. (3) The naming rule: each constant
is a positive statement of the exporting backend's own property --- no negation
in any name, every backend defining all constants explicitly, consumers picking
`when` or `unless` as the logic reads --- the hazard having been negation hidden
in macro conjunctions, which names now carry instead. The layering objection
dissolves by naming the real fact: the exit delay is the backend's
write-durability-at-exit --- browsers trash in-flight localStorage writes ---
so the constant says that, in its name or its haddock, and is a storage property
after all.

**Sum-typed frontend selection.**
`sfrontendANSI`/`sfrontendTeletype`/`sfrontendNull`/`sfrontendLazy` are four
independent `Bool`s (`ClientOptions.hs:, sfrontendANSI ::`) whose simultaneous
truth is resolved by guard order in two places
(`Frontend.hs:sfrontendNull soptions =`
and `Frontend.hs:frontendName :: ClientOptions`). One sum-typed field,
with the constructors `FrontendDefault`, `FrontendANSI`, `FrontendTeletype`,
`FrontendNull` and `FrontendLazy`, makes the guard chains total cases and turns
conflicting flags into a parse error. Mechanical, moderate churn (options
parser, UnitTestHelpers' stub options).

**Rulings** --- all three ruled on 2026-08-07. (1) The save format needs
no bump, confirming the analysis this field carried: `ClientOptions` derives
`Generic` with `instance Binary ClientOptions`
(`ClientOptions.hs:instance Binary ClientOptions`), is written into and read
back out of every client save (`Client/State.hs:put soptions`
and `Client/State.hs:soptions <- get`) and is carried by `UpdRestart`
(`CmdAtomic.hs:UpdRestart FactionId`), so four `Bool`s becoming one constructor
changes the wire format --- but `compatibleVersion` compares only the first
three version-branch components (`Save.hs:compatibleVersion ::`) and the package
is `0.12.0.0`, whose first three differ from every `0.11.*` release, so every
pre-existing save is refused by version comparison before `Binary` ever sees it;
recorded so no session re-derives the arithmetic. (2) Five constructors always,
an unavailable one erroring at startup with a message naming the build: the type
stays total everywhere --- `Binary` and `Show` identical per build, no feedback
into (1) --- and asking for a frontend the build does not carry becomes
an explicit error rather than a silent substitution, guard-order accidents
becoming errors being this practice's whole theme; the path is real beyond
the browser edge case, since 3.2's driver passes argv to pruned-chain builds.
Fall-through to `FrontendDefault` and the CPP-pruned type are refused. (3)
The parser reshape is sanctioned --- an `<|>` of `flag'`s, conflicts as parse
errors, `--help` free to change --- with one constraint riding it: the long-form
flag names stay byte-identical, so every Makefile target, script and doc
invocation survives verbatim.

**The RawFrontend contract, written down and tested.** The engine-side protocol
every frontend must fit is real but implicit: the `fshowNow` handshake's four
numbered steps are split between `display` (`Frontend.hs:putMVar fshowNow ()`)
and `frameTimeoutThread` (`Frontend.hs:frameTimeoutThread :: Int`) with a third
participant in `saveKMP` (`Common.hs:void $ tryTakeMVar $ fshowNow rf`);
`fdisplay` semantics legitimately differ per frontend (SDL blocks until drawn
via its frame-queue handshake, wasm snapshots-and-returns with painting deferred
to rAF, Dom schedules a rAF callback, lazy does nothing) --- yet nothing states
which properties are *required* and which are incidental; the threading rules
(SDL needs the bound main thread via
`startupBound`/`workaroundOnMainThreadMVar`, wasm is a single-threaded reactor)
are likewise folklore the wasm port had to reverse-engineer. Deliverables:
a haddock contract on `RawFrontend`'s fields; a tasty contract-test harness run
against `nullStartup`/`lazyStartup`/`Teletype` natively and, via
`make test-wasm`, against the real `Wasm.hs` (key delivered while a frame
is pending; Esc reset; shutdown with a frame queued; `FrontPressed` semantics);
and a "how to add a frontend" checklist. The input-side cases land together
with 0.1, which creates the harness module --- they guard exactly what
it rewires in `Wasm.hs`; the rest of the harness pairs with 0.3, and the haddock
and checklist wait on neither (ruling (4)).

**Rulings** --- (1) to (3) ruled on 2026-08-07, (4) by Mikolaj on 2026-10-20.
(1) The four names --- `nullStartup`, `lazyStartup`, `display`,
`frameTimeoutThread`, today only inside `#ifdef EXPOSE_INTERNAL`
(`Frontend.hs:#ifdef EXPOSE_INTERNAL`), which defaults off
(`LambdaHack.cabal:flag release`, `LambdaHack.cabal:if flag(release)`)
so a harness driving them would not compile --- hoist into the unconditional
export list under the "internal and used in unit tests" group,
the `emptyUnknownTile` precedent
(`Common/Kind.hs:, emptyUnknownTile, emptyUIFaction`), riding this item's first
commit as the Split now states; driving only `chanFrontendIO` is refused,
a whole frontend chain being unable to isolate the `fshowNow` handshake
the harness exists to pin. (2) One test module with an `#ifdef USE_WASM` split
over the shared case table --- the pattern the suite already uses for its SDL
cases, one registration in `test/Spec.hs`, the `[fe-invariant]` series countable
in one place; `Sdl.hs`/`ANSI.hs` and `Wasm.hs` are never co-exposed
(`LambdaHack.cabal:Game.LambdaHack.Client.UI.Frontend.Wasm`), which is what
forces a split of some kind. (3) The "how to add a frontend" checklist
is haddock in `Frontend/Common.hs`, beside the contract fields commit (1) writes
--- read where a frontend author is already looking and shipped to downstream
authors via Hackage; a `docs/` file and README are refused, one starting a third
documentation genre and the other addressing the wrong audience. (4) The haddock
contract and the checklist are commit (1) and may land alone, ahead of 0.1
and 0.3, taking no outcome line: they describe code that exists today.
The checklist is where a frontend author meets the two review rules,
so it carries both --- decode screen indices with `punindex (rwidth coscreen)`,
never `toEnum`, and ask of every new line "would another frontend have to copy
this?"

**Determinism goldens --- native first, then cross-backend.** The tasty suite
already compiles and runs under both native (`cabal test`) and wasm
(`make test-wasm`), and `test/SessionUIMock.hs` already unwinds key macros
through the real `HandleHumanM` machinery. Add golden tests --- fixed seed,
scripted keys, committed digests, in the two ruled families: frame-and-overlay
digests from the scripted machinery, which are 2.4's gate, and final-state
digests from a seeded whole game --- in two stages. The native-only harness
lands **before 2.4**: it guards that flip regardless of wasm, 2.1 having landed
ahead of it by the Log's ruling of 2026-10-23. Then the same goldens run
under `make test-wasm`, routine in CI since R2's first jobs landed --- catching
native-vs-wasm *behavioral* drift (FFI-adjacent paths, numeric assumptions)
that per-frontend unit tests can't. Medium effort.

**Rulings** --- all three ruled on 2026-08-07, (1) re-cut on 2026-10-23
for 2.1's landing. (1) Two golden families, as the body now states:
frame-and-overlay digests --- the family 2.4's flip actually gates on, which
a game-state digest cannot see --- and final-state digests, guarding macro
and command semantics and the cross-backend behavioral drift the wasm stage
exists for; the digest fold is shared between them. (2) Each family runs
on the harness that naturally produces its input: the frame family
on the scripted `CliMock`/`SessionUIMock` machinery
(`UnitTestHelpers.hs:newtype CliMock a = CliMock`, `scriptedFchanFrontend`
at `UnitTestHelpers.hs:scriptedFchanFrontend ::`, `scriptedCliState`
at `UnitTestHelpers.hs:scriptedCliState ::`), whose scripted frontend captures
the very frames the digest guards, and the state family on a seeded whole game
through `tieKnot` --- the `--frontendNull --benchmark --stopAfterFrames` shape
the Makefile bench targets use, which the suite already depends on and which
captures no frames, the reason it cannot serve the first family. (3) The digest
is a hand-rolled FNV-style fold over the `Binary` `encode` bytes ---
dependency-free, deterministic, trivially resolving under `wasm32-wasi-cabal`
--- with the digests as committed hex literals in the test file: no data file,
no preopen-path coupling in the wasm run, and a legitimate golden change is one
pasted literal in a reviewable diff; `tasty-golden` and `hashable` stay out
of the stanza.

**A CI smoke for every shipped frontend.** Before these smokes no frontend's
event loop but teletype's ran in CI, the `make test-gha` playtests driving
`--frontendTeletype` through whole games. `make test-wasm` compiles and links
`Wasm.hs` but runs the suite with `--frontendNull`, so the wasm frontend
is entered only by 0.3's frame case, through `startup` and `fdisplay`,
and by 0.1's contract cases, through `startup`,
and `make build-wasm`/`make build-ts` run in no workflow, leaving the reactor's
`--export=` wiring exercised by none. SDL2 was reached only via
the `slogPriority == Just 0` init-and-quit backdoor
(`Sdl.hs:if slogPriority == Just 0 then do`) --- which does run its font
discovery and decoding, once per configured fontset, ahead of the backdoor
branch, but never its event loop or renderer. Add an `xvfb-run` smoke driving
a real SDL game for a few frames (a tiny `--stopAfterFrames` variant
of `benchFrontendBattle`), a pty-driven ANSI startup/shutdown check, and ---
once 3.2 exists --- a short `nodeBench` run of the reactor build, though
on the null frontend, so `Wasm.hs` stays unexercised there. No shipped frontend
goes permanently untested the way `Dom.hs` did. Partly landed,
in the `Makefile`'s `smokeANSI` and `smokeSdl`, which the playtest job runs
on its optimized build, each gated as ruling (1) says. Still to do:
the `nodeBench` smoke, after 3.3.

**Rulings** --- all three ruled on 2026-08-07. (1) Each smoke gates on exit
status *and* a frame-count assertion against the benchmark report, each reading
its own source --- the repo fact that makes one source wrong per smoke:
the executable redirects its own stdout and stderr to `~/.LambdaHack/stdout.txt`
and `~/.LambdaHack/stderr.txt` whenever stdout is not a terminal
(`GameDefinition/Main.hs:unless isTerminal $ do`), so under `script` the pty
*is* a terminal and the ANSI smoke greps its pty capture, while under `xvfb-run`
it is not and the SDL smoke greps the harvested `~/.LambdaHack/stdout.txt`,
freshly overwritten each launch. The grep that must find the report line
is the built-in non-vacuity control; exit status alone cannot distinguish a game
that ran from one that quit at frame zero. (2) The SDL smoke
is `benchFrontendBattle`'s variant (`Makefile:benchFrontendBattle:`,
no `--frontend` flag, so SDL) at `--stopAfterFrames 60` --- past startup
and well into the render loop while keeping the job's game portion to seconds
--- and the playtest job's apt line gains `xvfb` in the same commit. (3)
The ANSI smoke's driver is `script -e`, present at `/usr/bin/script`
and propagating the child's exit status: zero new code, and the smoke's
termination is `--stopAfterFrames`'s business, so a python pty helper would
be maintained machinery ahead of need.

**Frontends pass widths explicitly.** Frontend code decodes linear indices
with the explicitly-parameterized `punindex (rwidth coscreen)`, never the `Enum`
instance: `toEnum i` depends on the global `speedupHackXSize` (the *dungeon*
width, correct at frontend call sites only via the engine's
`Client/UI/Content/Screen.hs`'s `rwidth == RK.rWidthMax` assertion). The switch
is the same arithmetic with one *fewer* global read. Its one live violation,
`setMapChar`'s `let Point{..} = toEnum i` ([Sdl.hs#L568][sdl-568]), went
with `Sdl.hs`'s adoption of `CellStyle` in `3fcc93a1e`, and a step
of `tools/checks.py` now fails that spelling in any frontend module
but `Dom.hs`. (`Dom.hs:let Point{..} = toEnum i` has the same pattern next
to the correct form at `Dom.hs:let Point{..} = punindex (rwidth coscreen) i`,
but it's a dead example file, R3 --- leave it.) The rule binds all live
and future frontend code from now on. The `Enum` instance itself and its global
stay engine-internal, permanently --- the engine-wide removal of the hack
is rejected outright (Appendix B has the ruling and the hack's documentation
of record).

**Rulings** --- ruled 2026-10-20 by Mikolaj, re-cut 2026-10-21: the grep
for `Point{..} = toEnum` in the frontend modules, then a line of the work list's
Done, becomes a standing check, a step of `tools/checks.py` added after
the commit that removes the violation, which lands on a branch carrying `Sdl.hs`
alone, so the rule has a mechanical floor once its one violation is gone.
It catches that spelling only, so the review question stands beside it.

**Functional core, imperative shell --- per frontend (the review bar).**
`CellStyle`/`InputDecision`/`OverlayLayout` and the `ts-src`
`*-core.ts`-vs-wiring split are instances of one rule: a frontend module
contains only event capture, output mutation, and plumbing; every *decision*
lives in shared pure code under test. The question to ask of any new line
in `Sdl.hs`/`Wasm.hs`/`terminal.ts`: "would a fourth frontend have to copy
this?" If yes, it belongs in core. This is the practice that generated
0.1/0.2/2.1, stated so it outlives them.

**Rulings** --- to decide, one thing to refuse: generalizing this rule
into a frontend-interface-as-value record is Appendix B.4's recorded don't-do,
deferred with named revisit triggers, so an agent proposing it is re-opening
a ruling rather than extending the practice. If a trigger has genuinely fired,
that is B.4's revision, not this item's.

## What would falsify this

Worth stating, because a campaign this long otherwise reads as unfalsifiable.
Three claims carry it, and each has a check that would sink it rather
than merely inconvenience it.

**G1, that one canonical definition can serve every frontend.** What would sink
it is a shared module that cannot decide without knowing which frontend
is asking --- a `CellStyle` or an `OverlayLayout` whose body branches
on the caller rather than on a parameter it was given. That is duplication
with a worse interface, and the check is 2.1's `Sdl.hs` refactor: if the native
consumer needs the module to know it is native, G1 has failed for that rule
and the rule belongs back in the frontend. The floor-glyph parameter
is the shape that passes; a frontend tag is the shape that fails.

**G2, that SDL2's behaviour is the bar.** What would sink it is SDL2 being
wrong, and it already was once: 0.0 assumed the three fill-only highlight kinds
draw no outline, and SDL2 in fact draws a black ring --- the erase half
of a workaround for a 2.0.16 rectangle bug, not a design choice anyone made.
Parity copied it faithfully. So the bar is SDL2's *intent* where the two can
be told apart, and where they cannot, the audit record in Appendix C is what has
to say which was checked. A second such discovery would not sink G2,
but a pattern of them would mean the bar is a workaround inventory.

**The boundary-cost ruling**, that a per-chunk `js_measureText` crossing is too
expensive to consider. It is the reason 2.1's pen is split
into a measurement-free cutoff and a fit function, and it rests
on no measurement of this codebase. R5's instrument is the check; the recorded
alternative in 2.1 --- batch the whole frame's measurements into one crossing
--- is what the ruling loses to if the numbers say so. Until then it stands,
and it is a ruling rather than a fact.

What would *not* falsify anything here: an item turning out larger than its row
says, or a **Decide first** resolving against the sketch in its body. Both
are the plan working. The Log is where they go.

## Out of scope

Sound (SDL2 has none either); window resizing (`windowResizable = False`
natively); tile/image graphics (no `sdl2-image` --- both frontends
are font-glyph renderers); NumLock/numpad perfection (a `KeyboardEvent`
limitation, already mitigated by on-page guidance); mouse hover / IME (SDL2
doesn't handle them either); a fontset picker UI (superseded by R4). Save-file
*persistence* was never a gap (it works, via `WasmFile.hs`); save-file
*robustness* is in scope as R1. Further deliberate exclusions with their
rationale: Appendix B.

## Appendix A --- Investigation: porting the GHCJS target to GHC's in-tree JavaScript backend

The port was decided **against** (R3, B.8); this appendix is kept
as the resurrection manual --- feasibility evidence, work items, effort estimate
and sources --- should that decision ever be revisited.

Conducted 2026-07-13. Method: every repo-side claim below was read
from the current working tree (file:line references); every ecosystem claim
was checked against a primary source that day --- the GHC user's guide, GHC's
GitLab (release branches and merge-request API), Hackage package pages
and released `.cabal` files fetched verbatim, upstream GitHub `master` branches,
and the ghcup cross-channel metadata. Where a source was secondary or a claim
rests on inference, that's said explicitly. Sources are listed in A.7 and cited
inline by number.

### A.1 The old GHCJS target is unbuildable --- waiting was never an option

- The repo requires GHC >= 9.10:
  `tested-with: GHC ==9.10.3 || ==9.12.4 || ==9.14.1`
  (`LambdaHack.cabal:tested-with: GHC ==9.10.3`) and `default-language: GHC2024`
  (`LambdaHack.cabal:default-language: GHC2024`), which only exists in GHC
  9.10+.
- The standalone GHCJS compiler's last releases track GHC 8.6/8.10 [8];
  it was never updated past that --- its successor *is* the in-tree backend.
- Therefore `Dom.hs` (283 lines), `JSFile.hs`, and every `impl(ghcjs)` block
  are dead code on any compiler that can build this package. There is no "keep
  the old target limping" option; the only choices are the in-tree JS backend,
  or freezing/deleting the code.

### A.2 The in-tree JavaScript backend: status and toolchain

- Merged into GHC for 9.6 (Nov 2022) [7][9]. As of the current release (9.14.1),
  the user's guide still says: *"included as a technical preview. At time
  of writing, it is being actively developed but is not suitable for serious
  projects and production environments"* and *"not distributed in the GHC
  bindist and requires a manual build"* [2].
- The second half of that quote is out of date in practice: ghcup's cross
  channel ships community bindists for `javascript-unknown-ghcjs` GHC 9.6.2,
  9.6.7, 9.10.2, 9.12.1 and 9.12.2 (no 9.14.x yet, unlike wasm32-wasi which has
  newer coverage) [3]. Emscripten is required as the configured C toolchain,
  at install time and for any C bits [2][7]. Version fit: the repo's floor
  (9.10) and middle (9.12) are both available; `tested-with`'s 9.14.1 has no JS
  bindist yet.
- Cabal identifies the new target as `arch(javascript)` (and `impl(ghc)`),
  **not** `impl(ghcjs)` --- the old conditionals evaluate false under it.
  Concretely, building this package with the JS backend today would fall through
  to the *native* branch and try to build `Sdl.hs` against `sdl2`.
  The ecosystem's own convention confirms this: jsaddle's and ghcjs-base's cabal
  files gate on `impl(ghcjs -any) || arch(javascript)` [10][11].

### A.3 Template Haskell --- the make-or-break requirement, and it holds

This repo cannot build without cross-target TH: `rcfgUIDefault` embeds
`config.ui.default` via a TH splice
(`GameDefinition/Content/RuleKind.hs:, rcfgUIDefault = $(do`),
`Client.UI.Content.Screen` runs a TH splice that reads and parses
`GameDefinition/PLAYING.md` at compile time (plus `embedDir` for fonts, though
that one is `#ifdef`'d away under `USE_BROWSER`).

- The JS backend runs TH splices through a node-based external interpreter:
  `compiler/GHC/Runtime/Interpreter/JS.hs` ("JavaScript interpreter",
  `spawnJSInterp`, `jsLinkRts`, `jsRunServer`) --- verified present in both
  the `ghc-9.10` and `ghc-9.12` release branches by fetching the file directly
  [4]. At the backend's initial merge TH was explicitly missing [7]; IOG's 2023
  update series documents the implementation work in between [9].
- This is the same architecture as the wasm backend's TH, which this repo's wasm
  build already depends on daily --- including qRunIO file-reading splices,
  which run on the build host and resolve repo paths normally. No new class
  of TH problem is introduced.

### A.4 FFI differences and their exact blast radius in this repo

- The new backend's documented convention for `foreign import javascript`
  is a **JS function expression** (typically an arrow function); the old GHCJS
  `$r = ...` result-assignment sugar is not documented as supported [1]. Repo
  impact: exactly two import strings,
  `JSFile.hs:= LZString.compressToUTF16($1);"`
  (`LZString.compressToUTF16`/`decompressFromUTF16`) --- a mechanical rewrite.
- There is no true `foreign export javascript` on the JS backend; callback-style
  quasi-exports go through `GHC.JS.Foreign.Callback`
  (`syncCallback*`/`asyncCallback*`), with `JSVal` and conversions
  in `GHC.JS.Prim` [1]. Repo impact: **none** --- `Dom.hs` registers event
  handlers from the Haskell side via ghcjs-dom's `on`/`EventM` and exports
  nothing; unlike `Wasm.hs`, whose whole design is JS-calls-Haskell exports.
  (This asymmetry is *why* `Dom.hs` doesn't need exports: under GHCJS/JS-backend
  the Haskell program owns the page; under wasm the TS loader owns the page.)
- An `interruptible` convention exists for async JS calls (continuation
  argument) [1]; nothing in `Dom.hs`/`JSFile.hs` needs it.

### A.5 The library chain Dom.hs sits on --- ported, but thinly maintained

`Dom.hs` imports only `GHCJS.DOM.*` (typed bindings); `JSFile.hs` imports
`Data.JSString`/`Data.JSString.Text` (ghcjs-base) plus `GHCJS.DOM.Storage`.
The chain those need:

- **ghcjs-dom 0.9.9.2** (May 2024) --- its released `.cabal` selects
  the implementation with:
  `if flag(jsffi) && (impl(ghcjs <9) || (impl(ghc >=9.6.4) && arch(javascript)))`,
  taking `ghcjs-dom-jsffi` for old GHCJS and **`ghcjs-dom-javascript`
  for the new backend** (fetched verbatim from Hackage) [5]. So the new backend
  was explicitly wired in, despite the package description and `tested-with`
  metadata still reading as old-GHCJS-era.
- **ghcjs-dom-javascript 0.9.9.3** (Sept 2024) --- the new-backend flavor;
  depends on `ghcjs-base` [6].
- **ghcjs-base 0.8.0.4** --- released on Hackage; its `master` cabal carries
  `if !arch(javascript)` conditionals, i.e. it builds natively on the new
  backend; repo activity as recent as 2026-03-26 [11].
- **jsaddle 0.9.9.4** (uploaded 2026-03-26 --- actively maintained) --- `master`
  cabal: `if impl(ghcjs -any) || arch(javascript)` -> `ghcjs-base`,
  so the jsaddle/`ghcjs-dom-jsaddle` route is a working fallback if the jsffi
  flavor misbehaves [10].

Caveats, stated plainly: package descriptions and `tested-with` fields across
this family are stale; Hackage build reports show failures (which means nothing
for a cross target --- Hackage builders have no JS toolchain --- but also means
there's no positive signal); and real-world mileage on the new backend is thin
next to wasm's. Miso is the flagship consumer of the backend generally. Budget
for small upstream patches and expect to test everything locally; do not expect
the first `cabal build` to succeed.

### A.6 Work items, effort, and risk placement

Total: **roughly 1.5--3 weeks of focused work**, deliberately front-loaded
so the cheapest step retires the most uncertainty.

1. **Spike, 1--2 days.** Install the 9.12.2 JS bindist (ghcup cross channel) +
   emsdk; make the cabal edits from item 2 in rough form; build the library
   and run a `--frontendNull`/teletype game under node. This surfaces the two
   genuine unknowns: (a) whether the full dependency tree builds
   under `arch(javascript)` (pure-Haskell deps like `miniutter`, `hsini`,
   `splitmix`, `enummapset` should; `async` relies on threads, which the JS
   backend implements green-threaded as GHCJS did); (b) **`ghc-compact`** ---
   the package is an unconditional dependency (`LambdaHack.cabal`), the wasm
   build really calls `compact` at startup, and the old-GHCJS escape hatch
   `#ifdef USE_JSFILE`
   (`let cops = copsRaw -- until GHCJS implements GHC.Compact`,
   `TieKnot.hs:#ifdef USE_JSFILE`) already exists --- keep it for the new
   backend until proven unnecessary. Everything after the spike is known-shape
   work.
2. **Cabal + `JSFile.hs`, 1--2 days.** `impl(ghcjs)` -> `arch(javascript)`
   everywhere --- all three sites: the `common options` cpp-options conditional
   (`LambdaHack.cabal:-DUSE_BROWSER -DUSE_GHCJS -DUSE_JSFILE`), inherited
   by every component, and the `library` stanza's frontend and file-backend
   conditionals (`LambdaHack.cabal:Game.LambdaHack.Client.UI.Frontend.Dom`).
   The executable has no `impl(ghcjs)` conditional of its own; it inherits them.
   Delete the dead old-compiler knobs (`ghcjs-options`: `GHCJS_GC_INTERVAL`,
   `GHCJS_BUSY_YIELD`, `-dedupe`, `GHCJS_BROWSER`,
   `LambdaHack.cabal:Haskell GC in GHCJS every 10s` --- none has a new-backend
   equivalent; the new backend takes plain `ghc-options`); revisit
   the `supportNodeJS` flag's meaning; rewrite the two `$r =` FFI strings
   as arrow functions. The `USE_GHCJS`/`USE_JSFILE` CPP names can stay (they
   name the *frontend/file-layer choice*, not the compiler), which keeps
   the churn out of the eight source files that test them.
3. **`Dom.hs` against ghcjs-dom, 2--5 days --- the tail risk.** In the good case
   it compiles nearly as-is (the API surface `Dom.hs` uses --- `on`, `EventM`,
   `RequestAnimationFrameCallback`, table/cell types --- exists in both
   flavors). The spread covers patching `ghcjs-dom-javascript`/ `ghcjs-base`
   where stale, or falling back to the jsaddle flavor.
4. **Build/deploy harness, 2--3 days.** Makefile targets mirroring
   `build-wasm`/`build-ts` (the compiler emits a `.jsexe` with `all.js` [2]);
   a JS-variant `index.html` (load `all.js` + `lz-string`, the old model ---
   `Dom.hs` builds its own DOM, needing only the `gameMap`/`pleaseWait`
   elements); deployment into the pages repo alongside the wasm artifacts.
5. **CI, optional, 1 day.** A job in the hand-written workflow with ghcup
   cross + emsdk, building and node-smoke-testing. Without it, the target rots
   again the way the last one did --- factor this into any future revival
   decision, not just the initial cost.

If Phase 0 has landed first, `Dom.hs` should consume `InputDecision`/`CellStyle`
as part of step 3 at near-zero marginal cost --- which is why a revival would
be cheaper after Phases 0--2 than before.

### A.7 Sources

Primary (fetched/inspected directly on 2026-07-13):

1. GHC User's Guide, "FFI and the JavaScript Backend" ---
   <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/javascript.html>
2. GHC 9.14.1 User's Guide, "GHC Backends" (JS backend status quote) ---
   <https://downloads.haskell.org/ghc/latest/docs/users_guide/codegens.html>
3. ghcup cross-channel metadata (bindist inventory) ---
   <https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-cross-0.0.9.yaml>
4. `GHC.Runtime.Interpreter.JS` in release branches (raw fetch, HTTP 200 +
   content on both) ---
   <https://gitlab.haskell.org/ghc/ghc/-/raw/ghc-9.10/compiler/GHC/Runtime/Interpreter/JS.hs>,
   `.../ghc-9.12/...`
5. ghcjs-dom 0.9.9.2 released `.cabal` (backend-selection conditional) ---
   <https://hackage.haskell.org/package/ghcjs-dom-0.9.9.2/ghcjs-dom.cabal>
6. ghcjs-dom-javascript ---
   <https://hackage.haskell.org/package/ghcjs-dom-javascript>
7. "JavaScript backend merged into GHC" (IOG Engineering, 2022-12-13; TH listed
   as missing at merge) ---
   <https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/>
8. GHCJS repository (last supported GHC versions) ---
   <https://github.com/ghcjs/ghcjs>
9. IOG GHC update series (2023; JS-backend TH implementation work) ---
   <https://engineering.iog.io/tags/ghc/>
10. jsaddle --- <https://hackage.haskell.org/package/jsaddle> and `master` cabal
    at <https://github.com/ghcjs/jsaddle>
11. ghcjs-base --- <https://hackage.haskell.org/package/ghcjs-base> and `master`
    cabal at <https://github.com/ghcjs/ghcjs-base>

Checked but yielding nothing decisive (recorded so nobody re-treads them): GHC
9.8.1/9.10.1/9.12.x release notes (no explicit "JS backend TH" announcement ---
the release notes are simply sparse on this backend; the branch-source check
in [4] is the authoritative evidence); the GHC wiki's javascript-backend page
(access-blocked at time of writing); GHC GitLab MR search for the TH merge
(the API surfaced only the initial backend merge, !9133, 2022-11-30, milestone
9.6.1).

---

## Appendix B --- Decisions against, and deferrals

Decisions by Mikolaj, 2026-07-13. Recorded so they aren't re-proposed,
and so the reasoning survives if circumstances change.

**B.1 No copying / screen-reader work beyond what the DOM gives free.** DOM text
is incidentally selectable/copyable and visible to screen readers ---
affordances SDL2 never had, which canvas-rendered overlay text (message log,
menus, help) loses after Phase 2. Decision: keep the affordances only where they
are free (the DOM `<span>` grid keeps them by nature; don't disable selection
there), but spend no effort preserving them for overlay text --- no hidden DOM
mirror, no copy button, no ARIA layer. SDL2 parity is the bar, and canvas meets
it. *Extended 2026-09-14 by B.10: the map's DOM affordances go with the grid,
and the ANSI frontend is the screen-reader route.*

**B.2 The GHCJS-era page's community features stay gone.** The old page
(pages-repo `index.html`, GHCJS era) carried a cactus.chat Matrix comments
widget, a feedback invitation, and SEO `meta keywords`; the wasm page rewrite
dropped them. They are deliberately not restored --- the old page's git history
is not a to-do list.

**B.3 GHCJS's two-edge highlight rendering is an artifact, not a feature.**
Context for 0.0's highlight rule: the GHCJS-era page CSS gave every table cell
`border:1px solid #000000`, so `Dom.hs`'s `border-color` writes produced visible
outlines --- including the spurious one on `HighlightBackground` cells whose
colour the wasm port faithfully reproduced; and because of collapsed table
borders, a highlight effectively showed only on a tile's bottom and right edges
(verified in a browser). Rulings: SDL2's look is canonical (a black ring,
not none, for `None`/`Background`/`NoneCursor` --- see 0.0); every kind outlines
all four tile sides; the wasm `inset` box-shadow's four-sided drawing is correct
and stays.

**B.4 No frontend-interface-as-value record (yet).** The idea: each frontend
module exports one value, e.g. `frontend :: FrontendImpl` (name, startup,
capability record), and `Frontend.hs` consumes only that --- so adding
a capability becomes adding a field the type-checker forces every frontend
to answer, instead of today's parallel conventions (a module exporting
`startup`/`frontendName` wired by CPP as `Chosen`,
`Frontend.hs:#ifdef USE_WASM`; `fprintScreen` as a dummy field overridden
post-construction by exactly one frontend,
`Common.hs:fprintScreen = return`/`Sdl.hs:let rf = rfWithoutPrintScreen`; 2.4's
`supportsMultiFont` constant). Rejected for now. Revisit if any trigger fires:
capabilities accumulate beyond the two now planned; a bug appears
that this design would have prevented (e.g. a frontend silently missing
a newly-required export); or another adopted item turns out to depend on it.
Weighed again on 2026-10-20 and kept, on a proposal to build it inside sum-typed
selection: no trigger has fired --- that item's rulings specify it without
this design, so it overlaps rather than depends, and R7, cited as a new
capability, changes no Haskell frontend module; the sum type's total `case`s
already make `-Wall` flag a frontend a dispatch misses; and this design would
check capabilities, not the explicit-widths and functional-core rules
the proposal meant to preserve.

**B.5 No hoisting of the frame-diff fold.** The changed-cell fold is duplicated
verbatim in Haskell (`Sdl.hs:U.foldM'_ setMapChar 0 $`,
`Dom.hs:<- newRequestAnimationFrameCallbackSync`), but it's two lines
of content-free traversal --- not worth the churn.

**B.6 Engine-wide `speedupHackXSize` removal: rejected outright.** The hack
stays permanently: it is set once at startup, it is now documented (below),
and any clean alternative would churn the engine's hottest loops for zero
player-visible value. No rework, no benchmark experiment. Why the hack exists
(confirmed from its own documentation,
`Point.hs:This is a hack to pass the X size of the dungeon`): a global
`NOINLINE` one-element `PrimArray` carries the dungeon's X size into the `Enum`
instances of `Point` and `Vector`, because `Enum` methods can't take a width
argument and these conversions sit in the engine's hottest loops; `PrimArray`
was chosen over `IORef` explicitly for lower read overhead; it is mutated
exactly once, at startup (`TieKnot.hs:speedupHackXSizeThawed <-`,
`unsafeThaw`/write/`unsafeFreeze`), before first use; the comment names Backpack
as the possible clean alternative, unverified over GHCJS-era doubts (moot
for wasm, untested for the JS backend). The candidate reworks that were
on the table (threading the width through call sites, or a Backpack signature)
would have touched FOV/pathfinding/`PointArray` hot loops and required
before/after runs of the bench battery to prove no regression --- cost
with no payoff, hence the rejection. This paragraph is the hack's documentation
of record. (The *frontend-side* rule --- explicit `punindex`, no benchmark
needed --- is adopted; see Multi-frontend practices.)

**B.7 No-action findings from the alignment analysis** (kept self-contained
since the superseded document may be deleted):

- *Per-frame cell diffing stays on the TS side.* A `buf[i] === prev[i]` check
  is content-free equality with no canonical second definition to drift from,
  and moving per-cell diffing into Haskell would trade one bulk buffer transfer
  per frame for potentially hundreds of wasm<->JS boundary crossings;
  a Haskell-diffs-then-marshals-changes middle ground transfers as much as today
  in the worst case while adding complexity.
- *DOM/grid construction stays in TS.* Grid dimensions already come from Haskell
  (`rwidth`/`rheight`); the rest (`gridTemplateColumns`, `lineHeight`,
  `whiteSpace`) is CSS mechanics with no Haskell-side counterpart, exactly
  as `Dom.hs`'s own `<table>` CSS choices aren't derived from anything deeper.
  *Moot since B.10 (2026-09-14): there is no grid to construct.*
- *Focus management stays in TS.* The bfcache/`pageshow`/mousedown-refocus
  orchestration in `terminal.ts` is browser-lifecycle work with no canonical
  Haskell value anywhere near it; `Dom.hs`'s single `focus divMap` call very
  likely has the same bfcache gap, just never tested.
- *`#screen` font-size (16px) is a web-presentational constant*
  with no canonical Haskell value behind it; nothing to generate.
  (Its player-facing counterpart is now handled properly by 1.5's
  `allFontsScale`.) *Superseded on 2026-08-07 by 2.2's ruling (a): the browser
  derives `lmHalfSize` from the map font's configured size times
  `sallFontsScale` and imposes the cell dimensions on the grid, so the 16 does
  have a canonical Haskell value behind it --- `16x16xwScalable`'s
  in `config.ui.default` --- and the grid takes it rather than hardcoding one.*
  *That ruling was re-cut on 2026-10-20 for B.10, which left no grid.*
- *Save persistence needs no rework* --- `WasmFile.hs` mirrors `JSFile.hs`'s
  exact localStorage format as a proper Haskell sibling module; it
  is the existing exemplar of the pattern this plan applies elsewhere.

**B.8 No JS-backend port --- one browser target is enough.** A port of the GHCJS
target to GHC's in-tree JavaScript backend was investigated and found feasible
at roughly 1.5--3 weeks (Appendix A), with real payoffs (typed DOM bindings ---
the one structural gap wasm can't close; a no-wasm fallback browser target).
Rejected: a second browser target means a second cross toolchain (including
emscripten) and CI surface maintained forever, for an engine that only needs
one. `Dom.hs`/`JSFile.hs` stay in-tree as documented-dead examples
of an alternative frontend and file backend; everything else GHCJS is ripped out
at parity (R3 has the inventory). Appendix A remains the resurrection manual
if this is ever revisited.

**B.9 No `document.title` from `stitle`.** SDL sets its window title
from the `stitle` option (`Sdl.hs:let title = T.pack $ fromJust stitle`) while
the page's `<title>` is hardcoded in `index.html`. Sourcing it from Haskell
at startup was proposed as a one-line G1 nicety and dropped: the page title
is page chrome, not game knowledge worth an FFI call.

**B.10 One canvas, no DOM grid.** Ruled 2026-09-14 by Mikolaj, on the review
that measured this plan against the code it governs: the browser draws map
and overlays on a single `<canvas>`, and the DOM span grid `terminal.ts` builds
today goes. The DOM-plus-canvas split the plan had assumed since 2.2 was never
ruled on, and it was the source of a cluster of work: 1.2's rasterizer
was a second renderer of the screen, R6 existed to keep two renderers coherent,
2.2 spent its body aligning a canvas to a grid it did not draw --- integer cell
metrics imposed on spans, a measured cell and a startup assert,
`overflow: hidden` imitating SDL2's glyph crop, a backing store for the overlay
alone --- and Appendix C's half-cell mouse gap followed from spans reporting
cells. On one canvas a screenshot is `toBlob` after the filename helper, there
is nothing to cohere, the crop is a clip rect per cell as in `Sdl.hs`,
the metrics are the painter's own, the pointer gives pixels and the browser's
`pointTranslate` becomes SDL2's division by `halfSize`, and the highlight ring
is a `strokeRect` one pixel inside the cell, which is `drawRect`. What goes
with the grid is the map's DOM text, selectable and visible to screen readers;
B.1 already gave that up for every overlay, and the ANSI frontend serves
a screen reader natively. Canvas text goes through the same font engine as DOM
text, so the outline map font should draw the same glyphs, which 0.4's Split (0)
checks with an `xwd` pair; the frame cost is the other thing not known
from the armchair, and the same step measures it against the baseline at Phase
2's intro before the painter merges, the number to be recorded here.
Consequences: 0.4 is the painter; 1.2 shrinks to the helper and one call; R6
retires; 2.2 loses its alignment and crop pitfalls and draws its three sections
through the painter's core; 1.3's transform stays for the outline font, R7
replacing it for bitmap fonts; B.7's grid-construction bullet is moot;
and the bitmap map fonts, which no DOM can show at all, become R7.

*Measured 2026-10-22 by 0.4's Split (0), on the painter of `9eb0c2c3c`
with the probe below applied, under Phase 2's protocol but for two things.
Firefox is now 159.0b5, so the DOM grid was re-measured beside the canvas,
as `c5120693f` builds it with Phase 2's probe, rather than set against
the 3.00ms read on 156.0b2: the two builds ran alternately, two runs each, each
site laid out as `make page-gate` lays out its scratch directory and served
by the server `make serve-wasm` runs. And the profile's `user.js` also
suppresses the terms-of-use dialog 159 opens over the page, which takes
the keyboard, with `termsofuse.bypassNotification`, an accepted `termsofuse`
version and date and `browser.preonboarding.enabled` off; its privacy-notice bar
stays. Read every 15 seconds for five minutes, the canvas's p95 settled
at **3.00ms** and **2.00ms**, means 0.80ms at n=4904 and 0.66ms at n=4478,
and the DOM grid's at 3.00ms and 2.00ms, means 0.76ms at n=4501 and 0.91ms
at n=3404: the spread between runs exceeds the difference between the builds,
so the move to the canvas costs nothing this instrument resolves, and the canvas
pair is the baseline 2.5 re-measures against. Each build's `xwd` frame
of the main menu, taken after the timing, shows the same glyph shapes
at the same places, 36 of the 12490 full-ink pixels present in one build only
and the antialiasing differing; the look is a human's to judge, 0.4's hand-back.
The painter without its clip also left stray pixels in blank cells: Firefox's
coloured antialiasing puts one pixel past the cell for 194 of the map font's 385
glyphs, mostly letters, and the per-cell diff kept that pixel wherever
the letter's cell alone was repainted. The clip to the cell, `Sdl.hs`'s crop,
removed them at no cost the runs resolve. The probe's diff:*

```diff
--- a/ts-src/src/terminal.ts
+++ b/ts-src/src/terminal.ts
@@ -181,6 +181,10 @@
   let pendingFrame: { buf: Uint32Array; w: number; h: number } | null = null;
   let rafHandle: number | null = null;
 
+  // THROWAWAY frame-timing probe (2026-08-07) -- never commit.
+  const lhProbeSamples: number[] = [];
+  let lhProbeT0 = 0;
+
   // Paints wait for the map font: measureText and fillText before it loads
   // would use a fallback face.
   function schedulePaint(): void {
@@ -190,12 +194,23 @@
       const frame = pendingFrame;
       pendingFrame = null;
       if (frame) applyFrame(frame.buf, frame.w, frame.h);
+      // THROWAWAY frame-timing probe (2026-08-07) -- never commit.
+      if (frame) {
+        lhProbeSamples.push(performance.now() - lhProbeT0);
+        const n = lhProbeSamples.length;
+        const sorted = [...lhProbeSamples].sort((a, b) => a - b);
+        const mean = lhProbeSamples.reduce((a, b) => a + b, 0) / n;
+        const p95 = sorted[Math.max(0, Math.ceil(n * 0.95) - 1)];
+        document.title =
+          `LHPROBE n=${n} mean=${mean.toFixed(2)} p95=${p95.toFixed(2)}`;
+      }
     });
   }
 
   // Snapshots the framebuffer and schedules a render; does not paint
   // synchronously itself (see applyFrame, deferred via rAF).
   function submitFrame(addr: number, w: number, h: number): void {
+    lhProbeT0 = performance.now();  // THROWAWAY probe -- never commit.
     // Snapshot synchronously: the wasm buffer at `addr` is only valid for
     // the duration of this call (Wasm.hs's display uses an `unsafe` FFI
     // import specifically so the GC can't move/reuse it mid-call). Reading
```

*Measured again on 2026-10-22 in Chromium 156.0.8078.47, under the protocol
above with Chromium and a fresh profile in place of firefox-beta, on the same
two trees and probes: read every 15 seconds for five minutes, the canvas's p95
ended at **9.40ms** and **13.40ms**, means 1.92ms at n=3184 and 3.11ms
at n=4581, and the DOM grid's at 12.40ms and 14.40ms, means 2.66ms at n=3109
and 3.77ms at n=4626. The canvas read lower in both pairs, by 3.00ms and 1.00ms
at p95, less than the 4.00ms between its own two runs, so in Chromium too
the move costs nothing this instrument resolves, and that canvas pair
is the baseline 2.5 re-measures against in Chromium. The two builds' main menus
show the same glyph shapes at the same places, the antialiasing differing:
the canvas's screen holds 54 colours to the grid's 159.*

**B.11 No generator executable for the TS tables.** Ruled 2026-10-20 by Mikolaj:
0.2's generated files are written and checked by a tasty case in the existing
suite, not, as designed until then, by a native executable of their own, run
by a Makefile target that `build-ts` depended on and diffed by a CI job carrying
the SDL apt line. What drifts is a handful of pure values, and the suite already
links the library, already has SDL in CI and, in its SDL fontset cases, already
reads committed files. Given up is the deploy gate: `make build-ts` bundles
the committed copies as they stand, so a stale one reaches the page unless
`cabal test` ran first. Accepted because a copy goes stale through an edit
to the Haskell it is computed from, which owes `cabal test` under the standing
checks in `CLAUDE.md`; a hand edit of a copy is caught by the same case,
at the next `cabal test`, locally or in CI. Should the gate be wanted back,
it is a prerequisite target with its own shell, never an inline step
in `build-ts`: that recipe sources `~/.ghc-wasm/env`, which points
`CC`/`LD`/`AR` at wasi-sdk while leaving `cabal` and `ghc` native, so a native
build inside it links with `wasm-ld` and fails.

---

## Appendix C --- Verified non-gaps (SDL2-vs-wasm audit record)

From a full line-by-line read of `Sdl.hs` (all 922 lines) against `Wasm.hs`,
`terminal.ts`, `loader.ts` and the shared `Common.hs`/`Frontend.hs` machinery,
hunting for anything SDL2 does that the wasm stack doesn't. Beyond the gaps
in this plan (cursor, screenshots, fullscreen incl. scaling, `allFontsScale`,
multi-font, the 0.0 fixes), everything else checked out as parity
or not-applicable. Recorded with evidence so nobody re-treads it:

*Re-audited from the source alone on 2026-09-07, and the "everything else" above
does not hold: six behaviours are recorded nowhere in this plan. (1) **Half-cell
mouse x is unreachable in the browser.**
`Sdl.hs:pointTranslate (SDL.P (SDL.V2 x y)) =` divides a click's pixel x
by `halfSize`, so SDL reports an odd `PointUI` x for a click in a cell's right
half; [Wasm.hs#L177-L181][wasm-177] and [Wasm.hs#L190-L194][wasm-190] build
their point with `squareToUI` (`PointUI.hs:squareToUI :: PointSquare`),
so the browser's is always even. Unobservable while the square layer is the only
one on screen; observable the moment Phase 2 lands the mono layer, whose pitch
is exactly `halfSize`. *0.4 closes it, ruled 2026-09-14 (B.10): a canvas pointer
gives pixels.* (2) **The shift flag handed to key translation differs
under a compound modifier.**
`Sdl.hs:let sym = SDL.keyboardEventKeysym keyboardEvent` computes it
from the raw left/right shift bits, independent of Ctrl and Alt;
`Wasm.hs:key = K.keyTranslateWeb (fromJSString` passes `modifier == K.Shift`,
which is `False` whenever Ctrl or Alt is also down --- so `C-S-<key>` translates
as shifted natively and as unshifted in the browser. (3) **`--printEachScreen`
has no browser counterpart.** `sprintEachScreen` is read
at `Sdl.hs:when sprintEachScreen $ printScreen sess` and nowhere else, and 1.2
restores `Ctrl+P` alone. (4) **Game-end page lifecycle.**
`Sdl.hs:SDL.destroyRenderer srenderer` frees the fonts, destroys renderer
and window and exits; `Wasm.hs:shutdown :: IO ()` is `shutdown = return ()`,
so a finished game leaves a live-looking frozen page. The "Window-close
semantics" bullet below covers the player closing the tab, not the engine
finishing. (5) **Fixed-pitch glyph cropping and centring.**
`Sdl.hs:scaleSurfaceToTexture ::` crops an oversized glyph surface to the cell
box and centres it over a black fill, for the square layer
(`Sdl.hs:scaleSurfaceToTexture boxSize`) and the mono one
(`Sdl.hs:scaleSurfaceToTexture halfSize`) alike; the DOM grid's `1ch`/`1em`
cells do not. 2.1 declines to generalize the *proportional* crop and says so ---
this is the other site. (6) **Keycode 167**
(`Sdl.hs:on some keyboards the key below ESC is paragraph`, the paragraph key)
has no `keyTranslateWeb` clause. Trivial; no binding uses it. Five of the six
are named at the live items a session would otherwise execute without them ---
(2) at 0.1's `KeyDecision` comment and (6) at its `keyTranslateWeb` test bullet,
(1) and (5) at 2.2's pitfalls, (3) at 1.2's body --- while (4) bears on no item
and is named at G2 instead. All four gaps, (3) to (6), were scheduled later
the same day, each with a ledger row: (3) into 1.2, (4) as a new 1.6 ---
so it bears on an item now --- (5) into 2.2 and (6) into 0.1; G2 names the same
four, and the two it leaves are (1) and (2).*

- **Mouse position attached to keyboard events.** SDL sends the *current* mouse
  position with every keypress (`getAbsoluteMouseLocation`,
  `Sdl.hs:rf kdModifier`); web frontends send `PointUI 0 0`, and every KMP
  overwrites the session's `spointer`
  (`MonadClientUI.hs:{spointer = K.kmpPointer`) --- so in the browser, each
  keystroke clobbers the remembered pointer. Traced every `spointer` consumer
  (three in `HandleHumanGlobalM.hs`, three in `HandleHumanLocalM.hs`, one each
  in `HandleHelperM.hs` and `SlideshowM.hs`) --- the list is exhaustive,
  by repo-wide grep for `getsSession spointer`, which the probe
  under this bullet counts. Every one of them reads `spointer` only
  on a mouse-button release, and the triggering mouse KMP itself sets `spointer`
  correctly first, so there is no observable difference today. Six reach
  that read from the mouse bindings themselves
  (`GameDefinition/.../Content/Input.hs:, ( "LeftButtonRelease"` --- the mouse
  section plus the `"safe1".."safe6"` `CmdInternal` pseudo-keys, which
  are not typeable on any keyboard and exist only for the mouse machinery
  and macros to reference). The other two are entered from the *keyboard*
  and gate the read on a `K.LeftButtonRelease` case inside their own confirm
  loop: `pickPoint` (`HandleHumanGlobalM.hs:pickPoint ::`, reached
  from `alterDirHuman`/`closeDirHuman`, bound to `M`/`m`)
  and `displayChoiceScreen`'s `interpretKey`
  (`SlideshowM.hs:PointUI mx my <- getsSession spointer`). The invariant
  is therefore about the *read*, not about the binding. This is a booby trap:
  **any future key binding for a `*WithPointer`/`ByArea` command would behave
  differently on web.** 0.1's `InputDecision` should document the invariant.

<!-- probe: git -C .. grep -h 'getsSession spointer' -- '*.hs' | wc -l :: 8 -->
- **Key auto-repeat.** Neither side filters it:
  `Sdl.hs:handleEvent e = case SDL.eventPayload e of` accepts every `Pressed`
  event without consulting the repeat flag; the browser keydown listener
  forwards repeats likewise. Parity by mutual omission.
- **Natural-scrolling wheel direction.** SDL branches
  on `mouseWheelEventDirection`/`ScrollFlipped`
  (`Sdl.hs:= SDL.mouseWheelEventPos`); browsers normalize direction
  into `deltaY` before the event reaches script. Handled at different layers,
  same result.
- **Window-close semantics.** SDL's close/quit path deliberately exits without
  a fresh save (`forceShutdown`; the `display` comment
  at `Sdl.hs:a forced shutdown,` spells out why) --- so a closed browser tab
  losing unsaved progress is parity, not a regression. Improving on it is R1.
- **Redraw on expose/restore/resize** (`Sdl.hs:SDL.WindowRestoredEvent{} ->`):
  repainting after occlusion is the DOM's job in a browser; nothing to port.
  Same for the texture-invalidation workarounds.
- **VSync and frame pacing.** SDL uses `AcceleratedVSyncRenderer`
  (non-benchmark) plus its own poll loop; the web side batches through
  `requestAnimationFrame` --- the browser analogue. The `--maxFps` logic itself
  lives in shared code (`Frontend.hs:fromMaybe defaultMaxFps`), not in any
  frontend.
- **HighDPI.** `windowHighDPI` (`Sdl.hs:, SDL.windowHighDPI = True`) has
  no DOM-grid counterpart to port (browser text is DPR-aware natively);
  the canvas-DPR concern is a 2.2 pitfall item. *That pitfall left 2.2,
  with its own backing store, in its re-cut for B.10 on 2026-10-20; the canvas
  is 0.4's since `9eb0c2c3c`, its backing store in device pixels and repainted
  when `devicePixelRatio` changes, and 2.5 keeps the HiDPI and browser-zoom spot
  checks.*
- **PrintScreen key (as opposed to `C-P`).** SDL maps the key
  (`Sdl.hs:KeycodePrintScreen -> K.PrintScreen`) but no binding uses
  `K.PrintScreen` bare
  (`GameDefinition/.../Content/Input.hs:("C-P", ([CmdMeta],` binds only `C-P`),
  so `keyTranslateWeb`'s lack of a `"PrintScreen"` case changes nothing.
- **Key-translation coverage.** `keyTranslateWeb` (`Key.hs:keyTranslateWeb ::`)
  covers the same command-relevant key set as SDL's `keyTranslate`
  (`Sdl.hs:keyTranslate :: Bool`) including F-keys, navigation, KP digits
  with the shift convention, and dead keys; the residual differences
  are the layout/NumLock quirks declared out of scope.
- **Frame-delivery blocking.** SDL's `display` blocks until the frame is drawn
  (`sframeQueue`/`sframeDrawn` handshake); wasm's returns after the synchronous
  buffer snapshot with painting deferred to rAF. A timing nuance inside the same
  shared `fshowNow` protocol, not a feature gap; if it ever matters, it will
  show up in R5's measurements.

The GHCJS files (`Dom.hs`, `JSFile.hs`, the `impl(ghcjs)` cabal stanzas,
and the GHCJS-era page) were likewise audited line-by-line against SDL2: every
difference is a browser-platform limitation, a DOM-instead-of-textures
mechanism, a JS-performance workaround (the disabled autosave,
the `GHCJS_GC_INTERVAL`/`-dedupe` knobs), or plain missing effort (the known
parity gaps) --- with faithful adaptations elsewhere (`JSFile.hs` preserves
`HSFile.hs`'s exact save envelope with lz-string in place of zlib;
the browser-zoom passthrough is deliberate). The GHCJS features that exceeded
SDL2 (runtime zoom, free DOM text affordances) are handled by 1.5, 0.1's
passthrough, and B.1.

[terminal-133]: https://github.com/LambdaHack/LambdaHack/blob/b2f76b8fec66713f8fa4b0c773a9d9c5071d9b35/ts-src/src/terminal.ts#L133-L134
[wasm-177]: https://github.com/LambdaHack/LambdaHack/blob/b2f76b8fec66713f8fa4b0c773a9d9c5071d9b35/engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs#L177-L181
[wasm-190]: https://github.com/LambdaHack/LambdaHack/blob/b2f76b8fec66713f8fa4b0c773a9d9c5071d9b35/engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs#L190-L194
[sdl-482]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L482
[sdl-493]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L493
[sdl-524]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L524
[sdl-526]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L526
[sdl-530]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L530
[sdl-531]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L531
[sdl-555]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L555
[sdl-568]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L568
[sdl-571]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L571
[sdl-574]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L574
[sdl-607]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L607
[sdl-648]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L648
[sdl-654]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L654
[sdl-658]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L658
[sdl-664]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L664
[sdl-666]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L666
[sdl-668]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L668
[sdl-674]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L674
[sdl-675]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L675
[sdl-678]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L678
[sdl-681]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L681
[sdl-684]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L684
[sdl-709]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L709
[sdl-876]: https://github.com/LambdaHack/LambdaHack/blob/5c58af62ec9d875dfcb051ba5fade405e5b36f8d/engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs#L876
