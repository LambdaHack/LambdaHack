#!/usr/bin/env python3
"""Check a campaign plan against itself, and the two plans against each other.

Usage: python3 tools/check-plan-crossrefs.py [DOC ...] [--allowlist FILE] [-v]
No DOC reads every document in the configuration table below, jointly;
one DOC reads that document alone; several read them jointly. Runs from
anywhere in the repository.

A plan here is a document of items, each closing with the same execution
block (**Split**, **Owns**, **Done**, **Hands back**, **Decide first**) and
each with a ledger row whose `depends on` cell names the items it waits
for. Two carry that grammar: `docs/wasm-frontend-work-list.md`, thirty-odd
items whose designs sit in `docs/wasm-frontend-unified-plan.md` since
2026-09-14, and `docs/leader-desync-migration.md`, whose rows took the block on
2026-09-04 so that the same executor is handed both campaigns' items the
same way. Between the items sits a graph nobody writes down as a graph:
which item writes which file, which item
creates the artifact another consumes, which `tools/doc-refs-allow.txt`
entry is whose to delete. The plan refuses a central table of that graph,
with reasons, so the graph is maintained by hand across the items and
drifts: a review campaign put a third to two thirds of its findings on
exactly this class, every one of them green under the four checkers that
read the document against the repository and never against itself. This
one derives the graph at run time from the fields and stores nothing,
which is the alternative that ruling did not have. The joint run derives
it over the union of both documents' items, which is the only run that can
see an edge between the campaigns: an item writing a file the other
campaign's item creates, or naming the other plan without the `docs` gate.

What it asserts, in the order the campaign measured their yield:

  A1  every artifact entry in the allowlist block the plan owns is claimed
      by exactly one item, and that item's **Owns** names the allowlist.
      An item claims an entry by naming its file in a **Split** or **Owns**
      sentence that speaks of the entry or its deletion, or by "the new
      `file`"; a sentence that speaks of the deletion without naming a file
      ("this item's two entries") claims every entry the item's **Owns**
      matches that no item names outright. An entry nobody claims is
      reported with the items that write its file, since the item that
      proposed an artifact and never spoke of its entry is the usual cause.
      A row that has landed or retired claims nothing, its text being
      history, so an entry only such a row names is claimed by no item.
      A document configured with no allowlist block has nothing here to
      read, and the run says so rather than passing an empty block.
  A2  retired on 2026-09-14 together with the locks it checked: the
      wasm plan runs one item at a time in one checkout, so no file is
      contended and no **Owns** owes a claimant list. It asserted that
      the contention graph was symmetric, and drew sixty findings about
      the plan's own bookkeeping and none about a landing. The number
      stays so the others keep theirs; the pointman work list still
      carries claimant lists, checked by nobody.
  A3  an item that writes a file another item creates reaches that item
      through the ledger's `depends on` cells, transitively: waiting on an
      item that waits on the creator is waiting on the creator. A row that
      has landed or retired writes nothing still to come, so A3 skips it,
      as a writer and as a creator.
  A4  an item whose **Owns** names a plan or the allowlist carries the
      `docs` gate in **Done**, which is what re-runs the document passes.
  A5  a numeral agrees with the list beside it, in the two narrow shapes
      the campaign met: "<number> <noun> --- `a`, `b` and `c`", and a
      **Split** opening "<number> commits." whose commits are then
      enumerated "(1) ... (2) ...". A list that trails off into prose
      ("and the crosshair module's") is left alone: a false positive here
      invites deleting the numeral, which sometimes loses a claim.
  A0  the grammar itself: every ledger row has an item body carrying the
      four labels, and every body has a row. The plan says a missing label
      is a defect rather than a shrug, so it is one here.

An item writes the files the first sentence of its **Owns** names, by path
or by a citation into one, the work list keeping that sentence the
exhaustive list. The rest of the field names a file to disclaim it or to
point at another item's list as often as to claim it, so it is read, as
**Split** is, for an entry's claim, a file called new and A5's counts,
and never for the files the item writes.

Read the unwrapped form (`wrap80 --unwrap`) and nothing else: both plans are
kept at 80 columns, an **Owns** field spans a dozen lines, and no
line-oriented parse of it is sound. Without wrap80 the run is BLOCKED at 2
rather than degraded, unlike check-doc-refs, whose spans lose little when
read wrapped.

Where the verdict and the document disagree the document may be right: it
is a live specification under review. Report, and never reword the
document to make this pass; a legitimately absent name is an allowlist
entry with its reason, and a wrong verdict is this checker's to fix.

Exit 0 clean, 1 with findings, 2 when the run did not happen: no document,
no allowlist, either not UTF-8, documents in two repositories, no wrap80,
a document that fits none of the grammars below
or two of them, two documents sharing an item id, or a document in which
its grammar found no ledger and no item --- each a retargeting error rather
than a clean plan.

Non-vacuity: `--self-test` runs the engine over a scratch plan in each
grammar and a scratch allowlist in a temporary directory, each document
alone and then both jointly, whose expected findings sit beside the
configuration below, one row per assertion and controls for the shapes
each must leave alone; that the self-test bites is `tools/mutants.py`'s to
show. The historical corpus is the other proof, and `tools/defects.json`
carries it as controls: the plan at `91f28c8f3`, before the campaign's
fixes, must draw the findings the campaign found by hand, and the joint
run at the migration document's first commit in this grammar must draw
the cross-campaign findings that commit left standing on purpose. This
checker encodes two documents' field grammars, so it stays in this
repository rather than among ~/slop's shared tools."""

import contextlib
import functools
import io
import os
import re
import subprocess
import sys
import tempfile

# --- per-document configuration ---------------------------------------
# One entry per document that carries the execution-block grammar.
# Retargeting this checker to another plan should mean adding an entry
# and a scratch document to the self-test, and nothing else. A document
# is matched to its entry by path, or, for a copy under another name (the
# self-test's, a defect record's), by which entry's openers find an item.
# The gate whose presence A4 asserts, as **Done** spells it, and the
# execution block's labels: the four every item carries, and the one that
# is present only where an item is several commits.
DOCS_GATE = "docs"
LABELS = ("Owns", "Done", "Hands back", "Decide first")
SPLIT = "Split"
FIELD_RE = re.compile(r"^\*\*(" + "|".join((SPLIT,) + LABELS) + r")\*\*"
                      r" --- ?(.*)$")
# The wasm plan's practices open on a bold paragraph carrying the practice's
# title, keyed here by the name its ledger row uses. Each also lists how prose
# refers to it, since "the capability-constants practice" is a mention. The
# pointman document has none.
PRACTICES = {
    "capability constants": ("Capability constants",
                             ("capability constants",
                              "capability-constants")),
    "sum-typed selection": ("Sum-typed frontend selection",
                            ("sum-typed selection",
                             "sum-typed frontend selection")),
    "RawFrontend contract": ("The RawFrontend contract",
                             ("RawFrontend contract", "contract item",
                              "contract harness")),
    "determinism goldens": ("Determinism goldens", ("determinism goldens",)),
    "frontend CI smokes": ("A CI smoke for every shipped frontend",
                           ("frontend CI smokes", "CI smokes",
                            "frontend smokes")),
    "explicit widths": ("Frontends pass widths explicitly",
                        ("explicit widths", "explicit-widths")),
    "functional core": ("Functional core, imperative shell",
                        ("functional core", "functional-core")),
}
GRAMMARS = [
    {
        "doc": "docs/wasm-frontend-work-list.md",
        # The allowlist is grouped by comment block; only the block whose
        # comment carries one of these phrases holds artifacts the campaign
        # proposes --- the second is the wording a snapshot before the
        # 2026-09-14 split carries, which a defect control still replays. The
        # other blocks are phantoms, foreign repositories, superseded documents
        # and toolchain output, and belong to no item.
        "allow_file": "tools/doc-refs-allow.txt",
        "allow_block": ("Artifacts the wasm campaign proposes",
                        "Artifacts docs/wasm-frontend-unified-plan.md proposes"),
        # The permanent record beside it, whose outcome lines a landing writes:
        # naming it asks for the docs gate as naming this file does.
        "record": "docs/wasm-frontend-unified-plan.md",
        # How an item opens: a `### N.N` heading or a bold `**RN --- ...**`
        # paragraph, group 1 being the id; the practices open on their titles.
        "openers": (re.compile(r"^### (\d\.\d) "),
                    re.compile(r"^\*\*(R\d) --- ")),
        "practices": PRACTICES,
    },
    {
        "doc": "docs/leader-desync-migration.md",
        # The pointman campaign proposes no artifact: every test module of its
        # sec. 05 has landed. A1 has nothing to read, and says so.
        "allow_file": None,
        "allow_block": None,
        # Its items open on `### C1 ---`, `### PR 0 ---`, `### 04.1 ---` and
        # `### 05 ---` headings; `PR N` carries a space, and `04.1` cannot be
        # read as the plan's `4.1` since a digit precedes it.
        "record": None,
        "openers": (re.compile(r"^### (C\d|PR \d|04\.\d|05) "),),
        "practices": {},
    },
]
# **Owns** names the document it sits in by these phrases as often as by path.
DOC_ALIASES = ("this document", "this plan")
# Where the first sentence of **Owns** ends: at a full stop before a capital, a
# backtick or bold.
FIRST_SENTENCE_RE = re.compile(r"(?<=\.)\s+(?=[A-Z`*])")
# The words a sentence uses when it speaks of an allowlist entry, the negations
# that make such a sentence say there is none to delete, and the shape that
# hands the deletion to another item ("rides 0.3's export half").
CLAIM_RE = re.compile(r"\b(?:entry|entries|deletion)\b")
NEG_CLAIM_RE = re.compile(r"\b(?:no|nothing|neither|none)\b", re.I)
ATTRIB_RE = re.compile(r"\b(?:rides?|is|are) (ID)'s\b")
NEW_RE = re.compile(r"\bnew `([^`\n]+)`")
# --- self-test rows ------------------------------------------------------
# A scratch plan in each grammar, and the findings each must draw alone
# and both must draw jointly. The controls matter as much as the failures.
# In the first: 1.1 writes a file 0.1 creates and is silent because its
# row waits on 0.2, which waits on 0.1; 1.2 names 0.2's new module after
# its first sentence, so it does not write it; the bare `cabal` two items
# name is a tool, not a file they write; the open list "and the module's"
# is not counted.
# `dropped.ts` is out of scope, in a block the phrase above does not head.
# In the second: C1, citing it by a text anchor, and C3 write the `Pure.hs`
# 04.1 creates and reach it through no cell, where 04.3 reaches it through 04.2
# and 04.1 reaches C2, which creates `Made.hs`, only through the middle of a
# `C1--C3` range --- its ends being plain mentions, the range's expansion is
# what the middle proves. Jointly: 04.3 names the first document without the
# `docs` gate, which the second alone cannot see.
SELF_TEST_ALLOW = """\
# Scratch allowlist for the self-test.

# Phantoms, out of scope.
dropped.ts

# Artifacts the wasm campaign proposes but has not built yet.
new-core.ts
new-core.test.ts
test/NewUnitTests.hs
test/RUnitTests.hs
twice.ts
orphan.ts
handed.ts
stranded.ts
make gen-scratch
"""
SELF_TEST_DOC = """\
# Scratch plan

## Ledger --- every item's state

| sec. | delivers | size | depends on | state |
|---|---|---|---|---|
| 0.1 | a | tiny | --- | open |
| 0.2 | b | tiny | 0.1 | open |
| 1.1 | c | tiny | 0.2 | open |
| 1.2 | d | tiny | --- | open |
| 1.3 | g | tiny | --- | landed -- 0123abc |
| R1 | e | tiny | 1.1 | open |
| determinism goldens | f | tiny | 0.1--0.2 | open |

## Handing an item to a session

**Owns** --- here this is prose, not a field, since no item is open.

## Phase 0

### 0.1 First

Body naming `foo.ts` as evidence, which is not an **Owns**.

**Split** --- two commits. (1) `ts-src/src/new-core.ts` and `ts-src/src/new-core.test.ts`, with the deletion of both `tools/doc-refs-allow.txt` entries. (2) the landing, and the `make gen-scratch` and `handed.ts` entries.

**Owns** --- the new `ts-src/src/new-core.ts`, `ts-src/src/new-core.test.ts`, `ts-src/src/loader.ts`, `ts-src/src/handed.ts`, `Makefile`, `tools/doc-refs-allow.txt` and this document. **The claimant list for `loader.ts` lives here**: 0.1, 0.2, 1.1 and R1, one holder at a time, as for `ts-src/src/twice.ts` at 0.2.

**Done** --- `ts`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 0.2 Second

**Owns** --- `ts-src/src/loader.ts`, `ts-src/src/terminal.ts`, `ts-src/src/foo.ts`, `ts-src/src/twice.ts`, `engine-src/X/Shared.hs`, the new `test/NewUnitTests.hs`, built with `cabal`, `tools/doc-refs-allow.txt`, whose `test/NewUnitTests.hs` and `twice.ts` entries this commit deletes, and `docs/wasm-frontend-work-list.md`. Not concurrent with 1.1 on `terminal.ts` and `foo.ts`; `loader.ts`'s claimant list is 0.1's.

**Done** --- `native`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

## Phase 1

### 1.1 Third

**Split** --- two files --- `ts-src/src/new-core.ts` and the module's --- rewritten; neither commit deletes a `tools/doc-refs-allow.txt` entry.

**Owns** --- `ts-src/src/new-core.ts`, `ts-src/src/terminal.ts`, `ts-src/src/foo.ts` and `ts-src/src/loader.ts`, the claimant list for which is at 0.1; `terminal.ts` and `foo.ts` are 0.2's as well.

**Done** --- `ts`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 1.2 Fourth

**Split** --- three commits. (1) `ts-src/src/new-core.ts` gains a case. (2) the landing, with the deletion of three entries --- `twice.ts` and `orphan-not.ts` --- from `tools/doc-refs-allow.txt`.

**Owns** --- `ts-src/src/new-core.ts`, `ts-src/src/terminal.ts`, `ts-src/src/twice.ts` and `tools/doc-refs-allow.txt`, built with `cabal`. `ts-src/src/foo.ts` is deliberately not here, and not `test/NewUnitTests.hs` either, though the evidence sits at (`test/NewUnitTests.hs:1`).

**Done** --- `ts`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 1.3 Landed

**Split** --- a single commit, deleting the `handed.ts` and `stranded.ts` entries from `tools/doc-refs-allow.txt`.

**Owns** --- `ts-src/src/handed.ts`, `ts-src/src/stranded.ts`, `tools/doc-refs-allow.txt` and this document.

**Done** --- `ts`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

## Related goals

**R1 --- Fifth.** Body.

**Owns** --- the new `test/RUnitTests.hs`, `ts-src/src/loader.ts` per the claimant list at 0.1, and this document. Not `ts-src/src/foo.ts`, whose claimants are 0.2 and 1.1.

**Done** --- `native`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

## Practices

**Determinism goldens --- scratch.** Body.

**Owns** --- nothing.

**Done** --- none.

**Decide first** --- nothing.
"""
SELF_TEST_FAIL = [
    "A0 determinism goldens --- no **Hands back** label",
    "A1 `orphan.ts` --- claimed by no item",
    # landed 1.3 names it, and a landed row claims nothing
    "A1 `stranded.ts` --- claimed by no item",
    "A1 `twice.ts` --- claimed by 0.2 and 1.2",
    "A1 `test/RUnitTests.hs` --- claimed by R1, whose **Owns** does not name"
    " `tools/doc-refs-allow.txt`",
    "A3 1.2 --- writes `new-core.ts`, created by 0.1",
    "A4 1.2 --- **Owns** names `tools/doc-refs-allow.txt` and **Done**"
    " lacks `docs`",
    "A5 1.2 --- says three entries and lists 2",
    "A5 1.2 --- **Split** says three commits and enumerates 2",
]
# Names that must appear in no FAIL line: the controls.
SELF_TEST_QUIET = ["loader.ts", "foo.ts", "dropped.ts", "new-core.test.ts",
                   # live 0.1 claims it; landed 1.3's claim is history, and so
                   # is its writing a file 0.1 now creates
                   "handed.ts", "A3 1.3",
                   "gen-scratch", "A3 1.1", "A4 0.1", "A4 0.2",
                   "A4 R1", "A5 0.1", "A5 1.1",
                   # 1.2's disclaimed mentions of 0.2's new module, which it
                   # does not depend on: read as ownership, A3 draws them.
                   "writes `NewUnitTests.hs`"]
SELF_TEST_DOC_2 = """\
# Scratch work list

## 00 -- Status

| row | delivers | size | depends on | state |
|---|---|---|---|---|
| C1 | a | tiny | --- | not applied |
| C2 | b | tiny | C1 | not applied |
| C3 | c | tiny | --- | not applied |
| 04.1 | d | tiny | C1--C3 | not applied |
| 04.2 | e | tiny | 04.1 | not applied |
| 04.3 | f | tiny | 04.2 | not applied |
| 05 | g | --- | --- | landed |

### C1 --- first

**Owns** --- `engine-src/X/Shared.hs`, `engine-src/X/Own.hs`, the case at `test/Pure.hs:pureCase ::` and this document. The wasm plan's 0.2 writes `Shared.hs` as well.

**Done** --- `native`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### C2 --- second

**Split** --- two commits. (1) the code.

**Owns** --- `engine-src/X/Own.hs`, the claimant list for which is at C3, the new `engine-src/X/Made.hs`, which 04.1 grows, and this document.

**Done** --- `native`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### C3 --- third

**Owns** --- `engine-src/X/Own.hs`, `engine-src/X/Old.hs` and `test/Pure.hs`, whose claimant list is at 04.1. **The claimant list for `Own.hs` lives here**: C1, C2 and C3.

**Done** --- `native`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

## 04 -- Steps

### 04.1 --- extract

**Owns** --- the new `test/Pure.hs`, `engine-src/X/Made.hs`, which C2 creates, and this document. **The claimant list for `Pure.hs` lives here**: 04.1, 04.3 and C3.

**Done** --- `native`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 04.2 --- name

**Split** --- ten commits. (1) a. (2) b. (3) c. (4) d. (5) e. (6) f. (7) g. (8) h. (9) i. (10) j.

**Owns** --- nothing.

**Done** --- `native`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 04.3 --- audit

**Owns** --- `test/Pure.hs` (list at 04.1), `docs/wasm-frontend-work-list.md` and this document.

**Done** --- `native`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 05 --- landed

**Owns** --- the new `engine-src/X/Old.hs`.

**Done** --- landed.

**Decide first** --- nothing.
"""
SELF_TEST_FAIL_2 = [
    "A0 05 --- no **Hands back** label",
    "A3 C1 --- writes `Pure.hs`, created by 04.1",
    "A3 C3 --- writes `Pure.hs`, created by 04.1",
    "A4 C2 --- **Owns** names `docs/leader-desync-migration.md` and **Done**"
    " lacks `docs`",
    "A4 04.3 --- **Owns** names `docs/leader-desync-migration.md` and"
    " **Done** lacks `docs`",
    "A5 C2 --- **Split** says two commits and enumerates 1",
]
# The second document's controls, among them C3 writing the file landed 05
# created and 04.2's ten commits, enumerated to (10).
SELF_TEST_QUIET_2 = ["Made.hs", "A3 04.1", "A3 04.3", "A4 C1", "A4 C3",
                     "A4 04.1", "Old.hs", "A5 04.2"]
# Quiet in either document alone, loud jointly: the other document's name in an
# **Owns**; `Shared.hs`, the cross-campaign edge A2 drew, is quiet everywhere
# now.
SELF_TEST_QUIET_ALONE = ["Shared.hs", "wasm-frontend-work-list.md"]
# What the second document's run must print in place of A1.
SELF_TEST_NOTE_2 = "A1 not run over "
# The joint run draws both lists and these, which neither draws alone.
SELF_TEST_FAIL_JOINT = [
    "A4 04.3 --- **Owns** names `docs/wasm-frontend-work-list.md` and"
    " **Done** lacks `docs`",
]
SELF_TEST_QUIET_JOINT = []
# --- end per-document configuration -------------------------------------

PATH_EXT = ("hs", "ts", "mjs", "py", "cabal", "html", "md", "yaml", "yml",
            "json", "sh", "txt", "c", "h", "js")
TICK_RE = re.compile(r"`([^`\n]+)`")
CITE_RE = re.compile(r":\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*$")
NUMBERS = {"two": 2, "three": 3, "four": 4, "five": 5, "six": 6,
           "seven": 7, "eight": 8, "nine": 9, "ten": 10, "eleven": 11,
           "twelve": 12}
NUMBER_LIST_RE = re.compile(
    r"\b(" + "|".join(NUMBERS) + r")\b"
    r"((?: (?:`[^`\n]+`|[\w'-]+)){0,3}) (entries|files|commits|modules|"
    r"targets|names|claimants|jobs|halves|clauses|tests|cases|sites|lines)"
    r"(?: --- |: )((?:`[^`\n]+`(?:'s)?(?:, and |, or |, |"
    r" and | or ))*`[^`\n]+`(?:'s)?)(.{0,6})")
COMMITS_RE = re.compile(r"\b(" + "|".join(NUMBERS) + r") commits\.")
MARKER_RE = re.compile(r"\((\d+)\)")


class Blocked(Exception):
    """The run did not happen; the message says why."""


def unwrapped(text):
    """The document one paragraph per line, or None without wrap80."""
    try:
        return subprocess.run(["wrap80", "--unwrap"], input=text, text=True,
                              capture_output=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError):
        return None


def toplevel(where="."):
    """The toplevel of the repository holding the directory WHERE, or ""
    outside one or with no git to ask."""
    # answered dropped-status: an empty top is the failure, and each caller
    # tests it
    try:
        return subprocess.run(["git", "-C", where, "rev-parse",
                               "--show-toplevel"],
                              capture_output=True, text=True).stdout.strip()
    except OSError:
        return ""


def chdir_root(paths):
    """Run from the repository root whatever the cwd -- the configuration's
    paths are root-relative -- and return PATHS rebased to it. The root is
    the repository holding the documents named, else the cwd's, and
    documents in two repositories exit 2, a root being one configuration.
    Outside a repository, or with no git to ask, nothing moves. The
    behaviour of ~/slop/tools/common.py's chdir_root with its OUTSIDE,
    copied (check-plan-crossrefs-08 to -10)."""
    tops = {t for t in (toplevel(os.path.dirname(os.path.abspath(p)))
                        for p in paths) if t}
    if len(tops) > 1:
        print("the documents named sit in %d repositories (%s), and a run"
              " reads one repository's settings: check each apart"
              % (len(tops), ", ".join(sorted(tops))), file=sys.stderr)
        sys.exit(2)
    top = tops.pop() if tops else toplevel()
    if not top:
        return paths
    paths = [os.path.relpath(os.path.abspath(p), top) for p in paths]
    os.chdir(top)
    return paths


def ambiguous_basenames():
    """Basenames git tracks more than once, which stay qualified as keys.
    Outside a repository, or with no git to ask, there are none, and keys
    are basenames."""
    try:
        p = subprocess.run(["git", "ls-files"], capture_output=True,
                           text=True)
    except OSError:
        return set()
    if p.returncode != 0:
        return set()
    seen, dup = set(), set()
    for path in p.stdout.split("\n"):
        b = os.path.basename(path)
        if b in seen:
            dup.add(b)
        seen.add(b)
    return dup


def is_file(base):
    """Whether a path's last component names a file: a Makefile or a name
    with a known extension. `cabal` and `ts` are a tool and a gate, not
    files, though both are extensions."""
    return base == "Makefile" or \
        ("." in base and base.rsplit(".", 1)[-1] in PATH_EXT)


def path_key(token, ambiguous):
    """The file a backticked token names, as the key the graph is built on:
    the basename, or the last two components where the basename is one the
    repository carries twice. A citation, `File.hs:` and a line number or
    a run of its text, names its file. None for a token that is not a
    path: a path ends in `/` or names a file."""
    t = token.strip()
    head = t.split(":", 1)[0]
    if head != t and is_file(head.rsplit("/", 1)[-1]):
        t = head
    if not t or t[0] in "~$/+-" or t.startswith(("../", "http")):
        return None
    if any(c in t for c in " *{<>()=\"'") or ".." in t:
        return None
    base = t.rstrip("/").rsplit("/", 1)[-1]
    if not base or base.startswith(".") or \
            not (t.endswith("/") or is_file(base)):
        return None
    if t.endswith("/"):
        return t
    if base in ambiguous:
        parts = t.split("/")
        return "/".join(parts[-2:])
    return base


def sentences(text):
    return re.split(r"(?<=[.;])\s+", text)


def entry_matches(token, entry):
    """Whether a backticked token names an allowlist entry: the entry
    itself, the same path by suffix or basename, a bare target for a `make`
    entry, or a stem ("url-options-core" for `url-options-core.ts`)."""
    t = CITE_RE.sub("", token.strip())
    if entry.startswith("make "):
        return t in (entry, entry[5:])
    if "*" in entry:
        return False
    if t == entry or t.endswith("/" + entry) or entry.endswith("/" + t):
        return True
    if entry.endswith("/"):
        return t.startswith(entry) or t + "/" == entry
    tb, eb = t.rsplit("/", 1)[-1], entry.rsplit("/", 1)[-1]
    if tb == eb and (t.count("/") == 0 or entry.count("/") == 0):
        return True
    return "/" not in t and "." not in t and "-" in t and eb.startswith(t + ".")


def opens(grammar, line):
    """The id a line opens under GRAMMAR, or None."""
    for r in grammar["openers"]:
        m = r.match(line)
        if m:
            return m.group(1)
    if line.startswith("**"):
        for name, (title, _) in grammar["practices"].items():
            if line.startswith("**" + title):
                return name
    return None


def is_configured(path, grammar):
    """Whether PATH is GRAMMAR's own document, by root-relative name or by
    an absolute path ending in it, as a defect record hands it over."""
    p, d = os.path.normpath(path), os.path.normpath(grammar["doc"])
    return p == d or p.endswith(os.sep + d)


def grammar_for(path, lines):
    """The configuration entry a document is read under: the one whose path
    it is, or --- for a copy under another name --- the one whose openers
    find an item in it. None where none does or two do."""
    for g in GRAMMARS:
        if is_configured(path, g):
            return g
    fits = [g for g in GRAMMARS
            if any(opens(g, line) is not None for line in lines)]
    return fits[0] if len(fits) == 1 else None


class Doc:
    """One document under its grammar: its ledger, its items, its entries."""

    def __init__(self, grammar, path, text, allow_text, ambiguous, findings):
        self.g = grammar
        self.path = path      # the configured name, or a copy's as given
        self.lines = text.split("\n")
        self.ambiguous = ambiguous
        self.findings = findings
        self.ledger = {}      # id -> depends-on cell
        self.state = {}       # id -> state cell
        self.order = []       # ledger ids in order
        self.items = {}       # id -> {"line": n, "fields": {label: text}}
        self.entries = (self.in_scope_entries(allow_text)
                        if grammar["allow_block"] else [])
        self.doc_key = path_key(grammar["doc"], ambiguous)
        self.record_key = (path_key(grammar["record"], ambiguous)
                           if grammar["record"] else None)
        self.allow_key = (path_key(grammar["allow_file"], ambiguous)
                          if grammar["allow_file"] else None)
        self.parse_ledger()
        self.parse_items()

    def in_scope_entries(self, allow_text):
        entries, comment, in_scope = [], [], False
        for line in allow_text.split("\n") + [""]:
            if not line.strip():
                comment, in_scope = [], False
                continue
            if line.startswith("#"):
                comment.append(line)
                if any(p in " ".join(comment) for p in self.g["allow_block"]):
                    in_scope = True
                continue
            body = line.split("#", 1)[0].strip()
            if body and in_scope:
                entries.append(body)
        return entries

    def parse_ledger(self):
        """Every five-cell table row whose fourth cell is not the header's
        `depends on`; both plans' ledgers are the only such tables."""
        for line in self.lines:
            if not line.startswith("| "):
                continue
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) != 5 or set(cells[0]) <= set("-") or \
                    cells[3] == "depends on":
                continue
            iid = cells[0]
            if iid in self.ledger:
                self.findings.append(f"A0 {iid} --- two ledger rows")
            self.ledger[iid] = cells[3]
            self.state[iid] = cells[4]
            self.order.append(iid)

    def parse_items(self):
        cur = None
        for n, line in enumerate(self.lines, 1):
            if line.startswith("## "):
                cur = None
                continue
            m = FIELD_RE.match(line)
            if m:
                if cur is not None:
                    if m.group(1) in cur["fields"]:
                        self.findings.append(
                            f"A0 {cur['id']} --- two **{m.group(1)}** labels")
                    cur["fields"][m.group(1)] = m.group(2)
                continue
            iid = opens(self.g, line)
            if iid is None:
                continue
            if iid in self.items:
                self.findings.append(f"A0 {iid} --- opens twice, line {n}")
            cur = {"id": iid, "line": n, "fields": {}, "doc": self}
            self.items[iid] = cur
        for iid in self.order:
            if iid not in self.items:
                self.findings.append(f"A0 {iid} --- ledger row with no item")
                continue
            for label in LABELS:
                if label not in self.items[iid]["fields"]:
                    self.findings.append(f"A0 {iid} --- no **{label}** label")
        for iid in self.items:
            if iid not in self.ledger:
                self.findings.append(f"A0 {iid} --- item with no ledger row")


class Plan:
    """The documents read together: one graph over the union of their
    items, each item keeping the document it came from."""

    def __init__(self, docs, findings):
        self.docs = docs
        self.findings = findings
        self.items = {}
        self.order = []
        self.row_doc = {}     # ledger id -> its document, item or not
        self.practices = {}
        seen = set()
        for d in docs:
            ids = set(d.order) | set(d.items)
            shared = ids & seen
            if shared:
                raise Blocked(f"item id {min(shared)} appears in two"
                              " documents; the joint run has no way to tell"
                              " them apart")
            seen |= ids
            self.order += d.order
            self.row_doc.update(dict.fromkeys(d.order, d))
            self.items.update(d.items)
            self.practices.update(d.g["practices"])
        ids = [i for i in self.order if i not in self.practices]
        aliases = [a for _, names in self.practices.values() for a in names]
        id_alt = "|".join(map(re.escape, ids))
        self.id_re = re.compile(r"(?<![\w.])("
                                + "|".join(map(re.escape, ids + aliases))
                                + r")(?!\d|\.\d)", re.I)
        self.range_re = re.compile(f"({id_alt})--({id_alt})")
        self.attrib_re = re.compile(
            ATTRIB_RE.pattern.replace("(ID)", f"({id_alt})"))
        # The documents and allowlists whose naming asks for the gate.
        self.gated = [(k, name) for d in docs
                      for k, name in ((d.doc_key, d.g["doc"]),
                                      (d.allow_key, d.g["allow_file"]),
                                      (d.record_key, d.g["record"]))
                      if k]

    # --- the derived graph -------------------------------------------

    def field(self, iid, label):
        return self.items.get(iid, {}).get("fields", {}).get(label, "")

    def doc_of(self, iid):
        """The document of IID's item, else of its ledger row, a row with
        no item being A0's to report; None for a name in a depends-on cell
        that has neither."""
        it = self.items.get(iid)
        return it["doc"] if it else self.row_doc.get(iid)

    def historical(self, iid):
        """Whether IID's row has landed or retired, its text being history."""
        return self.doc_of(iid).state.get(iid, "").startswith(
            ("landed", "retired"))

    def listed(self, iid):
        """The first sentence of IID's **Owns**, the exhaustive list of
        what it writes."""
        return FIRST_SENTENCE_RE.split(self.field(iid, "Owns"), 1)[0]

    # The fields stay as read, so what is derived from them is computed once a
    # run.
    @functools.cache
    def owns(self, iid):
        """The file keys IID's list names, its document by an alias too."""
        text, doc = self.listed(iid), self.doc_of(iid)
        if text.strip().lower().startswith("nothing"):
            return set()
        keys = {k for m in TICK_RE.finditer(text)
                if (k := path_key(m.group(1), doc.ambiguous))}
        if any(a in text for a in DOC_ALIASES):
            keys.add(doc.doc_key)
        return keys

    def canonical(self, mention):
        low = mention.lower()
        for name, (_, aliases) in self.practices.items():
            if low in (a.lower() for a in aliases):
                return name
        return mention

    @functools.cache
    def deps(self, iid):
        """The items a ledger cell names, a range `A--B` standing for every
        row of that document from A to B."""
        doc = self.doc_of(iid)
        if doc is None:
            return set()
        cell = doc.ledger.get(iid, "")
        out = set()
        for a, b in self.range_re.findall(cell):
            if a in doc.order and b in doc.order:
                out.update(doc.order[doc.order.index(a):
                                     doc.order.index(b) + 1])
        for m in self.id_re.findall(cell):
            out.add(self.canonical(m))
        out.discard(iid)
        return out

    def closure(self, iid):
        seen, todo = set(), [iid]
        while todo:
            x = todo.pop()
            for d in self.deps(x):
                if d not in seen:
                    seen.add(d)
                    todo.append(d)
        return seen

    def artifact_tokens(self, sentence, entries):
        """(position, token, matching entries) for each backticked token of
        a sentence that names an in-scope entry."""
        out = []
        for m in TICK_RE.finditer(sentence):
            hits = [e for e in entries if entry_matches(m.group(1), e)]
            if hits:
                out.append((m.start(), m.group(1), hits))
        return out

    @functools.cache
    def claims(self, doc):
        """entry -> set of claimants, over DOC's entries and DOC's items:
        explicit claims first and the anaphoric ones ("its two entries")
        only where nothing is explicit."""
        explicit = {e: set() for e in doc.entries}
        anaphoric = {e: set() for e in doc.entries}
        for iid in doc.items:
            if self.historical(iid):
                continue
            text = self.field(iid, SPLIT) + "\n" + self.field(iid, "Owns")
            for m in NEW_RE.finditer(text):
                for e in doc.entries:
                    if entry_matches(m.group(1), e):
                        explicit[e].add(iid)
            for s in sentences(text):
                if not CLAIM_RE.search(s):
                    continue
                toks = self.artifact_tokens(s, doc.entries)
                cut = toks[0][0] if toks else len(s)
                if NEG_CLAIM_RE.search(s[:cut]):
                    continue
                a = self.attrib_re.search(s)
                who = self.canonical(a.group(1)) if a else iid
                if toks:
                    written = self.owns(who) if who in self.items else set()
                    for _, _, hits in toks:
                        for e in hits:
                            if e.startswith("make ") or any(
                                    entry_matches(k, e) for k in written):
                                explicit[e].add(who)
                elif who == iid:
                    owned = self.owns(iid)
                    for e in doc.entries:
                        if any(entry_matches(k, e) for k in owned):
                            anaphoric[e].add(iid)
        return {e: explicit[e] or anaphoric[e] for e in doc.entries}

    # --- the assertions ----------------------------------------------

    def check_a1(self):
        notes = []
        for doc in self.docs:
            if not doc.g["allow_block"]:
                notes.append(f"A1 not run over {doc.path}: no allowlist block"
                             " is configured for it, the campaign proposing"
                             " no artifact")
                continue
            allow_file = doc.g["allow_file"]
            for e, who in self.claims(doc).items():
                if not who:
                    writers = sorted(x for x in doc.items
                                     if any(entry_matches(k, e)
                                            for k in self.owns(x)))
                    tail = ""
                    if writers:
                        tail = "; written by " + " and ".join(writers)
                        for x in writers:
                            if doc.allow_key not in self.owns(x):
                                tail += (f", and {x}'s **Owns** does not"
                                         f" name `{allow_file}`")
                    self.findings.append(
                        f"A1 `{e}` --- claimed by no item{tail}")
                elif len(who) > 1:
                    self.findings.append(
                        f"A1 `{e}` --- claimed by {' and '.join(sorted(who))}")
                for x in sorted(who):
                    if x in self.items and doc.allow_key not in self.owns(x):
                        self.findings.append(
                            f"A1 `{e}` --- claimed by {x}, whose **Owns**"
                            f" does not name `{allow_file}`")
        return notes

    def creators(self):
        """file key -> live items that create it: the claimants of its
        allowlist entry, and any **Owns** calling it new."""
        out = {}
        for doc in self.docs:
            for e, who in self.claims(doc).items():
                for iid in who:
                    for k in self.owns(iid):
                        if entry_matches(k, e):
                            out.setdefault(k, set()).add(iid)
        for iid in self.items:
            if self.historical(iid):
                continue
            for m in NEW_RE.finditer(self.field(iid, "Owns")):
                k = path_key(m.group(1), self.doc_of(iid).ambiguous)
                if k:
                    out.setdefault(k, set()).add(iid)
        return out

    def check_a3(self):
        made = self.creators()
        for x in sorted(self.items):
            if self.historical(x):
                continue
            reach = self.closure(x)
            for k in sorted(self.owns(x)):
                for y in sorted(made.get(k, ())):
                    if y != x and x not in made[k] and y not in reach:
                        self.findings.append(
                            f"A3 {x} --- writes `{k}`, created by {y}, which"
                            f" its depends-on cell does not reach")

    def check_a4(self):
        for x in sorted(self.items):
            owned = self.owns(x)
            for k, name in self.gated:
                if k in owned and f"`{DOCS_GATE}`" not in self.field(x, "Done"):
                    self.findings.append(
                        f"A4 {x} --- **Owns** names `{name}` and **Done**"
                        f" lacks `{DOCS_GATE}`")

    def check_a5(self):
        for x in sorted(self.items):
            for label in (SPLIT, "Owns"):
                text = self.field(x, label)
                for m in NUMBER_LIST_RE.finditer(text):
                    n, noun, lst, tail = (NUMBERS[m.group(1)], m.group(3),
                                          m.group(4), m.group(5))
                    if re.match(r"(?:, | and |, and | or )(?!`)", tail):
                        continue   # the list trails off into prose
                    k = len(TICK_RE.findall(lst))
                    if k != n:
                        self.findings.append(
                            f"A5 {x} --- says {m.group(1)} {noun} and lists"
                            f" {k}: {lst}")
            split = self.field(x, SPLIT)
            m = COMMITS_RE.search(split)
            markers = sorted({int(d) for d in MARKER_RE.findall(split)})
            if m and markers and markers[0] == 1 and \
                    markers == list(range(1, markers[-1] + 1)) and \
                    markers[-1] != NUMBERS[m.group(1)]:
                self.findings.append(
                    f"A5 {x} --- **Split** says {m.group(1)} commits and"
                    f" enumerates {markers[-1]}")

    def check(self):
        notes = self.check_a1()
        self.check_a3()
        self.check_a4()
        self.check_a5()
        return self.findings, notes

    def dump(self):
        print("derived graph:")
        for iid in self.order:
            print(f"  {iid}: owns {sorted(self.owns(iid))};"
                  f" depends on {sorted(self.deps(iid))}")
        print("  claims:")
        for doc in self.docs:
            for e, who in self.claims(doc).items():
                print(f"    {e}: {sorted(who)}")


def read_text(path):
    """PATH's text, or Blocked where it is missing or not UTF-8
    (check-plan-crossrefs-07)."""
    if not os.path.isfile(path):
        raise Blocked(f"no such file: {path}")
    try:
        with open(path, encoding="utf-8") as f:
            return f.read()
    except UnicodeDecodeError as e:
        raise Blocked(f"not UTF-8: {path} ({e})")


def load(doc_path, allow_override, ambiguous, findings):
    """DOC_PATH read under its grammar, or Blocked with the reason."""
    text = unwrapped(read_text(doc_path))
    if text is None:
        raise Blocked("wrap80 is not on PATH, and the fields here span"
                      " lines, so no line-oriented parse of them is sound")
    lines = text.split("\n")
    g = grammar_for(doc_path, lines)
    if g is None:
        raise Blocked(f"no ledger row or no item found in {doc_path} under"
                      " exactly one of the grammars in the configuration"
                      " table; none fits it, or two do")
    allow_text = ""
    if g["allow_file"]:
        allow_text = read_text(allow_override or g["allow_file"])
    shown = g["doc"] if is_configured(doc_path, g) else doc_path
    doc = Doc(g, shown, text, allow_text, ambiguous, findings)
    if not doc.order or not doc.items:
        raise Blocked(f"no ledger row or no item found in {doc_path}; the"
                      f" grammar for {g['doc']} does not fit it")
    return doc


def run(doc_paths, allow_override=None, verbose=False):
    """Check DOC_PATHS together; the exit status, findings printed."""
    findings = []
    ambiguous = ambiguous_basenames()
    try:
        docs = [load(p, allow_override, ambiguous, findings)
                for p in doc_paths]
        plan = Plan(docs, findings)
    except Blocked as b:
        print(f"BLOCKED: {b}")
        return 2
    if verbose:
        plan.dump()
    findings, notes = plan.check()
    for f in findings:
        print("FAIL " + f)
    for n in notes:
        print("NOTE " + n)
    rows = sum(len(d.order) for d in docs)
    items = sum(len(d.items) for d in docs)
    entries = sum(len(d.entries) for d in docs)
    print(f"\n{len(findings)} failed over {rows} ledger rows, {items} items"
          f" and {entries} allowlist entries in {len(docs)} document(s):"
          f" {', '.join(d.path for d in docs)}")
    return 1 if findings else 0


def capture(*args):
    """`run` over ARGS: its exit status and what it printed."""
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = run(*args)
    return rc, buf.getvalue()


def self_test():
    bad = []

    def check(label, rc, out, want_rc, fails, quiet, note=None):
        lines = [l for l in out.splitlines() if l.startswith("FAIL ")]
        if rc != want_rc:
            bad.append(f"{label}: exit {rc}, wanted {want_rc}")
        for want in fails:
            if not any(want in l for l in lines):
                bad.append(f"{label}: missing: {want}")
        for q in quiet:
            for l in lines:
                if q in l:
                    bad.append(f"{label}: control failed: {l}")
        if len(lines) != len(fails):
            bad.append(f"{label}: {len(lines)} FAIL lines, wanted"
                       f" {len(fails)}:\n" + "\n".join(lines))
        if note and note not in out:
            bad.append(f"{label}: missing note: {note}")

    # A document is its grammar's own by root-relative name or by an absolute
    # path ending in it, as a defect record hands one over, and a copy under
    # another name is not, whatever directory it sits in.
    own = GRAMMARS[0]["doc"]
    if not is_configured(own, GRAMMARS[0]) or \
            not is_configured("/x/y/" + own, GRAMMARS[0]) or \
            is_configured("/x/y/plan.md", GRAMMARS[0]) or \
            is_configured(own + ".bak", GRAMMARS[0]):
        bad.append("is_configured: the configured document is not told"
                   " from a copy by root-relative name and absolute suffix")
    with tempfile.TemporaryDirectory() as tmp:
        doc = os.path.join(tmp, "plan.md")
        doc2 = os.path.join(tmp, "work-list.md")
        allow = os.path.join(tmp, "allow.txt")
        open(doc, "w").write(SELF_TEST_DOC)
        open(doc2, "w").write(SELF_TEST_DOC_2)
        open(allow, "w").write(SELF_TEST_ALLOW)
        cwd = os.getcwd()
        os.chdir(tmp)   # outside any repository: keys are basenames
        try:
            check("plan alone", *capture([doc], allow), 1, SELF_TEST_FAIL,
                  SELF_TEST_QUIET + SELF_TEST_QUIET_ALONE)
            check("work list alone", *capture([doc2], allow), 1,
                  SELF_TEST_FAIL_2, SELF_TEST_QUIET_2 + SELF_TEST_QUIET_ALONE,
                  SELF_TEST_NOTE_2)
            check("joint", *capture([doc, doc2], allow), 1,
                  SELF_TEST_FAIL + SELF_TEST_FAIL_2 + SELF_TEST_FAIL_JOINT,
                  SELF_TEST_QUIET + SELF_TEST_QUIET_2 + SELF_TEST_QUIET_JOINT,
                  SELF_TEST_NOTE_2)
            rc, _ = capture([os.path.join(tmp, "absent.md")], allow)
            if rc != 2:
                bad.append(f"missing document: exit {rc}, wanted 2")
            open(doc, "w").write("# Nothing here\n\nProse.\n")
            rc, out = capture([doc], allow)
            if rc != 2 or "BLOCKED" not in out:
                bad.append(f"document without the grammar: exit {rc},"
                           f" wanted BLOCKED at 2")
            open(doc, "w").write(SELF_TEST_DOC_2)
            rc, out = capture([doc, doc2], allow)
            if rc != 2 or "two documents" not in out:
                bad.append(f"two documents sharing an id: exit {rc},"
                           f" wanted BLOCKED at 2")
            # An item and no ledger fits a grammar and is still no plan.
            open(doc, "w").write("# Scratch\n\n### 0.1 First\n\n"
                                 "**Owns** --- nothing.\n")
            rc, out = capture([doc], allow)
            if rc != 2 or "BLOCKED" not in out:
                bad.append(f"document with an item and no ledger: exit {rc},"
                           f" wanted BLOCKED at 2")
            # A depends-on cell naming a row with no item, and a practice with
            # neither: A0 reports the first, and A3's closure follows neither
            # (check-plan-crossrefs-06).
            open(doc, "w").write(
                "# Scratch\n\n| sec. | delivers | size | depends on | state |\n"
                "|---|---|---|---|---|\n| 0.1 | a | tiny | --- | open |\n"
                "| 0.2 | b | tiny | 0.1, functional core | open |\n\n"
                "### 0.2 Second\n\n**Owns** --- nothing.\n\n"
                "**Done** --- `ts`.\n\n**Hands back** --- nothing.\n\n"
                "**Decide first** --- nothing.\n")
            open(allow, "w").write("")
            check("ledger row with no item", *capture([doc], allow), 1,
                  ["A0 0.1 --- ledger row with no item"], [])
        finally:
            os.chdir(cwd)
    if bad:
        print("SELF-TEST FAILED:")
        for b in bad:
            print("  " + b)
        return 1
    print("self-test passed: every expected finding drawn, alone and"
          " jointly, every control quiet")
    return 0


USAGE = ("usage: check-plan-crossrefs.py [DOC ...] [--allowlist FILE] [-v]"
         " [--self-test]")


def main():
    argv = sys.argv[1:]
    verbose = "-v" in argv
    allow = None
    if "--allowlist" in argv:
        i = argv.index("--allowlist")
        if i + 1 >= len(argv):
            print(USAGE, file=sys.stderr)
            return 2
        allow = argv[i + 1]
        del argv[i:i + 2]
    args = [a for a in argv if a != "-v" and a != "--self-test"]
    if any(a.startswith("-") for a in args):
        print(USAGE, file=sys.stderr)
        return 2
    # The default allowlist is root-relative like the rest of the configuration;
    # only a path given on the command line is rebased.
    paths = chdir_root(args + ([allow] if allow else []))
    if "--self-test" in argv:
        return self_test()
    docs = paths[:len(args)] if args else [g["doc"] for g in GRAMMARS]
    return run(docs, paths[-1] if allow else None, verbose)


if __name__ == "__main__":
    sys.exit(main())
