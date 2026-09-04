#!/usr/bin/env python3
"""Check docs/wasm-frontend-unified-plan.md against itself.

Usage: python3 tools/check-plan-crossrefs.py [DOC] [--allowlist FILE] [-v]
DOC defaults to the plan named in the configuration block. Runs from
anywhere in the repository.

The plan is thirty-odd items, each closing with the same execution block
(**Split**, **Owns**, **Done**, **Hands back**, **Decide first**) and each
with a ledger row whose `depends on` cell names the items it waits for.
Between the items sits a graph nobody writes down as a graph: which item
writes which file, which items contend for one, which item creates the
artifact another consumes, which `tools/doc-refs-allow.txt` entry is whose
to delete. The plan refuses a central table of that graph, with reasons,
so the graph is maintained by hand across the items and drifts: a review
campaign put a third to two thirds of its findings on exactly this class,
every one of them green under the four checkers that read the document
against the repository and never against itself. This one derives the
graph at run time from the fields and stores nothing, which is the
alternative that ruling did not have.

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
  A2  the contention graph is symmetric. For every file two or more
      **Owns** name, each claimant's **Owns** names every other claimant,
      or names a claimant whose **Owns** holds a list naming it --- the
      one-list-per-file shape the plan settled on for `terminal.ts`,
      `loader.ts`, `run-wasm-game.mjs` and `index.html`. The plan document
      and the allowlist are excluded: the plan says of itself that the
      lock does not serialize on it, and of the allowlist that its
      claimants are every item that builds anything, by rule rather than
      by enumeration.
  A3  an item that writes a file another item creates reaches that item
      through the ledger's `depends on` cells, transitively: waiting on an
      item that waits on the creator is waiting on the creator.
  A4  an item whose **Owns** names the plan or the allowlist carries the
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

Read the unwrapped form (`wrap80 --unwrap`) and nothing else: the plan is
kept at 80 columns, an **Owns** field spans a dozen lines, and no
line-oriented parse of it is sound. Without wrap80 the run is BLOCKED at 2
rather than degraded, unlike check-doc-refs, whose spans lose little when
read wrapped.

Where the verdict and the document disagree the document may be right: it
is a live specification under review. Report, and never reword the
document to make this pass; a legitimately absent name is an allowlist
entry with its reason, and a wrong verdict is this checker's to fix.

Exit 0 clean, 1 with findings, 2 when the run did not happen: no document,
no allowlist, no wrap80, or a document in which the grammar found no
ledger and no item, which is a retargeting error rather than a clean plan.

Non-vacuity: `--self-test` runs the engine over a scratch plan and
allowlist in a temporary directory whose expected findings sit beside the
configuration below, one row per assertion and controls for the shapes
each must leave alone; that the self-test bites is `tools/mutants.py`'s to
show. The historical corpus is the other proof, and `tools/defects.json`
carries it as controls: the plan at `91f28c8f3`, before the campaign's
fixes, must draw the findings the campaign found by hand. This checker
encodes one document's field grammar, so it has no horde-ad twin and
`check-twin-sync.py` does not know it."""

import contextlib
import io
import os
import re
import subprocess
import sys
import tempfile

# --- per-document configuration ---------------------------------------
# Retargeting this checker to another campaign plan should mean editing
# this block and nothing else.
DOC = "docs/wasm-frontend-unified-plan.md"
ALLOW_FILE = "tools/doc-refs-allow.txt"
# The allowlist is grouped by comment block; only the block whose comment
# carries this phrase holds artifacts the plan proposes. The other blocks
# are phantoms, foreign repositories, superseded documents and toolchain
# output, and belong to no item.
ALLOW_BLOCK = "Artifacts docs/wasm-frontend-unified-plan.md proposes"
# The gate whose presence A4 asserts, as **Done** spells it.
DOCS_GATE = "docs"
# The execution block's labels: the four every item carries, and the one
# that is present only where an item is several commits.
LABELS = ("Owns", "Done", "Hands back", "Decide first")
SPLIT = "Split"
FIELD_RE = re.compile(r"^\*\*(Split|Owns|Done|Hands back|Decide first)\*\*"
                      r" --- ?(.*)$")
# How an item opens: a `### N.N` heading, a bold `**RN --- ...**`
# paragraph, or a bold paragraph opening with a practice's title, keyed
# here by the name its ledger row uses. Each practice also lists how prose
# refers to it, since "the capability-constants practice" is a mention.
NUMBERED_RE = re.compile(r"^### (\d\.\d) ")
RELATED_RE = re.compile(r"^\*\*(R\d) --- ")
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
# **Owns** names the plan itself by these phrases as often as by path.
DOC_ALIASES = ("this document", "this plan")
# Files the plan says the lock does not serialize on, so no A2 edge.
UNSERIALIZED = (DOC, ALLOW_FILE)
# **Owns** and **Split** name a file in order to disclaim it as often as
# to claim it: "Not `haskell-ci.yml`", "`Dom.hs` is deliberately not
# here", "`cursor.ts` there are 0.2's". The phrases are curated from the
# document and read in a window around the token --- the clause before
# it and the clause after, other tokens blanked so a list is transparent
# --- rather than over a whole sentence, where a bare "not" would strip
# 2.4 of the two files that sit beside its "should (1) not be". A
# citation in parentheses, "(`loader.ts:56`)", is evidence and names no
# file the item writes.
NEG_BEFORE_RE = re.compile(r"\b(?:not|never|neither|nor|no|nothing)\b"
                           r"(?:\s+[\w'-]+){0,3}\s*$", re.I)
NEG_AFTER_RE = re.compile(r"^'s (?:does|is) at\b|\b(?:is|are) (?:ID)'s\b"
                          r"|\bstays? in\b|\bleft this list\b|\bcannot arise\b"
                          r"|\bnot owned\b|\bnot here\b|\bnot edited\b"
                          r"|\bowned by (?:ID)\b", re.I)
WINDOW = 80
# The words a sentence uses when it speaks of an allowlist entry, the
# negations that make such a sentence say there is none to delete, and the
# shape that hands the deletion to another item ("rides 0.3's export
# half").
CLAIM_RE = re.compile(r"\b(?:entry|entries|deletion)\b")
NEG_CLAIM_RE = re.compile(r"\b(?:no|nothing|neither|none)\b", re.I)
ATTRIB_RE = re.compile(r"\b(?:rides?|is|are) (ID)'s\b")
NEW_RE = re.compile(r"\bnew `([^`\n]+)`")
# --- self-test rows ------------------------------------------------------
# A scratch plan in the grammar above, and the findings it must draw. The
# controls matter as much as the failures: `loader.ts` is contended four
# ways and silent because 0.1 holds its claimant list; 1.1 writes a file
# 0.1 creates and is silent because its row waits on 0.2, which waits on
# 0.1; R1's "Not `foo.ts`", its "`Makefile` is 0.1's" and its
# parenthesized citation of `terminal.ts` keep R1 off those files' edges,
# as 1.2's "is deliberately not here" does 1.2; the open list "and the
# module's" is not counted. `dropped.ts` is out of scope, in a block the
# phrase above does not head.
SELF_TEST_ALLOW = """\
# Scratch allowlist for the self-test.

# Phantoms, out of scope.
dropped.ts

# Artifacts docs/wasm-frontend-unified-plan.md proposes but has not built
# yet.
new-core.ts
new-core.test.ts
test/NewUnitTests.hs
test/RUnitTests.hs
twice.ts
orphan.ts
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
| R1 | e | tiny | 1.1 | open |
| determinism goldens | f | tiny | 0.1--0.2 | open |

## Handing an item to a session

**Owns** --- here this is prose, not a field, since no item is open.

## Phase 0

### 0.1 First

Body naming `foo.ts` as evidence, which is not an **Owns**.

**Split** --- two commits. (1) `ts-src/src/new-core.ts` and `ts-src/src/new-core.test.ts`, with the deletion of both `tools/doc-refs-allow.txt` entries. (2) the landing, and the `make gen-scratch` entry.

**Owns** --- the new `ts-src/src/new-core.ts`, `ts-src/src/new-core.test.ts`, `ts-src/src/loader.ts`, `Makefile`, `tools/doc-refs-allow.txt` and this document. **The claimant list for `loader.ts` lives here**: 0.1, 0.2, 1.1 and R1.

**Done** --- `ts`, `docs`.

**Hands back** --- nothing.

**Decide first** --- nothing.

### 0.2 Second

**Owns** --- `ts-src/src/loader.ts`, `ts-src/src/terminal.ts`, `ts-src/src/foo.ts`, `ts-src/src/twice.ts`, the new `test/NewUnitTests.hs`, `tools/doc-refs-allow.txt`, whose `test/NewUnitTests.hs` and `twice.ts` entries this commit deletes, and `docs/wasm-frontend-unified-plan.md`. Not concurrent with 1.1 on `terminal.ts` and `foo.ts`; `loader.ts`'s claimant list is 0.1's.

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

**Split** --- three commits. (1) `ts-src/src/new-core.ts` gains a case. (2) the landing, with the deletion of three entries --- `twice.ts` and `orphan-not.ts` --- from `tools/doc-refs-allow.txt`; `Makefile` gains nothing.

**Owns** --- `ts-src/src/new-core.ts`, `ts-src/src/terminal.ts`, `ts-src/src/twice.ts` and `tools/doc-refs-allow.txt`; `ts-src/src/foo.ts` is deliberately not here.

**Done** --- `ts`.

**Hands back** --- nothing.

**Decide first** --- nothing.

## Related goals

**R1 --- Fifth.** Body.

**Owns** --- the new `test/RUnitTests.hs`, `ts-src/src/loader.ts` per the claimant list at 0.1, and this document. Not `ts-src/src/foo.ts`, whose claimants are 0.2 and 1.1. `Makefile` is 0.1's, and the evidence sits at (`ts-src/src/terminal.ts:3`).

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
    "A1 `twice.ts` --- claimed by 0.2 and 1.2",
    "A1 `test/RUnitTests.hs` --- claimed by R1, whose **Owns** does not name"
    " `tools/doc-refs-allow.txt`",
    "A2 `new-core.ts` --- 0.1's **Owns** names neither 1.2 nor",
    "A2 `new-core.ts` --- 1.1's **Owns** names neither 1.2 nor",
    "A2 `new-core.ts` --- 1.2's **Owns** names neither 0.1, 1.1 nor",
    "A2 `twice.ts` --- 0.2's **Owns** names neither 1.2 nor",
    "A2 `twice.ts` --- 1.2's **Owns** names neither 0.2 nor",
    "A2 `terminal.ts` --- 0.2's **Owns** names neither 1.2 nor",
    "A2 `terminal.ts` --- 1.1's **Owns** names neither 1.2 nor",
    "A2 `terminal.ts` --- 1.2's **Owns** names neither 0.2, 1.1 nor",
    "A3 1.2 --- writes `new-core.ts`, created by 0.1",
    "A4 1.2 --- **Owns** names `tools/doc-refs-allow.txt` and **Done**"
    " lacks `docs`",
    "A5 1.2 --- says three entries and lists 2",
    "A5 1.2 --- **Split** says three commits and enumerates 2",
]
# Names that must appear in no FAIL line: the controls.
SELF_TEST_QUIET = ["loader.ts", "foo.ts", "dropped.ts", "new-core.test.ts",
                   "gen-scratch", "Makefile", "A3 1.1", "A4 0.1", "A4 0.2",
                   "A4 R1", "A5 0.1", "A5 1.1"]
# --- end per-document configuration -------------------------------------

PATH_EXT = ("hs", "ts", "mjs", "py", "cabal", "html", "md", "yaml", "yml",
            "json", "sh", "txt", "c", "h", "js")
TICK_RE = re.compile(r"`([^`\n]+)`")
CITE_RE = re.compile(r":\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*$")
NUMBERS = {"two": 2, "three": 3, "four": 4, "five": 5, "six": 6,
           "seven": 7, "eight": 8, "nine": 9, "ten": 10, "eleven": 11,
           "twelve": 12}
NUMBER_LIST_RE = re.compile(
    r"\b(two|three|four|five|six|seven|eight|nine|ten|eleven|twelve)\b"
    r"((?: (?:`[^`\n]+`|[\w'-]+)){0,3}) (entries|files|commits|modules|"
    r"targets|names|claimants|jobs|halves|clauses|tests|cases|sites|lines)"
    r"(?: --- |: )((?:`[^`\n]+`(?:'s)?(?:, and |, or |, |"
    r" and | or ))*`[^`\n]+`(?:'s)?)(.{0,6})")
COMMITS_RE = re.compile(r"\b(two|three|four|five|six|seven|eight|nine|ten)"
                        r" commits\.")
MARKER_RE = re.compile(r"\((\d)\)")


def unwrapped(text):
    """The document one paragraph per line, or None without wrap80."""
    try:
        return subprocess.run(["wrap80", "--unwrap"], input=text, text=True,
                              capture_output=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError):
        return None


def chdir_root(paths):
    """Run from the repository root whatever the cwd -- the configuration's
    paths are root-relative -- and return PATHS rebased to it. Outside a
    repository nothing moves."""
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                         capture_output=True, text=True).stdout.strip()
    if not top:
        return paths
    paths = [os.path.relpath(os.path.abspath(p), top) for p in paths]
    os.chdir(top)
    return paths


def ambiguous_basenames():
    """Basenames git tracks more than once, which stay qualified as keys.
    Outside a repository there are none, and keys are basenames."""
    p = subprocess.run(["git", "ls-files"], capture_output=True, text=True)
    if p.returncode != 0:
        return set()
    seen, dup = set(), set()
    for path in p.stdout.split("\n"):
        b = os.path.basename(path)
        if b in seen:
            dup.add(b)
        seen.add(b)
    return dup


def path_key(token, ambiguous):
    """The file a backticked token names, as the key the graph is built on:
    the basename, or the last two components where the basename is one the
    repository carries twice. None for a token that is not a path."""
    t = CITE_RE.sub("", token.strip())
    if not t or t[0] in "~$/+-" or t.startswith(("../", "http")):
        return None
    if any(c in t for c in " *{<>()=\"'") or ".." in t:
        return None
    base = t.rstrip("/").rsplit("/", 1)[-1]
    if not base or base.startswith("."):
        return None
    if "/" not in t and base != "Makefile" and \
            base.rsplit(".", 1)[-1] not in PATH_EXT:
        return None
    if "/" in t and not (base.rsplit(".", 1)[-1] in PATH_EXT or
                         t.endswith("/") or base == "Makefile"):
        return None
    if t.endswith("/"):
        return t
    if base in ambiguous:
        parts = t.split("/")
        return "/".join(parts[-2:])
    return base


BOUNDARY_RE = re.compile(r"(?<=[.;])\s+| --- ")


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


class Plan:
    def __init__(self, text, allow_text, ambiguous):
        self.lines = text.split("\n")
        self.findings = []
        self.ledger = {}      # id -> depends-on cell
        self.order = []       # ledger ids in order
        self.items = {}       # id -> {"line": n, "fields": {label: text}}
        self.entries = self.in_scope_entries(allow_text)
        self.ambiguous = ambiguous
        self.parse_ledger()
        self.parse_items()
        ids = [i for i in self.order if not i[0].isalpha() or i[0] == "R"]
        alts = [re.escape(i) for i in ids]
        for name, (_, aliases) in PRACTICES.items():
            alts += [re.escape(a) for a in aliases]
        self.id_re = re.compile(r"(?<![\w.])(" + "|".join(alts)
                                + r")(?!\d|\.\d)", re.I)
        self.neg_after_re = re.compile(NEG_AFTER_RE.pattern.replace(
            "(?:ID)", "(?:" + "|".join(re.escape(i) for i in ids) + ")"),
            re.I)
        self.attrib_re = re.compile(ATTRIB_RE.pattern.replace(
            "(ID)", "(" + "|".join(re.escape(i) for i in ids) + ")"))
        self.doc_key = path_key(DOC, ambiguous)
        self.allow_key = path_key(ALLOW_FILE, ambiguous)

    @staticmethod
    def in_scope_entries(allow_text):
        entries, comment, in_scope = [], [], False
        for line in allow_text.split("\n") + [""]:
            if not line.strip():
                comment, in_scope = [], False
                continue
            if line.startswith("#"):
                comment.append(line)
                if ALLOW_BLOCK in " ".join(comment):
                    in_scope = True
                continue
            body = line.split("#", 1)[0].strip()
            if body and in_scope:
                entries.append(body)
        return entries

    def parse_ledger(self):
        for line in self.lines:
            if not line.startswith("| ") or line.startswith("| sec."):
                continue
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if len(cells) != 5 or set(cells[0]) <= set("-"):
                continue
            iid = cells[0]
            if iid in self.ledger:
                self.findings.append(f"A0 {iid} --- two ledger rows")
            self.ledger[iid] = cells[3]
            self.order.append(iid)

    def opener(self, line):
        m = NUMBERED_RE.match(line) or RELATED_RE.match(line)
        if m:
            return m.group(1)
        if line.startswith("**"):
            for name, (title, _) in PRACTICES.items():
                if line.startswith("**" + title):
                    return name
        return None

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
            iid = self.opener(line)
            if iid is None:
                continue
            if iid in self.items:
                self.findings.append(f"A0 {iid} --- opens twice, line {n}")
            cur = {"id": iid, "line": n, "fields": {}}
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

    # --- the derived graph -------------------------------------------

    def field(self, iid, label):
        return self.items.get(iid, {}).get("fields", {}).get(label, "")

    def disclaimed(self, text, m):
        """Whether the token at match M is named to be disclaimed: a
        negation phrase closes the clause before it or opens the clause
        after, other tokens blanked, or it is a parenthesized citation."""
        if m.start() > 0 and text[m.start() - 1] == "(" and \
                CITE_RE.search(m.group(1)):
            return True
        before = TICK_RE.sub("", text[:m.start()])
        before = BOUNDARY_RE.split(before)[-1][-WINDOW:]
        after = TICK_RE.sub("", text[m.end():])
        after = BOUNDARY_RE.split(after)[0][:WINDOW]
        return bool(NEG_BEFORE_RE.search(before)
                    or self.neg_after_re.search(after))

    def keys_in(self, text):
        """The file keys a field names as written, less the disclaimed."""
        keys = set()
        if text.strip().lower().startswith("nothing"):
            return keys
        for m in TICK_RE.finditer(text):
            k = path_key(m.group(1), self.ambiguous)
            if k and not self.disclaimed(text, m):
                keys.add(k)
        if any(a in text for a in DOC_ALIASES):
            keys.add(self.doc_key)
        return keys

    def owns(self, iid):
        return self.keys_in(self.field(iid, "Owns"))

    def writes(self, iid):
        return self.owns(iid) | self.keys_in(self.field(iid, SPLIT))

    def mentions(self, iid):
        """The items an **Owns** names, by id or alias, itself excluded."""
        out = set()
        for m in self.id_re.findall(self.field(iid, "Owns")):
            out.add(self.canonical(m))
        out.discard(iid)
        return out

    def canonical(self, mention):
        low = mention.lower()
        for name, (_, aliases) in PRACTICES.items():
            if low in (a.lower() for a in aliases):
                return name
        return mention

    def deps(self, iid):
        cell = self.ledger.get(iid, "")
        out = set()
        for a, b in re.findall(r"(\d\.\d)--(\d\.\d)", cell):
            nums = [i for i in self.order if re.fullmatch(r"\d\.\d", i)]
            if a in nums and b in nums:
                out.update(nums[nums.index(a):nums.index(b) + 1])
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

    def artifact_tokens(self, sentence):
        """(position, token, matching entries) for each backticked token of
        a sentence that names an in-scope entry."""
        out = []
        for m in TICK_RE.finditer(sentence):
            hits = [e for e in self.entries if entry_matches(m.group(1), e)]
            if hits:
                out.append((m.start(), m.group(1), hits))
        return out

    def claims(self):
        """entry -> set of claimants, explicit claims first and the
        anaphoric ones ("its two entries") only where nothing is explicit."""
        explicit = {e: set() for e in self.entries}
        anaphoric = {e: set() for e in self.entries}
        for iid in self.items:
            text = self.field(iid, SPLIT) + "\n" + self.field(iid, "Owns")
            for m in NEW_RE.finditer(text):
                for e in self.entries:
                    if entry_matches(m.group(1), e):
                        explicit[e].add(iid)
            for s in sentences(text):
                if not CLAIM_RE.search(s):
                    continue
                toks = self.artifact_tokens(s)
                cut = toks[0][0] if toks else len(s)
                if NEG_CLAIM_RE.search(s[:cut]):
                    continue
                a = self.attrib_re.search(s)
                who = self.canonical(a.group(1)) if a else iid
                if toks:
                    written = self.writes(who) if who in self.items else set()
                    for _, _, hits in toks:
                        for e in hits:
                            if e.startswith("make ") or any(
                                    entry_matches(k, e) for k in written):
                                explicit[e].add(who)
                elif who == iid:
                    owned = self.owns(iid)
                    for e in self.entries:
                        if any(entry_matches(k, e) for k in owned):
                            anaphoric[e].add(iid)
        return {e: explicit[e] or anaphoric[e] for e in self.entries}

    # --- the assertions ----------------------------------------------

    def check_a1(self):
        for e, who in self.claims().items():
            if not who:
                writers = sorted(x for x in self.items
                                 if any(entry_matches(k, e)
                                        for k in self.writes(x)))
                tail = ""
                if writers:
                    tail = "; written by " + " and ".join(writers)
                    for x in writers:
                        if self.allow_key not in self.owns(x):
                            tail += (f", and {x}'s **Owns** does not name"
                                     f" `{ALLOW_FILE}`")
                self.findings.append(f"A1 `{e}` --- claimed by no item{tail}")
            elif len(who) > 1:
                self.findings.append(
                    f"A1 `{e}` --- claimed by {' and '.join(sorted(who))}")
            for x in sorted(who):
                if x in self.items and self.allow_key not in self.owns(x):
                    self.findings.append(
                        f"A1 `{e}` --- claimed by {x}, whose **Owns** does"
                        f" not name `{ALLOW_FILE}`")

    def contention(self):
        by_file = {}
        for iid in self.items:
            for k in self.owns(iid):
                by_file.setdefault(k, set()).add(iid)
        skip = {path_key(u, self.ambiguous) for u in UNSERIALIZED}
        return {f: c for f, c in by_file.items()
                if len(c) > 1 and f not in skip and not f.endswith("/")}

    def acknowledges(self, x, y, claimants, named):
        if y in named[x]:
            return True
        return any(y in named[lst] for lst in named[x]
                   if lst in claimants and lst != x)

    def check_a2(self):
        named = {i: self.mentions(i) for i in self.items}
        for f, claimants in sorted(self.contention().items()):
            for x in sorted(claimants):
                missing = [y for y in sorted(claimants) if y != x and
                           not self.acknowledges(x, y, claimants, named)]
                if missing:
                    self.findings.append(
                        f"A2 `{f}` --- {x}'s **Owns** names neither"
                        f" {', '.join(missing)} nor a claimant list naming"
                        f" {'it' if len(missing) == 1 else 'them'}")

    def creators(self):
        """file key -> items that create it: the claimants of its allowlist
        entry, and any **Owns** calling it new."""
        out = {}
        for e, who in self.claims().items():
            for iid in who:
                for k in self.writes(iid):
                    if entry_matches(k, e):
                        out.setdefault(k, set()).add(iid)
        for iid in self.items:
            for m in NEW_RE.finditer(self.field(iid, "Owns")):
                k = path_key(m.group(1), self.ambiguous)
                if k:
                    out.setdefault(k, set()).add(iid)
        return out

    def check_a3(self):
        made = self.creators()
        for x in sorted(self.items):
            reach = self.closure(x)
            for k in sorted(self.writes(x)):
                for y in sorted(made.get(k, ())):
                    if y != x and x not in made[k] and y not in reach:
                        self.findings.append(
                            f"A3 {x} --- writes `{k}`, created by {y}, which"
                            f" its depends-on cell does not reach")

    def check_a4(self):
        for x in sorted(self.items):
            owned = self.owns(x)
            for k, name in ((self.doc_key, DOC), (self.allow_key, ALLOW_FILE)):
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
        self.check_a1()
        self.check_a2()
        self.check_a3()
        self.check_a4()
        self.check_a5()
        return self.findings

    def dump(self):
        print("derived graph:")
        for iid in self.order:
            print(f"  {iid}: owns {sorted(self.owns(iid))}")
            extra = sorted(self.writes(iid) - self.owns(iid))
            if extra:
                print(f"      split also writes {extra}")
            print(f"      names {sorted(self.mentions(iid))};"
                  f" depends on {sorted(self.deps(iid))}")
        print("  claims:")
        for e, who in self.claims().items():
            print(f"    {e}: {sorted(who)}")
        print("  contended:")
        for f, c in sorted(self.contention().items()):
            print(f"    {f}: {sorted(c)}")


def run(doc, allow, verbose=False):
    """Check DOC against ALLOW; the exit status, findings printed."""
    for path in (doc, allow):
        if not os.path.isfile(path):
            print(f"no such file: {path}", file=sys.stderr)
            return 2
    text = unwrapped(open(doc, encoding="utf-8").read())
    if text is None:
        print("BLOCKED: wrap80 is not on PATH, and the fields here span"
              " lines, so no line-oriented parse of them is sound")
        return 2
    plan = Plan(text, open(allow, encoding="utf-8").read(),
                ambiguous_basenames())
    if not plan.order or not plan.items:
        print(f"BLOCKED: no ledger row or no item found in {doc}; the"
              " grammar in the configuration block does not fit it")
        return 2
    if verbose:
        plan.dump()
    findings = plan.check()
    for f in findings:
        print("FAIL " + f)
    print(f"\n{len(findings)} failed over {len(plan.order)} ledger rows,"
          f" {len(plan.items)} items and {len(plan.entries)} allowlist"
          f" entries")
    return 1 if findings else 0


def self_test():
    bad = []
    with tempfile.TemporaryDirectory() as tmp:
        doc = os.path.join(tmp, "plan.md")
        allow = os.path.join(tmp, "allow.txt")
        open(doc, "w").write(SELF_TEST_DOC)
        open(allow, "w").write(SELF_TEST_ALLOW)
        cwd = os.getcwd()
        os.chdir(tmp)   # outside any repository: keys are basenames
        try:
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                rc = run(doc, allow)
            out = buf.getvalue()
            fails = [l for l in out.splitlines() if l.startswith("FAIL ")]
            if rc != 1:
                bad.append(f"exit {rc}, wanted 1")
            for want in SELF_TEST_FAIL:
                if not any(want in l for l in fails):
                    bad.append(f"missing: {want}")
            for quiet in SELF_TEST_QUIET:
                for l in fails:
                    if quiet in l:
                        bad.append(f"control failed: {l}")
            if len(fails) != len(SELF_TEST_FAIL):
                bad.append(f"{len(fails)} FAIL lines, wanted"
                           f" {len(SELF_TEST_FAIL)}:\n" + "\n".join(fails))
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf), \
                    contextlib.redirect_stderr(io.StringIO()):
                rc = run(os.path.join(tmp, "absent.md"), allow)
            if rc != 2:
                bad.append(f"missing document: exit {rc}, wanted 2")
            open(doc, "w").write("# Nothing here\n\nProse.\n")
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                rc = run(doc, allow)
            if rc != 2 or "BLOCKED" not in buf.getvalue():
                bad.append(f"document without the grammar: exit {rc},"
                           f" wanted BLOCKED at 2")
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                rc = run(doc, allow)
        finally:
            os.chdir(cwd)
    if bad:
        print("SELF-TEST FAILED:")
        for b in bad:
            print("  " + b)
        return 1
    print("self-test passed: every expected finding drawn, every control"
          " quiet")
    return 0


def main():
    argv = sys.argv[1:]
    verbose = "-v" in argv
    allow = ALLOW_FILE
    if "--allowlist" in argv:
        i = argv.index("--allowlist")
        if i + 1 >= len(argv):
            print("usage: check-plan-crossrefs.py [DOC] [--allowlist FILE]"
                  " [-v] [--self-test]", file=sys.stderr)
            return 2
        allow = argv[i + 1]
        del argv[i:i + 2]
    args = [a for a in argv if a != "-v" and a != "--self-test"]
    if len(args) > 1:
        print("usage: check-plan-crossrefs.py [DOC] [--allowlist FILE]"
              " [-v] [--self-test]", file=sys.stderr)
        return 2
    # The default allowlist is root-relative like the rest of the
    # configuration; only a path given on the command line is rebased.
    explicit = allow != ALLOW_FILE or "--allowlist" in sys.argv[1:]
    paths = chdir_root(args + ([allow] if explicit else []))
    if "--self-test" in argv:
        return self_test()
    doc = paths[0] if args else DOC
    return run(doc, paths[-1] if explicit else ALLOW_FILE, verbose)


if __name__ == "__main__":
    sys.exit(main())
