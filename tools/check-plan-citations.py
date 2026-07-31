#!/usr/bin/env python3
"""Check that file:line citations in a planning document still resolve.

Usage: python3 tools/check-plan-citations.py [DOC]
DOC defaults to CLAUDE.md. Run from the repo root.

For every citation of the form `path/to/File.hs:12` or `File.hs:12-34`
(also .ts, .py, .c, .h, .cabal, .mjs, .html, .md, .txt, .yaml/.yml and
the Makefile — documents cite each other and the tools), the script
resolves the file, checks the line range exists, and prints the first
cited line so a human can compare it against what the surrounding
sentence claims. A `/.../` component in a cited path is treated as a
wildcard (the document uses it to abbreviate long paths). A citation may
name several lines or ranges, comma-separated with no space
(`Sdl.hs:353,362,771-781`); each member is resolved and printed on its
own line. The no-space rule is what the documents write and keeps prose
("`Sdl.hs:353`, four lines below") out of the match.

Exit status is nonzero if any citation is UNRESOLVED (no such file),
AMBIGUOUS (a bare basename matching several files — qualify it in the
document), or OUT-OF-RANGE (the file is shorter than the cited line).

Line numbers drift as commits land: after changing a cited file, re-run
this and eyeball the printed snippets; the document header records the
commit its citations were last verified against.

Pinned GitHub permalinks (`https://github.com/.../blob/<commit>/<path>#L12`
or `#L12-L34`) are also checked, against the pinned commit via
`git show <commit>:<path>` — they never drift, so this catches typos,
wrong ranges and links whose commit or path is not in this repository
(foreign-repo links cannot be verified locally and are reported as
failures).

`git show` proves only that the commit is in *this* clone's object
database, which an unpushed or squashed-away commit is too — such a link
resolves for one person on one machine and 404s everywhere else. So a
resolved permalink is then required to be an ancestor of PUBLISHED_REF,
and one that is not fails as UNPUBLISHED; if that ref is absent the run
stops (exit 2) rather than degrading to the weaker check, as
check-doc-refs.py does for an unmounted sibling.

The document's own stamp gets the same treatment, split by severity,
because the two states differ in whether they can heal. A stamp naming a
commit that is not an ancestor of HEAD is ORPHANED and fails: a squash or
amend dropped it and nothing but re-verification brings it back. A stamp
naming a commit that is in HEAD but not yet on PUBLISHED_REF only earns a
note: pushing the branch unrewritten makes it true. This repo has stood
in the first state — CLAUDE.md records that both pointman documents
briefly named a commit that was on no branch — and the plain pass said
nothing, because until now it never read a stamp at all.

Stamp failures are counted apart from citation failures, and deliberately:
--restamp refuses on a failed citation pass, so folding the stamp verdict
into that count would let an orphaned stamp block the one command that
repairs it. They still both set the exit status. For the same reason
--restamp *writes* an unpublished anchor rather than refusing -- an
orphaned stamp left in place is strictly worse than an unpushed one -- and
prints an advisory naming the push it depends on.

Scope limits, deliberate: prose-style citations ("config.ui.default line
67") are not extracted, nor is a range left dangling from its filename
("`LambdaHack.cabal:152-156` and `371-391`") — a bare `371-391` in
backticks is indistinguishable from any other pair of numbers, so a
document that wants the second range checked must repeat the filename.

Refuted, and not to be reopened without new evidence: extending that to
the *colon-led* continuation the documents also write ("`:379`, `:398`,
`:431` — three `defAction`s"), which unlike a bare number looks
unambiguous. Measured 2026-07-31 over every tracked `.md` here bar
CHANGELOG.md, attaching each `` `:NNNN` `` to the nearest preceding
citation: 58 of them, all in `docs/` — 45 in leader-desync-migration.md,
9 in the wasm plan, 4 in leader-desync-bug.md. Every one of the 58
resolves against the file it would attach to, so the rule reports a clean
pass over the lot. At least three of those passes are lies: the `:379`,
`:398`, `:431` above sit in a table row about `transition` in
`InventoryM`, but the nearest preceding citation is `MonadClientUI.hs`
2355 characters back, and at 498 lines that file is long enough to
swallow all three — `ok` printed against `toMsgShared`, a POSIX time
subtraction and `getPOSIXTime`. That is the "a citation that resolves can
still lie" class, manufactured by the checker meant to catch it.

No gap threshold separates the cases. Here correct attachments run to 660
characters (the `HandleHumanGlobalM` rows, whose subject really is the
nearest cited file) against 2339-2355 for the wrong ones; the horde-ad
copy measured correct ones to 184 and wrong ones from 1538. Two corpora,
two cuts, neither derivable from the other — the separation is an
artifact of each document's prose, not a rule. Repeat the filename.

And the *claims* around citations are not checked
— in particular, universally-quantified claims ("only X does Y", "exactly
two", "never") must be re-verified by repo-wide grep, not by re-reading
the cited file; that asymmetry is how a real error slipped in once.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): feed it a
scratch document holding one citation of each failing kind and confirm
all seven are reported and the exit status is 1 —

    UNRESOLVED       `NoSuchFile.hs:12`
    OUT-OF-RANGE     `FrameM.hs:999999`
    CONTINUATION     `Point.hs:26,999999` (the tail member must be checked)
    AMBIGUOUS        `LoopM.hs:10`        (Client/ and Server/ both have one)
    NON-SOURCE       `CLAUDE.md:999999`   (documents and tools cite each other)
    PERMALINK range  .../blob/b4d5cc2e4/CLAUDE.md#L99999
    PERMALINK repo   https://github.com/ghc/ghc/blob/0123456789abcdef/x.hs#L1

and, in scratch documents of their own because each needs a whole file to
itself, the two publication kinds and the two notes —

    UNPUBLISHED      .../blob/b4d5cc2e4/CLAUDE.md#L1-L3
    ORPHANED stamp   a stamp naming `0000000aa`
    note, two stamps a document quoting two other documents' stamps
    note, unpushed   a stamp naming a commit in HEAD but not yet pushed

The two permalink rows name the same commit deliberately, and the order
of the checks is what lets them: OUT-OF-RANGE is tested before
publication, so the range row fails as a range even though that commit is
also unpublished. Swap those two checks and the first row silently starts
proving the second one's branch instead.

A leading dot needs its own pair, since there the *extraction* failed and
not the check: `.hlint.yaml:24` must resolve and print `- arguments:
[-XNoStarIsType]`, and `.hlint.yaml:999999` must report OUT-OF-RANGE.
Until the dot was allowed into CITE_RE the first was extracted as
`hlint.yaml` and reported UNRESOLVED, which is how the bug read from the
outside -- a document could not cite a dotfile at all, and saying so
looked like a missing file. That the widening regresses nothing is
checked by extracting the four stamped documents with the old pattern and
the new and diffing the results: 355 citations, identical.

plus a control that must still pass (`Point.hs:26`). A run reporting
fewer than seven failures means extraction, resolution or the `git show`
branch has silently stopped covering that kind. Two rows are there
because their kind was silently uncovered for a while. NON-SOURCE:
only Haskell and web sources were extracted, so a citation into a `.md`,
`.py` or `.yml` file was skipped rather than checked, and a document
citing nothing but those reported a clean zero. CONTINUATION: extraction
took only the first number of a comma-continued citation, so seven
sub-references in `docs/wasm-frontend-unified-plan.md` had never been
checked while the run reported "85 citations checked, 0 failed" over the
rest — a silent search of exactly the kind CLAUDE.md's portable notes
warn about, and invisible in the exit status by construction.

Reproduced 2026-07-30: seven failures and exit 1, the control resolving
to the `Point.hs` hack comment. A recipe with no date behind it is a
claim like any other.

The four publication branches, reproduced 2026-07-31: a document pinning
`b4d5cc2e4` at a valid range (that commit resolves, at 716 lines, while
being an ancestor of neither HEAD nor `origin/master`) reported
UNPUBLISHED, exit 1; one stamped `0000000aa` reported ORPHANED, exit 1;
one carrying two stamps printed the quotation note and exited 0; one
stamped `c2872c219`, in HEAD and not yet pushed, printed the unpushed
note and exited 0; and a copy of this script with PUBLISHED_REF set to
`origin/no-such-ref` stopped with exit 2. Each ran with `Point.hs:26` as
a control, resolving throughout, and the seven-kind recipe above re-ran
unchanged at seven. `0000000aa` is well-formed and nameless rather than a
real dropped commit: a reflog hash would prove the same branch today and
become unresolvable at the next gc, which is how a live row turns vacuous
without anyone touching it.

None of those five branches has a live control in the tracked corpus, and
two of them cannot have one here at all. The seven stamped documents all
name commits that are ancestors of both HEAD and `origin/master`, so
ORPHANED, the unpushed note and the two-stamps note each need a scratch
document written for the occasion; `c2872c219` stops serving the moment
master is pushed unrewritten, and a squash before that orphans it
instead, so either way this paragraph then wants a fresh hash. Neither
permalink branch can be exercised by a tracked document, nor can the
exit-2 stop: no `.md` here carries a pinned `blob/<sha>/` link at all,
README's whole-file pointers being deliberately `blob/master` -- eleven
of those across three documents, which is what proves the search for the
pinned form non-vacuous rather than merely silent. The stop sits inside
the permalink loop, so a bogus PUBLISHED_REF says nothing without one:
this script copied with PUBLISHED_REF set to `origin/no-such-ref` exits 0
on `CLAUDE.md` and 2 on a scratch document holding a single link pinned
at `2b20a8284`. Write that scratch document rather than hunting the tree
for one that serves.

Passing --restamp rewrites the document's own stamp, so the ritual the
documents ask for -- "re-run the pass and restamp after any replay of
these commits" -- stops depending on memory. By the leader-desync
document's own count that ritual had already been missed four times, each
time leaving a stamp naming a commit no longer in the repository, which no
reader can check.

The commit it writes is not HEAD but the newest commit touching anything
the document cites. That referent is the one that survives editing the
document: a commit carrying only prose touches no cited file, so amending
or replaying it cannot move the answer, whereas a HEAD-based stamp is
falsified by the very commit that records it. It also means the stamp can
name a commit well behind HEAD -- correctly, because the cited lines come
from there, and re-verification is owed when *they* move, not when
anything moves.

The date a stamp carries is the day the reading was done, not the date of
the commit it names. The two are spelled alike and mean different things,
which is why this docstring gives commit dates in words wherever it names
one.

What the flag cannot do is know that you *read* the document. The stamp
asserts two things -- that the citations resolve in some named tree, and
that they still say what the surrounding claims need -- and only the
first is mechanical. So restamp a document you have just re-read, not one
you have merely run this over; that asymmetry is why the flag is opt-in
rather than the default.

It refuses to write when anything is off, and the refusals are the point:

    a stale stamp, clean tree, clean pass  -> hash and date rewritten, 0
    the same run again                     -> "already current", no write, 0
    one unresolved citation                -> refuses, file untouched, 1
    a cited file modified against HEAD     -> refuses, file untouched, 1
    no stamp, or two stamps                -> refuses, file untouched, 1
    a stamp but no file:line citation      -> refuses, file untouched, 1
    the anchor it would write is unpushed  -> writes, plus an advisory, 0

The dirty-cited-file refusal is the subtle one: with a cited file
modified, the pass verified the working tree, and no commit hash names
what was checked, so a stamp would be a false statement rather than a
stale one.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous", applied to a
writer rather than a reader): reproduce the six rows above with scratch
documents -- one citing `tools/heading-outline.py:1` with a stamp reading
`0000000aa` (2020-01-01) for the first two rows, one citing
`NoSuchFile.hs:12` for the third, one citing a file you have just touched
for the fourth, two more with zero and two stamps, and one with a stamp
and no citation at all. Check the exit status without a pipe: `tail`
swallows it, which is how a first run of this recipe read five successes
that were four refusals. Reproduced 2026-07-29: rows in order 0, 0, 1, 1,
1, 1, with the file rewritten in the first row only -- and the hash it
wrote was the last commit touching `tools/heading-outline.py`, not HEAD,
which is the row that would have passed vacuously under the earlier
HEAD-based rule. Re-run 2026-07-31 with the seventh row added: 0, 0, 1,
1, 1, 1, 0, the last writing `c2872c219` and printing the advisory. Pick
that seventh row's document by what it cites, not by habit: the anchor a
citation of `tools/heading-outline.py` now yields is published, so that
scratch document drives the first two rows and no longer this one, which
needs a cited file whose newest commit is still unpushed -- any document
citing `CLAUDE.md` supplies one while this branch runs ahead of
`origin/master`. The
dirty-cited-file row needs no file deliberately touched -- point the
scratch document at whatever the working tree already has modified.

Seven failing kinds here, six in the horde-ad copy: the AMBIGUOUS row
needs a basename shared by two files *and* absent from the repo root,
since `resolve` returns at its `os.path.exists` check before reaching the
ambiguity branch. `LoopM.hs`, in both `Client/` and `Server/`, is such a
pair; `CLAUDE.md`, in the root and in `test/`, is not, and neither is
horde-ad's `bench/`-and-`test/` pair, which the root file shadows the
same way. So this is the only live proof of that branch — keep the row
even if the duplicate is ever resolved. In code the two copies differ in
SEARCH_ROOTS and in nothing else; their docstrings differ further than
dates, controls and worked examples, so don't read a divergence there as
one copy having fallen behind. Each names the branches its own repo
cannot exercise, which are not the same branches; the horde-ad copy
states two facts about the shared code that this one leaves out, that a
stamp's date is the day of the reading rather than of the commit it
names, and that extracted citations are deduplicated; and the refusal
table here has a seventh row its copy lacks, for an advisory both of them
print.
"""

import datetime
import os
import re
import subprocess
import sys

SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".github", ".claude", "."]
# The ref a pinned commit has to be reachable from to count as published.
# `git show` is an object-database lookup with no reachability requirement,
# so without this a link or stamp naming an unpushed or squashed-away
# commit resolves here and nowhere else.
PUBLISHED_REF = "origin/master"
CITE_RE = re.compile(
    r"`?(\.?[A-Za-z][A-Za-z0-9_./-]*"
    r"\.(?:hs|ts|py|c|h|cabal|mjs|html|md|txt|yaml|yml)|Makefile)"
    r":(\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*)")
URL_RE = re.compile(
    r"https://github\.com/[\w.-]+/[\w.-]+/blob/([0-9a-f]{7,40})/"
    r"([A-Za-z0-9_./-]+)#L(\d+)(?:-L(\d+))?")
# The document's own stamp, in either shape the documents use: an inline
# `hash` or a blockquoted **hash**, always followed by an ISO date in
# parentheses. The date is what keeps this from matching the other commit
# hashes documents mention (a bug's commit, a baseline's commit).
STAMP_RE = re.compile(
    r"(verified against[^`*]{0,120}?commit\s*>?\s*(?:`|\*\*))"
    r"([0-9a-f]{7,40})"
    r"((?:`|\*\*)\s*\()(\d{4}-\d{2}-\d{2})(\))")


def spans(spec):
    """Expand a citation's line spec into (lo, hi) pairs.

    A spec is one or more lines or ranges, comma-separated:
    "353", "377-381", "119,1271,1359", "353,362,771-781".
    """
    out = []
    for part in spec.split(","):
        lo, _, hi = part.partition("-")
        out.append((int(lo), int(hi or lo)))
    return out


def reachable_from(sha, ref):
    """Is sha an ancestor of ref? None if ref does not exist here."""
    if subprocess.run(["git", "rev-parse", "--verify", "--quiet",
                       f"{ref}^{{commit}}"], capture_output=True).returncode:
        return None
    return subprocess.run(["git", "merge-base", "--is-ancestor", sha, ref],
                          capture_output=True).returncode == 0


def published(sha):
    """Is sha reachable from PUBLISHED_REF? None if that ref is absent."""
    return reachable_from(sha, PUBLISHED_REF)


def all_files_named(basename):
    out = subprocess.run(
        ["bash", "-c",
         "find " + " ".join(SEARCH_ROOTS[:-1])
         + f" -name {basename} 2>/dev/null; ls {basename} 2>/dev/null"],
        capture_output=True, text=True).stdout.split()
    return sorted(set(out))


def resolve(name):
    """Return (path, error) — exactly one of the two is None."""
    if "/.../" in name:
        prefix, suffix = name.split("/.../", 1)
        hits = [h for h in all_files_named(os.path.basename(name))
                if h.startswith(prefix) and h.endswith("/" + suffix)]
        if len(hits) == 1:
            return hits[0], None
        return None, f"wildcard resolves to {hits or 'nothing'}"
    if os.path.exists(name):
        return name, None
    hits = [h for h in all_files_named(os.path.basename(name))
            if h.endswith("/" + name)]
    if len(hits) == 1:
        return hits[0], None
    if not hits:
        return None, "UNRESOLVED"
    return None, f"AMBIGUOUS: {hits} — qualify the citation"


def require_readable(paths):
    """Exit cleanly on a mistyped name rather than with a traceback.

    Exit 2 means the run did not happen, as distinct from 1, which means
    it ran and found something.
    """
    for p in paths:
        if not os.path.isfile(p):
            print(f"no such document: {p}", file=sys.stderr)
            sys.exit(2)


def restamp(doc, text, cited_paths, failures):
    """Rewrite the document's own stamp to name HEAD. Refuse if unsure.

    Returns the exit status. Writes at most one file, and only when the
    pass was clean, the cited files are unmodified against HEAD (so that
    a commit really is what got checked) and the document holds exactly
    one stamp.
    """
    if failures:
        print(f"\nnot restamping {doc}: {failures} citation(s) failed")
        return 1
    others = sorted({p for p in cited_paths if os.path.abspath(p)
                     != os.path.abspath(doc)})
    if others:
        # --porcelain, never colourised, unlike --short
        dirty = [ln for ln in subprocess.run(
            ["git", "status", "--porcelain", "--"] + others,
            capture_output=True, text=True).stdout.splitlines() if ln.strip()]
        if dirty:
            print(f"\nnot restamping {doc}: cited files differ from HEAD, so"
                  f" the pass verified the working tree rather than a commit:")
            for ln in dirty:
                print("   " + ln)
            return 1
    stamps = list(STAMP_RE.finditer(text))
    if len(stamps) != 1:
        which = "no stamp" if not stamps else f"{len(stamps)} stamps"
        print(f"\nnot restamping {doc}: {which} found — a stamp reads"
              f' "verified against ... commit `<hash>` (<date>)"')
        return 1
    if not others:
        print(f"\nnot restamping {doc}: no file:line citations, so no commit"
              f" to name")
        return 1
    # The stamp names the commit the cited *code* comes from, not HEAD: the
    # newest commit touching anything the document cites. That is what makes
    # it survive amending or replaying the commit that carries the document,
    # which touches no cited file and so cannot move the answer.
    anchor = subprocess.run(
        ["git", "log", "-1", "--format=%h", "--abbrev=9", "--"] + others,
        capture_output=True, text=True).stdout.strip()
    if not anchor:
        print(f"\nnot restamping {doc}: no commit in history touches its"
              f" cited files")
        return 1
    today = datetime.date.today().isoformat()
    m = stamps[0]
    if (m.group(2), m.group(4)) == (anchor, today):
        print(f"\n{doc}: stamp already names {anchor} ({today}) — unchanged")
        return 0
    open(doc, "w", encoding="utf-8").write(
        text[:m.start()] + m.group(1) + anchor + m.group(3) + today
        + m.group(5) + text[m.end():])
    print(f"\n{doc}: stamp {m.group(2)} ({m.group(4)})"
          f" -> {anchor} ({today})")
    # Written, not refused: leaving an orphaned stamp in place would be
    # strictly worse than naming a commit that is merely unpushed. But the
    # unpushed state is exactly what a later squash turns into an orphan,
    # so it is said every time rather than left to be remembered.
    if published(anchor) is False:
        print(f"  advisory: {anchor} is not on {PUBLISHED_REF}. Push this"
              f" branch without rewriting that commit, or re-run --restamp"
              f" after the push; a squash before it orphans the stamp.")
    return 0


def main():
    flags = {a for a in sys.argv[1:] if a.startswith("--")}
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    unknown = flags - {"--restamp"}
    if unknown:
        print(f"unknown flag(s): {' '.join(sorted(unknown))};"
              f" only --restamp is understood", file=sys.stderr)
        sys.exit(2)
    doc = args[0] if args else "CLAUDE.md"
    require_readable([doc])
    text = open(doc, encoding="utf-8").read()
    cites = sorted({(m.group(1),) + span
                    for m in CITE_RE.finditer(text)
                    for span in spans(m.group(2))})
    failures = 0
    for name, lo, hi in cites:
        path, err = resolve(name)
        if err:
            print(f"FAIL {name}:{lo}-{hi} — {err}")
            failures += 1
            continue
        lines = open(path, encoding="utf-8",
                     errors="replace").read().splitlines()
        if hi > len(lines):
            print(f"FAIL {name}:{lo}-{hi} — OUT-OF-RANGE "
                  f"(file has {len(lines)} lines)")
            failures += 1
            continue
        span = f"{lo}" if lo == hi else f"{lo}-{hi}"
        print(f"ok   {name}:{span} | {lines[lo - 1].strip()[:80]}")
    urlcites = sorted({(m.group(1), m.group(2), int(m.group(3)),
                        int(m.group(4) or m.group(3)))
                       for m in URL_RE.finditer(text)})
    for sha, path, lo, hi in urlcites:
        proc = subprocess.run(["git", "show", f"{sha}:{path}"],
                              capture_output=True, text=True)
        if proc.returncode != 0:
            print(f"FAIL {path}#L{lo}-L{hi} @ {sha[:9]} — commit or path"
                  f" not in this repository")
            failures += 1
            continue
        lines = proc.stdout.splitlines()
        if hi > len(lines):
            print(f"FAIL {path}#L{lo}-L{hi} @ {sha[:9]} — OUT-OF-RANGE "
                  f"(file has {len(lines)} lines at that commit)")
            failures += 1
            continue
        pub = published(sha)
        if pub is None:
            print(f"stopping: {PUBLISHED_REF} does not exist here, so"
                  f" whether {sha[:9]} is published cannot be told."
                  f" Fetch it, or set PUBLISHED_REF to the ref this"
                  f" repository publishes from.", file=sys.stderr)
            sys.exit(2)
        if not pub:
            print(f"FAIL {path}#L{lo}-L{hi} @ {sha[:9]} — UNPUBLISHED"
                  f" (resolves here but is not an ancestor of"
                  f" {PUBLISHED_REF}, so the link 404s for everyone else)")
            failures += 1
            continue
        span = f"L{lo}" if lo == hi else f"L{lo}-L{hi}"
        print(f"ok   {path}#{span} @ {sha[:9]}"
              f" | {lines[lo - 1].strip()[:70]}")
    # Counted apart from citation failures: --restamp is gated on those,
    # and an orphaned stamp is the very thing it repairs, so folding it in
    # would make a bad stamp block its own fix.
    stamp_failures = 0
    found = list(STAMP_RE.finditer(text))
    # Only a document's *own* stamp is checked, and a document has one.
    # Several means it is quoting other documents' stamps -- which a
    # findings or handover document does -- and a quotation is not a claim
    # about this file's tree. This is the same precondition --restamp
    # enforces, so the two agree on what counts as a stamp.
    if len(found) > 1:
        print(f"note {len(found)} stamps found — quotations, not this"
              f" document's own; none checked")
        found = []
    for m in found:
        sha = m.group(2)
        if reachable_from(sha, "HEAD") is False:
            print(f"FAIL stamp @ {sha[:9]} — ORPHANED (not an ancestor of"
                  f" HEAD; a squash or amend dropped it, so no clone can"
                  f" resolve the tree this document claims to name)")
            stamp_failures += 1
        elif published(sha) is False:
            print(f"note stamp @ {sha[:9]} — not on {PUBLISHED_REF} yet;"
                  f" sound only if this branch is pushed without"
                  f" rewriting that commit")
    print(f"\n{len(cites) + len(urlcites)} citations checked,"
          f" {failures} failed"
          f" — now eyeball the snippets against the document's claims.")
    if "--restamp" in flags:
        resolved = [resolve(name)[0] for name, _lo, _hi in cites]
        return restamp(doc, text, [p for p in resolved if p], failures)
    return 1 if failures or stamp_failures else 0


if __name__ == "__main__":
    sys.exit(main())
