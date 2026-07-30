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

Scope limits, deliberate: prose-style citations ("config.ui.default line
67") are not extracted, nor is a range left dangling from its filename
("`LambdaHack.cabal:152-156` and `371-391`") — a bare `371-391` in
backticks is indistinguishable from any other pair of numbers, so a
document that wants the second range checked must repeat the filename.
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
HEAD-based rule.

Seven here, six in the horde-ad copy: the AMBIGUOUS row needs two files
sharing a basename, which this repo has (`LoopM.hs` in both `Client/`
and `Server/`) and that one has nowhere. So this is the only live proof
of that branch — keep the row even if the duplicate is ever resolved.
The two copies otherwise differ only in SEARCH_ROOTS.
"""

import datetime
import os
import re
import subprocess
import sys

SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".github", ".claude", "."]
CITE_RE = re.compile(
    r"`?([A-Za-z][A-Za-z0-9_./-]*"
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
        span = f"L{lo}" if lo == hi else f"L{lo}-L{hi}"
        print(f"ok   {path}#{span} @ {sha[:9]}"
              f" | {lines[lo - 1].strip()[:70]}")
    print(f"\n{len(cites) + len(urlcites)} citations checked,"
          f" {failures} failed"
          f" — now eyeball the snippets against the document's claims.")
    if "--restamp" in flags:
        resolved = [resolve(name)[0] for name, _lo, _hi in cites]
        return restamp(doc, text, [p for p in resolved if p], failures)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
