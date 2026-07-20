#!/usr/bin/env python3
"""Check that file:line citations in a planning document still resolve.

Usage: python3 tools/check-plan-citations.py [DOC]
DOC defaults to CLAUDE.md. Run from the repo root.

For every citation of the form `path/to/File.hs:12` or `File.hs:12-34`
(also .ts, .cabal, .mjs, .html and the Makefile), the script resolves the
file, checks the line range exists, and prints the first cited line so a
human can compare it against what the surrounding sentence claims. A
`/.../` component in a cited path is treated as a wildcard (the document
uses it to abbreviate long paths).

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
67") are not extracted, and the *claims* around citations are not checked
— in particular, universally-quantified claims ("only X does Y", "exactly
two", "never") must be re-verified by repo-wide grep, not by re-reading
the cited file; that asymmetry is how a real error slipped in once.
"""

import os
import re
import subprocess
import sys

SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "."]
CITE_RE = re.compile(
    r"`?([A-Za-z][A-Za-z0-9_./-]*\.(?:hs|ts|cabal|mjs|html)|Makefile)"
    r":(\d+)(?:-(\d+))?")
URL_RE = re.compile(
    r"https://github\.com/[\w.-]+/[\w.-]+/blob/([0-9a-f]{7,40})/"
    r"([A-Za-z0-9_./-]+)#L(\d+)(?:-L(\d+))?")


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


def main():
    doc = sys.argv[1] if len(sys.argv) > 1 else "CLAUDE.md"
    text = open(doc, encoding="utf-8").read()
    cites = sorted({(m.group(1), int(m.group(2)),
                     int(m.group(3) or m.group(2)))
                    for m in CITE_RE.finditer(text)})
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
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
