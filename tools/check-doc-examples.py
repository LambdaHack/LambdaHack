#!/usr/bin/env python3
"""Check that a document's example code still matches the library.

Usage: python3 tools/check-doc-examples.py [DOC ...]
DOC defaults to README.md. Run from the repo root.

This covers what the other two checkers structurally cannot. They read
*backticked* tokens and resolve paths, modules, targets and flags; a
document's illustrative Haskell lives in fenced blocks, unbackticked, and
its claim is not "this name exists" but "this is what the library does".
Two shapes of that claim are mechanical:

  types    a capitalised name in a ```hs block that is neither defined in
           the document itself -- including the constructors of a `data`
           it declares -- nor present in any tracked .hs file. That is a
           name the example invents or has outlived.
  outputs  a `>>> expr` block whose printed result does not occur in any
           tracked .hs file. The results shown in a README are usually
           pinned by a test somewhere; when the library's output moves and
           the test is updated, the document is what silently stops
           matching. Whitespace and `;` are normalised away, because a
           document pretty-prints across lines what a test holds as one
           string with explicit separators.

Both are deliberately narrow. What they cannot see is the more common
defect: an example naming a real thing that is nonetheless the *wrong*
real thing. Where the wrong name and the right one both exist in the
sources, no existence check can choose between them; only the signature
in question settles it. So a clean run here is not a substitute for
reading the document, and must never be reported as one; it is the part
of that reading a machine can repeat.

Exit 1 if anything is unresolved, 2 if the run did not happen (a document
that cannot be read), matching the other checkers.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): run

    python3 tools/check-doc-examples.py --self-test

It builds the control document itself, against this repo's real source
list, and confirms both branches fire -- one type finding, one output
finding -- while none of the passing controls is reported. Exit 0 on
PASS, 1 on FAIL. Building it beats writing one out by hand, which gets
skipped, or assembled a little differently each time and then proves
whatever that day's document happened to hold.

The self-test was itself proved non-vacuous by breaking each branch in a
copy: blanking the name pattern, disabling the output comparison, and
dropping the doc-local exclusion turned it red at 0/1, 1/0 and 2/1
findings. A control that cannot fail proves nothing, self-tests included.

Two things the self-test cannot cover here. No document in this repo has
a `>>>` block, so the outputs branch has no *live document* to run
against -- keep the branch anyway; the horde-ad copy uses it. And for the
types branch there are live positive controls worth running: on the four
documents with fenced Haskell it should extract and resolve 18, 0, 12 and
6 capitalised names (leader-desync-bug, leader-desync-migration,
promptgetkey-hygiene, wasm-frontend-unified-plan) with none unresolved,
the count being the thing to check, since "0 unresolved" reads the same
whether the extractor works or finds nothing. The zero is expected and is
its own control: that document's only fence is an all-comment haddock
draft, so a nonzero count there would mean comment stripping had stopped
working. The counts are not printed -- load the module and call
`strip_comments`/`NAME_RE` over `FENCE_RE` yourself.

The noise this had to survive, since a later reader may be tempted to
widen it: comments inside a fenced block contribute words like `NOTE`
and `MUTATES`, and promptgetkey-hygiene.md declares the `MacroStep`
constructors it is proposing. Stripping comments and treating a
document's own `data` constructors as local took twelve false positives
to zero across both repos, with horde-ad's one true positive still
reported.
"""

import contextlib
import io
import os
import re
import subprocess
import sys

# --- per-repo configuration -----------------------------------------
# The fenced-block languages whose contents are read as this repo's
# source language, and the command listing the sources to resolve
# against. Empty either one to switch its check off.
FENCE_LANGS = ("hs", "haskell")
SOURCE_LIST = 'git ls-files "*.hs"'
# --- end per-repo configuration --------------------------------------

FENCE_RE = re.compile(
    r"^```(?:" + "|".join(FENCE_LANGS) + r")\n(.*?)^```", re.S | re.M)
DECL_RE = re.compile(r"^(?:type|data|newtype|class)\s+([A-Z][A-Za-z0-9_]*)",
                     re.M)
CTOR_RE = re.compile(r"^(?:data|newtype)\s+[A-Z].*?=(.*?)(?=^\S|\Z)", re.S | re.M)
NAME_RE = re.compile(r"\b([A-Z][A-Za-z0-9_]{3,})\b")


def sources():
    return subprocess.run(["bash", "-c", "cat $(" + SOURCE_LIST + ")"],
                          capture_output=True, text=True).stdout


def strip_comments(code):
    code = re.sub(r"\{-.*?-\}", "", code, flags=re.S)
    return re.sub(r"--.*$", "", code, flags=re.M)


def local_names(code):
    """Names the document defines itself, so cannot be drift."""
    out = set(DECL_RE.findall(code))
    for m in CTOR_RE.finditer(code):
        out |= set(re.findall(r"\b([A-Z][A-Za-z0-9_]*)\b", m.group(1)))
    return out


def norm(s):
    """Whitespace- and separator-insensitive, for layout vs `;`."""
    return re.sub(r"\s+", "", s).replace(";", "")


def check_types(doc, text, src):
    if not FENCE_LANGS:
        return 0
    code = strip_comments("".join(FENCE_RE.findall(text)))
    if not code.strip():
        return 0
    loc = local_names(code)
    failures = 0
    for n in sorted(set(NAME_RE.findall(code))):
        if n in loc or re.search(r"\b" + n + r"\b", src):
            continue
        print(f"FAIL {doc}: type {n} — defined neither here nor in the sources")
        failures += 1
    return failures


def check_outputs(doc, text, src):
    lines = text.splitlines()
    normsrc = norm(src)
    failures = i = 0
    while i < len(lines):
        if not lines[i].startswith(">>>"):
            i += 1
            continue
        expr, out, j = lines[i], [], i + 1
        while j < len(lines) and not lines[j].startswith((">>>", "```")):
            out.append(lines[j])
            j += 1
        body = norm("\n".join(out))
        if len(body) > 40 and body not in normsrc:
            print(f"FAIL {doc}: output of `{expr[4:][:50]}`"
                  f" occurs in no tracked source")
            failures += 1
        i = j
    return failures


SELF_TEST_TYPE = "ControlTypeThatCannotExistAnywhere"
SELF_TEST_OUT = "control output present in no tracked source of this repository"


def self_test():
    """Build the control document and confirm every branch still fires.

    A recipe that must be assembled by hand is one that gets skipped, or
    assembled a little differently each time, and then proves whatever
    that day's document happened to contain. Building it here makes the
    control live in the sense that matters: it runs against this repo's
    real source list, so it exercises the extraction and the source
    lookup too, not merely the regexes.

    The passing controls are the half that catches a checker gone quiet.
    `Local` and its constructors are declared by the document, so
    reporting them would mean every design document that proposes a type
    drowns in its own; the ALL-CAPS comment word must not read as a type;
    and the last output is lifted verbatim from the sources, so failing
    to match it would mean the output branch had stopped resolving
    anything at all.
    """
    src = sources()
    real = next((ln.strip() for ln in src.splitlines()
                 if len(ln.strip()) > 45 and "`" not in ln), "")
    if not real:
        print("self-test: no usable source line found", file=sys.stderr)
        return 2
    doc = (
        "# check-doc-examples self-test control\n\n"
        "```hs\n"
        "-- NOTE ALL CAPS words in comments must not be read as types\n"
        "data Local = A | B\n"
        "useLocal :: Local -> Local\n"
        "useLocal A = B\n"
        f"useLocal _ = {SELF_TEST_TYPE}\n"
        "```\n\n"
        "```hs\n"
        ">>> controlExpr\n"
        f"{SELF_TEST_OUT}\n"
        "```\n\n"
        "```hs\n"
        ">>> realExpr\n"
        f"{real}\n"
        "```\n")
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        types = check_types("<self-test>", doc, src)
        outs = check_outputs("<self-test>", doc, src)
    lines = [ln for ln in buf.getvalue().splitlines() if ln.strip()]
    ok = (types == 1 and outs == 1
          and any(SELF_TEST_TYPE in ln for ln in lines)
          and not any("Local" in ln or "NOTE" in ln for ln in lines))
    for ln in lines:
        print("  " + ln)
    print(f"\nself-test: {types} type finding(s), {outs} output finding(s),"
          f" expected 1 and 1")
    print("self-test: PASS — both branches fire and no control was reported"
          if ok else
          "self-test: FAIL — a branch has stopped firing, or a control"
          " was reported")
    return 0 if ok else 1


def require_readable(paths):
    """Exit cleanly on a mistyped name rather than with a traceback.

    Exit 2 means the run did not happen, as distinct from 1, which means
    it ran and found something.
    """
    for p in paths:
        if not os.path.isfile(p):
            print(f"no such document: {p}", file=sys.stderr)
            sys.exit(2)


def main():
    if "--self-test" in sys.argv[1:]:
        return self_test()
    docs = sys.argv[1:] or ["README.md"]
    require_readable(docs)
    src = sources()
    failures = 0
    for doc in docs:
        text = open(doc, encoding="utf-8").read()
        failures += check_types(doc, text, src)
        failures += check_outputs(doc, text, src)
    print(f"\n{failures} unresolved in {len(docs)} document(s)"
          f" — a clean run here still does not read the document for you.")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
