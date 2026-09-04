#!/usr/bin/env python3
"""Check that a document's example code still matches the library.

Usage: python3 tools/check-doc-examples.py [DOC ...]
DOC defaults to README.md. Runs from anywhere in the repository.

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
           string with explicit separators. A result of 40 normalised
           characters or fewer is not checked at all -- too short to be
           distinctive in this much source, it would match by coincidence
           -- so a short output rests on the reading alone.

A fenced block that declares `module Main` is skipped by the types branch: it
is a self-contained program (an issue reproducer, say) whose names resolve
against its own imports rather than this repo's sources. The self-test carries
a control for the skip. A reproducer that needs no `main` names some other
module instead, and that name is a declaration rather than a reference, so
`local_names` takes it: its own control is a block declaring a module and using
nothing.

Both are deliberately narrow. What they cannot see is the more common
defect: an example naming a real thing that is nonetheless the *wrong*
real thing. Where the wrong name and the right one both exist in the
sources, no existence check can choose between them; only the signature
in question settles it. So a clean run here is not a substitute for
reading the document, and must never be reported as one; it is the part
of that reading a machine can repeat.

Exit 1 if anything is unresolved, 2 if the run did not happen (a document
that cannot be read, or a SOURCE_LIST naming no file), matching the other
checkers.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): run

    python3 tools/check-doc-examples.py --self-test

It builds the control document itself, against this repo's real source list,
and confirms both branches fire -- one type finding, one output finding --
while none of the passing controls is reported, that a source list naming
nothing reads as none rather than as an empty corpus (added when a run from
`docs/` was found to hang on the old `cat $(...)` and, with stdin closed, to
fail every name), and that a run from a subdirectory reports what one from the
root does, the root run having happened: agreement at exit 2 is two runs that
did not. Exit 0 on PASS, 1 on FAIL. Building it beats writing one out by hand,
which gets skipped, or assembled a little differently each time and then proves
whatever that day's document happened to hold.

That the self-test bites is mutants.py's to show: a control that cannot fail
proves nothing, self-tests included.

Three things the self-test cannot cover here. No document in this repo
has a `>>>` block, so the outputs branch has no *live document* to run
against -- keep the branch anyway; the horde-ad copy uses it, its
README.md holding both shapes. Nor does any document here declare
`module Main`, so the skip above rests on its self-test control alone,
the live one being horde-ad's. And for the types branch there are live
positive controls worth running: on the four documents with fenced
Haskell it should extract and resolve 18, 0, 12 and 31 capitalised names
(leader-desync-bug, leader-desync-migration, promptgetkey-hygiene,
wasm-frontend-unified-plan; measured 2026-07-31) with none unresolved,
the count being the thing to check, since "0 unresolved" reads the same
whether the extractor works or finds nothing to work on. The zero is
expected and is its own control: that document's two fences are both
all-comment haddock drafts, so a nonzero count there would mean comment
stripping had stopped working. The counts are not printed -- load the
module and call `strip_comments`/`NAME_RE` over `FENCE_RE` yourself.
They move whenever those documents gain a fence, so re-take them with
the edit rather than reading a stale one as a pass: the 31 stood at 6
until it was re-measured here.

The noise this had to survive, since a later reader may be tempted to
widen it: comments inside a fenced block contribute words like `NOTE`
and `MUTATES`, and promptgetkey-hygiene.md declares the `MacroStep`
constructors it is proposing. Stripping comments and treating a
document's own `data` constructors as local took twelve false positives
to zero across both repos, with horde-ad's one true positive still
reported. Any widening should be measured the same way before it is
kept.
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


def chdir_root(paths):
    """Run from the repository root whatever the cwd -- the configuration's
    paths are root-relative -- and return PATHS rebased to it. Outside a
    repository nothing moves."""
    # answered dropped-status: an empty top is the failure, and the next line tests it
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                         capture_output=True, text=True).stdout.strip()
    if not top:
        return paths
    paths = [os.path.relpath(os.path.abspath(p), top) for p in paths]
    os.chdir(top)
    return paths


def sources():
    """The concatenated sources, or None where there are none to read.

    Listed from the repository root, so the cwd does not decide what the
    check sees, and read here rather than by `cat $(...)`: with nothing
    listed that cat read stdin, hanging on a terminal, and an empty corpus
    failed every name. None switches the check off (an empty SOURCE_LIST)
    or blocks the run (a list naming nothing); main tells them apart.
    """
    if not SOURCE_LIST:
        return None
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                         capture_output=True, text=True).stdout.strip()
    p = subprocess.run(["bash", "-c", SOURCE_LIST], capture_output=True,
                       text=True, cwd=top or None)
    paths = p.stdout.split()
    if p.returncode != 0 or not paths:
        return None
    return "".join(open(os.path.join(top or ".", f), encoding="utf-8",
                        errors="replace").read() for f in paths)


def strip_comments(code):
    code = re.sub(r"\{-.*?-\}", "", code, flags=re.S)
    return re.sub(r"--.*$", "", code, flags=re.M)


def local_names(code):
    """Names the document defines itself, so cannot be drift.

    A block's own `module` header is one of them: the name there is a
    declaration and not a reference, so a self-contained block that is not
    `module Main` -- an issue reproducer that needs no `main` -- must not be
    failed for naming itself. The self-test's control is such a block.
    """
    out = set(DECL_RE.findall(code))
    for m in re.finditer(r"^module\s+([\w.]+)", code, re.M):
        out |= set(m.group(1).split("."))
    for m in CTOR_RE.finditer(code):
        out |= set(re.findall(r"\b([A-Z][A-Za-z0-9_]*)\b", m.group(1)))
    return out


def norm(s):
    """Whitespace- and separator-insensitive, for layout vs `;`."""
    return re.sub(r"\s+", "", s).replace(";", "")


def check_types(doc, text, src):
    if not FENCE_LANGS or src is None:
        return 0
    # A fence that declares `module Main` is a self-contained program (an
    # issue reproducer, say), not an excerpt of this repo's API: its names
    # resolve against its own imports, which this checker cannot see, so
    # the block is skipped rather than failed.
    bodies = [b for b in FENCE_RE.findall(text)
              if not re.search(r"^module\s+Main\b", b, re.M)]
    code = strip_comments("".join(bodies))
    if not code.strip():
        return 0
    loc = local_names(code)
    failures = 0
    for n in sorted(set(NAME_RE.findall(code))):
        if n in loc or re.search(r"\b" + n + r"\b", src):
            continue
        print(f"FAIL {doc}: type {n} --- defined neither here nor in the sources")
        failures += 1
    return failures


def check_outputs(doc, text, src):
    if src is None:
        return 0
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
SELF_TEST_SKIP = "ControlTypeInsideStandaloneProgram"
SELF_TEST_MOD = "ControlModuleNamingItself"
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
    if src is None:
        print("self-test: BLOCKED, SOURCE_LIST names no file",
              file=sys.stderr)
        return 2
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
        "module Main (main) where\n"
        f"standalone :: {SELF_TEST_SKIP}\n"
        "```\n\n"
        "```hs\n"
        f"module {SELF_TEST_MOD} (only) where\n"
        "only :: Int\n"
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
          and not any("Local" in ln or "NOTE" in ln
                      or SELF_TEST_SKIP in ln or SELF_TEST_MOD in ln
                      for ln in lines))
    for ln in lines:
        print("  " + ln)
    global SOURCE_LIST
    saved, SOURCE_LIST = SOURCE_LIST, "true"
    try:
        empty = sources()
    finally:
        SOURCE_LIST = saved
    if empty is not None:
        ok = False
        print("  a source list naming nothing did not come back as None")
    script = os.path.abspath(__file__)
    here = subprocess.run([sys.executable, script, "README.md"],
                          capture_output=True, text=True)
    there = subprocess.run([sys.executable, script,
                            os.path.join("..", "README.md")],
                           capture_output=True, text=True,
                           cwd=os.path.dirname(script))
    if (here.returncode, here.stdout.strip().splitlines()[-1:]) != \
            (there.returncode, there.stdout.strip().splitlines()[-1:]):
        ok = False
        print("  a run from a subdirectory disagrees with one from the root")
    # Agreement at exit 2 is two runs that did not happen: with README.md
    # absent both said so alike and the row passed (check-doc-examples-05).
    if here.returncode == 2:
        ok = False
        print("  the from-the-root control did not run")
    print(f"\nself-test: {types} type finding(s), {outs} output finding(s),"
          f" expected 1 and 1")
    print("self-test: PASS --- both branches fire and no control was reported"
          if ok else
          "self-test: FAIL --- a branch has stopped firing, or a control"
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
    # From the root before anything, the self-test included: its
    # from-the-root control is a relative path, and dispatched first the
    # subdirectory-agreement row fired from any subdirectory on an unbroken
    # checker (check-doc-examples-04).
    args = [a for a in sys.argv[1:] if a != "--self-test"]
    docs = chdir_root(args)
    if "--self-test" in sys.argv[1:]:
        return self_test()
    docs = docs or ["README.md"]
    require_readable(docs)
    src = sources()
    if src is None and SOURCE_LIST:
        print(f"BLOCKED: {SOURCE_LIST!r} listed no file, nothing checked")
        return 2
    failures = 0
    for doc in docs:
        text = open(doc, encoding="utf-8").read()
        failures += check_types(doc, text, src)
        failures += check_outputs(doc, text, src)
    print(f"\n{failures} unresolved in {len(docs)} document(s)"
          f" --- a clean run here still does not read the document for you.")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())