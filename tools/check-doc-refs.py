#!/usr/bin/env python3
"""Check that paths, build targets and flags named in a document exist.

Usage: python3 tools/check-doc-refs.py [DOC ...]
DOC defaults to CLAUDE.md. Run from the repo root.

This is pass 2 of the document-verification discipline (the
`doc-verification` skill), mechanized. Pass 1
(`check-plan-citations.py`) resolves `file:line` citations; this one
resolves the things named *without* a line number, which is most of what
a document says about the repo. Pass 3, the quantified-claims grep, is by
hand.

Deliberately conservative: prose backticks hold a mix of paths, type
names, identifiers and code fragments, and a checker that tried to resolve
all of them would drown the real failures in noise and stop being read.
The per-repo settings live in one block at the top: search roots, the
options file, the owned module namespace, the makefile, the allowlist
path. Porting this to another repository should mean editing that block
and nothing else, and each setting may be left empty, which switches its
check off rather than breaking the run.

Only six unambiguous shapes are checked, and anything else is counted as
unclassified rather than guessed at:

  paths    a backticked, space-free token that resolves — directly, as a
           glob, or as a unique-enough path suffix (`Frontend/ANSI.hs`,
           which is how the documents abbreviate). A token that resolves
           is reported ok whatever its shape, so extensionless ones
           (`cabal.project.local.development`) pass too; only a token that
           looks like a path (contains `/`, or ends in a known
           source/config extension) and does *not* resolve is a failure.
           Tokens starting with `~` or `/` name things outside any
           checkout and are reported, not checked; `../` ones and bare
           upstream names go to the sibling checkouts — see the sibling
           policy below.
  modules  a dotted name whose every component is capitalised, resolved
           as the path it spells (`Definition.*` as a directory). This
           one is upgrade-only: `Ability.SkMove` and `K.KM` are qualified
           references shaped exactly like module names, so an unresolved
           one merely stays unclassified. The single exception is our own
           `Game.LambdaHack.*` namespace, where nothing else has that
           shape, so an unresolved name there is a failure — that is the
           only place a renamed or misspelt module gets caught
           mechanically.
  targets  `make <target>` anywhere in the document, resolved against the
           makefile's target list; a `*` in the name is a glob, so
           `make bench*` passes if any target matches. Off in a repo that
           has no makefile.
  cabal    `cabal test|bench|build|haddock|run <name>` resolved against
           the stanza names declared in the repo's cabal file(s), plus the
           package name itself. Only a name immediately following the
           subcommand is read, so the flag-first spellings the documents
           also use (`cabal test --enable-optimization`) are skipped
           rather than guessed at.
  flags    `--flag` anywhere in the document, resolved against the
           `long "..."` options of OPTIONS_FILE. A flag that is not one of
           ours but occurs somewhere else in the repo (a cabal or hlint
           flag in a workflow) is reported as external; one found nowhere
           is listed for eyeballing, never failed, because third-party
           tools own flags this repo never mentions. That corroborating
           grep skips tools/ as well as the documents: the non-vacuity
           recipe below names a bogus flag, and a checker that reads its
           own documentation as evidence would call it real — as this one
           did until the recipe was first run.
  cabal    `+name` against the flags declared in the repo's cabal
  flags    file(s). Upgrade-only like modules, because prose reaches for a
           leading plus too — a size column reading "small (+spike)" must
           not be read as a flag that has gone missing.

Exit 1 for an unresolved path, an unknown `make` target or an unknown
cabal target — the kinds that are unambiguously this repo's own drift.
Exit 2 means the run did not happen at all: a document that cannot be
read, or an absent sibling checkout. Nothing else fails the run.

Sibling policy (Mikolaj's ruling, 2026-07-29): **use `../foo` whenever it
is available; if it is not, flag it and stop.** So SIBLING_ROOTS — here
the deployment repo `../lambdahack.github.io` — are resolved for real,
and a name that is no longer there fails like any other drift. When a
configured sibling is absent the run exits 2 without checking anything,
rather than quietly downgrading to a weaker check.

Allowlisting upstream names was the earlier design, and it "checks only
that a name is spelled the way the allowlist spells it": a rename on the
other side sailed through. A stop is louder than a shrug.

To proceed with a sibling missing, pass --without-siblings — a human
decision, not this script's default, and one whose cost the recipe below
spells out. Don't take that route unprompted.

A planning document legitimately names things that do not exist yet, and
a superseded document is named as history, so tools/doc-refs-allow.txt
lists globs to report as "allow" instead of failing. Every entry there
carries its reason; an unexplained one hides the drift this checker
exists to find. Pass -v to also list the unclassified backticks.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): feed it a
scratch document holding

    `engine-src/Game/LambdaHack/NoSuchModule.hs`  FAIL, unresolved path
    `make no-such-target`                  FAIL, unknown make target
    `cabal build no-such-stanza`            FAIL, unknown cabal target
    `Game.LambdaHack.Client.NoSuch`        FAIL, unknown module
    `../lambdahack.github.io/no-such.js`   FAIL, missing in a live sibling
    `--noSuchFlag`                         listed, never failed
    `+noSuchFlag`                          upgrade-only, unclassified
    `CellStyle.hs`                         allow-listed, not failed
    `ghc_wasm_jsffi.mjs`                   ok, resolved in the sibling
    `Makefile`, `make play`, `--sniff`     controls, must pass
    `cabal run LambdaHack`                 cabal-target control
    `wasm32-wasi-cabal build exe:LambdaHack`  wrapper + component prefix
    `Client.UI`, `Definition.*`            module controls
    `+with_expensive_assertions`           cabal-flag control
    `Ability.SkMove`, `K.KM`               upgrade-only, unclassified
    `blob/master`, `group/bench`, `KP_/`   must stay unclassified
    `.../ghc-9.12/...`                     an elision, not a sibling path

and confirm exactly five failures and exit status 1. The controls matter
as much as the failures: without them an extractor that silently matches
nothing would look like a clean document. The last rows guard the other
direction — those are prose, and a checker that "resolves" them is about
to start crying wolf on every document in the repo. Two of them are not
decoration but recorded false positives: the plain `\\bcabal` pattern
matched inside `wasm32-wasi-cabal` and read `exe:LambdaHack` as a stanza
called "exe", and `.../ghc-9.12/...` was read as a sibling path until the
`../` test was made to require the slash.

Then three runs for the ways a run can fail to happen. Pass a document
name that does not exist and confirm one line on stderr and exit 2, not
a traceback. Point SIBLING_ROOTS at a name that does not exist and
confirm BLOCKED with exit 2 and no checking at all; re-run that with
--without-siblings and confirm it proceeds, the sibling rows downgrading
to SKIP/UNVERIFIED rather than failing. That
second run also shows the cost, which is why the recipe keeps it visible:
the genuinely-local `engine-src/.../NoSuchModule.hs` downgrades to SKIP
as well, because without the checkouts nothing tells a missing local file
from an upstream one.

Reproduced 2026-07-29: five failures and exit 1, then exit 2, then the
degraded run.

The horde-ad copy of this script differs only in the configuration block
and in this recipe. Two branches this recipe exercises are dead there —
that repo has no makefile and no command-line parser — so the `make` and
`--flag` cases are proven here and nowhere else; keep them.
"""

import fnmatch
import glob
import os
import re
import subprocess
import sys

# --- per-repo configuration -----------------------------------------
# Porting this script to another repository should mean editing this
# block and nothing else; everything below it is repo-agnostic. Each
# setting may be left empty, which switches its check off rather than
# breaking the run.
SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".claude", ".github"]
# Where the executable's `long "…"` options are declared; "" if the repo
# ships no command-line parser, in which case every `--flag` falls
# through to the external/unknown buckets.
OPTIONS_FILE = "engine-src/Game/LambdaHack/Server/Commandline.hs"
# Module prefix owned by this repo, the one namespace where an
# unresolved module is a failure rather than a shrug; "" to disable.
OUR_NAMESPACE = "Game.LambdaHack."
# The file whose targets `make <target>` is resolved against; "" if the
# repo has no makefile, which switches that check off.
MAKEFILE = "Makefile"
# Sibling checkouts whose files this repo's documents cite. They are
# resolved for real whenever present — see the sibling policy above — and
# their absence stops the run rather than degrading it. [] to disable.
SIBLING_ROOTS = ["../lambdahack.github.io"]
ALLOW_FILE = "tools/doc-refs-allow.txt"
# --- end per-repo configuration --------------------------------------

PATH_EXT = ("hs", "ts", "mjs", "py", "cabal", "html", "md", "yaml", "yml",
            "json", "sh", "txt", "c", "h")

TICK_RE = re.compile(r"`([^`\n]+)`")
FENCE_RE = re.compile(r"^\s*(```|~~~)")
CITE_RE = re.compile(r":\d+(-\d+)?$")
MAKE_RE = re.compile(r"\bmake ([A-Za-z0-9_*.-]+)")
# A cabal invocation, possibly through a toolchain wrapper
# (`wasm32-wasi-cabal`), naming a stanza directly or through a component
# prefix (`exe:LambdaHack`). Without the wrapper and prefix cases, the
# wasm build lines read as a stanza literally named "exe".
CABAL_RE = re.compile(
    r"(?<![\w.-])(?:[\w.-]+-)?cabal (?:v2-)?"
    r"(?:test|bench|build|haddock|run)[ \t]+"
    r"(?:(?:exe|lib|test|bench|flib):)?([A-Za-z][\w-]*)")
FLAG_RE = re.compile(r"(?<![\w-])--([A-Za-z][A-Za-z0-9-]*)")
TARGET_RE = re.compile(r"^([^\s:=#][^:=#]*?)\s*:(?!=)")
LONG_RE = re.compile(r'long "([^"]+)"')
MODULE_RE = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)+$")
# A `Foo.*` stem may be a single component: `Definition.*` is a directory.
STEM_RE = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)*$")
CABAL_FLAG_RE = re.compile(r"^flag\s+([A-Za-z][A-Za-z0-9_-]*)", re.M)
CABAL_STANZA_RE = re.compile(
    r"^(?:test-suite|benchmark|library|executable)\s+(\S+)", re.M)
CABAL_NAME_RE = re.compile(r"^name:\s*(\S+)", re.M)
# A repo path cannot hold these; they mark URLs, templates and the
# brace shorthand the documents use (`HordeAd.OpsTensor{,Ranked}`).
NOT_IN_PATH = set("<>#?&=…{}")


def cabal_text():
    """The repo's cabal file(s), concatenated; "" if there are none."""
    return "\n".join(open(p, encoding="utf-8").read()
                     for p in sorted(glob.glob("*.cabal")))


def cabal_flags(text):
    """Flags declared in the repo's cabal file(s), for `+name` tokens."""
    return set(CABAL_FLAG_RE.findall(text))


def cabal_stanzas(text):
    """Buildable stanza names, plus the package name `cabal build` takes."""
    return set(CABAL_STANZA_RE.findall(text)) | set(CABAL_NAME_RE.findall(text))


def repo_paths():
    """Every tracked-or-present path under the search roots, plus the root."""
    roots = [r for r in SEARCH_ROOTS if os.path.isdir(r)] or ["."]
    out = subprocess.run(
        ["bash", "-c", "find " + " ".join(roots)
         + " -not -path '*/node_modules/*' 2>/dev/null; ls -1 2>/dev/null"],
        capture_output=True, text=True).stdout.split("\n")
    return [p for p in out if p]


def missing_siblings():
    """Configured sibling checkouts that are not on disk right now."""
    return [r for r in SIBLING_ROOTS if not os.path.isdir(r)]


def sibling_paths():
    """Every path under the sibling checkouts that are present."""
    roots = [r for r in SIBLING_ROOTS if os.path.isdir(r)]
    if not roots:
        return []
    out = subprocess.run(
        ["bash", "-c", "find " + " ".join(roots)
         + " -not -path '*/dist-newstyle/*' -not -path '*/.git/*'"
         + " -not -path '*/node_modules/*' 2>/dev/null"],
        capture_output=True, text=True).stdout.split("\n")
    return [p for p in out if p]


def resolves(token, known):
    if os.path.exists(token):
        return True
    if "*" in token and glob.glob(token):
        return True
    bare = token.rstrip("/")
    if "*" in bare:
        return any(fnmatch.fnmatch(p, "*" + bare) for p in known)
    return any(p == bare or p.endswith("/" + bare) for p in known)


def path_shaped(token, top_level):
    """Would a reader read this as a path in *this* repo?

    Not enough to contain a slash: `blob/master` and `group/bench` are
    prose. It must carry a known extension, end in a slash, or start at a
    directory that exists here.
    """
    if NOT_IN_PATH & set(token):
        return False
    ext = token.rsplit(".", 1)[-1] if "." in token[1:] else None
    # A lone trailing slash is not enough. Demand an inner slash or a
    # known first component, so `samplesData/` and `tools/` still count.
    dir_shaped = token.endswith("/") and (token.count("/") > 1
                                          or token[:-1] in top_level)
    return ext in PATH_EXT or dir_shaped or token.split("/")[0] in top_level


def command_text(text):
    """The parts of a document that quote commands, not prose.

    English says "make a rule unsatisfiable" and "make lines too long";
    only backticked spans and code blocks are read for `make` targets,
    cabal targets and flags.

    Both kinds of code block count. Reading fenced ones alone made this
    check almost vacuous where a document indents its blocks instead: of
    six `cabal test <suite>` lines in CLAUDE.md exactly one, written
    inline in backticks, was ever examined. An indented block is taken to
    be four or more spaces, which is markdown's rule; the cost is that a
    deeply nested list item is read as a command too, and since only
    `make`/`cabal`/`--flag` shapes are extracted from it, that is a
    missing check at worst, never a false failure.
    """
    parts = TICK_RE.findall(text)
    in_fence = False
    for line in text.splitlines():
        if FENCE_RE.match(line):
            in_fence = not in_fence
        elif in_fence or line.startswith("    ") or line.startswith("\t"):
            parts.append(line)
    return "\n".join(parts)


def module_path(token):
    """The file or directory a Haskell module name would live in.

    `HordeAd.Core.AstSimplify` is a path spelled with dots, and the
    documents name modules far more often than files. A trailing `.*`
    means the directory: `Definition.*` is `Definition/`.
    """
    if token.endswith(".*"):
        stem = token[:-2]
        return stem.replace(".", "/") if STEM_RE.match(stem) else None
    return token.replace(".", "/") + ".hs" if MODULE_RE.match(token) else None


def is_allowed(token, globs):
    """Match an allow entry against both spellings of a module.

    An entry is written as a path, but the documents name the same thing
    as `HordeAd.Core.AstTraverse` too, so each glob is also tried against
    the module's path and as a path suffix — one entry covers both,
    instead of drifting apart in two spellings.
    """
    candidates = [token]
    mpath = module_path(token)
    if mpath:
        candidates.append(mpath)
    return any(fnmatch.fnmatch(c, g) or fnmatch.fnmatch(c, "*/" + g)
               for c in candidates for g in globs)


def allowed():
    """Globs for names a document may state although they are absent.

    A line reading `make foo` or `cabal foo` allows that target instead of
    a path, so one file covers all three kinds without three formats.
    """
    paths, make, cabal = [], set(), set()
    if os.path.exists(ALLOW_FILE):
        for line in open(ALLOW_FILE, encoding="utf-8"):
            line = line.split("#", 1)[0].strip()
            if line.startswith("make "):
                make.add(line[5:])
            elif line.startswith("cabal "):
                cabal.add(line[6:])
            elif line:
                paths.append(line)
    return paths, make, cabal


def make_targets():
    if not MAKEFILE or not os.path.exists(MAKEFILE):
        return set()
    targets = set()
    for line in open(MAKEFILE, encoding="utf-8").read().splitlines():
        m = TARGET_RE.match(line)
        if m:
            targets.update(m.group(1).split())
    return targets


def our_flags():
    if not OPTIONS_FILE or not os.path.exists(OPTIONS_FILE):
        return set()
    text = open(OPTIONS_FILE, encoding="utf-8").read()
    return set(LONG_RE.findall(text))


def sibling_hit(token, siblings):
    """Where a bare upstream name resolves inside a sibling checkout.

    The documents abbreviate upstream files exactly as they abbreviate
    local ones (`Arith/Internal.hs` for ox-arrays'
    ops/Data/Array/Strided/Arith/Internal.hs), so the same suffix match
    applies; the hit is reported with its full path, which is the part a
    reader needs.
    """
    bare = token.rstrip("/")
    for p in siblings:
        if p == bare or p.endswith("/" + bare):
            return p
    return None


def check_doc(doc, known, top_level, allow_paths, cabalflags, siblings,
              sib_active, out):
    """Resolve every backticked token of one document. Returns failures."""
    text = open(doc, encoding="utf-8").read()
    failures = 0
    for token in sorted({t for t in TICK_RE.findall(text) if " " not in t}):
        if CITE_RE.search(token) or token.startswith("-"):
            continue                      # pass 1 and the flag pass own these
        if not any(c.isalnum() for c in token):
            continue                      # bare punctuation, e.g. `,` `<$>`
        if token.startswith("+"):
            # A cabal flag as the docs write it, `+with_expensive_assertions`.
            # Upgrade-only: prose reaches for a leading plus too (a size
            # column reading "small (+spike)"), so an unknown one is a
            # shrug, not a failure.
            if token[1:] in cabalflags:
                print(f"ok   flag   {token} (cabal flag)")
            else:
                out["unclassified"].append(token)
        elif token.startswith("../"):
            # A sibling-relative path; the trailing slash is required, or
            # a document's `.../ghc-9.12/...` elision is read as one.
            # The run has already stopped if the checkout it names is
            # absent, so here it is resolved for real and a miss is this
            # repo's drift, not a missing mount.
            if not sib_active:
                out["external"].append(token)
            elif os.path.exists(token):
                print(f"ok   sibling {token}")
            else:
                print(f"FAIL sibling {token} — no such path in the checkout")
                failures += 1
        elif token[0] in "~/":
            out["external"].append(token)
        elif resolves(token, known):
            print(f"ok   path   {token}")
        elif path_shaped(token, top_level) and sibling_hit(token, siblings):
            # Gated on path_shaped, or a bare prose word resolves against
            # the sheer size of a sibling tree: `tests` "found"
            # ../orthotope/tests and reported ok.
            print(f"ok   sibling {token} — {sibling_hit(token, siblings)}")
        elif is_allowed(token, allow_paths):
            print(f"allow path   {token} — absent on purpose, see"
                  f" {ALLOW_FILE}")
        elif module_path(token):
            if resolves(module_path(token), known):
                print(f"ok   module {token}")
            elif sibling_hit(module_path(token), siblings):
                # An upstream module named in our vocabulary, e.g.
                # `Data.Array.Strided.Arith`. Resolving it here is what
                # turns an upstream rename from invisible into a failure.
                print(f"ok   module {token} —"
                      f" {sibling_hit(module_path(token), siblings)}")
            elif OUR_NAMESPACE and token.startswith(OUR_NAMESPACE):
                print(f"FAIL module {token} — no such module in"
                      f" {OUR_NAMESPACE}*")
                failures += 1
            else:
                out["unclassified"].append(token)
        elif path_shaped(token, top_level):
            if sib_active:
                print(f"FAIL path   {token} — does not resolve")
                failures += 1
            else:
                # --without-siblings: this may be an upstream name that
                # would have resolved, so calling it drift would be a
                # guess. Flagged, not failed — the mode is the human
                # having said to carry on without the checkouts.
                print(f"SKIP path   {token} — unresolved locally, and"
                      f" siblings not consulted")
                out["unverified"].append(token)
        else:
            out["unclassified"].append(token)
    return failures


def check_commands(doc, targets, stanzas, ours, allow_make, allow_cabal, out):
    """Resolve the `make`, `cabal` and `--flag` mentions of one document."""
    commands = command_text(open(doc, encoding="utf-8").read())
    failures = 0

    if MAKEFILE:
        for name in sorted(set(MAKE_RE.findall(commands))):
            if name in targets or ("*" in name and
                                   fnmatch.filter(targets, name)):
                print(f"ok   target make {name}")
            elif name in allow_make:
                print(f"allow target make {name} — absent on purpose, see"
                      f" {ALLOW_FILE}")
            else:
                print(f"FAIL target make {name} — no such makefile target")
                failures += 1

    for name in sorted(set(CABAL_RE.findall(commands))):
        if name in stanzas:
            print(f"ok   target cabal {name}")
        elif name in allow_cabal:
            print(f"allow target cabal {name} — absent on purpose, see"
                  f" {ALLOW_FILE}")
        else:
            print(f"FAIL target cabal {name} — no such cabal stanza")
            failures += 1

    for flag in sorted(set(FLAG_RE.findall(commands))):
        if flag in ours:
            print(f"ok   flag   --{flag}")
        elif subprocess.run(["git", "grep", "-qF", "--", "--" + flag,
                             "--", ":!*.md", ":!tools/"]).returncode == 0:
            print(f"ok   flag   --{flag} (external tool, used in the repo)")
        else:
            out["unknown_flags"].append(flag)
    return failures


def require_readable(paths):
    """Exit cleanly on a mistyped name rather than with a traceback.

    Exit 2 means the run did not happen, which is also what an absent
    sibling reports, as distinct from 1, which means it ran and found
    something.
    """
    for p in paths:
        if not os.path.isfile(p):
            print(f"no such document: {p}", file=sys.stderr)
            sys.exit(2)


def main():
    flags = {"-v", "--without-siblings"}
    args = [a for a in sys.argv[1:] if a not in flags]
    verbose = "-v" in sys.argv[1:]
    no_siblings = "--without-siblings" in sys.argv[1:]
    docs = args or ["CLAUDE.md"]
    require_readable(docs)

    missing = [] if no_siblings else missing_siblings()
    if missing:
        print("BLOCKED — sibling checkout(s) not available: "
              + ", ".join(missing))
        print("\nThe documents cite files in these, and the policy is to"
              " resolve them for real\nrather than take their spelling on"
              " trust. Mount them and re-run.")
        print("\nTo proceed without them anyway — which downgrades every"
              " upstream reference to\nan unchecked one, verifiable only"
              " by hand against the cabal store — re-run\nwith"
              " --without-siblings.")
        return 2

    top_level = {d for d in os.listdir(".") if os.path.isdir(d)}
    known = repo_paths()
    siblings = [] if no_siblings else sibling_paths()
    allow_paths, allow_make, allow_cabal = allowed()
    text = cabal_text()
    cabalflags, stanzas = cabal_flags(text), cabal_stanzas(text)
    targets, ours = make_targets(), our_flags()
    out = {"external": [], "unclassified": [], "unknown_flags": [],
           "unverified": []}
    failures = 0
    if no_siblings and SIBLING_ROOTS:
        print("NOTE: --without-siblings, so upstream references are not"
              " resolved here.\n")

    for doc in docs:
        if len(docs) > 1:
            print(f"\n=== {doc} ===")
        failures += check_doc(doc, known, top_level, allow_paths,
                              cabalflags, siblings,
                              bool(SIBLING_ROOTS) and not no_siblings, out)
        failures += check_commands(doc, targets, stanzas, ours,
                                   allow_make, allow_cabal, out)

    for label, items, always in (
            ("UNVERIFIED — would have been resolved against the sibling"
             " checkouts", out["unverified"], True),
            ("outside the repo, not checked", out["external"], True),
            ("flags not found in the repo — eyeball these",
             out["unknown_flags"], True),
            ("unclassified backticks, not checked",
             out["unclassified"], False)):
        items = sorted(set(items))
        if not items:
            continue
        print(f"\n{len(items)} {label}"
              + (":" if always or verbose else " (-v to list)"))
        if always or verbose:
            print("  " + ", ".join(items))
    print(f"\n{failures} failed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
