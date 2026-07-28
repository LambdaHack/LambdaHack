#!/usr/bin/env python3
"""Check that paths, Makefile targets and flags named in a document exist.

Usage: python3 tools/check-doc-refs.py [DOC]
DOC defaults to CLAUDE.md. Run from the repo root.

This is pass 2 of the document-verification discipline, mechanized. Pass 1
(`check-plan-citations.py`) resolves `file:line` citations; this one
resolves the things named *without* a line number, which is most of what a
document says about the repo.

Deliberately conservative: prose backticks hold a mix of paths, type
names, identifiers and code fragments, and a checker that tried to resolve
all of them would drown the real failures in noise and stop being read.
The per-repo settings live in one block at the top: search roots, the
options file, the owned module namespace, the allowlist path. Porting
this to another repository should mean editing that block and nothing
else, and each setting may be left empty, which switches its check off
rather than breaking the run.

Only five unambiguous shapes are checked, and anything else is counted as
unclassified rather than guessed at:

  paths    a backticked, space-free token that resolves — directly, as a
           glob, or as a unique-enough path suffix (`Frontend/ANSI.hs`).
           A token that resolves is reported ok whatever its shape, so
           extensionless ones (`cabal.project.local.development`) pass
           too; only a token that looks like a path (contains `/`, or
           ends in a known source/config extension) and does *not*
           resolve is a failure. Tokens starting with `~`, `/` or `..`
           name things outside the repo and are reported, not checked.
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
           Makefile's target list; a `*` in the name is a glob, so
           `make bench*` passes if any target matches.
  flags    `--flag` anywhere in the document, resolved against the
           `long "..."` options in Server/Commandline.hs. A flag that is
           not one of ours but occurs somewhere else in the repo (a cabal
           or hlint flag in the Makefile) is reported as external; one
           found nowhere is listed for eyeballing, never failed, because
           third-party tools own flags this repo never mentions. That
           corroborating grep skips tools/ as well as the documents: the
           non-vacuity recipe below names a bogus flag, and a checker
           that reads its own documentation as evidence would call it
           real — as this one did until the recipe was first run.

  cabal    `+name` against the flags declared in the repo's cabal
           file(s). Upgrade-only like modules, because prose reaches for
           a leading plus too — a size column reading "small (+spike)"
           must not be read as a flag that has gone missing.

Exit status is nonzero only for an unresolved path or an unknown `make`
target — the two kinds that are unambiguously this repo's own drift.

A planning document legitimately names things that do not exist yet, and
a superseded document is named as history, so tools/doc-refs-allow.txt
lists globs to report as "allow" instead of failing. Every entry there
carries its reason; an unexplained one hides the drift this checker
exists to find. Pass -v to also list the unclassified backticks.

Non-vacuity (per CLAUDE.md's "prove a checker non-vacuous"): feed it a
scratch document holding

    `engine-src/Game/LambdaHack/NoSuchModule.hs`   FAIL, unresolved path
    `make no-such-target`                          FAIL, unknown target
    `Game.LambdaHack.Client.NoSuch`                FAIL, unknown module
    `--noSuchFlag`                                 listed, never failed
    `CellStyle.hs`                                 allow-listed, not failed
    `+noSuchFlag`                                  upgrade-only, unclassified
    `Makefile`, `make play`, `--sniff`             controls, must pass
    `Client.UI`, `Definition.*`                    module controls
    `+with_expensive_assertions`                   cabal-flag control
    `Ability.SkMove`, `K.KM`                       upgrade-only, unclassified
    `blob/master`, `group/bench`, `KP_/`           must stay unclassified

and confirm exactly three failures and exit status 1. The controls matter
as much as the failures: without them an extractor that silently matches
nothing would look like a clean document. The last row guards the other
direction — those three are prose, and a checker that "resolves" them is
about to start crying wolf on every document in the repo.
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
ALLOW_FILE = "tools/doc-refs-allow.txt"
# --- end per-repo configuration --------------------------------------

PATH_EXT = ("hs", "ts", "mjs", "py", "cabal", "html", "md", "yaml", "yml",
            "json", "sh", "txt")

TICK_RE = re.compile(r"`([^`\n]+)`")
FENCE_RE = re.compile(r"^\s*(```|~~~)")
CITE_RE = re.compile(r":\d+(-\d+)?$")
MAKE_RE = re.compile(r"\bmake ([A-Za-z0-9_*.-]+)")
FLAG_RE = re.compile(r"(?<![\w-])--([A-Za-z][A-Za-z0-9-]*)")
TARGET_RE = re.compile(r"^([^\s:=#][^:=#]*?)\s*:(?!=)")
LONG_RE = re.compile(r'long "([^"]+)"')
MODULE_RE = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)+$")
# A `Foo.*` stem may be a single component: `Definition.*` is a directory.
STEM_RE = re.compile(r"^[A-Z][A-Za-z0-9_']*(\.[A-Z][A-Za-z0-9_']*)*$")
CABAL_FLAG_RE = re.compile(r"^flag\s+([A-Za-z][A-Za-z0-9_-]*)", re.M)
# A repo path cannot hold these; they mark URLs, templates and the
# brace shorthand the documents use (`HandleHuman{Local,Global}M.hs`).
NOT_IN_PATH = set("<>#?&=…{}")


def cabal_flags():
    """Flags declared in the repo's cabal file(s), for `+name` tokens."""
    flags = set()
    for path in glob.glob("*.cabal"):
        text = open(path, encoding="utf-8").read()
        flags.update(CABAL_FLAG_RE.findall(text))
    return flags


def repo_paths():
    """Every tracked-or-present path under the search roots, plus the root."""
    roots = [r for r in SEARCH_ROOTS if os.path.isdir(r)] or ["."]
    out = subprocess.run(
        ["bash", "-c", "find " + " ".join(roots)
         + " -not -path '*/node_modules/*' 2>/dev/null; ls -1 2>/dev/null"],
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
    # A lone trailing slash is not enough: PLAYING.md's `KP_/` is a keypad
    # key, not a directory. Demand an inner slash or a known first
    # component, so `ts-src/src/generated/` and `docs/` still count.
    dir_shaped = token.endswith("/") and (token.count("/") > 1
                                          or token[:-1] in top_level)
    return ext in PATH_EXT or dir_shaped or token.split("/")[0] in top_level


def command_text(text):
    """The parts of a document that quote commands, not prose.

    English says "make a rule unsatisfiable" and "make lines too long";
    only backticked spans and fenced blocks are read for `make` targets
    and flags.
    """
    parts = TICK_RE.findall(text)
    in_fence = False
    for line in text.splitlines():
        if FENCE_RE.match(line):
            in_fence = not in_fence
        elif in_fence:
            parts.append(line)
    return "\n".join(parts)


def module_path(token):
    """The file or directory a Haskell module name would live in.

    `Game.LambdaHack.Client.UI.FrameM` is a path spelled with dots, and
    the documents name modules far more often than files. A trailing `.*`
    means the directory: `Definition.*` is `Definition/`.
    """
    if token.endswith(".*"):
        stem = token[:-2]
        return stem.replace(".", "/") if STEM_RE.match(stem) else None
    return token.replace(".", "/") + ".hs" if MODULE_RE.match(token) else None


def is_allowed(token, globs):
    """Match an allow entry against both spellings of a module.

    An entry is written as a path, but the documents name the same thing
    as `Game.LambdaHack.Client.UI.Frontend.CellStyle` too, so each glob is
    also tried against the module's path and as a path suffix — one entry
    covers both, instead of drifting apart in two spellings.
    """
    candidates = [token]
    mpath = module_path(token)
    if mpath:
        candidates.append(mpath)
    return any(fnmatch.fnmatch(c, g) or fnmatch.fnmatch(c, "*/" + g)
               for c in candidates for g in globs)


def allowed():
    """Globs for names a document may state although they are absent."""
    paths, targets = [], set()
    if os.path.exists(ALLOW_FILE):
        for line in open(ALLOW_FILE, encoding="utf-8"):
            line = line.split("#", 1)[0].strip()
            if line.startswith("make "):
                targets.add(line[5:])
            elif line:
                paths.append(line)
    return paths, targets


def make_targets():
    targets = set()
    for line in open("Makefile", encoding="utf-8").read().splitlines():
        m = TARGET_RE.match(line)
        if m:
            targets.update(m.group(1).split())
    return targets


def our_flags():
    if not OPTIONS_FILE or not os.path.exists(OPTIONS_FILE):
        return set()
    text = open(OPTIONS_FILE, encoding="utf-8").read()
    return set(LONG_RE.findall(text))


def main():
    args = [a for a in sys.argv[1:] if a != "-v"]
    verbose = "-v" in sys.argv[1:]
    doc = args[0] if args else "CLAUDE.md"
    text = open(doc, encoding="utf-8").read()
    commands = command_text(text)
    top_level = {d for d in os.listdir(".") if os.path.isdir(d)}
    known = repo_paths()
    allow_paths, allow_targets = allowed()
    cabalflags = cabal_flags()
    failures = 0
    external, unclassified, unknown_flags = [], [], []

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
                unclassified.append(token)
        elif token[0] in "~/" or token.startswith(".."):
            external.append(token)
        elif resolves(token, known):
            print(f"ok   path   {token}")
        elif is_allowed(token, allow_paths):
            print(f"allow path   {token} — absent on purpose, see"
                  f" {ALLOW_FILE}")
        elif module_path(token):
            if resolves(module_path(token), known):
                print(f"ok   module {token}")
            elif OUR_NAMESPACE and token.startswith(OUR_NAMESPACE):
                print(f"FAIL module {token} — no such module in"
                      f" {OUR_NAMESPACE}*")
                failures += 1
            else:
                unclassified.append(token)
        elif path_shaped(token, top_level):
            print(f"FAIL path   {token} — does not resolve")
            failures += 1
        else:
            unclassified.append(token)

    targets = make_targets()
    for name in sorted(set(MAKE_RE.findall(commands))):
        if name in targets or ("*" in name and
                               fnmatch.filter(targets, name)):
            print(f"ok   target make {name}")
        elif name in allow_targets:
            print(f"allow target make {name} — absent on purpose, see"
                  f" {ALLOW_FILE}")
        else:
            print(f"FAIL target make {name} — no such Makefile target")
            failures += 1

    ours = our_flags()
    for flag in sorted(set(FLAG_RE.findall(commands))):
        if flag in ours:
            print(f"ok   flag   --{flag}")
        elif subprocess.run(["git", "grep", "-qF", "--", "--" + flag,
                             "--", ":!*.md", ":!tools/"]).returncode == 0:
            print(f"ok   flag   --{flag} (external tool, used in the repo)")
        else:
            unknown_flags.append(flag)

    for label, items, always in (
            ("outside the repo, not checked", external, True),
            ("flags not found in the repo — eyeball these",
             unknown_flags, True),
            ("unclassified backticks, not checked", unclassified, False)):
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
