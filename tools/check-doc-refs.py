#!/usr/bin/env python3
"""Check that paths, build targets and flags named in a document exist.

Usage: python3 tools/check-doc-refs.py [DOC ...]
DOC defaults to CLAUDE.md. Runs from anywhere in the repository.

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

  paths    a backticked, space-free token that resolves --- directly, as a
           glob, or as a unique-enough path suffix (`Frontend/ANSI.hs`,
           which is how the documents abbreviate). A token that resolves
           is reported ok whatever its shape, so extensionless ones
           (`cabal.project.local.development`) pass too; only a token that
           looks like a path (contains `/`, or ends in a known
           source/config extension) and does *not* resolve is a failure.
           Tokens starting with `~` or `/` name things outside any
           checkout and are reported, not checked; `../` ones and bare
           upstream names go to the sibling checkouts --- see the sibling
           policy below.
  modules  a dotted name whose every component is capitalised, resolved
           as the path it spells (`Definition.*` as a directory). This
           one is upgrade-only: `Ability.SkMove` and `K.KM` are qualified
           references shaped exactly like module names, so an unresolved
           one merely stays unclassified. The single exception is our own
           `Game.LambdaHack.*` namespace, where nothing else has that
           shape, so an unresolved name there is a failure --- that is the
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
           grep skips this script as well as the documents: the
           non-vacuity rows below name a bogus flag, and a checker that
           reads its own documentation as evidence would call it real ---
           as this one did until the recipe was first run. It skipped all
           of tools/ until 2026-08-28, which left every flag of the tools
           scripts in the eyeball list for good.
  cabal    `+name` against the flags declared in the repo's cabal
  flags    file(s). Upgrade-only like modules, because prose reaches for a
           leading plus too --- a size column reading "small (+spike)" must
           not be read as a flag that has gone missing.

Exit 1 for an unresolved path, an unknown `make` target or an unknown
cabal target --- the kinds that are unambiguously this repo's own drift.
Exit 2 means the run did not happen at all: a document that cannot be
read, or an absent sibling checkout. Nothing else fails the run.

Sibling policy (Mikolaj's ruling, 2026-07-29): **use `../foo` whenever it
is available; if it is not, flag it and stop.** So SIBLING_ROOTS --- here
the deployment repo `../lambdahack.github.io` --- are resolved for real,
a bare `ghc_wasm_jsffi.mjs` resolving to the file there and reported with
its full path, and a name that is no longer there fails like any other
drift. When a configured sibling is absent the run exits 2 without
checking anything, rather than quietly downgrading to a weaker check.

Allowlisting upstream names was the earlier design, and it "checks only
that a name is spelled the way the allowlist spells it": a rename on the
other side sailed through. A stop is louder than a shrug.

To proceed with a sibling missing, pass --without-siblings --- a human
decision, not this script's default, and one whose cost the self-test
asserts. Don't take that route unprompted.

A planning document legitimately names things that do not exist yet, and
a superseded document is named as history, so tools/doc-refs-allow.txt
lists globs to report as "allow" instead of failing. Every entry there
carries its reason; an unexplained one hides the drift this checker
exists to find. Pass -v to also list the unclassified backticks.

Non-vacuity: run `python3 tools/check-doc-refs.py --self-test`. The
scratch document and its expected verdicts live in the configuration
block (the SELF_TEST_* settings), repo-specific like the rest of it; the
engine below asserts the failure count, every FAIL and ok row, the
unclassified tail, the missing-document exit, the --without-siblings
degradation and the liveness of the absent-sibling stop. The controls
matter as much as the failures: without them an extractor that silently
matches nothing would look like a clean document. And the
must-stay-unclassified rows guard the other direction, and one of them is
what proves the path-shape gate: sibling matching is gated on path shape
because a bare prose token once resolved inside the horde-ad copy's
upstream checkout, and `../lambdahack.github.io` invites exactly that,
holding `README.md`, `LICENSE`, `COPYLEFT`, `index.html` and
`manifest.json` --- names any document uses as prose. Those five all
resolve locally too, so the row that bites is the deployment sibling's
own `LambdaHack.wasm` and `bundle.js`: present there, absent here, and
carrying extensions this checker does not read as paths, so the gate is
the only thing keeping them out of the ok column. A big foreign tree
turns a checker into a rubber stamp faster than a small one, and the
sibling test is widened only against these rows. Two rows of 2026-08-28:
the same document through main() from a subdirectory, every checker
having said "run from the repo root" and none having enforced it, and a
PATH without wrap80, where the run must say that spans were read wrapped
and the wrapped-span row must then not fire. Each went red with its fix
reverted in a copy. The self-test was itself proved non-vacuous by
breaking the checker in a copy (2026-08-14): a dead cabal-target loop, a
dead sibling resolution, dropping the path-shape gate off the sibling
arm, and a CITE_RE blind to the range citation each turned it red --- the
path-shape break on exactly the rubber-stamp rows, the CITE_RE one on
exactly the skipped-citation guard.

Four of the rows are recorded false positives rather than decoration. The
plain `\\bcabal` pattern matched inside `wasm32-wasi-cabal` and read
`exe:LambdaHack` as a stanza called "exe". `.../ghc-9.12/...` was read as
a sibling path until the `../` test was made to require the slash. And
NOT_IN_PATH was taken to mark URLs, which it does not --- a plain URL
carries no query and no fragment, hence none of its characters --- so a
backticked `https://example.com/a/b.md` was reported as a path that does
not resolve, while a URL ending in a slash failed through `dir_shaped`
instead, a separate arm needing its own row. No document here backticks a
bare URL today, every occurrence being written as a markdown link, so
those two rows are that branch's only control.

The ways a run can fail to happen are asserted too: a document name that
does not exist must give one line on stderr and exit 2, not a traceback,
and an absent sibling must BLOCK rather than degrade. What
--without-siblings costs is asserted rather than described:
`engine-src/Game/LambdaHack/Client/NoSuchModule.hs` is real local drift,
and without the checkouts nothing distinguishes a missing local file from
an upstream one, so it degrades to SKIP alongside the upstream rows ---
the flag does not merely leave upstream unchecked, it blunts the local
path check as well. Reason enough to mount the sibling instead of
reaching for the flag.

A check can be live on its scratch document and vacuous on the corpus:
reading fenced code blocks only, the horde-ad copy's cabal-target check
had been examining one of six `cabal test` lines in that repo's
CLAUDE.md, since the document indents its blocks instead of fencing them.
Both copies now take the whole document, and the shape is worth keeping
in view here, where README.md writes ten of its commands as four-space
indented lines. So the corpus is worth a look too, whatever the self-test
says.

Two shapes this repo exercises are dead in the horde-ad copy, which has
neither a makefile nor a command-line parser: the `make <target>` branch
and the `--flag` branch are proven here and nowhere else, so keep their
rows. Below the configuration block the two copies are meant to stay
identical (`tools/check-twin-sync.py` compares them whenever both
checkouts are mounted). What differs by design is the docstring and the
configuration block, the self-test rows among them --- each repo's own ---
so a reader syncing one file to the other syncs the code below the block
and nothing else."""

import contextlib
import fnmatch
import glob
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile

# --- per-repo configuration -----------------------------------------
# Porting this script to another repository should mean editing this
# block and nothing else; everything below it is repo-agnostic. Each
# setting may be left empty, which switches its check off rather than
# breaking the run.
SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".claude", ".github"]
# Where the executable's `long "..."` options are declared; "" if the repo
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
# resolved for real whenever present --- see the sibling policy above --- and
# their absence stops the run rather than degrading it. [] to disable.
SIBLING_ROOTS = ["../lambdahack.github.io"]
ALLOW_FILE = "tools/doc-refs-allow.txt"
# --self-test rows: one scratch document exercising every branch against
# this repo's real tree and sibling, with the expected verdicts beside
# it. Repo-specific like the rest of this block; the engine below is not.
SELF_TEST_DOC = """\
`engine-src/Game/LambdaHack/Client/NoSuchModule.hs` is unresolved local
drift, and `cabal build no-such-stanza` an unknown cabal target;
`make no-such-target` is an unknown makefile target,
`Game.LambdaHack.Client.NoSuch` a missing module of our namespace and
`../lambdahack.github.io/no-such.js` missing in a live sibling: all five
must fail, as must the wrapped-span row below.
`--noSuchFlag` is listed, never failed, while `--restamp` is external,
another tool's; `+noSuchFlag` stays unclassified.
A span the formatter wrapped, `two words
across a break`, must not hide `engine-src/Game/LambdaHack/Client/NoSuchTwin.hs`,
which fails like the first path.
`ghc_wasm_jsffi.mjs` resolves in the deployment sibling, by path.
`Frontend/ANSI.hs` and `tools/leader-census.py` and `Makefile` and
`make play` and `cabal run LambdaHack` and
`wasm32-wasi-cabal build exe:LambdaHack` and `Game.LambdaHack.Client.UI`
and `Definition.*` and `--sniff` and `+with_expensive_assertions` are
passing controls; `make -n shot` is the flag-skipping one, and names a
target no other row does.
`Point.hs:26,32` and `Point.hs:26` and `Point.hs:26-32` are pass 1's to
check, while the malformed `Point.hs:26,` stays unclassified, which keeps
the comma form from being a blanket accept.
`Ability.SkMove` and `K.KM` are upgrade-only, unclassified.
`blob/master` and `group/bench` and `KP_/` are prose, and so are the
deployment sibling's own artifacts `LambdaHack.wasm` and `bundle.js`,
whose extensions this checker does not read as paths: the path-shape gate
is what keeps a big foreign tree from resolving them.
`.../ghc-9.12/...` is an elision, not a sibling path;
`https://example.com/a/b.md` and `https://lambdahack.github.io/` are
URLs. None of these may be read as a path.
"""
SELF_TEST_FAILURES = 6
SELF_TEST_FAIL = ["NoSuchModule.hs", "NoSuchTwin.hs", "no-such-stanza",
                  "no-such-target", "Game.LambdaHack.Client.NoSuch",
                  "../lambdahack.github.io/no-such.js"]
SELF_TEST_OK = ["Frontend/ANSI.hs", "tools/leader-census.py", "Makefile",
                "make play", "make shot", "cabal LambdaHack",
                "ghc_wasm_jsffi.mjs", "Game.LambdaHack.Client.UI",
                "Definition.*", "--sniff", "+with_expensive_assertions",
                "--restamp"]
# Citation shapes pass 1 owns: skipped here, in no bucket and no row.
SELF_TEST_SKIPPED = ["Point.hs:26,32", "Point.hs:26", "Point.hs:26-32"]
SELF_TEST_UNCLASSIFIED = ["+noSuchFlag", "Point.hs:26,", "Ability.SkMove",
                          "K.KM", "blob/master", "group/bench", "KP_/",
                          "LambdaHack.wasm", "bundle.js",
                          ".../ghc-9.12/...",
                          "https://example.com/a/b.md",
                          "https://lambdahack.github.io/"]
# The one local-drift row that must degrade to SKIP with the sibling
# off, and the failure count that survives the degradation.
SELF_TEST_DEGRADED_FAILURES = 3
SELF_TEST_DEGRADED_SKIP = "engine-src/Game/LambdaHack/Client/NoSuchModule.hs"
# --- end per-repo configuration --------------------------------------

PATH_EXT = ("hs", "ts", "mjs", "py", "cabal", "html", "md", "yaml", "yml",
            "json", "sh", "txt", "c", "h")

TICK_RE = re.compile(r"`([^`\n]+)`")
FENCE_RE = re.compile(r"^\s*(```|~~~)")


def unwrapped(text):
    """The document with each paragraph on one line, via `wrap80 --unwrap`.

    Shelled out to rather than reimplemented, so that what counts as a
    paragraph here is what counts as one everywhere else: the formatter that
    writes these documents is the only definition worth having. Without
    wrap80 the text is returned as it came, which loses nothing that a
    line-based read did not already lose -- so this degrades to the previous
    behaviour rather than failing, unlike the wrapping check, which reports
    nothing at all when it cannot run.
    """
    global WRAP80_MISSING
    try:
        return subprocess.run(["wrap80", "--unwrap"], input=text, text=True,
                              capture_output=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError):
        WRAP80_MISSING = True
        return text


WRAP80_MISSING = False


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
CITE_RE = re.compile(r":\d+(?:-\d+)?(?:,\d+(?:-\d+)?)*$")
# The comma form is `check-plan-citations.py`'s: its `spans` expands
# "222,1581" and "353,362,771-781" and checks every member. Recognising
# only ":222" and ":222-230" here left the comma form in the unclassified
# tail -- not a failure, which is worse: the tail is what a reader
# eyeballs, and it carried items the sibling had already checked.
# Leading flags are skipped, so `make -n foo` and `make --dry-run foo` name
# the target `foo` rather than a target called `-n`. A document that shows
# how to gate a target it cannot run writes the flag somewhere, and the
# obvious placement was reported as a missing target. MAKE_RE is read only
# under `if MAKEFILE:`, so it is live in LambdaHack, which sets one, and
# inert in horde-ad, which does not; the copies are kept identical so that
# the config block above stays the only thing to edit.
MAKE_RE = re.compile(r"\bmake +(?:-[A-Za-z0-9-]+ +)*([A-Za-z0-9_*.][A-Za-z0-9_*.-]*)")
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
# A repo path cannot hold these; they mark templates and the brace
# shorthand the documents use (`HordeAd.OpsTensor{,Ranked}`), and URLs
# carrying a query or a fragment. A plain URL has none of them -- only
# `:` and `/` -- so it needs its own test, below; until 2026-07-31 this
# set was described as marking URLs and a backticked
# `https://example.com/a/b.md` was reported as a path that does not
# resolve, as was any URL ending in a slash, which failed through
# `dir_shaped` rather than here.
NOT_IN_PATH = set("<>#?&={}\u2026")  # U+2026, an ellipsis
URL_SCHEME_RE = re.compile(r"^[A-Za-z][A-Za-z0-9+.-]*://")


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
    # answered dropped-status: the find's failure is an empty listing, and
    # a path absent from it is reported as such
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
    # answered dropped-status: as above, an empty listing
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
    if NOT_IN_PATH & set(token) or URL_SCHEME_RE.match(token):
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

    Backticked spans are read off the document unwrapped, one line per
    paragraph, because TICK_RE cannot span a newline and a wrapped document
    puts newlines wherever the width falls. Reflowing README.md moved a
    break inside `ssum0 . foo`, and that reference plus `srepl 1.0` after it
    left this check's sight while a third, `; the reason is that `, the text
    between their halves, entered it -- no line of output changed, since the
    count is not printed and both were resolving anyway. What a checker can
    see must not depend on where the prose happens to break.
    """
    parts = TICK_RE.findall(unwrapped(text))
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
    the module's path and as a path suffix --- one entry covers both,
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
    # Read unwrapped, as check_commands reads its spans: on the wrapped text
    # a multi-word span broken by the formatter flips the backtick phase,
    # and the path token after it is read as prose and never checked.
    for token in sorted({t for t in TICK_RE.findall(unwrapped(text))
                         if " " not in t}):
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
                print(f"FAIL sibling {token} --- no such path in the checkout")
                failures += 1
        elif token[0] in "~/":
            out["external"].append(token)
        elif resolves(token, known):
            print(f"ok   path   {token}")
        elif path_shaped(token, top_level) and sibling_hit(token, siblings):
            # Gated on path_shaped, or a bare prose word resolves against
            # the sheer size of a sibling tree: `tests` "found"
            # ../orthotope/tests and reported ok.
            print(f"ok   sibling {token} --- {sibling_hit(token, siblings)}")
        elif is_allowed(token, allow_paths):
            print(f"allow path   {token} --- absent on purpose, see"
                  f" {ALLOW_FILE}")
        elif module_path(token):
            if resolves(module_path(token), known):
                print(f"ok   module {token}")
            elif sibling_hit(module_path(token), siblings):
                # An upstream module named in our vocabulary, e.g.
                # `Data.Array.Strided.Arith`. Resolving it here is what
                # turns an upstream rename from invisible into a failure.
                print(f"ok   module {token} ---"
                      f" {sibling_hit(module_path(token), siblings)}")
            elif OUR_NAMESPACE and token.startswith(OUR_NAMESPACE):
                print(f"FAIL module {token} --- no such module in"
                      f" {OUR_NAMESPACE}*")
                failures += 1
            else:
                out["unclassified"].append(token)
        elif path_shaped(token, top_level):
            if sib_active:
                print(f"FAIL path   {token} --- does not resolve")
                failures += 1
            else:
                # --without-siblings: this may be an upstream name that
                # would have resolved, so calling it drift would be a
                # guess. Flagged, not failed --- the mode is the human
                # having said to carry on without the checkouts.
                print(f"SKIP path   {token} --- unresolved locally, and"
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
                print(f"allow target make {name} --- absent on purpose, see"
                      f" {ALLOW_FILE}")
            else:
                print(f"FAIL target make {name} --- no such makefile target")
                failures += 1

    for name in sorted(set(CABAL_RE.findall(commands))):
        if name in stanzas:
            print(f"ok   target cabal {name}")
        elif name in allow_cabal:
            print(f"allow target cabal {name} --- absent on purpose, see"
                  f" {ALLOW_FILE}")
        else:
            print(f"FAIL target cabal {name} --- no such cabal stanza")
            failures += 1

    top = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                         capture_output=True, text=True).stdout.strip()
    self_path = os.path.relpath(os.path.abspath(__file__), top or ".")
    for flag in sorted(set(FLAG_RE.findall(commands))):
        if flag in ours:
            print(f"ok   flag   --{flag}")
        elif subprocess.run(["git", "grep", "-qF", "--", "--" + flag,
                             "--", ":!*.md", ":(top,exclude)" + self_path]
                            ).returncode == 0:
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


def self_test():
    """Run the configuration block's SELF_TEST_DOC through the real
    machinery and hold it to the verdicts recorded beside it. In-process,
    so the --without-siblings degradation and the absent-sibling stop can
    be exercised by flipping the same switches main() flips."""
    global SIBLING_ROOTS
    missing = missing_siblings()
    if missing:
        print("BLOCKED --- sibling checkout(s) not available ("
              + ", ".join(missing) + "), and the self-test's sibling rows"
              " need them; mount and re-run")
        return 2
    bad = []
    fd, doc = tempfile.mkstemp(suffix=".md")
    os.write(fd, SELF_TEST_DOC.encode("utf-8"))
    os.close(fd)

    def run_doc(no_siblings):
        top_level = {d for d in os.listdir(".") if os.path.isdir(d)}
        known = repo_paths()
        siblings = [] if no_siblings else sibling_paths()
        allow_paths, allow_make, allow_cabal = allowed()
        text = cabal_text()
        out = {"external": [], "unclassified": [], "unknown_flags": [],
               "unverified": []}
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            n = check_doc(doc, known, top_level, allow_paths,
                          cabal_flags(text), siblings,
                          bool(SIBLING_ROOTS) and not no_siblings, out)
            n += check_commands(doc, make_targets(), cabal_stanzas(text),
                                our_flags(), allow_make, allow_cabal, out)
        return n, buf.getvalue(), out

    try:
        failures, output, out = run_doc(no_siblings=False)
        if failures != SELF_TEST_FAILURES:
            bad.append("expected %d failures, got %d"
                       % (SELF_TEST_FAILURES, failures))
        fails = [l for l in output.splitlines() if l.startswith("FAIL")]
        oks = [l for l in output.splitlines()
               if l.startswith(("ok", "allow"))]
        for t in SELF_TEST_FAIL:
            if not any(t in l for l in fails):
                bad.append("no FAIL line names %r" % t)
        for t in SELF_TEST_OK:
            if not any(t in l for l in oks):
                bad.append("no ok line names %r" % t)
        for t in SELF_TEST_UNCLASSIFIED:
            if t not in out["unclassified"]:
                bad.append("%r not among the unclassified" % t)
        for t in SELF_TEST_SKIPPED:
            if t in out["unclassified"] or any(t in l for l in fails + oks):
                bad.append("%r was not skipped as a citation" % t)
        if "noSuchFlag" not in out["unknown_flags"]:
            bad.append("'--noSuchFlag' not among the unknown flags")

        try:
            with contextlib.redirect_stderr(io.StringIO()):
                require_readable(["no-such-document.md"])
            bad.append("a missing document did not stop the run")
        except SystemExit as e:
            if e.code != 2:
                bad.append("a missing document exited %r, not 2" % e.code)

        failures, output, out = run_doc(no_siblings=True)
        if failures != SELF_TEST_DEGRADED_FAILURES:
            bad.append("degraded run: expected %d failures, got %d"
                       % (SELF_TEST_DEGRADED_FAILURES, failures))
        if SELF_TEST_DEGRADED_SKIP not in out["unverified"]:
            bad.append("local drift did not degrade to SKIP alongside"
                       " the upstream rows")

        # The same document through main(), from a subdirectory: the
        # configuration's paths are root-relative, so a run from elsewhere
        # used to report every sibling unmounted.
        script = os.path.abspath(__file__)
        p = subprocess.run([sys.executable, script, doc],
                           capture_output=True, text=True,
                           cwd=os.path.dirname(script))
        if p.returncode != 1 or f"{SELF_TEST_FAILURES} failed" not in p.stdout:
            bad.append("run from a subdirectory: exit %d, %r" % (
                p.returncode, p.stdout.strip().splitlines()[-1:]))
        with tempfile.TemporaryDirectory() as bindir:
            for tool in ("git", "bash", "find"):
                os.symlink(shutil.which(tool), os.path.join(bindir, tool))
            p = subprocess.run([sys.executable, script, doc],
                               capture_output=True, text=True,
                               env=dict(os.environ, PATH=bindir))
        if "NOTE: wrap80" not in p.stdout:
            bad.append("no note about spans read wrapped without wrap80")
        if "NoSuchTwin.hs" in p.stdout:
            bad.append("the wrapped-span row fired without wrap80, so"
                       " the note's claim is false")

        saved = SIBLING_ROOTS
        SIBLING_ROOTS = ["../no-such-checkout-for-self-test"]
        try:
            if not missing_siblings():
                bad.append("missing_siblings is blind to an absent"
                           " checkout")
        finally:
            SIBLING_ROOTS = saved
    finally:
        os.unlink(doc)
    for b in bad:
        print("FAIL: %s" % b)
    if not bad:
        print("ok:   every self-test case behaved as expected")
    return 1 if bad else 0


def main():
    flags = {"-v", "--without-siblings", "--self-test"}
    args = [a for a in sys.argv[1:] if a not in flags]
    verbose = "-v" in sys.argv[1:]
    no_siblings = "--without-siblings" in sys.argv[1:]
    if "--self-test" in sys.argv[1:]:
        return self_test()
    docs = chdir_root(args) or ["CLAUDE.md"]
    require_readable(docs)

    missing = [] if no_siblings else missing_siblings()
    if missing:
        print("BLOCKED --- sibling checkout(s) not available: "
              + ", ".join(missing))
        print("\nThe documents cite files in these, and the policy is to"
              " resolve them for real\nrather than take their spelling on"
              " trust. Mount them and re-run.")
        print("\nTo proceed without them anyway --- which downgrades every"
              " upstream reference to\nan unchecked one, verifiable only"
              " by hand against the cabal store --- re-run\nwith"
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
            ("UNVERIFIED --- would have been resolved against the sibling"
             " checkouts", out["unverified"], True),
            ("outside the repo, not checked", out["external"], True),
            ("flags not found in the repo --- eyeball these",
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
    if WRAP80_MISSING:
        print("\nNOTE: wrap80 is not on PATH, so spans were read off the"
              " wrapped text; a\nspan the formatter broke across lines hides"
              " every token after it on that line.")
    print(f"\n{failures} failed")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())