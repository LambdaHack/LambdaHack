#!/usr/bin/env python3
"""Check that the twin tools here and in the sibling repo have not drifted.

Usage: python3 tools/check-twin-sync.py [--self-test]
Runs from anywhere in the repository.

The document checkers are kept as twin copies in the repos that adopted
them, identical apart from a per-repo configuration and their docstrings
(the non-vacuity prose and the examples are each repo's own). That claim
had no instrument: a fix landing in one copy only was invisible unless a
session happened to mount both repos and think of diffing them. This is
that diff, mechanized: for every tools/*.py present in both checkouts,
compare everything below the module docstring, with the marked per-repo
configuration block stripped from both sides and, for scripts that
predate the marker, the assignments to the names in CONFIG_NAMES. Any
other file under tools/ is compared whole, except the per-repo ones
named in TWIN_SKIP (the allowlist); until 2026-08-28 only `*.py` was
looked at, so a shared shell script could drift unseen.

TWIN_ROOT names the sibling's tools directory. Absent -- unmounted, or
hidden by the outer wrapper -- the run is BLOCKED with exit 2, the same
ruling as check-doc-refs.py's siblings: an unverified sync must not read
as a passing one. Exit 1 when a shared tool drifts, 0 when none does.
Files present in only one checkout are noted, not failed: a tool can be
legitimately unported.

Scope limits, deliberate: only single-line CONFIG_NAMES assignments are
stripped outside a marker block, so a script that grows a multi-line
config constant should grow the marker block instead; and a drift
verdict names the file, not the hunk -- the fix is to diff the two
copies and port, which no summary replaces.

Non-vacuity: run --self-test. It copies this repo's tools into a scratch
"twin", confirms the identical copies pass, then confirms that a
docstring-only change and a configuration-only change both still pass
while a mutated code line fails, and that a shared shell script is
compared whole while a TWIN_SKIP file is not. The self-test was itself proved
non-vacuous by breaking the checker in a copy (2026-08-14): a
comparable() that returns the empty string for every script turns the
mutated-code case green and the self-test red. Each mutation now asserts
that its target text exists, the configuration case having replaced a
literal TWIN_ROOT that only this repo's copy carries.
"""
import ast
import glob
import os
import shutil
import subprocess
import sys
import tempfile

# --- per-repo configuration -----------------------------------------
TWIN_ROOT = "../horde-ad/tools"
# Files under tools/ that are each repo's own, compared with nothing.
TWIN_SKIP = ("doc-refs-allow.txt", "checks.py", "mutants.py", "defects.json")
# --- end per-repo configuration --------------------------------------

MARKER_BEGIN = "# --- per-repo configuration"
MARKER_END = "# --- end per-repo configuration"
# Config constants of scripts that predate the marker block.
CONFIG_NAMES = ("SEARCH_ROOTS", "PUBLISHED_REF", "TWIN_ROOT")


def chdir_root():
    """Run from the repository root whatever the cwd, TWIN_ROOT being
    root-relative. Outside a repository nothing moves."""
    # answered dropped-status: an empty top is the failure, and the next line tests it
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                         capture_output=True, text=True).stdout.strip()
    if top:
        os.chdir(top)


def comparable(text):
    """The part of a script the twins must agree on; a file that is not
    Python is compared whole."""
    lines = text.split("\n")
    try:
        tree = ast.parse(text)
    except SyntaxError:
        return text
    body = tree.body
    if (body and isinstance(body[0], ast.Expr)
            and isinstance(body[0].value, ast.Constant)
            and isinstance(body[0].value.value, str)):
        end = body[0].end_lineno
        lines = [""] * end + lines[end:]
    out, in_config = [], False
    for ln in lines:
        s = ln.strip()
        if s.startswith(MARKER_BEGIN):
            in_config = True
            continue
        if s.startswith(MARKER_END):
            in_config = False
            continue
        if in_config:
            continue
        if any(s.startswith(name + " =") or s.startswith(name + "=")
               for name in CONFIG_NAMES):
            continue
        out.append(ln)
    return "\n".join(l for l in out if l.strip())


def compare(root_a, root_b):
    """(drifted, only_a, only_b, same) basename lists for the two roots."""
    def files(root):
        return {os.path.basename(p) for p in glob.glob(os.path.join(root, "*"))
                if os.path.isfile(p) and os.path.basename(p) not in TWIN_SKIP}
    a, b = files(root_a), files(root_b)
    drifted, same = [], []
    for name in sorted(a & b):
        ta = open(os.path.join(root_a, name), encoding="utf-8").read()
        tb = open(os.path.join(root_b, name), encoding="utf-8").read()
        (drifted if comparable(ta) != comparable(tb) else same).append(name)
    return drifted, sorted(a - b), sorted(b - a), same


def self_test():
    """Scratch-twin controls; the module docstring records what each
    case proves."""
    here = os.path.dirname(os.path.abspath(__file__))
    myself = os.path.basename(__file__)
    bad = []
    with tempfile.TemporaryDirectory() as td:
        twin = os.path.join(td, "tools")
        shutil.copytree(here, twin,
                        ignore=shutil.ignore_patterns("__pycache__"))
        drifted, _, _, same = compare(here, twin)
        if drifted or not same:
            bad.append("identical twin read as drifted: %r" % drifted)
        victim = os.path.join(twin, myself)
        text = open(victim, encoding="utf-8").read()

        def mutate(case, old, new):
            # A replacement that matches nothing leaves the twin identical
            # and the case passing for nothing, which is how the
            # configuration case read in the other repo, whose TWIN_ROOT
            # this file's literal did not name (2026-08-28).
            if old not in text:
                bad.append(f"{case}: nothing to mutate, {old!r} absent")
            open(victim, "w").write(text.replace(old, new, 1))
            return compare(here, twin)[0]

        if mutate("docstring", "Check that the twin tools",
                  "CHANGED docstring words"):
            bad.append("docstring-only change read as drift")
        if mutate("configuration", 'TWIN_ROOT = "%s"' % TWIN_ROOT,
                  'TWIN_ROOT = "../no-such-twin/tools"'):
            bad.append("configuration-only change read as drift")
        # A line of comparable() itself: the first anchor here was a
        # literal only this call carried, so the case mutated the self-test
        # in the twin and not the function it names (check-twin-sync-03).
        if myself not in mutate("code", "        tree = ast.parse(text)\n",
                                "        tree = ast.parse(text)  # mutated\n"):
            bad.append("a mutated code line was not read as drift")
        # Shared files that are not Python are compared whole, and the
        # per-repo ones in TWIN_SKIP not at all: two scratch copies, so
        # that nothing is written into the real tools/.
        mine = os.path.join(td, "mine")
        shutil.copytree(twin, mine)
        for root in (mine, twin):
            open(os.path.join(root, "probe.sh"), "w").write("echo a\n")
            open(os.path.join(root, TWIN_SKIP[0]), "w").write(root + "\n")
        drifted, _, _, same = compare(mine, twin)
        if "probe.sh" not in same or TWIN_SKIP[0] in drifted + same:
            bad.append("a shared shell script was not compared, or a"
                       " per-repo file was: %r %r" % (drifted, same))
        open(os.path.join(twin, "probe.sh"), "w").write("echo b\n")
        if "probe.sh" not in compare(mine, twin)[0]:
            bad.append("a mutated shell script was not read as drift")
    for b in bad:
        print("FAIL: %s" % b)
    if not bad:
        print("ok:   every self-test case behaved as expected")
    return 1 if bad else 0


def main():
    if sys.argv[1:] == ["--self-test"]:
        return self_test()
    if sys.argv[1:]:
        print("usage: check-twin-sync.py [--self-test]", file=sys.stderr)
        return 2
    chdir_root()
    here = os.path.dirname(os.path.abspath(__file__))
    if not os.path.isdir(TWIN_ROOT):
        print(f"BLOCKED --- twin checkout not available: {TWIN_ROOT}")
        print("An unverified sync is not a passing one; mount the checkout"
              " and re-run.")
        return 2
    drifted, only_here, only_twin, same = compare(here, TWIN_ROOT)
    for name in same:
        print(f"ok   {name}: identical below docstring and configuration")
    for name in only_here:
        print(f"note {name}: no copy in {TWIN_ROOT}")
    for name in only_twin:
        print(f"note {name}: only in {TWIN_ROOT}")
    for name in drifted:
        print(f"FAIL {name}: the copies disagree below docstring and"
              f" configuration --- a fix landed in one repo only; diff the"
              f" two and port it")
    print(f"\n{len(drifted)} drifted")
    return 1 if drifted else 0


if __name__ == "__main__":
    sys.exit(main())
