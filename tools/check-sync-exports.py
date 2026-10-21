#!/usr/bin/env python3
"""Check that every sync JSFFI export in the tree is built on `syncExport`.

Usage: python3 tools/check-sync-exports.py [--self-test]
Reads every tracked `*.hs` file of the working directory's repository.

A sync JSFFI export, `foreign export javascript "NAME sync"`, owes two
things that GHC does not do for it: it passes no exception to its JS caller,
and it leaves the threads it wakes in the run queue, where nothing runs them
until JS calls into the RTS again. `syncExport`, in
`engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs`, does both, and its
haddock says why and names the GHC issue. So each equation that defines
a sync export's Haskell binding must have `syncExport "NAME"` at the head
of its right-hand side, NAME being the export's JS name; and `syncExport`
is applied to no other name, an async export owing neither.

A dynamic sync export, `foreign import javascript "wrapper sync"`, makes
a JS function of whatever closure it is given at run time, which no textual
check can follow to a definition, so each one is a finding until this
checker learns its shape. Comments are blanked before anything is matched,
so a commented-out export is not one; an equation whose `=` is not on
its first line, a guarded one for instance, is a finding, never a pass,
and so is a signature broken before its `::`.

Every run prints each sync export it found, so a tree with none says so
rather than passing in silence. Exit 0 when every one is built on
`syncExport`, 1 with findings, 2 when the run did not happen: outside a git
repository, or with no tracked `*.hs` file.

Non-vacuity: `--self-test` runs the engine over scratch sources, one per
finding and controls for the shapes it must leave alone; that the self-test
bites is `tools/mutants.py`'s to show.
"""

import contextlib
import io
import os
import re
import subprocess
import sys

STATIC_RE = re.compile(
    r'foreign\s+export\s+javascript\s+"(\w+)\s+sync"\s+(\w+)\s*::')
WRAPPER_RE = re.compile(r'foreign\s+import\s+javascript\s+"wrapper\s+sync"')
APPLIED_RE = re.compile(r'\bsyncExport\s+"(\w+)"')
# A run of two or more dashes not followed by another symbol character
# begins a line comment; `{-` begins a block comment, pragmas included.
LINE_COMMENT_RE = re.compile(r"--+(?![!#$%&*+./<=>?@\\^|~:-]).*")
BLOCK_COMMENT_RE = re.compile(r"\{-.*?-\}", re.S)


def blank(m):
    return re.sub(r"[^\n]", " ", m.group(0))


def strip_comments(text):
    """The text with every comment blanked, newlines kept, so that a match
    offset still gives the line it was found on."""
    return LINE_COMMENT_RE.sub(blank, BLOCK_COMMENT_RE.sub(blank, text))


def line_of(text, pos):
    return text.count("\n", 0, pos) + 1


def equations(code, hs):
    """The offset of each equation of `hs` at column 0 and of its
    right-hand side, past the `=` and the whitespace after it, or None
    for an equation whose `=` is not on its first line."""
    start_re = re.compile(rf"(?m)^{re.escape(hs)}(?![\w'])(?![ \t]*::)")
    eq_re = re.compile(r"[^\n=]*=(?![=>])\s*")
    eqs = []
    for m in start_re.finditer(code):
        e = eq_re.match(code, m.end())
        eqs.append((m.start(), e.end() if e else None))
    return eqs


def scan(files):
    """`files` maps a path to its text. Returns the lines to print, ok
    lines first, the number of findings among them and the number of sync
    exports found."""
    oks, fails = [], []
    static = {}
    codes = {path: strip_comments(text) for path, text in files.items()}
    for path, code in codes.items():
        for m in STATIC_RE.finditer(code):
            static[m.group(1)] = (path, line_of(code, m.start()), m.group(2))
    for js, (path, line, hs) in sorted(static.items()):
        code = codes[path]
        eqs = equations(code, hs)
        if not eqs:
            fails.append(f"FAIL {js} ({path}:{line}): no equation of `{hs}`"
                         f" with its `=` on the first line in {path}")
            continue
        unseen = [s for s, rhs in eqs if rhs is None]
        bad = [s for s, rhs in eqs if rhs is not None and
               not re.match(rf'syncExport\s+"{re.escape(js)}"(?![\w"])',
                            code[rhs:])]
        for s in unseen:
            fails.append(f"FAIL {js} ({path}:{line}): the equation of `{hs}`"
                         f" at {path}:{line_of(code, s)} has no `=` on its"
                         f" first line, so it was not checked")
        for s in bad:
            fails.append(f"FAIL {js} ({path}:{line}): the equation of `{hs}`"
                         f" at {path}:{line_of(code, s)} is not"
                         f' `syncExport "{js}" ...`')
        bad += unseen
        if not bad:
            oks.append(f"ok   {js} ({path}:{line}) is built on syncExport")
    for path, code in sorted(codes.items()):
        for m in APPLIED_RE.finditer(code):
            if m.group(1) not in static:
                fails.append(f'FAIL syncExport "{m.group(1)}" at'
                             f" {path}:{line_of(code, m.start())} names no"
                             f" sync export")
        for m in WRAPPER_RE.finditer(code):
            fails.append(f"FAIL {path}:{line_of(code, m.start())}: a"
                         f' "wrapper sync" import, whose closures this'
                         f" checker cannot follow to syncExport")
    return oks + fails, len(fails), len(static)


def run(files):
    if not files:
        print("BLOCKED: no tracked *.hs file; nothing checked")
        return 2
    lines, n, found = scan(files)
    for l in lines:
        print(l)
    if not found:
        print(f"no sync export in {len(files)} tracked *.hs files;"
              f" nothing to require")
    print(f"{found} sync export(s), {n} finding(s)")
    return 1 if n else 0


SELF_TEST_FILES = {
    "Good.hs": '''\
module Good where

lhKey :: JSVal -> IO ()
lhKey e = syncExport "lhKey" $ do
  pure ()

foreign export javascript "lhKey sync"
  lhKey :: JSVal -> IO ()

jsName :: IO ()
jsName =
  syncExport "otherName" $ pure ()

foreign export javascript "otherName sync" jsName :: IO ()

lhWheel :: Int -> IO ()
lhWheel _ = pure ()

foreign export javascript "lhWheel"
  lhWheel :: Int -> IO ()

-- foreign export javascript "lhComment sync" lhComment :: IO ()
{- foreign export javascript "lhBlock sync"
     lhBlock :: IO () -}
-- | foreign import javascript "wrapper sync" mk :: IO () -> IO JSVal

syncExport :: String -> IO () -> IO ()
syncExport name body = body
''',
    "Bad.hs": '''\
module Bad where

lhPaste :: JSString -> IO ()
lhPaste s = pure ()

foreign export javascript "lhPaste sync" lhPaste :: JSString -> IO ()

lhCopy :: IO ()
lhCopy = syncExport "lhKeyz" $ pure ()

foreign export javascript "lhCopy sync" lhCopy :: IO ()

foreign export javascript "lhGone sync" lhGone :: IO ()

lhTwo :: Int -> IO ()
lhTwo 0 = syncExport "lhTwo" $ pure ()
lhTwo _ = pure ()

foreign export javascript "lhTwo sync" lhTwo :: Int -> IO ()

lhMouse :: Int -> IO ()
lhMouse _ = syncExport "lhMouse" $ pure ()

foreign export javascript "lhMouse" lhMouse :: Int -> IO ()

foreign import javascript "wrapper sync"
  mkCallback :: IO () -> IO JSVal

(--|) :: IO () -> IO () -> IO ()
a --| b = a >> b

lhPipe :: IO ()
lhPipe = pure () --| syncExport "lhPiped" (pure ())

lhGuard :: Int -> IO ()
lhGuard 0 = syncExport "lhGuard" $ pure ()
lhGuard n
  | n > 0 = pure ()

foreign export javascript "lhGuard sync" lhGuard :: Int -> IO ()
''',
}

SELF_TEST_OK = ["ok   lhKey (Good.hs:7)", "ok   otherName (Good.hs:14)"]
SELF_TEST_FAIL = [
    "FAIL lhPaste (Bad.hs:6): the equation of `lhPaste` at Bad.hs:4",
    "FAIL lhCopy (Bad.hs:11): the equation of `lhCopy` at Bad.hs:9",
    "FAIL lhGone (Bad.hs:13): no equation of `lhGone`",
    "FAIL lhTwo (Bad.hs:19): the equation of `lhTwo` at Bad.hs:17",
    'FAIL syncExport "lhKeyz" at Bad.hs:9 names no sync export',
    'FAIL syncExport "lhMouse" at Bad.hs:22 names no sync export',
    'FAIL Bad.hs:26: a "wrapper sync" import',
    'FAIL syncExport "lhPiped" at Bad.hs:33 names no sync export',
    "FAIL lhGuard (Bad.hs:40): the equation of `lhGuard` at Bad.hs:37 has no",
]
# Shapes that must stay quiet: an async export without syncExport, the
# commented-out exports and wrapper, syncExport's own definition, a JS
# name that is not the Haskell one, the wrapped equations of lhTwo and
# lhGuard, and every type signature.
SELF_TEST_QUIET = ["lhWheel", "lhComment", "lhBlock", "Good.hs:25",
                   "Good.hs:27", "Good.hs:28", "otherName (Good.hs:14): ",
                   "at Bad.hs:16", "at Bad.hs:36", "at Good.hs:3 ",
                   "at Good.hs:10 ", "at Bad.hs:3 ", "at Bad.hs:8 ",
                   "at Bad.hs:15 ", "at Bad.hs:35 "]


def self_test():
    bad = []
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = run(SELF_TEST_FILES)
    out = buf.getvalue()
    lines = out.splitlines()
    if rc != 1:
        bad.append(f"scratch sources: exit {rc}, wanted 1")
    for want in SELF_TEST_OK + SELF_TEST_FAIL:
        if not any(l.startswith(want) for l in lines):
            bad.append(f"missing: {want}")
    oks = [l for l in lines if l.startswith("ok   ")]
    if len(oks) != len(SELF_TEST_OK):
        bad.append(f"{len(oks)} ok lines, wanted {len(SELF_TEST_OK)}:\n"
                   + "\n".join(oks))
    fails = [l for l in lines if l.startswith("FAIL ")]
    if len(fails) != len(SELF_TEST_FAIL):
        bad.append(f"{len(fails)} FAIL lines, wanted {len(SELF_TEST_FAIL)}:\n"
                   + "\n".join(fails))
    for q in SELF_TEST_QUIET:
        for l in fails:
            if q in l:
                bad.append(f"control failed: {l}")
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = run({"Good.hs": SELF_TEST_FILES["Good.hs"]})
    if rc != 0:
        bad.append(f"good source alone: exit {rc}, wanted 0:\n"
                   + buf.getvalue())
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = run({"None.hs": "module None where\n"})
    if rc != 0 or "no sync export in 1 tracked" not in buf.getvalue():
        bad.append(f"source with no sync export: exit {rc}, wanted 0 and"
                   f" the nothing-to-require line:\n" + buf.getvalue())
    buf = io.StringIO()
    with contextlib.redirect_stdout(buf):
        rc = run({})
    if rc != 2:
        bad.append(f"no source: exit {rc}, wanted 2")
    if bad:
        print("SELF-TEST FAILED:")
        for b in bad:
            print("  " + b)
        return 1
    print("self-test passed: every expected finding drawn, every control"
          " quiet")
    return 0


USAGE = "usage: check-sync-exports.py [--self-test]"


def main():
    argv = sys.argv[1:]
    if argv == ["--self-test"]:
        return self_test()
    if argv:
        print(USAGE, file=sys.stderr)
        return 2
    try:
        root = subprocess.run(["git", "rev-parse", "--show-toplevel"],
                              capture_output=True, text=True,
                              check=True).stdout.strip()
        os.chdir(root)
        names = subprocess.run(["git", "ls-files", "*.hs"],
                               capture_output=True, text=True,
                               check=True).stdout.split()
    except (OSError, subprocess.CalledProcessError) as e:
        print(f"BLOCKED: cannot list the repository's files: {e}")
        return 2
    files = {}
    for n in names:
        with open(n, encoding="utf-8") as f:
            files[n] = f.read()
    return run(files)


if __name__ == "__main__":
    sys.exit(main())
