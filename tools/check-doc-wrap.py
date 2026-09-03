#!/usr/bin/env python3
"""Check that no paragraph of a document is wrapped by hand, in whichever
form it keeps.

Usage: python3 tools/check-doc-wrap.py [DOC ...]
DOC defaults to every Markdown file git tracks. Runs from anywhere in the
repository.

A document here keeps one of two forms, and both are the formatter's: prose
wrapped to a column limit, or one line per paragraph, which is how a text
bound for a GitHub issue body has to be written since a single newline
renders there as a hard break. Both are fixed points of the same pass, so
both can be asked for, and neither has to be maintained by hand.

This asks for the formatter's output rather than for a width, which is
stronger and cheaper at once. Stronger, because a width sees only the
lines past it: a document left under-wrapped, and a line ending on a
dangling "a" or "the", both pass an "under 80" test and both fail this
one. Cheaper, because the fix is an unwrap and the commit hook, with
nothing left to judge (`wrap80 -i DOC` only where the document has no
commit for the hook to read) -- where a width invites fixing the
offending line, which does not converge, since shortening one line
pushes its last words onto the next.

No width appears here. The number lives in `wrap80` alone, as its name
and its default, so there is nowhere for a second copy to drift from.

Exit 1 when a document fails -- hand-wrapping found, or a refusal: an
untracked document, a committed version at neither fixed point, both
having a fix to name. Exit 2 when nothing could be checked at all
(wrap80 missing), which `wrap80 -i` would not fix and which used to be
misreported: committed_form met the missing tool first and called the
document "at neither fixed point", so on a wrap80-less machine every
document failed with a wrong diagnosis while the BLOCKED branch sat
unreachable below it.

Non-vacuity: run `python3 tools/check-doc-wrap.py --self-test`. It
builds a scratch git repository holding a wrapped document, a
one-line-per-paragraph one and a wrapped bulleted list, and asserts
every branch: both untouched forms pass; a hand-lengthened line and a
re-wrap to a narrower width fail as hand-wrapping; one paragraph left on
one line, and one list item joined back to one line, pass as mid-edit; a
planted "2b." enumerator fails; a hand-wrapped committed document and an
untracked one are refused; a committed document checked from its own
subdirectory passes; a repository tracking no Markdown reports BLOCKED
rather than "0 of 0 failed"; and a PATH carrying git but no wrap80
reports BLOCKED, exit 2. The self-test was itself proved non-vacuous by
breaking the checker in a copy (2026-08-14): disabling the
fake-enumerator branch, counting every differing paragraph as mid-edit,
and folding BLOCKED back into exit 1 each turned it red, naming exactly
the cases those branches carry; the subdirectory and no-document rows
(2026-08-28) each went red with its fix reverted.

What the hand-run history of this check still testifies to, kept because
each was a real catch or a real ruling: the one-line-per-paragraph half
caught a paragraph in horde-ad's CREDITS.md broken across two lines,
which nothing had ever looked at; the unit of judgment is the line inside a block, not
the block, because a bulleted run is one block holding several
paragraphs, and an edit to one item left a whole-block comparison
calling the list hand-wrapped (re-worded and proven 2026-08-14); the
mid-edit tolerance exists because the whole-file test this replaces
called a file with one unwrapped paragraph 11 lines wrong; and deriving
the form from `git show HEAD:DOC` reproduced the two hand-maintained
lists it replaced exactly, which is what let the lists go.
"""

import contextlib
import difflib
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile

# Which form a document keeps cannot be read off its bytes -- long lines mean
# badly wrapped or deliberately unwrapped, and nothing in the file says which
# -- but it CAN be read off its history. The committed version is at whichever
# fixed point the document is kept in, so `git show HEAD:DOC` answers what two
# hand-maintained lists used to, and answers it for a document mid-edit too,
# HEAD being unaffected by the working copy. A document whose committed
# version is at neither fixed point, and one git does not have, are refused
# rather than guessed at, exactly as an unlisted one was.
#
# The lists this replaces named ten documents and had to be added to by hand
# once per new document, in a repo whose CLAUDE.md is itself one of the ten.
# Deriving them reproduced all ten, which is the check that let them go.


def committed_form(rel):
    """("--unwrap" flag, name) for the form DOC's last commit is in, or None.

    None where git has no such file, and where the committed version sits at
    neither fixed point -- someone else's hand-wrapping, or a document nobody
    has run the formatter over yet. Both are refusals rather than guesses.
    A missing or failing wrap80 raises instead (OSError or
    CalledProcessError) and check() reports it as BLOCKED: swallowing it
    here read as "neither fixed point", which diagnoses the document for a
    fault of the tooling.
    """
    # `HEAD:path` is read from the repository root whatever the cwd, so a
    # run from a subdirectory needs the prefix, or it reports a committed
    # document as "neither fixed point".
    prefix = subprocess.run(["git", "rev-parse", "--show-prefix"],
                            capture_output=True, text=True).stdout.strip()
    p = subprocess.run(["git", "show", "HEAD:" + os.path.normpath(
                            os.path.join(prefix, rel))],
                       capture_output=True, text=True)
    if p.returncode != 0:
        return None
    base = p.stdout
    w = subprocess.run(["wrap80"], input=base, capture_output=True,
                       text=True, check=True).stdout
    u = subprocess.run(["wrap80", "--unwrap"], input=base,
                       capture_output=True, text=True, check=True).stdout
    if base == w:                      # a short document is at both, and the
        return ([], "wrapped")         # wrapped pass is then a no-op anyway
    if base == u:
        return (["--unwrap"], "one line per paragraph")
    return None


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


def tracked_markdown():
    """Every Markdown file git has, which is what the lists used to spell."""
    p = subprocess.run(["git", "ls-files", "*.md"],
                       capture_output=True, text=True)
    return p.stdout.split() if p.returncode == 0 else []


# A line that looks like an enumerated item to a writer and is not one to
# Markdown, which wants a bullet or plain digits: "2b.", "iv.", "A.", "a)".
# It renders as a paragraph, so it loses the numbering it was written for and
# the indentation under it stops being a list's, which is a shape this tool
# then declines to touch -- a defect that hid a broken code span in the
# doc-verification skill until it was looked for. Ordinary prose does not
# trip it: over every document in these repos it matched nothing, while a
# plain "1990. The year" is a real list item and is left alone.
FAKE_MARKER = re.compile(r"^\s*(\d+[a-z]|[a-z]|[A-Z]|[ivxlcIVXLC]+)[.)]\s")
REAL_MARKER = re.compile(r"^\s*([-*+]\s|\d{1,9}[.)]\s)")
FENCE = re.compile(r"^\s*(`{3,}|~{3,})")


def fake_markers(text):
    """[(line number, line)] for enumerators Markdown will not read as such."""
    # The open fence, kind and length: CommonMark closes a block only with
    # a fence of the same character at least as long, so a backtick fence
    # shown inside a tilde block is content. One boolean flipped by any
    # fence line read it as the closer (check-doc-wrap-06).
    out, fence = [], None
    for i, l in enumerate(text.split("\n"), 1):
        m = FENCE.match(l)
        if m and fence is None:
            fence = m.group(1)
        elif m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):
            fence = None
        elif fence is None and FAKE_MARKER.match(l) and not REAL_MARKER.match(l):
            out.append((i, l.strip()))
    return out


def check(doc):
    """0 if no paragraph is wrapped by hand, 1 if one is or the document
    needs the formatter, 2 if nothing could be checked -- which `wrap80 -i`
    would not fix, so the summary counts it apart rather than as a failure."""
    rel = os.path.normpath(doc)
    try:
        got = committed_form(rel)
    except OSError:
        print(f"BLOCKED {rel}: wrap80 is not on PATH, so nothing was checked")
        return 2
    except subprocess.CalledProcessError as e:
        print(f"BLOCKED {rel}: wrap80 failed ({e.returncode}), nothing checked")
        return 2
    if got is None:
        print(f"FAIL {rel}: its committed version is at neither of wrap80's"
              f" fixed points, or git has no such file, so the form it keeps"
              f" cannot be told --- run the formatter over it and commit that")
        return 1
    flag, form = got
    try:
        want = subprocess.run(["wrap80"] + flag + [rel], capture_output=True,
                              text=True, check=True).stdout
    except OSError:
        print(f"BLOCKED {rel}: wrap80 is not on PATH, so nothing was checked")
        return 2
    except subprocess.CalledProcessError as e:
        print(f"BLOCKED {rel}: wrap80 failed ({e.returncode}), nothing checked")
        return 2
    have = open(rel, encoding="utf-8").read()
    fake = fake_markers(have)
    if fake:
        i, l = fake[0]
        print(f"FAIL {rel}: line {i} opens with an enumerator Markdown does not"
              f" read as one --- {l[:40]!r}; use a bullet or plain digits"
              + (f" ({len(fake)} such lines)" if len(fake) > 1 else ""))
        return 1
    if want == have:
        print(f"ok   {rel}: no paragraph wrapped by hand, {form}")
        return 0
    # WHAT IS FORBIDDEN IS HAND-WRAPPING, and that is a property of a
    # PARAGRAPH. Asking the whole file to be exactly as wrap80 leaves it fails
    # a document with one paragraph edited and left long -- which is the state
    # the standing rule asks for, an edit being made at whatever length falls
    # out of it. So this went red on an ordinary edit and the way to green was
    # to wrap; wrapping between edits moves the breaks the next exact-match
    # edit has to quote, so the way on was to unwrap, and the cycle repeated
    # per edit. The pressure was this check, so this is where it is removed,
    # in what it asks and in what it says when it passes: a verdict phrased
    # as a state of the file names the command that makes it true.
    #
    # A paragraph mid-edit is one of two innocent things: as wrap80 leaves it,
    # or entirely on one line. Hand-wrapping is neither. Blocks align by index
    # because wrapping never adds or removes a blank line; where they do not,
    # something outside this check's subject moved one and the whole-file
    # comparison below is the honest thing left to report.
    #
    # NO LIVE CONTROL, and named rather than left to look exercised: over
    # the eleven documents of these two repos the counts never differ, and
    # by construction cannot. The branch earns its place because `zip`
    # truncates to the shortest, so without it a mismatch would under-check
    # in silence rather than fall back loudly.
    try:
        flat = subprocess.run(["wrap80", "--unwrap", rel], capture_output=True,
                              text=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError):
        flat = None
    # Judged LINE by line inside a block, because a block is not a paragraph:
    # a bulleted run is one block holding several, and an edit to one item
    # leaves that block matching neither form -- the formatter would re-wrap
    # the item, the unwrapped form would put every sibling on its own line --
    # so a whole-block comparison called a list mid-edit hand-wrapped and
    # failed it. A line an edit left long is one the unwrapped form has, a
    # line the formatter would produce is in its own output, and hand-wrapping
    # is what is in neither.
    hp, wp, fp = (t.split("\n\n") for t in (have, want, flat or ""))
    if flat is not None and len(hp) == len(wp) == len(fp):
        hand, loose = [], 0
        for i, (h, w, f) in enumerate(zip(hp, wp, fp)):
            if h == w:
                continue
            ok = set(w.split("\n")) | set(f.split("\n"))
            if all(l in ok for l in h.split("\n")):
                loose += 1
            else:
                hand.append(i)
        if not hand:
            print(f"ok   {rel}: no paragraph wrapped by hand, {form};"
                  f" {loose} still on one line, so it is mid-edit")
            return 0
        # Summed rather than searched for: a short block can occur as a
        # substring of an earlier one, and `index` would then send the reader
        # to a paragraph that is fine.
        at = have.count("\n", 0, sum(len(b) + 2 for b in hp[:hand[0]])) + 1
        fix = " ".join(["wrap80"] + flag + ["-i", rel])
        # Two causes, one artifact: canonical lines with a long one among them
        # is what an Edit mid-stretch leaves AND what hand-lengthening leaves,
        # so this cannot tell them apart and must name both remedies. Naming
        # the formatter alone sent a session round wrap-then-edit-then-red five
        # times in one write-up (2026-08-16), and naming `wrap80 -i` as the
        # done-case fix taught the next to wrap for a check: the remedy is
        # the unwrap, and the commit hook wraps a tracked document back.
        undo = " ".join(["wrap80", "--unwrap", "-i", rel])
        # For a document kept one line per paragraph the unwrap IS the
        # fix, so the no-commit clause would name the same command twice.
        spare = "" if flag else f"; `{fix}` is for a document with no commit"
        print(f"FAIL {rel}: {len(hand)} paragraph(s) wrapped by hand, {form}"
              f" --- first at line {at}; unwrap it (`{undo}`) and work there,"
              f" the commit hook restoring its committed form{spare}."
              f" Never re-wrap a line by hand")
        return 1
    # Diffed rather than compared by position: one inserted line shifts every
    # line under it, so a position count reports the whole file as changed and
    # hides the one line worth looking at.
    d = list(difflib.unified_diff(have.split("\n"), want.split("\n"),
                                  lineterm="", n=0))
    n = sum(1 for l in d if l[:1] in "+-" and not l.startswith(("---", "+++")))
    at = next((m.group(1) for l in d
               for m in [re.match(r"@@ -(\d+)", l)] if m), "?")
    undo = " ".join(["wrap80", "--unwrap", "-i", rel])
    print(f"FAIL {rel}: not as wrap80 leaves it, {form} ({n} line(s), from"
          f" line {at}) --- unwrap it (`{undo}`) and the commit hook restores"
          f" its committed form; never re-wrap a line by hand")
    return 1


def self_test():
    """Build a scratch repository and confirm every branch fires.

    A hand recipe gets skipped, or assembled a little differently each
    time; building the controls here keeps them from expiring silently.
    The module docstring holds what each case proves.
    """
    if not shutil.which("wrap80") or not shutil.which("git"):
        print("BLOCKED: wrap80 or git not on PATH, self-test did not run")
        return 2
    script = os.path.abspath(__file__)
    prev = os.getcwd()
    bad = []

    def expect(case, code, want_code, out, *needles):
        if code != want_code:
            bad.append(f"{case}: exit {code}, expected {want_code}")
        for n in needles:
            if n not in out:
                bad.append(f"{case}: output lacks {n!r}")

    def run_check(doc):
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = check(doc)
        return code, buf.getvalue()

    with tempfile.TemporaryDirectory() as td:
        os.chdir(td)
        try:
            for args in (["init", "-q"], ["config", "user.email", "t@t"],
                         ["config", "user.name", "t"],
                         ["config", "commit.gpgsign", "false"]):
                subprocess.run(["git"] + args, check=True, capture_output=True)
            para1 = " ".join(["alpha beta gamma delta epsilon"] * 8)
            para2 = " ".join(["zeta eta theta iota kappa lambda"] * 8)
            raw = "# Control\n\n" + para1 + "\n\n" + para2 + "\n"
            open("w.md", "w").write(raw)
            open("u.md", "w").write(raw)
            lst = ("# L\n\n- " + " ".join(["one two three four five"] * 6)
                   + "\n- " + " ".join(["six seven eight nine ten"] * 6)
                   + "\n")
            open("l.md", "w").write(lst)
            subprocess.run(["wrap80", "-i", "w.md"], check=True)
            subprocess.run(["wrap80", "--unwrap", "-i", "u.md"], check=True)
            subprocess.run(["wrap80", "-i", "l.md"], check=True)
            open("n.md", "w").write("# T\n\nalpha\nbeta gamma\ndelta\n")
            subprocess.run(["git", "add", "-A"], check=True,
                           capture_output=True)
            subprocess.run(["git", "commit", "-qm", "c"], check=True,
                           capture_output=True)
            wrapped = open("w.md").read()
            blocks = wrapped.split("\n\n")

            code, out = run_check("w.md")
            expect("wrapped untouched", code, 0, out, "ok")
            code, out = run_check("u.md")
            expect("unwrapped untouched", code, 0, out,
                   "one line per paragraph")
            code, out = run_check("n.md")
            expect("committed hand-wrapping", code, 1, out, "neither")
            open("t.md", "w").write(raw)
            code, out = run_check("t.md")
            expect("untracked", code, 1, out, "neither")

            lines = wrapped.split("\n")
            k = next(i for i, l in enumerate(lines)
                     if l and not l.startswith("#"))
            open("w.md", "w").write("\n".join(
                lines[:k] + [lines[k] + " stray tail"] + lines[k + 1:]))
            code, out = run_check("w.md")
            expect("hand-lengthened line", code, 1, out, "wrapped by hand")

            p = subprocess.run(["wrap80", "-w", "60"], input=wrapped,
                               capture_output=True, text=True, check=True)
            open("w.md", "w").write(p.stdout)
            code, out = run_check("w.md")
            expect("rewrapped narrower", code, 1, out, "wrapped by hand")

            open("w.md", "w").write(
                "# Control\n\n" + para1 + "\n\n" + blocks[2])
            code, out = run_check("w.md")
            expect("paragraph mid-edit", code, 0, out, "mid-edit")

            open("w.md", "w").write(wrapped + "\n2b. not a list item\n")
            code, out = run_check("w.md")
            expect("fake enumerator", code, 1, out, "enumerator")
            open("w.md", "w").write(wrapped)
            # A fence of the other kind inside a block is content, and so
            # is a shorter one of the same kind; a closer of the same kind
            # at least as long ends the block.
            got = fake_markers("~~~\n" + "`" * 3 + "\n1a. shown\n~~~\n")
            expect("fence of another kind inside a block", len(got), 0, "")
            got = fake_markers("`" * 4 + "\n" + "`" * 3 + "\n1a. shown\n"
                               + "`" * 4 + "\n")
            expect("shorter fence inside a block", len(got), 0, "")
            got = fake_markers("~~~\nx\n~~~\n1a. shown\n")
            expect("closed block", len(got), 1, "")

            wl = open("l.md").read()
            flat_l = subprocess.run(["wrap80", "--unwrap", "l.md"],
                                    capture_output=True, text=True,
                                    check=True).stdout
            wblock = wl.split("\n\n")[1].rstrip("\n")
            fblock = flat_l.split("\n\n")[1].rstrip("\n")
            item1_flat = fblock.split("\n")[0]
            wlines = wblock.split("\n")
            k2 = next(i for i, l in enumerate(wlines)
                      if i > 0 and l.startswith("- "))
            open("l.md", "w").write(
                "# L\n\n" + "\n".join([item1_flat] + wlines[k2:]) + "\n")
            code, out = run_check("l.md")
            expect("list item joined", code, 0, out, "mid-edit")

            os.makedirs("sub")
            open("sub/s.md", "w").write(wrapped)
            subprocess.run(["git", "add", "sub/s.md"], check=True,
                           capture_output=True)
            subprocess.run(["git", "commit", "-qm", "sub"], check=True,
                           capture_output=True)
            os.chdir("sub")
            code, out = run_check("s.md")
            os.chdir(td)
            expect("run from a subdirectory", code, 0, out, "ok")
            p = subprocess.run([sys.executable, script, "s.md"],
                               capture_output=True, text=True,
                               cwd=os.path.join(td, "sub"))
            expect("main from a subdirectory", p.returncode, 0, p.stdout,
                   "ok")

            empty = os.path.join(td, "empty")
            os.makedirs(empty)
            subprocess.run(["git", "init", "-q"], cwd=empty, check=True)
            p = subprocess.run([sys.executable, script],
                               capture_output=True, text=True, cwd=empty)
            expect("no tracked document", p.returncode, 2, p.stdout,
                   "BLOCKED")

            bindir = os.path.join(td, "bin")
            os.makedirs(bindir)
            os.symlink(shutil.which("git"), os.path.join(bindir, "git"))
            p = subprocess.run([sys.executable, script, "w.md"],
                               capture_output=True, text=True, cwd=td,
                               env=dict(os.environ, PATH=bindir))
            expect("no wrap80 on PATH", p.returncode, 2, p.stdout, "BLOCKED")
        finally:
            os.chdir(prev)
    for b in bad:
        print(f"FAIL: {b}")
    if not bad:
        print("ok:   every self-test case behaved as expected")
    return 1 if bad else 0


def main():
    if sys.argv[1:] == ["--self-test"]:
        return self_test()
    docs = chdir_root(sys.argv[1:]) or tracked_markdown()
    if not docs:
        print("BLOCKED: git tracks no Markdown file here, nothing checked")
        return 2
    missing = [d for d in docs if not os.path.isfile(d)]
    if missing:
        print(f"no such file: {', '.join(missing)} (run from the repo root)")
        return 2
    results = [check(d) for d in docs]
    bad, blocked = results.count(1), results.count(2)
    print(f"\n{bad} of {len(docs)} document(s) failed"
          + (f", {blocked} could not be checked at all" if blocked else "")
          + ".")
    return 1 if bad else (2 if blocked else 0)


if __name__ == "__main__":
    sys.exit(main())
