#!/usr/bin/env python3
"""Benchmark the SDL frontend's runtime for one commit against an earlier one.

Usage: python3 tools/bench-sdl.py [--rounds N] [--ghc GHC] [[BASE] X]
       python3 tools/bench-sdl.py --self-test

X, unless given, is the latest commit at HEAD or before it that changes
a file under one of the directories UI_DIRS names, and BASE, unless given,
the latest such commit strictly before X, so that a commit changing the UI
is measured against the previous one that did, whatever was committed after
it. Run it unsandboxed, from inside the repository: Xvfb needs to write
/tmp/.X11-unix.

Each side is built optimized with GHC (ghc-9.12.4 unless --ghc) and without
cabal.project.local, and its binary is cached by commit hash. Then the
Makefile's benchFrontendBattle and benchFrontendCrawl, which draw through
Sdl.hs, and benchBattle, a null-frontend control that never runs it, are run
with the flags `make -n` prints from X's committed Makefile, each run under
xvfb-run with EGL pinned to Mesa, pinned to one CPU, and in a fresh HOME,
where the game writes its report.

The judged counts are user-space instructions (perf stat, which under
llvmpipe counts the renderer's threads too) and bytes allocated (+RTS -t).
A binary repeats both across runs within a few thousandths of a percent, so
by default each bench runs once per side, and the run exits 1 where X
exceeds BASE by more than 0.2% in instructions or 0.02% in allocation in
either SDL bench. FPS, moving by percents between runs of one binary, is
then printed and not judged where the counts pass; where they flag, the run
goes on to ESCALATE_ROUNDS rounds, which judge FPS too and add their verdict
to the report, the exit staying 1. Rounds alternate the sides' order, each
quantity is the median of the per-round ratios X/BASE, and FPS down by more
than 2% is a finding. --rounds N runs N rounds from the start, everything
judged. The control is printed and never judged.

Exit 0 clean, 1 with findings, 2 when the comparison did not happen:
a revision that does not resolve, no commit changing the UI to pick, a tool
missing, a failed build or run, a value missing from a run, or the two sides
differing in frames or in game clips (the session time times the clips per
second), so having played different games. Where perf cannot count, the
instructions are left out and the run says so loudly, judging allocation
alone.

The working area is dist-newstyle/bench-sdl/, which git ignores. A build
extracts its commit to src/ there with git archive, so nothing tracked
changes and no worktree is registered, and builds into build/, kept across
runs so that a rebuild is incremental. bins/ holds the cached binaries,
named by hash with no hyphen, since the game names its report directory
after its executable's name up to the first hyphen; last/ holds the latest
run's raw files.

Non-vacuity: --self-test puts synthetic rows through the judging, a scratch
repository through the commit picking, sample output through the parsers
and a sample Makefile through the recipe reader; that it bites is
tools/mutants.py's to show.
"""

import argparse
import os
import re
import shlex
import shutil
import signal
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

# A commit changing a file under these is what the bench is for, and the
# default BASE is the previous such commit.
UI_DIRS = ("engine-src/Game/LambdaHack/Client/UI/",
           "GameDefinition/game-src/Client/UI/")
# (name, Makefile target, judged): the two SDL benches and the control.
BENCHES = (("frontendBattle", "benchFrontendBattle", True),
           ("frontendCrawl", "benchFrontendCrawl", True),
           ("battle", "benchBattle", False))
INS_BAR = 1.002
ALLOC_BAR = 1.0002
FPS_BAR = 0.98
ESCALATE_ROUNDS = 10
CPU = "3"
BUSY_BAR = 5.0
RUN_TIMEOUT = 600
RECIPE_HEAD = "$(cabal list-bin exe:LambdaHack) "
XVFB = ["xvfb-run", "-a", "-s", "-screen 0 1280x1024x24"]
GL_ENV = {"__EGL_VENDOR_LIBRARY_FILENAMES":
          "/usr/share/glvnd/egl_vendor.d/50_mesa.json",
          "LIBGL_ALWAYS_SOFTWARE": "1"}


class NotRun(Exception):
    """The comparison did not happen, for the reason given."""


NUM = r"([0-9]+(?:\.[0-9]+)?(?:e[+-]?[0-9]+)?)"
REPORT_RE = re.compile(
    r"Session time: " + NUM + r"s; frames: ([0-9]+)\. Average clips per"
    r" second: " + NUM + r"\. Average FPS: " + NUM + r"\.")
PERF_RE = re.compile(r"^([0-9]+),[^,]*,instructions", re.M)
ALLOC_RE = re.compile(r'\("bytes allocated", "([0-9]+)"\)')


def parse_run(report, perf, rts):
    """One run's values from the game's stdout.txt, perf's CSV (None where
    perf is not used) and the RTS's -t file."""
    m = REPORT_RE.search(report)
    if not m:
        raise NotRun("no Session line in the game's report")
    a = ALLOC_RE.search(rts)
    if not a:
        raise NotRun("no bytes allocated in the RTS statistics")
    row = dict(frames=int(m.group(2)), fps=float(m.group(4)),
               clips=round(float(m.group(1)) * float(m.group(3))),
               alloc=int(a.group(1)), ins=None)
    if perf is not None:
        i = PERF_RE.search(perf)
        if not i:
            raise NotRun("no instruction count in perf's output")
        row["ins"] = int(i.group(1))
    return row


def recipe_args(makefile, target):
    """The arguments the Makefile's recipe for TARGET gives the game."""
    try:
        p = subprocess.run(["make", "-n", "--no-print-directory", "-f", "-",
                            target], input=makefile, capture_output=True,
                           text=True)
    except OSError as e:
        raise NotRun(f"cannot run make: {e}")
    if p.returncode:
        raise NotRun(f"make -n {target} failed: {p.stderr.strip()}")
    lines = [l for l in p.stdout.splitlines() if l.strip()]
    if len(lines) != 1 or not lines[0].startswith(RECIPE_HEAD):
        raise NotRun(f"the recipe of {target} is not one line starting"
                     f" {RECIPE_HEAD.strip()}: {p.stdout!r}")
    args = shlex.split(lines[0][len(RECIPE_HEAD):])
    if "--stopAfterFrames" not in args:
        raise NotRun(f"the recipe of {target} has no --stopAfterFrames")
    return args


def pct(ratio):
    return f"{(ratio - 1) * 100:+.4f}%"


def judge(rows, rounds_mode, perf_ok):
    """(status, lines, findings) for rows {(round, bench, side): values},
    side being "BASE" or "X"."""
    lines = [f"{'bench':15} {'n':>2} {'frames':>6} {'clips':>7}"
             f" {'instr X/BASE':>12} {'alloc X/BASE':>12}"
             f" {'FPS BASE':>8} {'FPS X':>8} {'FPS X/BASE':>10}"]
    findings = []
    for bench, _, judged in BENCHES:
        rounds = sorted({r for (r, b, _) in rows if b == bench})
        if not rounds:
            return 2, lines + [f"{bench}: no runs"], findings
        pairs = []
        for r in rounds:
            if (r, bench, "BASE") not in rows or (r, bench, "X") not in rows:
                return 2, lines + [f"{bench}: round {r} lacks a side"], \
                    findings
            pairs.append((rows[(r, bench, "BASE")], rows[(r, bench, "X")]))
        for r, (a, x) in zip(rounds, pairs):
            if a["frames"] != x["frames"] or a["clips"] != x["clips"]:
                return 2, lines + [
                    f"{bench} round {r}: BASE {a['frames']} frames"
                    f" {a['clips']} clips, X {x['frames']} frames"
                    f" {x['clips']} clips, so they played different games"], \
                    findings

        def med(k):
            return statistics.median(x[k] / a[k] for a, x in pairs)
        alloc, fps = med("alloc"), med("fps")
        ins = med("ins") if perf_ok else None
        bad = []
        if judged:
            if ins is not None and ins > INS_BAR:
                bad.append(f"{bench} instructions {pct(ins)}")
            if alloc > ALLOC_BAR:
                bad.append(f"{bench} allocation {pct(alloc)}")
            if rounds_mode and fps < FPS_BAR:
                bad.append(f"{bench} FPS {pct(fps)}")
        findings += bad
        note = "  REGRESSION" if bad else "" if judged else "  (control)"
        lines.append(
            f"{bench:15} {len(pairs):2} {pairs[0][0]['frames']:6}"
            f" {pairs[0][0]['clips']:7}"
            f" {pct(ins) if ins is not None else 'n/a':>12} {pct(alloc):>12}"
            f" {statistics.median(a['fps'] for a, _ in pairs):8.1f}"
            f" {statistics.median(x['fps'] for _, x in pairs):8.1f}"
            f" {(fps - 1) * 100:+9.2f}%{note}")
    return (1 if findings else 0), lines, findings


def rounds_to_add(status, forced):
    """The rounds the FPS stage adds to the first: the rest of
    ESCALATE_ROUNDS where the counts of the default mode flagged, none where
    they passed or did not run, nor under --rounds, which judged everything
    from the start."""
    return ESCALATE_ROUNDS - 1 if status == 1 and not forced else 0


def git(root, *args):
    p = subprocess.run(["git", "-C", str(root), *args], capture_output=True,
                       text=True)
    if p.returncode:
        raise NotRun(f"git {' '.join(args)}: {p.stderr.strip()}")
    return p.stdout.strip()


def commit(root, rev):
    try:
        return git(root, "rev-parse", "--verify", "--quiet", rev + "^{commit}")
    except NotRun:
        raise NotRun(f"{rev} is no commit") from None


def describe(root, h):
    return git(root, "log", "-1", "--format=%h %s", h)


def pick(root, revs):
    """(X, BASE, why X, why BASE) for REVS, which is [[BASE] X]."""
    ui = " or ".join(UI_DIRS)
    if revs:
        x, xwhy = commit(root, revs[-1]), "given"
    else:
        x = git(root, "log", "-1", "--format=%H", "HEAD", "--", *UI_DIRS)
        if not x:
            raise NotRun("no commit changes " + ui)
        xwhy = "the latest commit at HEAD or before it that changes " + ui
    if len(revs) == 2:
        return x, commit(root, revs[0]), xwhy, "given"
    base = git(root, "log", "-1", "--format=%H", x + "^", "--", *UI_DIRS)
    if not base:
        raise NotRun("no commit before X changes " + ui)
    return x, base, xwhy, "the latest commit before X that changes " + ui


def busy():
    """The share of the CPUs not idle over two seconds, in percent."""
    def read():
        with open("/proc/stat") as f:
            v = [int(x) for x in f.readline().split()[1:9]]
        return sum(v), v[3] + v[4]
    t1, i1 = read()
    time.sleep(2)
    t2, i2 = read()
    d = t2 - t1
    return 100.0 if d <= 0 else 100.0 * (d - (i2 - i1)) / d


def wait_idle(tries):
    for k in range(tries):
        b = busy()
        if b < BUSY_BAR:
            print(f"machine busy {b:.1f}%, under {BUSY_BAR}%")
            return
        if k + 1 < tries:
            print(f"machine busy {b:.1f}%, waiting")
            time.sleep(28)
    print(f"WARNING: machine busy {b:.1f}%, at or over {BUSY_BAR}%;"
          f" FPS is unreliable", flush=True)


def build(root, work, h, ghc):
    """The binary of commit H, built unless cached."""
    binp = work / "bins" / ghc / h[:12]
    if binp.is_file():
        print(f"cached {binp.relative_to(root)}")
        return binp
    src = work / "src"
    if src.exists():
        shutil.rmtree(src)
    src.mkdir(parents=True)
    with subprocess.Popen(["git", "-C", str(root), "archive", "--format=tar",
                           h], stdout=subprocess.PIPE) as arch:
        tar = subprocess.run(["tar", "-x", "-m", "-C", str(src)],
                             stdin=arch.stdout)
    if arch.returncode or tar.returncode:
        raise NotRun(f"cannot extract {h[:12]} to {src}")
    flags = ["exe:LambdaHack", "--enable-optimization", "-w", ghc,
             "--builddir", str(work / "build")]
    (work / "logs").mkdir(exist_ok=True)
    log = work / "logs" / f"build-{h[:12]}.log"
    print(f"build {h[:12]} start {time.strftime('%T')}", flush=True)
    with open(log, "w") as f:
        p = subprocess.run(["cabal", "build", *flags], cwd=src, stdout=f,
                           stderr=subprocess.STDOUT)
    if p.returncode:
        raise NotRun(f"the build of {h[:12]} failed, see {log}")
    p = subprocess.run(["cabal", "list-bin", *flags], cwd=src,
                       capture_output=True, text=True)
    if p.returncode or not os.path.isfile(p.stdout.strip()):
        raise NotRun(f"cabal list-bin found no binary for {h[:12]}")
    binp.parent.mkdir(parents=True, exist_ok=True)
    tmp = binp.with_name(binp.name + ".tmp")
    shutil.copy2(p.stdout.strip(), tmp)
    os.replace(tmp, binp)
    print(f"build {h[:12]} done {time.strftime('%T')}")
    return binp


def run_one(work, binp, args, rundir, perf_ok):
    """One run of the game: its values, or NotRun naming the run's files."""
    rundir.mkdir(parents=True)
    home = Path(tempfile.mkdtemp(prefix="home", dir=work))
    try:
        cmd = [*XVFB, "taskset", "-c", CPU]
        if perf_ok:
            cmd += ["perf", "stat", "-x,", "-e", "instructions:u", "-o",
                    str(rundir / "perf.csv")]
        cmd += [str(binp), *args, "+RTS", f"-t{rundir / 'rts.txt'}",
                "--machine-readable", "-RTS"]
        env = dict(os.environ, HOME=str(home), **GL_ENV)
        with open(rundir / "run.log", "w") as f:
            p = subprocess.Popen(cmd, env=env, stdout=f,
                                 stderr=subprocess.STDOUT,
                                 start_new_session=True)
            try:
                rc = p.wait(timeout=RUN_TIMEOUT)
            except subprocess.TimeoutExpired:
                os.killpg(p.pid, signal.SIGKILL)
                p.wait()
                raise NotRun(f"{rundir} did not end in {RUN_TIMEOUT} s")
        game = home / ("." + binp.name.split("-")[0])
        for name in ("stdout.txt", "stderr.txt"):
            if (game / name).is_file():
                shutil.copy2(game / name, rundir / name)
        if rc:
            raise NotRun(f"{rundir} exited {rc}")

        def read(name):
            try:
                return (rundir / name).read_text()
            except OSError:
                return ""
        try:
            return parse_run(read("stdout.txt"),
                             read("perf.csv") if perf_ok else None,
                             read("rts.txt"))
        except NotRun as e:
            raise NotRun(f"{rundir}: {e}")
    finally:
        shutil.rmtree(home, ignore_errors=True)


def perf_counts(work):
    out = work / "perf-probe.csv"
    try:
        p = subprocess.run(["perf", "stat", "-x,", "-e", "instructions:u",
                            "-o", str(out), "true"], capture_output=True)
        return p.returncode == 0 and PERF_RE.search(out.read_text())
    except OSError:
        return False


def compare(args):
    root = Path(git(Path.cwd(), "rev-parse", "--show-toplevel"))
    x, base, xwhy, basewhy = pick(root, args.revs)
    print(f"X     {describe(root, x)}\n      ({xwhy})")
    print(f"BASE  {describe(root, base)}\n      ({basewhy})", flush=True)

    for tool in ("cabal", "make", "tar", "xvfb-run", "taskset"):
        if not shutil.which(tool):
            raise NotRun(f"{tool} is not on PATH")
    try:
        os.makedirs("/tmp/.X11-unix", exist_ok=True)
    except OSError:
        pass
    if not os.access("/tmp/.X11-unix", os.W_OK):
        raise NotRun("/tmp/.X11-unix is not writable, so Xvfb cannot"
                     " listen: run unsandboxed")
    work = root / "dist-newstyle" / "bench-sdl"
    work.mkdir(parents=True, exist_ok=True)
    perf_ok = perf_counts(work)
    if not perf_ok:
        print("WARNING: perf cannot count instructions:u here, so"
              " instructions are left out and allocation alone is judged",
              flush=True)

    makefile = git(root, "show", f"{x}:Makefile") + "\n"
    benches = {b: recipe_args(makefile, t) for b, t, _ in BENCHES}
    base_makefile = git(root, "show", f"{base}:Makefile") + "\n"
    if any(recipe_args(base_makefile, t) != benches[b]
           for b, t, _ in BENCHES):
        print("WARNING: BASE's Makefile gives the benches other flags;"
              " both sides run X's")

    bins = {"BASE": build(root, work, base, args.ghc),
            "X": build(root, work, x, args.ghc)}
    rounds = args.rounds or 1
    wait_idle(10 if args.rounds else 1)
    last = work / "last"
    if last.exists():
        shutil.rmtree(last)
    sides = f"{x[:9]} against {base[:9]}"
    rows = {}

    def stage(first, last_round, rounds_mode):
        print(f"rounds {first}--{last_round} start {time.strftime('%T')},"
              f" raw files in {last.relative_to(root)}", flush=True)
        for r in range(first, last_round + 1):
            order = ("BASE", "X") if r % 2 else ("X", "BASE")
            for bench, _, _ in BENCHES:
                for side in order:
                    rows[(r, bench, side)] = run_one(
                        work, bins[side], benches[bench],
                        last / f"{r}-{bench}-{side}", perf_ok)
        print(f"rounds end {time.strftime('%T')},"
              f" machine busy {busy():.1f}%")
        status, lines, findings = judge(rows, rounds_mode, perf_ok)
        print("\n".join(lines))
        if status == 2:
            return status, f"NOT RUN, {lines[-1]}"
        if status == 1:
            return status, f"REGRESSION, {'; '.join(findings)}"
        judged = ("instructions, " if perf_ok else "") + "allocation" + \
            (", FPS" if rounds_mode else "")
        return status, f"PASS, {judged} within the bars" + \
            ("" if perf_ok else " (instructions NOT judged: no perf)")

    status, said = stage(1, rounds, bool(args.rounds))
    more = rounds_to_add(status, bool(args.rounds))
    if not more:
        print(f"bench-sdl: {said}, {sides}")
        return status
    print(f"bench-sdl counts: {said}, {sides}\nthe counts flagged, so"
          f" {more} rounds more judge FPS too", flush=True)
    wait_idle(10)
    try:
        _, said2 = stage(rounds + 1, rounds + more, True)
    except NotRun as e:
        said2 = f"NOT RUN, {e}"
    print(f"bench-sdl rounds: {said2}, {sides}")
    print(f"bench-sdl: {said}, {sides}; with {rounds + more} rounds: {said2}")
    return status


# The self-test.

BASE_ROW = {"frontendBattle": dict(frames=2012, clips=6624, fps=325.0,
                                   ins=40_000_000_000, alloc=9_000_000_000),
            "frontendCrawl": dict(frames=7010, clips=24500, fps=343.0,
                                  ins=120_000_000_000, alloc=30_000_000_000),
            "battle": dict(frames=1500, clips=6040, fps=2550.0,
                           ins=3_500_000_000, alloc=1_336_000_000)}


def synth(rounds=1, changes=None, perf=True):
    """Rows of ROUNDS rounds, X equal to BASE but where CHANGES maps
    (bench, key) to a factor, or to a list of one factor per round."""
    rows = {}
    for r in range(1, rounds + 1):
        for bench, base in BASE_ROW.items():
            a = dict(base) if perf else dict(base, ins=None)
            x = dict(a)
            for (b, k), f in (changes or {}).items():
                if b == bench:
                    f = f[r - 1] if isinstance(f, list) else f
                    x[k] = type(a[k])(round(a[k] * f)) \
                        if isinstance(a[k], int) else a[k] * f
            rows[(r, bench, "BASE")], rows[(r, bench, "X")] = a, x
    return rows


CASES = [
    # (name, rows, rounds mode, perf counts, status, a finding's text)
    ("equal sides pass", synth(), False, True, 0, None),
    ("instructions +0.21% in frontendBattle",
     synth(changes={("frontendBattle", "ins"): 1.0021}), False, True, 1,
     "frontendBattle instructions"),
    ("instructions +0.19% in frontendBattle pass",
     synth(changes={("frontendBattle", "ins"): 1.0019}), False, True, 0,
     None),
    ("allocation +0.021% in frontendCrawl",
     synth(changes={("frontendCrawl", "alloc"): 1.00021}), False, True, 1,
     "frontendCrawl allocation"),
    ("allocation +0.019% in frontendCrawl passes",
     synth(changes={("frontendCrawl", "alloc"): 1.00019}), False, True, 0,
     None),
    ("FPS -3% in frontendCrawl under --rounds",
     synth(3, {("frontendCrawl", "fps"): 0.97}), True, True, 1,
     "frontendCrawl FPS"),
    ("FPS -1.9% in frontendCrawl under --rounds passes",
     synth(3, {("frontendCrawl", "fps"): 0.981}), True, True, 0, None),
    ("FPS -3% by default, not judged",
     synth(changes={("frontendCrawl", "fps"): 0.97}), False, True, 0, None),
    ("the control rising, never judged",
     synth(3, {("battle", "ins"): 1.05, ("battle", "alloc"): 1.05,
               ("battle", "fps"): 0.5}), True, True, 0, None),
    ("one round's outlier, the median passing",
     synth(3, {("frontendBattle", "alloc"): [1.0, 1.01, 1.0]}), True, True,
     0, None),
    ("most rounds rising, the median failing",
     synth(3, {("frontendBattle", "alloc"): [1.0, 1.001, 1.001]}), True,
     True, 1, "frontendBattle allocation"),
    ("frames differing", synth(changes={("frontendCrawl", "frames"): 1.001}),
     False, True, 2, None),
    ("game clips differing in the control",
     synth(2, {("battle", "clips"): [1.0, 1.001]}), True, True, 2, None),
    ("a side missing",
     {k: v for k, v in synth(2).items() if k != (2, "frontendBattle", "X")},
     True, True, 2, None),
    ("no perf, allocation judged alone",
     synth(changes={("frontendBattle", "alloc"): 1.00021}, perf=False),
     False, False, 1, "frontendBattle allocation"),
    ("no perf, equal allocation passes", synth(perf=False), False, False, 0,
     None),
]

SAMPLE_REPORT = ("Session time: 0.587921384s; frames: 1500. Average clips per"
                 " second: 10273.48241512508. Average FPS: 2551.361526935036."
                 "\n")
SAMPLE_PERF = ("# started on Sat Nov 21 13:51:29 2026\n\n"
               "3520594120,,instructions:u,615228958,100.00,,\n")
SAMPLE_RTS = (" [(\"bytes allocated\", \"1336074840\")\n"
              " ,(\"num_GCs\", \"14\")\n"
              " ,(\"allocated_bytes\", \"1336074840\")\n")
SAMPLE_MAKEFILE = """\
RNGOPTS1=--setDungeonRng "SMGen 127 123" --setMainRng "SMGen 127 125"

benchSample:
\t$$(cabal list-bin exe:LambdaHack) --newGame 3 --stopAfterFrames 2000 $(RNGOPTS1)

benchEndless:
\t$$(cabal list-bin exe:LambdaHack) --newGame 3 $(RNGOPTS1)
"""


def pick_test():
    """pick over a scratch repository whose commits change no UI file,
    the engine's Client/UI, no UI file, the game's Client/UI and no UI file
    again; the paths are the directories' own, so UI_DIRS is checked too."""
    bad = []
    with tempfile.TemporaryDirectory() as d:
        root = Path(d)
        git(root, "init", "-q")
        h = {}
        for name, path in (
                ("init", "README"),
                ("engine", "engine-src/Game/LambdaHack/Client/UI/A.hs"),
                ("docs1", "README"),
                ("game", "GameDefinition/game-src/Client/UI/B.hs"),
                ("docs2", "README")):
            (root / path).parent.mkdir(parents=True, exist_ok=True)
            (root / path).write_text(name)
            git(root, "add", path)
            git(root, "-c", "user.name=t", "-c", "user.email=t@t", "-c",
                "commit.gpgsign=false", "commit", "-q", "-m", name)
            h[name] = git(root, "rev-parse", "HEAD")
        names = {v: k for k, v in h.items()}
        for revs, want in (([], ("game", "engine")),
                           (["docs2"], ("docs2", "game")),
                           (["init", "docs1"], ("docs1", "init"))):
            try:
                got = pick(root, [h[r] for r in revs])[:2]
            except NotRun as e:
                bad.append(f"pick {revs}: {e}")
                continue
            got = tuple(names.get(g, g) for g in got)
            if got != want:
                bad.append(f"pick {revs}: X {got[0]}, BASE {got[1]},"
                           f" wanted {want[0]}, {want[1]}")
        try:
            pick(root, [h["engine"]])
            bad.append("pick ['engine'] found a BASE, wanted NotRun")
        except NotRun:
            pass
    return bad


def self_test():
    bad = pick_test()
    for name, rows, rounds_mode, perf_ok, want, text in CASES:
        status, lines, findings = judge(rows, rounds_mode, perf_ok)
        if status != want:
            bad.append(f"{name}: status {status}, wanted {want}:\n    "
                       + "\n    ".join(lines))
        elif text and not any(text in f for f in findings):
            bad.append(f"{name}: no finding names {text!r}: {findings}")
        if not rounds_mode:
            more = rounds_to_add(status, False)
            wanted = ESCALATE_ROUNDS - 1 if want == 1 else 0
            if more != wanted:
                bad.append(f"{name}: {more} rounds added, wanted {wanted}")
    status, _, _ = judge(synth(changes={("frontendCrawl", "alloc"): 1.00021}),
                         True, True)
    if status != 1 or rounds_to_add(status, True):
        bad.append(f"flagged under --rounds: status {status}, "
                   f"{rounds_to_add(status, True)} rounds added, wanted 1, 0")
    row = parse_run(SAMPLE_REPORT, SAMPLE_PERF, SAMPLE_RTS)
    want = dict(frames=1500, fps=2551.361526935036, clips=6040,
                alloc=1336074840, ins=3520594120)
    if row != want:
        bad.append(f"sample run parsed as {row}, wanted {want}")
    for name, report, perf, rts in (
            ("no Session line", "", SAMPLE_PERF, SAMPLE_RTS),
            ("perf not counting", SAMPLE_REPORT,
             SAMPLE_PERF.replace("3520594120", "<not counted>"), SAMPLE_RTS),
            ("no bytes allocated", SAMPLE_REPORT, SAMPLE_PERF, "")):
        try:
            parse_run(report, perf, rts)
            bad.append(f"{name}: parsed, wanted NotRun")
        except NotRun:
            pass
    try:
        args = recipe_args(SAMPLE_MAKEFILE, "benchSample")
        want = ["--newGame", "3", "--stopAfterFrames", "2000",
                "--setDungeonRng", "SMGen 127 123",
                "--setMainRng", "SMGen 127 125"]
        if args != want:
            bad.append(f"sample recipe read as {args}, wanted {want}")
    except NotRun as e:
        bad.append(f"sample recipe: {e}")
    for target in ("benchEndless", "benchAbsent"):
        try:
            recipe_args(SAMPLE_MAKEFILE, target)
            bad.append(f"{target}: read, wanted NotRun")
        except NotRun:
            pass
    if bad:
        print("SELF-TEST FAILED:")
        for b in bad:
            print("  " + b)
        return 1
    print(f"self-test passed: {len(CASES)} judged row sets, the commits"
          f" picked, the parsers and the recipe reader as wanted")
    return 0


def main():
    ap = argparse.ArgumentParser(
        description="Benchmark the SDL frontend, X against BASE.")
    ap.add_argument("revs", nargs="*", metavar="REV",
                    help="[[BASE] X]; X defaults to the latest commit"
                    " changing the UI and BASE to the one before X")
    ap.add_argument("--rounds", type=int, metavar="N",
                    help="N interleaved rounds, FPS judged too")
    ap.add_argument("--ghc", default="ghc-9.12.4")
    ap.add_argument("--self-test", action="store_true")
    args = ap.parse_args()
    if args.self_test:
        return self_test()
    if len(args.revs) > 2 or (args.rounds is not None and args.rounds < 1):
        ap.print_usage(sys.stderr)
        return 2
    try:
        return compare(args)
    except NotRun as e:
        print(f"bench-sdl: NOT RUN: {e}")
        return 2


if __name__ == "__main__":
    sys.exit(main())
