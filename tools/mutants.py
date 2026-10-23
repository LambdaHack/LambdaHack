"""The mutants of tools/: each checker broken on purpose, and its self-test red.

Read by `selftest-mutants.py tools`, which makes a shared clone of this
repository, applies each mutant to the clone and requires the judge -- the
tool's own self-test -- to FAIL on it; a mutant whose anchor has moved is
LOST, not caught. These replace the proofs that were dated sentences in each
tool's docstring, "proved non-vacuous by breaking the checker in a copy
(2026-08-14)", which expire the moment the code under them moves, with
nothing to say so. Each is a mutant of this repository's own tools,
documents or code a check reads, watched failing before it was written down;
the shared document checkers' are in ~/slop/tools/mutants.py, with
the checkers.

The clone is what lets the judges run: every tool finds the repository from
its own path and asks git about it.
"""

import importlib.util
import pathlib

COPY = 'clone'
SIBLINGS = []
TIMEOUT = 600

ST = ['python3', '{file}', '--self-test']
# The probe runner is the doc-verification skill's, user-scope, outside this
# tree; a judge over a document rather than over a tool.
PROBES = ['python3', str(pathlib.Path.home() / '.claude/skills/doc-verification/run-probes.py'), '{file}']


def step(name):
    """The argv of a checks.py step, so that a mutant of what the step checks
    is judged by the step as check-all runs it, not by a copy of it."""
    spec = importlib.util.spec_from_file_location(
        'checks', pathlib.Path(__file__).with_name('checks.py'))
    checks = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checks)
    return next(s[1] for s in checks.STEPS if s[0] == name)


MUTANTS = [
    # A document probe with its expectation moved: the doc-probes step of
    # checks.py, judged by the probe runner over that document, must fail.
    ('doc probe expectation moved', '../docs/leader-desync-migration.md',
     "':!docs/leader-desync-migration.md' | wc -l :: 12 -->",
     "':!docs/leader-desync-migration.md' | wc -l :: 99 -->",
     PROBES),
    # check-plan-crossrefs: one break per assertion, each proved caught the
    # day the checker was written (2026-09-04), plus the disclaimer window
    # and the grammar gates it rests on
    ('check-plan-crossrefs A1 claim vocabulary blanked', 'check-plan-crossrefs.py',
     'CLAIM_RE = re.compile(r"\\b(?:entry|entries|deletion)\\b")\n',
     'CLAIM_RE = re.compile(r"(?!x)x")\n', ST),
    ('check-plan-crossrefs A1 allowlist-in-Owns check disabled', 'check-plan-crossrefs.py',
     "                    if x in self.items and doc.allow_key not in self.owns(x):\n",
     "                    if False:\n", ST),
    ('check-plan-crossrefs A1 counts a landed row as a claimant', 'check-plan-crossrefs.py',
     "            if self.historical(iid):\n                continue\n",
     "", ST),
    ('check-plan-crossrefs A3 counts a landed row as a writer', 'check-plan-crossrefs.py',
     "            if self.historical(x):\n                continue\n            reach",
     "            reach", ST),
    ('check-plan-crossrefs A3 closure reaches every item', 'check-plan-crossrefs.py',
     "        seen, todo = set(), [iid]\n", "        seen, todo = set(self.items), [iid]\n", ST),
    ('check-plan-crossrefs A3 closure no longer transitive', 'check-plan-crossrefs.py',
     "                    seen.add(d)\n                    todo.append(d)\n",
     "                    seen.add(d)\n", ST),
    ('check-plan-crossrefs A4 gate check disabled', 'check-plan-crossrefs.py',
     '                if k in owned and f"`{DOCS_GATE}`" not in self.field(x, "Done"):\n',
     '                if False:\n', ST),
    ('check-plan-crossrefs A5 list count never compared', 'check-plan-crossrefs.py',
     "                    if k != n:\n", "                    if False:\n", ST),
    ('check-plan-crossrefs A5 commit count never compared', 'check-plan-crossrefs.py',
     "                    markers[-1] != NUMBERS[m.group(1)]:\n",
     "                    False:\n", ST),
    ('check-plan-crossrefs disclaimer before a token ignored', 'check-plan-crossrefs.py',
     'NEG_BEFORE_RE = re.compile(r"\\b(?:not|never|neither|nor|no|nothing)\\b"\n'
     '                           r"(?:\\s+[\\w\'-]+){0,3}\\s*$", re.I)\n',
     'NEG_BEFORE_RE = re.compile(r"(?!x)x")\n', ST),
    ('check-plan-crossrefs absolute path to the configured document read as a copy', 'check-plan-crossrefs.py',
     "    return p == d or p.endswith(os.sep + d)\n", "    return p == d\n", ST),
    # check-plan-crossrefs: the bare-extension guard in path_key (`cabal`,
    # `ts` read as files) had A2 as its only witness and lost it when A2
    # retired on 2026-09-14; the guard stays, and no assertion can now see
    # it break, so it carries no mutant.
    ('check-plan-crossrefs pointer at another list read as ownership', 'check-plan-crossrefs.py',
     "NEG_AFTER_RE = re.compile(r\"^'s (?:does|is) at\\b|^ at (?:ID)\\b\"\n",
     "NEG_AFTER_RE = re.compile(r\"^'s (?:does|is) at\\b\"\n", ST),
    ('check-plan-crossrefs disclaimer after a token ignored', 'check-plan-crossrefs.py',
     "                    or self.neg_after_re.search(after))\n",
     "                    or False)\n", ST),
    ('check-plan-crossrefs parenthesized citation read as ownership', 'check-plan-crossrefs.py',
     '        if m.start() > 0 and text[m.start() - 1] == "(" and \\\n',
     '        if False and \\\n', ST),
    ('check-plan-crossrefs missing label not a finding', 'check-plan-crossrefs.py',
     '                if label not in self.items[iid]["fields"]:\n',
     '                if False:\n', ST),
    ('check-plan-crossrefs document without the grammar read as clean', 'check-plan-crossrefs.py',
     "    if not doc.order or not doc.items:\n", "    if False:\n", ST),
    # check-plan-crossrefs: the second grammar and the joint run
    # (2026-09-04), one break per branch the pointman document added
    ('check-plan-crossrefs grammar chosen by position, never by fit', 'check-plan-crossrefs.py',
     "    return fits[0] if len(fits) == 1 else None\n", "    return GRAMMARS[0]\n", ST),
    ('check-plan-crossrefs A1 silent over a document with no allowlist block', 'check-plan-crossrefs.py',
     '                notes.append(f"A1 not run over {doc.path}: no allowlist block"\n',
     '                str(f"A1 not run over {doc.path}: no allowlist block"\n', ST),
    ('check-plan-crossrefs joint run keeps the first document\'s items only', 'check-plan-crossrefs.py',
     "            self.items.update(d.items)\n",
     "            self.items.update(d.items) if not self.items else None\n", ST),
    ('check-plan-crossrefs depends-on range no longer expanded', 'check-plan-crossrefs.py',
     "        for a, b in self.range_re.findall(cell):\n", "        for a, b in []:\n", ST),
    ('check-plan-crossrefs A4 asks only the item\'s own document', 'check-plan-crossrefs.py',
     "            for k, name in self.gated:\n",
     "            for k, name in [(self.doc_of(x).doc_key, self.doc_of(x).path)]:\n", ST),
    ('check-plan-crossrefs two documents sharing an id read as one', 'check-plan-crossrefs.py',
     "            if shared:\n", "            if False:\n", ST),
    ('check-plan-crossrefs a ledger row with no item crashes the closure', 'check-plan-crossrefs.py',
     '        return it["doc"] if it else self.row_doc.get(iid)\n',
     '        return it["doc"]\n', ST),
    # check-sync-exports: one break per finding and per control it rests on,
    # each watched failing the day the checker was written (2026-10-21)
    ('check-sync-exports comments read as code', 'check-sync-exports.py',
     "    return LINE_COMMENT_RE.sub(blank, BLOCK_COMMENT_RE.sub(blank, text))\n",
     "    return text\n", ST),
    ('check-sync-exports a dash operator read as a comment', 'check-sync-exports.py',
     'LINE_COMMENT_RE = re.compile(r"--+(?![!#$%&*+./<=>?@\\\\^|~:-]).*")\n',
     'LINE_COMMENT_RE = re.compile(r"--+.*")\n', ST),
    ('check-sync-exports an async export read as sync', 'check-sync-exports.py',
     "    r'foreign\\s+export\\s+javascript\\s+\"(\\w+)\\s+sync\"\\s+(\\w+)\\s*::')\n",
     "    r'foreign\\s+export\\s+javascript\\s+\"(\\w+)(?:\\s+sync)?\"\\s+(\\w+)\\s*::')\n", ST),
    ('check-sync-exports a signature read as an equation', 'check-sync-exports.py',
     "    start_re = re.compile(rf\"(?m)^{re.escape(hs)}(?![\\w'])(?![ \\t]*::)\")\n",
     "    start_re = re.compile(rf\"(?m)^{re.escape(hs)}(?![\\w'])\")\n", ST),
    ('check-sync-exports an equation it cannot read passes', 'check-sync-exports.py',
     "            if rhs is None:\n                fails.append(",
     "            if rhs is None:\n                str(", ST),
    ('check-sync-exports a missing equation passes', 'check-sync-exports.py',
     "        if not eqs:\n", "        if False:\n", ST),
    ('check-sync-exports every equation passes', 'check-sync-exports.py',
     "            elif not built_on(code, rhs, js):\n",
     "            elif False:\n", ST),
    ('check-sync-exports the JS name not compared', 'check-sync-exports.py',
     "rf'syncExport\\s+\"{re.escape(js)}\"(?![\\w\"])\\s*'",
     "rf'syncExport\\s+\"\\w+\"(?![\\w\"])\\s*'", ST),
    ('check-sync-exports code after the argument passes', 'check-sync-exports.py',
     '    if arg.startswith("$"):\n        return True\n', '    return True\n', ST),
    ('check-sync-exports syncExport for no sync export passes', 'check-sync-exports.py',
     "            if m.group(1) not in static:\n", "            if False:\n", ST),
    ('check-sync-exports a wrapper sync import passes', 'check-sync-exports.py',
     "        for m in WRAPPER_RE.finditer(code):\n", "        for m in []:\n", ST),
    ('check-sync-exports silent over a tree with none', 'check-sync-exports.py',
     "    if not found:\n", "    if False:\n", ST),
    ('check-sync-exports no source read as a clean tree', 'check-sync-exports.py',
     "    if not files:\n", "    if False:\n", ST),
    # The explicit-widths step of checks.py, judged by that step: the decode
    # by the Enum instance it exists to catch, put back into Sdl.hs.
    ('explicit widths toEnum decode restored',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Sdl.hs',
     '          let Point{..} = punindex (rwidth coscreen) i\n',
     '          let Point{..} = toEnum i\n', step('explicit widths')),
]
