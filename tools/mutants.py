"""The mutants of tools/: each checker broken on purpose, and its self-test red.

Read by `selftest-mutants.py tools`, which makes a shared clone of this
repository (and of the sibling checkouts beside it, where mounted), applies
each mutant to the clone and requires the judge -- the tool's own self-test
-- to FAIL on it; a mutant whose anchor has moved is LOST, not caught. These
replace the proofs that were dated sentences in each tool's docstring,
"proved non-vacuous by breaking the checker in a copy (2026-08-14)", which
expire the moment the code under them moves, with nothing to say so.

Every row here came over with the tools themselves, from the horde-ad copy
that minted them: below their docstrings and per-repo configuration the two
sets of checkers are one program, which `check-twin-sync.py` holds them to,
so an anchor that matches there matches here. A mutant of this repo's own
making goes in beside them, watched failing before it is written down.

The clone is what lets the judges run: every tool finds the repository from
its own path and asks git about it, `check-doc-refs`' self-test wants the
deployment sibling as a real directory, and `check-twin-sync`'s live run
wants the twin checkout. With a sibling unmounted that tool's judge is
BLOCKED at 2 in the clone and its mutants are LOST, which is the honest
reading.
"""

COPY = 'clone'
SIBLINGS = ['../horde-ad', '../lambdahack.github.io']
TIMEOUT = 600

ST = ['python3', '{file}', '--self-test']

MUTANTS = [
    # check-doc-examples: "blanking the name pattern ... 0/1 findings"
    ('check-doc-examples name pattern blanked', 'check-doc-examples.py',
     'NAME_RE = re.compile(r"\\b([A-Z][A-Za-z0-9_]{3,})\\b")\n', 'NAME_RE = re.compile(r"(?!x)x")\n', ST),
    # check-doc-examples: "disabling the output comparison ... 1/0"
    ('check-doc-examples output comparison disabled', 'check-doc-examples.py',
     "        if len(body) > 40 and body not in normsrc:\n", "        if False:\n", ST),
    # check-doc-examples: "dropping the doc-local exclusion ... 2/1" (3/1 today, the
    # module-name control having been added since)
    ('check-doc-examples doc-local exclusion dropped', 'check-doc-examples.py',
     '        if n in loc or re.search(r"\\b" + n + r"\\b", src):\n',
     '        if re.search(r"\\b" + n + r"\\b", src):\n', ST),
    # check-doc-examples: "removing the skip in a copy turned it red" (module Main)
    ('check-doc-examples module Main skip removed', 'check-doc-examples.py',
     '              if not re.search(r"^module\\s+Main\\b", b, re.M)]\n', '              if True]\n', ST),
    # check-doc-examples: "removing that line from a copy reported the module's name"
    ('check-doc-examples module header no longer a doc-local name', 'check-doc-examples.py',
     '    for m in re.finditer(r"^module\\s+([\\w.]+)", code, re.M):\n        out |= set(m.group(1).split("."))\n',
     '    pass\n', ST),
    # check-doc-examples: "a source list naming nothing reads as none rather than as an
    # empty corpus" (2026-08-28)
    ('check-doc-examples empty source list read as an empty corpus', 'check-doc-examples.py',
     "    if p.returncode != 0 or not paths:\n        return None\n",
     "    if p.returncode != 0 or not paths:\n        return \"\"\n", ST),
    # check-doc-refs: "a dead cabal-target loop" (2026-08-14)
    ('check-doc-refs cabal-target loop dead', 'check-doc-refs.py',
     "    for name in sorted(set(CABAL_RE.findall(commands))):\n", "    for name in []:\n", ST),
    # check-doc-refs: "a dead sibling resolution"
    ('check-doc-refs sibling resolution dead', 'check-doc-refs.py',
     "    roots = [r for r in SIBLING_ROOTS if os.path.isdir(r)]\n    if not roots:\n        return []\n",
     "    return []\n", ST),
    # check-doc-refs: "dropping the path-shape gate off the sibling arm ... on exactly
    # the `tests` rubber-stamp row"
    ('check-doc-refs path-shape gate dropped off the sibling arm', 'check-doc-refs.py',
     "        elif path_shaped(token, top_level) and sibling_hit(token, siblings):\n",
     "        elif sibling_hit(token, siblings):\n", ST),
    # check-doc-refs: "a CITE_RE blind to the range citation ... on exactly the
    # skipped-citation guard"
    ('check-doc-refs CITE_RE blind to the range citation', 'check-doc-refs.py',
     'CITE_RE = re.compile(r":\\d+(?:-\\d+)?(?:,\\d+(?:-\\d+)?)*$")\n',
     'CITE_RE = re.compile(r":\\d+(?:,\\d+)*$")\n', ST),
    # check-doc-refs: ".../ghc-9.12/... was read as a sibling path until the ../ test
    # was made to require the slash"
    ('check-doc-refs sibling-path test no longer requires the slash', 'check-doc-refs.py',
     '        elif token.startswith("../"):\n', '        elif token.startswith(".."):\n', ST),
    # check-doc-wrap: "disabling the fake-enumerator branch" (2026-08-14)
    ('check-doc-wrap fake-enumerator branch disabled', 'check-doc-wrap.py',
     "    fake = fake_markers(have)\n", "    fake = []\n", ST),
    # check-doc-wrap: "counting every differing paragraph as mid-edit"
    ('check-doc-wrap every differing paragraph counted as mid-edit', 'check-doc-wrap.py',
     '            if all(l in ok for l in h.split("\\n")):\n                loose += 1\n',
     '            if True:\n                loose += 1\n', ST),
    # check-doc-wrap: "folding BLOCKED back into exit 1"
    ('check-doc-wrap BLOCKED folded back into exit 1', 'check-doc-wrap.py',
     "    return 1 if bad else (2 if blocked else 0)\n", "    return 1 if bad or blocked else 0\n", ST),
    # check-doc-wrap: a fence closed by one of another kind (check-doc-wrap-06)
    ('check-doc-wrap closes a block with a fence of any kind', 'check-doc-wrap.py',
     "        elif m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):\n",
     "        elif m:\n", ST),
    # check-doc-wrap: "a repository tracking no Markdown reports BLOCKED rather than
    # 0 of 0 failed" (2026-08-28)
    ('check-doc-wrap repository tracking no Markdown reported as 0 of 0 failed', 'check-doc-wrap.py',
     '        print("BLOCKED: git tracks no Markdown file here, nothing checked")\n        return 2\n',
     '        pass\n', ST),
    # check-plan-citations: "disabling the PROSE-LINE refusal" (2026-08-14)
    ('check-plan-citations PROSE-LINE refusal disabled', 'check-plan-citations.py',
     '        if name.endswith(".md"):\n', '        if False:\n', ST),
    # check-plan-citations: "short-circuiting the publication test"
    ('check-plan-citations publication test short-circuited', 'check-plan-citations.py',
     "    return reachable_from(sha, PUBLISHED_REF)\n", "    return True\n", ST),
    # check-plan-citations: a stamp the formatter wrapped inside a blockquote
    # read as no stamp at all -- --restamp refused it and its orphan and
    # publication checks passed in silence (2026-09-04)
    ('check-plan-citations stamp regex blind to a wrapped line', 'check-plan-citations.py',
     '    r"((?:`|\\*\\*)[\\s>]*\\()(\\d{4}-\\d{2}-\\d{2})(\\))")\n',
     '    r"((?:`|\\*\\*)\\s*\\()(\\d{4}-\\d{2}-\\d{2})(\\))")\n', ST),
    # check-plan-citations: "disabling the dirty-cited-file refusal"
    ('check-plan-citations dirty-cited-file refusal disabled', 'check-plan-citations.py',
     "        if dirty:\n", "        if False:\n", ST),
    # check-plan-citations: a failed git status read as clean (check-plan-citations-05)
    ('check-plan-citations reads a failed git status as clean', 'check-plan-citations.py',
     "        if p.returncode != 0:\n            print(f\"\\nnot restamping {doc}: git status could not be read\"\n",
     "        if False:\n            print(f\"\\nnot restamping {doc}: git status could not be read\"\n", ST),
    # check-plan-citations: "line-zero, backwards-range ... rows" (2026-08-28); the bare
    # condition occurs twice, so the anchor carries the print line
    ('check-plan-citations line zero and backwards range accepted', 'check-plan-citations.py',
     '        if lo < 1 or lo > hi or hi > len(lines):\n            print(f"FAIL {name}:{lo}-{hi} --- OUT-OF-RANGE "\n',
     '        if hi > len(lines):\n            print(f"FAIL {name}:{lo}-{hi} --- OUT-OF-RANGE "\n', ST),
    # check-plan-citations: "second-document row" -- until 2026-08-28 only the first
    # document was checked
    ('check-plan-citations only the first document checked', 'check-plan-citations.py',
     "    for doc in docs:\n        if len(docs) > 1:\n", "    for doc in docs[:1]:\n        if len(docs) > 1:\n", ST),
    # check-twin-sync: "a comparable() that returns the empty string for every script"
    # (2026-08-14)
    ('check-twin-sync comparable() returns the empty string for every script', 'check-twin-sync.py',
     '    return "\\n".join(l for l in out if l.strip())\n', '    return ""\n', ST),
    # check-twin-sync: "a shared shell script is compared whole" (2026-08-28 row)
    ('check-twin-sync non-Python file no longer compared whole', 'check-twin-sync.py',
     "    except SyntaxError:\n        return text\n", "    except SyntaxError:\n        return \"\"\n", ST),
    # check-twin-sync: "a TWIN_SKIP file is not" compared
    ('check-twin-sync TWIN_SKIP allowlist ignored', 'check-twin-sync.py',
     "                if os.path.isfile(p) and os.path.basename(p) not in TWIN_SKIP}\n",
     "                if os.path.isfile(p)}\n", ST),
    # heading-outline: "a closing fence is not a heading's text ... reported '## ```'"
    # (2026-08-28)
    ('heading-outline closing fence read as a Setext heading text', 'heading-outline.py',
     "            prev = ''      # neither a fence nor its contents underlines\n", "            prev = line\n", ST),
    # heading-outline: "the list item the same day, reported as '## - item'"
    ('heading-outline list item read as a Setext heading text', 'heading-outline.py',
     "                     and not LIST_ITEM.match(prev))\n", "                     and True)\n", ST),
    # heading-outline: the original fenced-`#`/`===` branch of the hand recipe
    ('heading-outline fenced lines read as headings', 'heading-outline.py',
     "        if m or fence:\n", "        if False:\n", ST),
    # heading-outline: a fence closed by one of another kind (heading-outline-03)
    ('heading-outline closes a block with a fence of any kind', 'heading-outline.py',
     "        elif m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):\n",
     "        elif m:\n", ST),
    # heading-outline: any leading rule taken for frontmatter (heading-outline-04)
    ('heading-outline any leading rule read as frontmatter', 'heading-outline.py',
     "        if lines[i].strip() and not YAML_LINE.match(lines[i]):\n            return 0\n",
     "        if False:\n            return 0\n", ST),
    # check-doc-refs: a fence closed by one of another kind (check-doc-refs-05)
    ('check-doc-refs closes a block with a fence of any kind', 'check-doc-refs.py',
     "        elif m and m.group(1)[0] == fence[0] and len(m.group(1)) >= len(fence):\n",
     "        elif m:\n", ST),
    # check-doc-refs: SIBLING_ROOTS = [] degrades local drift to SKIP (check-doc-refs-06)
    ('check-doc-refs no sibling configured degrades local drift', 'check-doc-refs.py',
     "            if sib_active or not SIBLING_ROOTS:\n", "            if sib_active:\n", ST),
    # check-doc-refs: the self-test dispatched before the move to the root
    # (check-doc-refs-07); judged from the tool's own directory, where the
    # root's judge cannot tell
    ('check-doc-refs self-test dispatched before chdir_root', 'check-doc-refs.py',
     '    docs = chdir_root(args)\n    if "--self-test" in sys.argv[1:]:\n        return self_test()\n',
     '    if "--self-test" in sys.argv[1:]:\n        return self_test()\n    docs = chdir_root(args)\n',
     'cd {dir} && python3 {file} --self-test'),
    # check-doc-examples: the same (check-doc-examples-04)
    ('check-doc-examples self-test dispatched before chdir_root', 'check-doc-examples.py',
     '    docs = chdir_root(args)\n    if "--self-test" in sys.argv[1:]:\n        return self_test()\n',
     '    if "--self-test" in sys.argv[1:]:\n        return self_test()\n    docs = chdir_root(args)\n',
     'cd {dir} && python3 {file} --self-test'),
    # check-plan-citations: CITE_RE blind to json and sh (check-plan-citations-07)
    ('check-plan-citations CITE_RE blind to json and sh', 'check-plan-citations.py',
     '    r"\\.(?:hs|ts|py|c|h|cabal|mjs|html|md|txt|yaml|yml|json|sh)|Makefile)"\n',
     '    r"\\.(?:hs|ts|py|c|h|cabal|mjs|html|md|txt|yaml|yml)|Makefile)"\n', ST),
    # check-doc-wrap: indented code no longer exempt (check-doc-wrap-07)
    ('check-doc-wrap indented code block read as prose', 'check-doc-wrap.py',
     "        elif fence is None and indented and (blank or code):\n", "        elif False:\n", ST),
    # check-doc-wrap: a fence line at any indentation opens or closes a block again
    # (check-doc-wrap-08)
    ('check-doc-wrap indented fence line read as a fence', 'check-doc-wrap.py',
     'FENCE = re.compile(r"^ {0,3}(`{3,}|~{3,})")\n', 'FENCE = re.compile(r"^\\s*(`{3,}|~{3,})")\n', ST),
    # check-doc-examples: the from-the-root control may agree at exit 2 again
    # (check-doc-examples-05). Judged with README.md aside, where the self-test
    # must FAIL: the judge is that failure, so it passes on the guarded checker
    # and fails on the mutant, which reports PASS over two runs that did not
    # happen. A judge whose setup fails exits 0 and the mutant survives, loudly.
    ('check-doc-examples control passes over two runs that did not happen', 'check-doc-examples.py',
     "    if here.returncode == 2:\n        ok = False\n", "    if False:\n        ok = False\n",
     'cd {dir} && mv ../README.md ../README.md.aside || exit 0; python3 {file} --self-test; rc=$?; '
     'mv ../README.md.aside ../README.md; test $rc -ne 0'),
    # check-twin-sync: the code case's mutation of comparable() writes nothing
    ('check-twin-sync code case mutates nothing', 'check-twin-sync.py',
     '            lines[at] += "  # mutated"\n', '            lines[at] += ""\n', ST),
    # check-plan-crossrefs: one break per assertion, each proved caught the
    # day the checker was written (2026-09-04), plus the disclaimer window
    # and the grammar gates it rests on
    ('check-plan-crossrefs A1 claim vocabulary blanked', 'check-plan-crossrefs.py',
     'CLAIM_RE = re.compile(r"\\b(?:entry|entries|deletion)\\b")\n',
     'CLAIM_RE = re.compile(r"(?!x)x")\n', ST),
    ('check-plan-crossrefs A1 allowlist-in-Owns check disabled', 'check-plan-crossrefs.py',
     "                    if x in self.items and doc.allow_key not in self.owns(x):\n",
     "                    if False:\n", ST),
    ('check-plan-crossrefs A2 every claimant acknowledged', 'check-plan-crossrefs.py',
     "        if y in named[x]:\n            return True\n",
     "        if True:\n            return True\n", ST),
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
     "            for iid in sorted(ids & seen):\n", "            for iid in []:\n", ST),
]
