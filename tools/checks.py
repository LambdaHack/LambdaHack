"""The checks of this repository's tools and documents, run by `check-all tools`.

`{bin}` is the shared bin directory (`~/.claude/bin`), `{root}` this
directory, `tools/`. What is here is the seconds-to-minutes half of
CLAUDE.md's standing checks. The document checkers themselves are the shared
ones in ~/slop/tools, whose own checks run there. The Haskell build, the
test suites, the playtests and hlint stay in CLAUDE.md's list: a build
inside a command meant to run often makes it run rarely.

A linter that cannot be found is a finding and not a skip: a probe decides
and its silence fails the step by name. The reference check is BLOCKED at
exit 2 with a sibling checkout unmounted, which check-all reports as a check
that did not happen, never as one that passed.
"""

SCAN = ['.']
TIMEOUT = 3600

# One run of the checker over every document: each takes the list and
# aggregates as check-all does, a failure over a document that could not be
# checked, so no loop re-derives the verdict here -- the loop that did kept
# 0 over a status it did not define (checks-04) and split a name on its
# space (checks-05). What is left to say is that the pass did not happen: a
# root that cannot be entered or a listing with nothing in it, git failing
# among its causes, is 2 with its reason, where it was a loop over nothing
# exiting 0 (checks-02). A status the checkers do not define reaches
# check-all as it is, and anything but 0 and 2 is a failure there.
DOCS_LISTED = ('cd "{root}/.." || { echo "cannot enter the repository root"; exit 2; }; '
               "readarray -t docs < <(git ls-files '*.md' ':!CHANGELOG.md'); "
               '[ ${#docs[@]} -gt 0 ] || { echo "git lists no tracked Markdown file, or could not be run; nothing checked"; exit 2; }; ')
DOCS = DOCS_LISTED + 'python3 ../tools/%s "${docs[@]}"'

STEPS = [
    # The shared checker over this repository's rows, which reach the
    # makefile and option-parser branches horde-ad's cannot.
    ('check-doc-refs self-test', ['python3', '{root}/../../tools/check-doc-refs.py', '--self-test'], '{root}/..'),
    ('check-plan-crossrefs self-test', ['python3', '{root}/check-plan-crossrefs.py', '--self-test']),
    ('check-sync-exports self-test', ['python3', '{root}/check-sync-exports.py', '--self-test']),
    ('pyflakes', ['bash', '-c',
                  'cd "{root}" && { python3 -m pyflakes --version >/dev/null 2>&1 || { echo "python3 -m pyflakes --version failed, so tools/*.py went unlinted"; exit 1; }; } && python3 -m pyflakes *.py']),
    ('shellcheck', ['bash', '-c',
                    'cd "{root}/.." && { command -v shellcheck >/dev/null || { echo "shellcheck is not on PATH (command -v shellcheck finds nothing), so the shell scripts went unlinted"; exit 1; }; } && files=$(git ls-files "*.sh") && if [ -z "$files" ]; then echo "no tracked shell script; nothing linted"; else shellcheck -S warning -f gcc $files; fi']),
    ('doc citations',          ['bash', '-c', DOCS % 'check-plan-citations.py']),
    ('doc refs',               ['bash', '-c', DOCS % 'check-doc-refs.py']),
    ('doc wrap',               ['bash', '-c', DOCS % 'check-doc-wrap.py']),
    ('doc examples',           ['bash', '-c', DOCS % 'check-doc-examples.py']),
    # The probes the documents carry beside their counted claims: each is a
    # claim's own check, run where the document sits, and the runner is the
    # doc-verification skill's, which is user-scope and lives outside this tree.
    ('doc probes',             ['bash', '-c', DOCS_LISTED + 'python3 $HOME/.claude/skills/doc-verification/run-probes.py "${docs[@]}"']),
    # Both campaign plans read against themselves and each other: the
    # cross-reference graph no pass over every document can see.
    ('plan crossrefs',         ['python3', '{root}/check-plan-crossrefs.py'], '{root}/..'),
    # Every sync JSFFI export built on syncExport, whose scheduler run GHC's
    # sync exports lack, Wasm.hs saying why; and the cabal files' --export=
    # flags naming exactly the foreign exports.
    ('sync exports',           ['python3', '{root}/check-sync-exports.py'], '{root}/..'),
    # No frontend decodes a screen index by the Enum instance of Point,
    # which reads the dungeon's width: the explicit-widths practice of
    # docs/wasm-frontend-unified-plan.md. It catches that spelling only.
    # Dom.hs, a dead example, is left alone, as the practice leaves it.
    # git grep exits 1 when it finds nothing and above 1 when it fails.
    ('explicit widths',        ['bash', '-c',
                                "git grep -nE 'Point\\{\\.\\.\\} = toEnum' -- "
                                "'engine-src/Game/LambdaHack/Client/UI/Frontend/*.hs' "
                                "':!engine-src/Game/LambdaHack/Client/UI/Frontend/Dom.hs'; "
                                'case $? in 0) echo "a frontend decodes a screen index with toEnum, not punindex (rwidth coscreen)"; exit 1;; '
                                '1) exit 0;; *) echo "git grep failed, so nothing was checked"; exit 2;; esac'],
     '{root}/..'),
    ('records validate',       ['python3', '{bin}/defect-cases.py', '{root}']),
    ('cases, ok direction',    ['python3', '{bin}/defect-run.py', '{root}']),
    ('cases, bug direction',   ['python3', '{bin}/defect-run.py', '--audit', '{root}']),
    ('source lint',            ['python3', '{bin}/defect-lint.py', '{root}']),
    ('selftest mutants',       ['python3', '{bin}/selftest-mutants.py', '{root}']),
]

# leader-census.py is temporary, like the migration document it serves, and
# is deleted with it; it has no self-test and takes no mutant.
UNCOVERED = {'leader-census.py': 'temporary, deleted with docs/leader-desync-migration.md'}
