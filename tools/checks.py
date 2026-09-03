"""The checks of this repository's tools and documents, run by `check-all tools`.

`{bin}` is the shared bin directory (`~/.claude/bin`), `{root}` this
directory, `tools/`. What is here is the seconds-to-minutes half of
CLAUDE.md's standing checks: every tool's self-test, the two linters over
the Python and the shell here, the twin sync against horde-ad, the
mechanical document passes over every tracked `.md` but CHANGELOG.md, the
defect records in both directions and the mutants. The Haskell build, the
test suites, the playtests and hlint stay in CLAUDE.md's list: a build
inside a command meant to run often makes it run rarely.

A linter off PATH is a finding and not a skip, so `command -v` decides and
its silence fails the step by name. The twin sync and the reference check
are BLOCKED at exit 2 with a sibling checkout unmounted, which check-all
reports as a check that did not happen, never as one that passed.
"""

SCAN = ['.']
TIMEOUT = 3600

# A checker's 2 is a document that could not be checked, not one that failed,
# and the loop hands it on as 2; a 1 anywhere is 1.
DOCS = ("cd \"{root}/..\" && worst=0; for d in $(git ls-files '*.md' | grep -v '^CHANGELOG.md$'); "
        "do python3 tools/%s \"$d\"; rc=$?; [ $rc -eq 1 ] && worst=1; "
        "[ $rc -eq 2 ] && [ $worst -ne 1 ] && worst=2; done; exit $worst")

STEPS = [
    ('check-doc-examples self-test', ['python3', '{root}/check-doc-examples.py', '--self-test']),
    ('check-doc-refs self-test', ['python3', '{root}/check-doc-refs.py', '--self-test']),
    ('check-doc-wrap self-test', ['python3', '{root}/check-doc-wrap.py', '--self-test']),
    ('check-plan-citations self-test', ['python3', '{root}/check-plan-citations.py', '--self-test']),
    ('check-twin-sync self-test', ['python3', '{root}/check-twin-sync.py', '--self-test']),
    ('heading-outline self-test', ['python3', '{root}/heading-outline.py', '--self-test']),
    ('pyflakes', ['bash', '-c',
                  'cd "{root}" && { python3 -m pyflakes --version >/dev/null 2>&1 || { echo "pyflakes is not on PATH (command -v pyflakes finds nothing), so tools/*.py went unlinted"; exit 1; }; } && python3 -m pyflakes *.py']),
    ('shellcheck', ['bash', '-c',
                    'cd "{root}/.." && { command -v shellcheck >/dev/null || { echo "shellcheck is not on PATH (command -v shellcheck finds nothing), so the shell scripts went unlinted"; exit 1; }; } && files=$(git ls-files "*.sh") && { [ -z "$files" ] || shellcheck -S warning -f gcc $files; }']),
    ('twin sync',              ['python3', '{root}/check-twin-sync.py']),
    ('doc citations',          ['bash', '-c', DOCS % 'check-plan-citations.py']),
    ('doc refs',               ['bash', '-c', DOCS % 'check-doc-refs.py']),
    ('doc wrap',               ['bash', '-c', DOCS % 'check-doc-wrap.py']),
    ('doc examples',           ['bash', '-c', DOCS % 'check-doc-examples.py']),
    ('records validate',       ['python3', '{bin}/defect-cases.py', '{root}']),
    ('cases, ok direction',    ['python3', '{bin}/defect-run.py', '{root}']),
    ('cases, bug direction',   ['python3', '{bin}/defect-run.py', '--audit', '{root}']),
    ('source lint',            ['python3', '{bin}/defect-lint.py', '{root}']),
    ('selftest mutants',       ['python3', '{bin}/selftest-mutants.py', '{root}']),
]

# leader-census.py is temporary, like the migration document it serves, and
# is deleted with it; it has no self-test and takes no mutant.
UNCOVERED = {'leader-census.py': 'temporary, deleted with docs/leader-desync-migration.md'}
