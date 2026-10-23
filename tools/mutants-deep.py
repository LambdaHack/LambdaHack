"""The page gate's controls, as mutants its own run must catch.

Read by `selftest-mutants.py tools/mutants-deep.py`, on demand rather than by
`check-all tools`: each judge is `make page-gate` in a shared clone of the
repository, which links the wasm reactor and drives two browsers, so a replay
takes long, and like the gate it runs unsandboxed. Replay it when the gate,
or the page it loads, changes in a way these could see. The clone carries the
working tree's edits to tracked files and no untracked file, so a new file is
tracked first, and no edit may land while it runs.

Each judge seeds its copy with this checkout's wasm build and ts-src's
packages, so that only what a mutant touches is rebuilt; `{root}` names
this checkout's tools/, not the copy's.
"""

COPY = 'clone'
SIBLINGS = []
TIMEOUT = 3600

# make exits 2 whenever a recipe fails, which selftest-mutants.py reads as
# a run that did not happen, and its message carries the recipe's own
# status: so the judge exits 1 only where the page gate itself failed, and 2
# where the seeding or a prerequisite did.
PAGE_GATE = ('top=$(git -C {dir} rev-parse --show-toplevel) && cd "$top" && '
             'mkdir -p dist-newstyle/build && '
             'cp -a {root}/../dist-newstyle/build/wasm32-wasi dist-newstyle/build/ && '
             'cp -a {root}/../dist-newstyle/packagedb dist-newstyle/ && '
             'cp -a {root}/../ts-src/node_modules ts-src/ && '
             'touch ts-src/node_modules/.npm-ci || exit 2; '
             'make page-gate > gate.log 2>&1; s=$?; cat gate.log; '
             'if [ $s -eq 0 ]; then exit 0; '
             "elif grep -q 'page-gate] Error 1$' gate.log; then exit 1; "
             'else exit 2; fi')

MUTANTS = [
    # The site without the binary: no frame arrives, so the frame
    # assertions and the key assertions after them fail.
    ('page gate: the site without LambdaHack.wasm', '../Makefile',
     '\tcp "$$W" "$(1)/LambdaHack.wasm"\n', '\ttrue\n', PAGE_GATE),
    # No '?': the insert-coin self-play goes on painting, so the screen
    # never goes quiet, where an earlier form of the assertion, a hash
    # changed after the key, passed.
    ("page gate: the '?' press removed", '../ts-src/page-gate.mjs',
     '    await press("?");\n', '', PAGE_GATE),
    # A sync export that leaves the threads it wakes unscheduled: on the
    # quiet screen nothing else enters the RTS, so Escape changes nothing.
    ('page gate: syncExport without its scheduler run',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     '  Ex.handle reportException body\n  rts_scheduleWork\n',
     '  Ex.handle reportException body\n', PAGE_GATE),
    # Overlays required before 2.4 submits any; 2.4's commit (4), which
    # deletes the switch, deletes this mutant with its anchor.
    ('page gate: overlays required before 2.4', '../ts-src/page-gate.mjs',
     'const wantOverlays = process.env.PAGE_GATE_OVERLAYS === "1";\n',
     'const wantOverlays = true;\n', PAGE_GATE),
]
