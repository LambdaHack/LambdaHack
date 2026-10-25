"""The controls of the slow make targets -- the page gate, the doctest count,
the ANSI smoke and the reactor driver -- and of the test suites' cases whose
mutants want a rebuild, as mutants their own runs must catch.

Read by `selftest-mutants.py tools/mutants-deep.py`, on demand rather than by
`check-all tools`: each judge is one of those targets, or the cases a tasty
pattern selects in one test executable, in a shared clone of the repository,
which builds what the target needs and runs it, so a replay takes long, and
like the targets it runs unsandboxed; and without -j, since two page gates at
once contend for the gate's fixed ports. Replay it when a target, or what it
runs, changes in a way these could see. The clone carries the working tree's
edits to tracked files and no untracked file, so a new file is tracked first,
and no edit may land while it runs.

Each judge seeds its copy with this checkout's build of the platform it
needs, and the wasm ones with ts-src's packages, the native ones with
cabal.project.local, so that only what a mutant touches is rebuilt; `{root}`
names this checkout's tools/, not the copy's.
"""

COPY = 'clone'
SIBLINGS = []
TIMEOUT = 3600

SEED = ('top=$(git -C {dir} rev-parse --show-toplevel) && cd "$top" && '
        'mkdir -p dist-newstyle/build && '
        'cp -a {root}/../dist-newstyle/packagedb dist-newstyle/ && ')
WASM = SEED + ('cp -a {root}/../dist-newstyle/build/wasm32-wasi '
               'dist-newstyle/build/ && '
               'cp -a {root}/../ts-src/node_modules ts-src/ && '
               'touch ts-src/node_modules/.npm-ci')
NATIVE = SEED + ('cp -a {root}/../dist-newstyle/build/x86_64-linux '
                 'dist-newstyle/build/ && '
                 'cp {root}/../cabal.project.local .')


# make exits 2 whenever a recipe fails, which selftest-mutants.py reads as a run
# that did not happen, and its message carries the recipe's own status: so the
# judge exits 1 only where the target itself failed, and 2 where the seeding,
# the build before it or a prerequisite did.
def judge(seed, target, build=''):
    return (seed + ' || exit 2; ' + build +
            'make ' + target + ' > judge.log 2>&1; s=$?; cat judge.log; '
            'if [ $s -eq 0 ]; then exit 0; '
            "elif grep -q '" + target + "] Error 1$' judge.log; then exit 1; "
            'else exit 2; fi')


PAGE_GATE = judge(WASM, 'page-gate')
TEST_REACTOR = judge(WASM, 'test-reactor')
# make doctest builds inside its recipe, so a failed build would end in the
# target's own Error 1, and smokeANSI runs the binary it finds and builds
# nothing: the judge of each builds first.
DOCTEST = judge(NATIVE, 'doctest',
                'cabal build > build.log 2>&1 || { cat build.log; exit 2; }; ')
SMOKE_ANSI = judge(NATIVE, 'smokeANSI',
                   'cabal build exe:LambdaHack > build.log 2>&1 '
                   '|| { cat build.log; exit 2; }; ')


# The cases the tasty pattern selects in the test executable named: exit 0 where
# tasty passed, a pattern selecting no case included, 1 where it reported a
# failed case, and 2 otherwise, as where the seeding or the build failed or
# tasty could not parse the pattern.
def tasty(component, pattern):
    return (NATIVE + ' || exit 2; '
            'cabal build ' + component + ' > build.log 2>&1 '
            '|| { cat build.log; exit 2; }; '
            '"$(cabal list-bin ' + component + ')" -p \'' + pattern + '\' '
            '> judge.log 2>&1; s=$?; cat judge.log; '
            'if [ $s -eq 0 ]; then exit 0; '
            "elif grep -q ' tests failed ' judge.log; then exit 1; "
            'else exit 2; fi')

MUTANTS = [
    # The site without the binary: no frame arrives, so the frame assertions and
    # the key assertions after them fail.
    ('page gate: the site without LambdaHack.wasm', '../Makefile',
     '\tcp "$$W" "$(1)/LambdaHack.wasm"\n', '\ttrue\n', PAGE_GATE),
    # No '?': the insert-coin self-play goes on painting, so the screen never
    # goes quiet, where an earlier form of the assertion, a hash changed after
    # the key, passed.
    ("page gate: the '?' press removed", '../ts-src/page-gate.mjs',
     '    await press("?");\n', '', PAGE_GATE),
    # A sync export that leaves the threads it wakes unscheduled: on the quiet
    # screen nothing else enters the RTS, so Escape changes nothing.
    ('page gate: syncExport without its scheduler run',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     '  Ex.handle reportException body\n  rts_scheduleWork\n',
     '  Ex.handle reportException body\n', PAGE_GATE),
    # The browser without the multi-font capability, as before 2.4: its frames
    # carry no overlays, so the overlay assertion fails.
    ('page gate: the browser without the multi-font capability',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     'supportsMultiFont = True\n', 'supportsMultiFont = False\n', PAGE_GATE),
    # No '?' through lhKey: the self-play goes on painting, so the stream never
    # stops, as in the page gate's twin above.
    ("test-reactor: the '?' key not sent", '../ts-src/run-wasm-game.mjs',
     '  if (lh.lhKey(help) instanceof Promise) fail("lhKey is not a sync'
     ' export");\n'
     '  if (help.prevented !== 1 || help.stopped !== 1) {\n'
     '    fail(\n'
     '      `lhKey left \'?\' to the browser: preventDefault ${help.prevented}`'
     ' +\n'
     '        ` times, stopPropagation ${help.stopped} times`,\n'
     '    );\n'
     '  }\n'
     '  ok("lhKey takes \'?\' from the browser before it returns");\n',
     '', TEST_REACTOR),
    # The page gate's scheduler mutant, judged by the driver: once the stream
    # stops, an event on the quiet screen repaints nothing.
    ('test-reactor: syncExport without its scheduler run',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     '  Ex.handle reportException body\n  rts_scheduleWork\n',
     '  Ex.handle reportException body\n', TEST_REACTOR),
    # lhKey's Shift flag false under Ctrl again, as Wasm-02 records: Ctrl and
    # Shift with the numpad's '/' reaches C-/ in place of C-?.
    ("test-reactor: lhKey's Shift flag as Shift held alone",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     '      key = K.keyTranslateWeb (fromJSString jsKey) modShift\n',
     '      key = K.keyTranslateWeb (fromJSString jsKey)'
     ' (modifier == K.Shift)\n', TEST_REACTOR),
    # Every frame saved under --printEachScreen again, as Wasm-03 records:
    # the release past a menu label repaints the menu unchanged and saves a
    # screenshot the driver does not count.
    ('test-reactor: a screenshot of every frame under --printEachScreen',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Wasm.hs',
     '    unless (curFrame == prevFrame) $ do\n',
     '    unless False $ do\n', TEST_REACTOR),
    # A repl that runs nothing and exits 0, which doctest's own exit status
    # cannot tell from a pass: no run prints an example count.
    ('doctest: a repl that runs nothing', '../Makefile',
     '--with-repl=doctest ', '--with-repl=true ', DOCTEST),
    # Every count read as 0, as the libraries holding no example print it:
    # lib:LambdaHack's, which holds them all, must not be.
    ('doctest: every count read as 0', '../Makefile',
     "'s/.*Examples: \\([0-9]*\\) .*/\\1/p'",
     "'s/.*Examples: \\([0-9]*\\) .*/0/p'", DOCTEST),
    # A game that quits at its first frame, with exit status 0: the frame count
    # of the benchmark report falls short of --stopAfterFrames.
    ('smokeANSI: a game that stops at frame 1', '../Makefile',
     '--stopAfterFrames $(SMOKEFRAMES) ', '--stopAfterFrames 1 ', SMOKE_ANSI),
    # The stub suite's width left at the hack's initial 80, the game's,
    # as UnitTestHelpers-02 records the harness once did for a test run
    # alone: AS7's fixture, 4 wide, is refused by executorCli's assertion.
    ("test: main leaving the Point width at the game's",
     '../test/Spec.hs', '  setSpeedupHackXSize stubWidth\n', '',
     tasty('test', '$NF == "contract AS7: promptGetKey draws a HUD frame on'
           ' the stub board"')),
    # Monospace text one column wider, which only the multi-font setup lays text
    # out in: the multi-font goldens of help, history and the item's description
    # fail, every single-font one standing.
    ('goldens: monospace text one column wider',
     '../engine-src/Game/LambdaHack/Client/UI/Overlay.hs',
     'textSize MonoFont l = length l\n',
     'textSize MonoFont l = length l + 1\n', tasty('test-game', '/fe-golden/')),
    # Heroes one HP hardier, starting the state goldens' games with more.
    ('goldens: heroes with one more HP',
     '../GameDefinition/Content/ItemKindActor.hs',
     'AddSkill SkMaxHP 80  -- partially from clothes and first aid\n',
     'AddSkill SkMaxHP 81  -- partially from clothes and first aid\n',
     tasty('test-game', '/fe-golden: the state/')),
]
