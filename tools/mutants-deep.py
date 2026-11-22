"""The controls of the slow make targets -- the page gate, the doctest count,
the ANSI and nodeBench smokes and the reactor driver -- and of the test suites'
cases, whose mutants want a rebuild or ts-src's packages, as mutants their own
runs must catch.

Read by `selftest-mutants.py tools/mutants-deep.py`, on demand rather than by
`check-all tools`, which tests only that each mutant still applies: each judge
is one of those targets, a run of the reactor driver or the page gate's orphan
case, or the cases a tasty or vitest pattern selects in one test executable
or test file, in a shared clone of the repository, which builds what the
target needs and runs it, so a replay takes long, and like the targets it runs
unsandboxed; and without -j, since two page gates at once contend for the gate's
fixed ports. Replay it when a target, or what it runs, changes in a way these
could see. The clone carries the working tree's edits to tracked files and no
untracked file, so a new file is tracked first, and no edit may land while it
runs.

Each judge seeds its copy with what it needs of this checkout's: the wasm and
native ones with the build of their platform, the wasm and TypeScript ones with
ts-src's packages and the native ones with cabal.project.local, so that only
what a mutant touches is rebuilt; `{root}` names this checkout's tools/, not the
copy's.
"""

COPY = 'clone'
SIBLINGS = []
TIMEOUT = 3600

TOP = 'top=$(git -C {dir} rev-parse --show-toplevel) && cd "$top" && '
PACKAGES = ('cp -a {root}/../ts-src/node_modules ts-src/ && '
            'touch ts-src/node_modules/.npm-ci')
SEED = TOP + ('mkdir -p dist-newstyle/build && '
              'cp -a {root}/../dist-newstyle/packagedb dist-newstyle/ && ')
WASM = SEED + ('cp -a {root}/../dist-newstyle/build/wasm32-wasi '
               'dist-newstyle/build/ && ') + PACKAGES
NATIVE = SEED + ('cp -a {root}/../dist-newstyle/build/x86_64-linux '
                 'dist-newstyle/build/ && '
                 'cp {root}/../cabal.project.local .')
TS = TOP + PACKAGES


# A build step, its output in build.log and shown only where it fails, which
# ends the judge at 2.
def built(cmd):
    return cmd + ' > build.log 2>&1 || { cat build.log; exit 2; }; '


# What leaves in $W the binary of a wasm component and in jsffi.mjs its glue, as
# the Makefile's recipes post-link it.
def postlinked(component):
    return ('W=$(wasm32-wasi-cabal list-bin ' + component +
            ' --enable-optimization) && '
            '~/.ghc-wasm/wasm32-wasi-ghc/lib/post-link.mjs --input "$W" '
            '--output jsffi.mjs')


# The run whose status is the verdict, its output in judge.log and shown: exit 0
# where it passed, 1 where the log holds the line a failure of its own prints,
# which `grep` matches by the pattern `failed`, and 2 otherwise.
def verdict(run, failed, grep='grep -q'):
    return (run + ' > judge.log 2>&1; s=$?; cat judge.log; '
            'if [ $s -eq 0 ]; then exit 0; '
            'elif ' + grep + " '" + failed + "' judge.log; then exit 1; "
            'else exit 2; fi')


# make exits 2 whenever a recipe fails, which selftest-mutants.py reads as a run
# that did not happen, and its message carries the recipe's own status: so the
# judge exits 1 only where the target itself failed, with `status`, and 2 where
# the seeding, the build before it or a prerequisite did.
def judge(seed, target, build='', status=1):
    return (seed + ' || exit 2; ' + build
            + verdict('make ' + target, target + '] Error %d$' % status))


PAGE_GATE = judge(WASM, 'page-gate')
# The case of page-gate-06, which runs the gate over a blank page of its own and
# exits 0 where the gate left no process of its run, 1 where it left one and 2
# where the run did not happen: it needs only ts-src's packages.
ORPHANS = TS + ' || exit 2; bash tools/page-gate-orphans/run.sh'
TEST_REACTOR = judge(WASM, 'test-reactor')
# The reactor driver, which exits 0 where its run passed and 1 where it failed,
# under --frontendNull given the arguments named, on the binary make build-wasm
# builds: exit 0 where it exited with `must`, 1 where with the other of the two,
# and 2 otherwise, as where the seeding, the build or the post-link failed or
# the run did not happen.
def driver(args, must):
    return (WASM + ' || exit 2; ' + built('make build-wasm')
            + '. ~/.ghc-wasm/env && ' + postlinked('exe:LambdaHack')
            + ' || exit 2; '
            'node ts-src/run-wasm-game.mjs "$W" jsffi.mjs --frontendNull ' +
            args + '; '
            'case $? in %d) exit 0;; %d) exit 1;; *) exit 2;; esac'
            % (must, 1 - must))


# Given an argument lhStart's parse refuses, which ends in ExitFailure 1, the
# driver must fail the run.
REACTOR_REFUSED = driver('--stopAfterFrames notanumber', 1)
# Given --help, which ends in exitWith ExitSuccess, the driver must pass the
# run.
REACTOR_HELP = driver('--help', 0)
# make doctest builds inside its recipe, so a failed build would end in the
# target's own Error 1, and smokeANSI runs the binary it finds and builds
# nothing: the judge of each builds first.
DOCTEST = judge(NATIVE, 'doctest', built('cabal build'))
SMOKE_ANSI = judge(NATIVE, 'smokeANSI', built('cabal build exe:LambdaHack'))
# smokeNodeBench builds the reactor through its prerequisite build-wasm, so
# a failed build is that target's error and the judge's 2; a game its timeout
# kills fails the target with timeout's own status, 124.
SMOKE_NODE = judge(WASM, 'smokeNodeBench')
SMOKE_NODE_BOUND = judge(WASM, 'smokeNodeBench', status=124)


# The cases the tasty pattern selects in the test executable named, built
# natively or, for wasm, built and run as `make test-wasm` builds and runs it:
# exit 0 where tasty passed, a pattern selecting no case included, 1 where it
# reported a failed case or, given `every`, the number of cases the pattern
# selects, where every one of them failed, and 2 otherwise, as where the seeding
# or the build failed or tasty could not parse the pattern.
def tasty(component, pattern, wasm=False, every=None):
    if wasm:
        cabal = '. ~/.ghc-wasm/env && wasm32-wasi-cabal'
        opts = ' --enable-optimization'
        run = (postlinked(component)
               + ' && node ts-src/run-wasm-test.mjs "$W" jsffi.mjs')
    else:
        cabal, opts = 'cabal', ''
        run = '"$(cabal list-bin ' + component + ')"'
    return ((WASM if wasm else NATIVE) + ' || exit 2; '
            + built(cabal + ' build ' + component + opts)
            + verdict("{ " + run + " -p '" + pattern + "'; }",
                      ' tests failed ' if every is None
                      else '^%d out of %d tests failed ' % (every, every)))


# The rows of placementTests in test/OverlayLayoutUnitTests.hs, numbered from 1
# as its haddock numbers them; and the judge of the rows named, 1 only where
# every one of them failed.
PLACEMENT = [
    'propCutoff: a pen short of the last column goes on',
    'propCutoff: a pen at the last column stops',
    'propFit: a chunk narrower than the rest of the row fits',
    'propFit: a chunk exactly as wide as the rest of the row fits',
    'propFit: an overrun is cut a cell short, raising the marker',
    'propFit: an all-space overrun is cut at the edge, no marker',
    'propFit: a cut chunk ends its line',
    'propFit: a cell and a pixel left cut one pixel wide',
]


def placement(*rows):
    return tasty('test', ' || '.join('$NF == "%s"' % PLACEMENT[r - 1]
                                     for r in rows), every=len(rows))


# The cases the vitest pattern selects in one file of ts-src's suite, the file
# named from the repository's root, so that --changed sees an edit of it, with
# ts-src's packages seeded and nothing built: exit 0 where vitest passed, a
# pattern selecting no case included, 1 where it reported a failed case, and 2
# otherwise, as where the seeding failed or the file could not be loaded.
def vitest(path, pattern):
    return (TS + ' || exit 2; '
            + verdict('ts-src/node_modules/.bin/vitest run --root ts-src '
                      + path + " -t '" + pattern + "'",
                      ' Tests +[0-9]+ failed', 'grep -qE'))


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
    # The canvas opaque again, as before the fix of terminal-02: both browsers
    # draw its text with subpixel antialiasing, so the HUD's map font holds
    # colours no palette colour blends to with the background, at 1:1 and 2:1.
    ('page gate: the canvas opaque again', '../ts-src/src/terminal.ts',
     '  const ctx = canvas.getContext("2d")!;\n',
     '  const ctx = canvas.getContext("2d", { alpha: false })!;\n', PAGE_GATE),
    # A face whose file fails to load never counted down: the frames held for
    # the faces go unpainted, so a load without the overlay fonts' files warns
    # and draws no overlay text.
    ('page gate: a failed font load holding the frames',
     '../ts-src/src/terminal.ts',
     '        .catch((e: unknown) =>\n'
     '          console.warn(`[lh] font ${spec.file} failed to load`, e),\n'
     '        )\n'
     '        .finally(() => {\n',
     '        .catch((e: unknown) => {\n'
     '          console.warn(`[lh] font ${spec.file} failed to load`, e);\n'
     '          throw e;\n'
     '        })\n'
     '        .then(() => {\n', PAGE_GATE),
    # A face whose file fails to load releases its frames without a word, so
    # the load without the overlay fonts' files draws overlay text and warns
    # of nothing.
    ('page gate: a failed font load not warned of',
     '../ts-src/src/terminal.ts',
     '          console.warn(`[lh] font ${spec.file} failed to load`, e),\n',
     '          e,\n', PAGE_GATE),
    # Every child in the gate's own process group: the exit handler's kill of
    # each child's group finds none, so every variant leaves the server and the
    # browsers running.
    ("page gate orphans: the children in the gate's process group",
     '../ts-src/page-gate.mjs',
     '  const child = spawn(command, args, { ...options, detached: true });\n',
     '  const child = spawn(command, args, options);\n', ORPHANS),
    # SIGINT and SIGTERM left to their defaults: the gate dies of either with
    # no exit handler run, so the sigint and sigterm variants leave every child
    # running.
    ('page gate orphans: SIGINT and SIGTERM not ending the run',
     '../ts-src/page-gate.mjs',
     'for (const s of ["SIGINT", "SIGTERM"]) process.on(s, () => blocked(s));\n',
     '', ORPHANS),
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
    # A rejection of lhStart's promise no longer failing the run under
    # --frontendNull: the run whose argument the game's parse refuses, which
    # ends in ExitFailure 1, passes.
    ('driver under --frontendNull: a rejection of lhStart not failing the run',
     '../ts-src/run-wasm-game.mjs',
     '    fail(`lhStart rejected with ${e}`);\n', '', REACTOR_REFUSED),
    # lhStart rejecting its promise for a clean exit, its catch of ExitSuccess
    # gone: the run given --help, which ends in one, fails.
    ('driver under --frontendNull: lhStart without its catch of ExitSuccess',
     '../GameDefinition/Main.hs',
     'lhStart = main `Ex.catch` \\case\n'
     '  ExitSuccess -> return ()\n'
     '  e -> Ex.throwIO e\n',
     'lhStart = main\n', REACTOR_HELP),
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
    ('smokeNodeBench: a game that stops at frame 1', '../Makefile',
     '--stopAfterFrames $(SMOKEFRAMES) ', '--stopAfterFrames 1 ', SMOKE_NODE),
    # A time bound shorter than the game.
    ('smokeNodeBench: a game past the time bound', '../Makefile',
     'NODEBENCHTIMEOUT=600\n', 'NODEBENCHTIMEOUT=0.1\n', SMOKE_NODE_BOUND),
    # The stub suite's width left at the hack's initial 80, the game's,
    # as UnitTestHelpers-02 records the harness once did for a test run
    # alone: AS7's fixture, 4 wide, is refused by executorCli's assertion.
    ("test: main leaving the Point width at the game's",
     '../test/Spec.hs', '  setSpeedupHackXSize stubWidth\n', '',
     tasty('test', '$NF == "contract AS7: promptGetKey draws a HUD frame on'
           ' the stub board"')),
    # MonadClientUI-01 put back: getFontSetup resolving the fonts at every
    # call, which without the font table fails with "Font not defined".
    ('fonts: getFontSetup resolving the fonts at every call',
     '../engine-src/Game/LambdaHack/Client/UI/MonadClientUI.hs',
     '                  && Frontend.hasAuxiliaryFonts soptions\n',
     '                  && either error (isJust . Frontend.cfAuxiliary)\n'
     '                            (Frontend.resolveFonts soptions)\n',
     tasty('test', 'getFontSetup reads the fontset')),
    # hasAuxiliaryFonts reading the bitmap map font's name for the proportional
    # one: the fontset naming no auxiliary font then reads as naming them.
    ('fonts: hasAuxiliaryFonts reading the wrong font of the fontset',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/Fonts.hs',
     '    Just fontset -> not $ T.null $ fontPropRegular fontset\n',
     '    Just fontset -> not $ T.null $ fontMapBitmap fontset\n',
     tasty('test', 'hasAuxiliaryFonts says what resolveFonts resolves')),
    # Monospace text one column wider, which only the multi-font setup lays text
    # out in: the multi-font goldens of help, history and the item's description
    # fail, every single-font one standing.
    ('goldens: monospace text one column wider',
     '../engine-src/Game/LambdaHack/Client/UI/Overlay.hs',
     'textSize MonoFont l = length l\n',
     'textSize MonoFont l = length l + 1\n', tasty('test-game', '/fe-golden/')),
    # The browser's history screen offering the key that dumps it to a file, as
    # the native one does: the browser's history goldens fail under wasm.
    ("goldens: the dump key in the browser's history",
     '../engine-src/Game/LambdaHack/Client/UI/HandleHumanLocalM.hs',
     '#if !defined(USE_JSFILE) && !defined(USE_WASMFILE)\n'
     "        : K.mkChar '.'\n",
     "#if 1\n        : K.mkChar '.'\n",
     tasty('test-game', '/fe-golden: history/', wasm=True)),
    # Heroes one HP hardier, starting the state goldens' games with more.
    ('goldens: heroes with one more HP',
     '../GameDefinition/Content/ItemKindActor.hs',
     'AddSkill SkMaxHP 80  -- partially from clothes and first aid\n',
     'AddSkill SkMaxHP 81  -- partially from clothes and first aid\n',
     tasty('test-game', '/fe-golden: the state/')),
    # The breaks of propCutoff and propFit that placementTests' rows catch, each
    # judged by the rows it fails. The cutoff weakened fails the fifth, the
    # seventh and the eighth by propFit's assertion that a cut chunk ends its
    # line; strengthened, the eighth by propFit's precondition; every other row
    # fails by its comparison.
    ("placement: propCutoff's >= weakened to >",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  pcurXPx >= (lmWidth lm - 1) * lmBoxSize lm\n',
     '  pcurXPx > (lmWidth lm - 1) * lmBoxSize lm\n', placement(2, 5, 7, 8)),
    ("placement: propCutoff's >= strengthened to pcurXPx + 1 >=",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  pcurXPx >= (lmWidth lm - 1) * lmBoxSize lm\n',
     '  pcurXPx + 1 >= (lmWidth lm - 1) * lmBoxSize lm\n', placement(1, 8)),
    ("placement: propFit's fit test's <= weakened to <",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  width | widthRaw <= remaining = widthRaw\n',
     '  width | widthRaw < remaining = widthRaw\n', placement(4)),
    ("placement: propFit's fitting width replaced by remaining",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  width | widthRaw <= remaining = widthRaw\n',
     '  width | widthRaw <= remaining = remaining\n', placement(3)),
    ("placement: propFit's cut without its - boxSize",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '        | otherwise = remaining - boxSize\n',
     '        | otherwise = remaining\n', placement(5, 7, 8)),
    ("placement: propFit's not allSpace flipped",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  trimmed = width /= widthRaw && not allSpace\n',
     '  trimmed = width /= widthRaw && allSpace\n', placement(5, 6, 7, 8)),
    ("placement: propFit's all-space cut a cell short",
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '        | allSpace = remaining\n',
     '        | allSpace = remaining - boxSize\n', placement(6)),
    # The walk placing a chunk after the cut one, its guard gone from the
    # walk SDL draws through: the seventh row's line gets a second fit, which
    # propFit's precondition refuses.
    ('placement: walkPropLine without its cutoff guard',
     '../engine-src/Game/LambdaHack/Client/UI/Frontend/OverlayLayout.hs',
     '  go !cur _ | propCutoff lm cur = return ()\n', '', placement(7)),
    # A proportional text op a pixel right of its fill, so that the fills alone
    # no longer cover every cell the overlays draw over: overlayOps' case
    # pinning each op but a fill to a fill's rectangle fails.
    ('overlay-core: a text op a pixel off its fill',
     '../ts-src/src/overlay-core.ts',
     '        kind: "textLeft",\n        ...box,\n',
     '        kind: "textLeft",\n        ...box,\n        x: box.x + 1,\n',
     vitest('ts-src/src/overlay-core.test.ts', 'lays every op but a fill')),
    # overlaidCells visiting every op and not the fills alone: its case over a
    # stroke that lies on no fill gets the stroke's cell back too.
    ('overlay-core: overlaidCells visiting every op',
     '../ts-src/src/overlay-core.ts',
     '    if (op.kind !== "fill") continue;\n', '',
     vitest('ts-src/src/overlay-core.test.ts', 'the other ops skipped')),
]
