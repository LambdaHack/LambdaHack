"""Settings for ~/slop/tools/check-doc-refs.py in this repository, read by its
common.repo_config: the block that stood at the head of the checker while it
lived in this repository's tools/, and which says what each setting does.
"""

SEARCH_ROOTS = ["engine-src", "definition-src", "GameDefinition", "ts-src",
                "test", "tools", "docs", ".claude", ".github"]
# Where the executable's `long "..."` options are declared; "" if the repo
# ships no command-line parser, in which case every `--flag` falls
# through to the external/unknown buckets.
OPTIONS_FILE = "engine-src/Game/LambdaHack/Server/Commandline.hs"
# Module prefix owned by this repo, the one namespace where an
# unresolved module is a failure rather than a shrug; "" to disable.
OUR_NAMESPACE = "Game.LambdaHack."
# The file whose targets `make <target>` is resolved against; "" if the
# repo has no makefile, which switches that check off.
MAKEFILE = "Makefile"
# Sibling checkouts whose files this repo's documents cite. They are
# resolved for real whenever present, and their absence stops the run
# rather than degrading it, the checker's sibling policy. [] to disable.
SIBLING_ROOTS = ["../lambdahack.github.io",
                 # and ~/slop's shared tools
                 "../tools"]
# The shared tools, whose flags the documents name as another tool's.
FLAG_ROOTS = ["../tools"]
ALLOW_FILE = "tools/doc-refs-allow.txt"
# --self-test rows: one scratch document exercising every branch against
# this repo's real tree and sibling, with the expected verdicts beside
# it. Repo-specific like the rest of this block; the checker is not.
SELF_TEST_DOC = """\
`engine-src/Game/LambdaHack/Client/NoSuchModule.hs` is unresolved local
drift, and `cabal build no-such-stanza` an unknown cabal target;
`make no-such-target` is an unknown makefile target,
`Game.LambdaHack.Client.NoSuch` a missing module of our namespace and
`../lambdahack.github.io/no-such.js` missing in a live sibling: all five
must fail, as must the wrapped-span row below.
`--noSuchFlag` is listed, never failed, while `--restamp` is external,
another tool's; `+noSuchFlag` stays unclassified.
A span the formatter wrapped, `two words
across a break`, must not hide `engine-src/Game/LambdaHack/Client/NoSuchTwin.hs`,
which fails like the first path.
`ghc_wasm_jsffi.mjs` resolves in the deployment sibling, by path.
`Frontend/ANSI.hs` and `tools/leader-census.py` and `Makefile` and
`make play` and `cabal run LambdaHack` and
`wasm32-wasi-cabal build exe:LambdaHack` and `Game.LambdaHack.Client.UI`
and `Definition.*` and `--sniff` and `+with_expensive_assertions` are
passing controls; `make -n shot` is the flag-skipping one, and names a
target no other row does.
`Point.hs:26,32` and `Point.hs:26` and `Point.hs:26-32` are pass 1's to
check, while the malformed `Point.hs:26,` stays unclassified, which keeps
the comma form from being a blanket accept.
`Ability.SkMove` and `K.KM` are upgrade-only, unclassified.
`blob/master` and `group/bench` and `KP_/` are prose, and so are the
deployment sibling's own artifacts `LambdaHack.wasm` and `bundle.js`,
whose extensions this checker does not read as paths: the path-shape gate
is what keeps a big foreign tree from resolving them.
`.../ghc-9.12/...` is an elision, not a sibling path;
`https://example.com/a/b.md` and `https://lambdahack.github.io/` are
URLs. None of these may be read as a path.
A tilde block showing a backtick fence must close only on a tilde fence:
~~~
```
~~~
so this prose line, make no-such-prose-target, is not a command, while a
block after it still is:
```
make no-such-fenced-target
```
"""
SELF_TEST_FAILURES = 7
SELF_TEST_FAIL = ["NoSuchModule.hs", "NoSuchTwin.hs", "no-such-stanza",
                  "no-such-target", "Game.LambdaHack.Client.NoSuch",
                  "../lambdahack.github.io/no-such.js",
                  "no-such-fenced-target"]
# Prose that a fence of the wrong kind once turned into command text: in no
# FAIL, ok or allow line.
SELF_TEST_PROSE = ["no-such-prose-target"]
SELF_TEST_OK = ["Frontend/ANSI.hs", "tools/leader-census.py", "Makefile",
                "make play", "make shot", "cabal LambdaHack",
                "ghc_wasm_jsffi.mjs", "Game.LambdaHack.Client.UI",
                "Definition.*", "--sniff", "+with_expensive_assertions",
                "--restamp"]
# Citation shapes pass 1 owns: skipped here, in no bucket and no row.
SELF_TEST_SKIPPED = ["Point.hs:26,32", "Point.hs:26", "Point.hs:26-32"]
SELF_TEST_UNCLASSIFIED = ["+noSuchFlag", "Point.hs:26,", "Ability.SkMove",
                          "K.KM", "blob/master", "group/bench", "KP_/",
                          "LambdaHack.wasm", "bundle.js",
                          ".../ghc-9.12/...",
                          "https://example.com/a/b.md",
                          "https://lambdahack.github.io/"]
# The one local-drift row that must degrade to SKIP with the sibling
# off, and the failure count that survives the degradation.
SELF_TEST_DEGRADED_FAILURES = 4
SELF_TEST_DEGRADED_SKIP = "engine-src/Game/LambdaHack/Client/NoSuchModule.hs"
