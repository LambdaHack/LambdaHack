# Test harness guidance (`test/`)

The engine-wide conventions and gotchas stay in the repo-root
`CLAUDE.md`.

File:line references were verified against the tree at commit
`2815391d4` (2026-07-28); the citation pass proves a cited line exists,
this stamp that it still says what the claim around it needs.

## The mock and frontend stubs

`test/UnitTestHelpers.hs` provides `CliMock` (`UnitTestHelpers.hs:496`), a
real `MonadClientUI` implementation over `StateT CliState IO`, plus two
frontend stubs: the default answers every `FrontKey` request with ESC;
`scriptedFchanFrontend` (`UnitTestHelpers.hs:126`) plays a scripted key
list first, then falls back to ESC, and is wired into a fixture by
`partyCliStateScripted` (`UnitTestHelpers.hs:486`).

## The stub world: board and party fixtures

- The stub board is 3x3 unknown, unwalkable tiles: aiming/projection
  pipelines fail deterministically ("aiming obstructed by terrain"), but
  code that checks stores before aiming (e.g. `projectHuman`) is testable.
  Anything that indexes `ltile` by `Point` must keep positions on row 0:
  the `Enum` width hack (`speedupHackXSize`, see the gotchas in the
  repo-root `CLAUDE.md`) keeps its default 80 in the test binary, so on the
  3x3 board only row-0 lookups stay in bounds.
- Party fixtures `partyCliState`/`partyCliState3`/`partyCliStateBanned`
  (the family starts at `UnitTestHelpers.hs:455`) model the sample game's
  hero faction. `emptyUIFaction` defaults
  `fhasPointman = False`, which alone forces `noRunWithMulti` and disables
  the run machinery — set it `True` in any faction fixture that must run or
  restore the pointman.

## Driving keys, commands and dialogs

- `promptGetKey` runs under the mock with blank frames (`onBlank = True`)
  and with rendered ones (`drawHudFrame` over the stub board — pinned by
  the AS7 case, `FrameMUnitTests.hs:194`). Even whole dialogs can be
  driven — see the ESC store-dialog test
  (`HandleHumanLocalMUnitTests.hs:210`) — given two
  things: an item both held by the actor and registered in `sitemD` (a
  separate `updateItemD` step; without it the store reads as empty), and
  a screen wider than 4 (dialog prompts assert that — enlarge `coscreen`
  per-test; the level can stay 3x3).
- Real key bindings come from the *sample game's* `standardKeysAndMouse`
  (module `Client.UI.Content.Input` — the game's, per the
  duplicate-basename gotcha in the repo-root `CLAUDE.md`) via
  `IC.makeData Nothing`, baked into the fixture CCUI by `stubSessionUI`
  (`UnitTestHelpers.hs:343`) — no hand-rolled `InputContent` needed. To
  run a whole `HumanCmd` the way the key loop does, use `dispatchCmd`
  (`HandleHelperMUnitTests.hs:46`), which resolves the command's real key
  from those bindings and feeds both to `cmdSemInCxtOfKM`.

## Characterization tags: `[LR-flip]` and `[contract]`

Some tests are deliberate characterizations of known-buggy behaviour,
tagged `[LR-flip]` in comments with the post-fix expectation stated
inline; `[contract]`-tagged tests pin behaviour that must survive both
planned designs (live-read, then abort-split — see
`docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md`) unchanged.
Don't "fix" a green `[LR-flip]` test — flip it together with the engine
change it documents, and verify the flip by temporarily applying the
candidate fix before committing either.

Each tag also opens the test's own name, so a series runs as a unit:

```
cabal test --test-options='-p "/contract/"'   # 25 tests, must stay green
cabal test --test-options='-p "/LR-flip/"'    # 8 tests, flip with the fix
```

A test carrying both concerns — the bridge tests X1 and X2, whose
`promptGetKey` half is contract and whose final cycling outcome flips —
is tagged `LR-flip`, because a test that must be edited when the design
lands is not one that stays green unchanged. Its contract half is
described in the comment above it.
