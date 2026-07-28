# Test harness guidance (`test/`)

The engine-wide conventions and gotchas stay in the repo-root
`CLAUDE.md`.

## The mock and frontend stubs

`test/UnitTestHelpers.hs` provides `CliMock`, a real `MonadClientUI`
implementation over `StateT CliState IO`, plus two frontend stubs: the
default answers every `FrontKey` request with ESC;
`scriptedFchanFrontend` (wired into a fixture by `partyCliStateScripted`)
plays a scripted key list first, then falls back to ESC.

## The stub world: board and party fixtures

- The stub board is 3x3 unknown, unwalkable tiles: aiming/projection
  pipelines fail deterministically ("aiming obstructed by terrain"), but
  code that checks stores before aiming (e.g. `projectHuman`) is testable.
  Anything that indexes `ltile` by `Point` must keep positions on row 0:
  the `Enum` width hack (`speedupHackXSize`, see the gotchas in the
  repo-root `CLAUDE.md`) keeps its default 80 in the test binary, so on the
  3x3 board only row-0 lookups stay in bounds.
- Party fixtures `partyCliState`/`partyCliState3`/`partyCliStateBanned`
  model the sample game's hero faction. `emptyUIFaction` defaults
  `fhasPointman = False`, which alone forces `noRunWithMulti` and disables
  the run machinery — set it `True` in any faction fixture that must run or
  restore the pointman.

## Driving keys, commands and dialogs

- `promptGetKey` runs under the mock with blank frames (`onBlank = True`)
  and with rendered ones (`drawHudFrame` over the stub board — pinned by
  `FrameMUnitTests.AS7`). Even whole dialogs can be driven — see the ESC
  store-dialog test in `test/HandleHumanLocalMUnitTests.hs` — given two
  things: an item both held by the actor and registered in `sitemD` (a
  separate `updateItemD` step; without it the store reads as empty), and
  a screen wider than 4 (dialog prompts assert that — enlarge `coscreen`
  per-test; the level can stay 3x3).
- Real key bindings come from the *sample game's* `standardKeysAndMouse`
  (module `Client.UI.Content.Input` — the game's, per the
  duplicate-basename gotcha in the repo-root `CLAUDE.md`) via
  `IC.makeData Nothing`, baked into the fixture CCUI by `stubSessionUI` —
  no hand-rolled `InputContent` needed. To run a whole `HumanCmd` the way
  the key loop does, use `dispatchCmd` in `test/HandleHelperMUnitTests.hs`,
  which resolves the command's real key from those bindings and feeds both
  to `cmdSemInCxtOfKM`.

## Characterization tags: `[LR-flip]` and `[contract]`

Some tests are deliberate characterizations of known-buggy behaviour,
tagged `[LR-flip]` in comments with the post-fix expectation stated
inline; `[contract]`-tagged tests pin behaviour that must survive both
planned designs (live-read, then abort-split — see
`docs/leader-desync-bug.md` and `docs/promptgetkey-hygiene.md`) unchanged.
Don't "fix" a green `[LR-flip]` test — flip it together with the engine
change it documents, and verify the flip by temporarily applying the
candidate fix before committing either.
