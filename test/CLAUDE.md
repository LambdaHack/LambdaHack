# Test harness guidance (`test/`)

The engine-wide conventions and gotchas stay in the repo-root `CLAUDE.md`.

File:line references were verified against the tree at commit `8b5703e87` (2026-07-29); the citation pass proves a cited line exists — this stamp, that it still says what the claim around it needs.

## The mock and frontend stub

`test/UnitTestHelpers.hs` provides `CliMock` (`UnitTestHelpers.hs:630`), a real `MonadClientUI` implementation over `StateT CliState IO`, plus a frontend stub that answers every `FrontKey` request with ESC.

## The stub world

The stub board is 3x3 unknown, unwalkable tiles: aiming/projection pipelines fail deterministically ("aiming obstructed by terrain"), but code that checks stores before aiming (e.g. `projectHuman`) is testable. Anything that indexes `ltile` by `Point` must keep positions on row 0: the `Enum` width hack (`speedupHackXSize`, see the gotchas in the repo-root `CLAUDE.md`) keeps its default 80 in the test binary, so on the 3x3 board only row-0 lookups stay in bounds.
