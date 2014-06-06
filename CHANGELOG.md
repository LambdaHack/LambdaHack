## [v0.2.12](https://github.com/kosmikus/LambdaHack/compare/v0.2.10...v0.2.12)

- improve and simplify dungeon generation
- simplify running and permit multi-actor runs
- let items explode and generate shrapnel projectiles
- add game difficulty setting (initial HP scaling right now)
- allow recording, playing back and looping commands
- implement pathfinding via per-actor BFS over the whole level
- extend setting targets for actors in UI tremendously
- implement autoexplore, go-to-target, etc., as macros
- let AI use pathfinding, switch leaders, pick levels to swarm to
- force level/leader changes on spawners (even when played by humans)
- extend and redesign UI bottom status lines
- get rid of CPS style monads, aborts and WriterT
- benchmark and optimize the code, in particular using Data.Vector
- split off and use the external library assert-failure
- simplify config files and limit the number of external dependencies

## [v0.2.10](https://github.com/kosmikus/LambdaHack/compare/v0.2.8...v0.2.10)

- screensaver game modes (AI vs AI)
- improved AI (can now climbs stairs, etc.)
- multiple, multi-floor staircases
- multiple savefiles
- configurable framerate and combat animations

## [v0.2.8](https://github.com/kosmikus/LambdaHack/compare/v0.2.6.5...v0.2.8)

- cooperative and competitive multiplayer (shared-screen only in this version)
- overhauled searching
- rewritten engine code to have a single server that sends restricted game state updates to many fat clients, while a thin frontend layer multiplexes visuals from a subset of the clients

## [v0.2.6.5](https://github.com/kosmikus/LambdaHack/compare/v0.2.6...v0.2.6.5)

- this is a minor release, primarily intended to fix the broken haddock documentation on Hackage
- changes since 0.2.6 are mostly unrelated to gameplay:
    - strictly typed config files split into UI and rules
    - a switch from Text to String throughout the codebase
    - use of the external library miniutter for English sentence generation

## [v0.2.6](https://github.com/kosmikus/LambdaHack/compare/v0.2.1...v0.2.6)

- the Main Menu
- improved and configurable mode of squad combat

## [v0.2.1](https://github.com/kosmikus/LambdaHack/compare/v0.2.0...v0.2.1)

- missiles flying for three turns (by an old kosmikus' idea)
- visual feedback for targeting
- animations of combat and individual monster moves

## [v0.2.0](https://github.com/kosmikus/LambdaHack/compare/release-0.1.20110918...v0.2.6)

- the LambdaHack engine becomes a Haskell library
- the LambdaHack game depends on the engine library
