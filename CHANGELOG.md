## [v0.4.100.0, aka 'The last thaw'](https://github.com/LambdaHack/LambdaHack/compare/v0.4.99.0..v0.4.100.0)

- unexpectedly thaw and freeze again v0.5.0.0 of LambdaHack content API
- unexpectedly implement timeouts and temporary effects easily without FRP
- make a couple of skill levels meaningful and tweak skills of some actors
- make AI prefer exploration of easier levels
- permit overfull HP and Calm
- let non-projectile actors block view
- start the game with a screensaver safari mode
- add i386 Linux and Windows compilation targets to Makefile

## [v0.4.99.0, aka 'Player escapes'](https://github.com/LambdaHack/LambdaHack/compare/v0.2.14...v0.4.99.0)

- balance the example game content a bit (campaign still unbalanced)
- various code and documentation tweaks and fixes
- add cabal flag expose_internal that reveals internal library operations
- merge FactionKind into ModeKind and rework completely the semantics
- compatibility tweaks for Nixpkgs
- define AI tactics, expose them to UI and add one more: follow-the-leader
- share leader target between the UI and AI client of each faction
- specify monster spawn rate per-cave
- extend content validation and make it more user friendly
- freeze v0.5.0.0 of LambdaHack content API

## [v0.2.14, aka 'Out of balance'](https://github.com/LambdaHack/LambdaHack/compare/v0.2.12...v0.2.14)

- tons of new (unbalanced) content, content fields, effects and descriptions
- add a simple cabal test in addition to make-test and travis-test
- generate items and actors according to their rarities at various depths
- redo weapon choice, combat bonuses and introduce armor
- introduce skill levels for abilities (boolean for now, WIP)
- remove regeneration, re-add through periodically activating items
- ensure passable areas of randomly filled caves are well connected
- make secondary factions leaderless
- auto-tweak digital line epsilon to let projectiles evade obstacles
- add shrapnel (explosions) and organs (body parts)
- express actor kinds as item kinds (their trunk)
- add dynamic lights through items, actors, projectiles
- fix and improve item kind and item stats identification
- make aspects additive from all equipment and organ items
- split item effects into aspects, effects and item features
- rework AI and structure it according to the Ability type
- define Num instance for Dice to make writing it in content easier
- remove the shared screen multiplayer mode and all support code, for now
- rename all modules and nearly all other code entities
- check and consume HP when calling friends and Calm when summoning
- determine sight radius from items and cap it at current Calm/5
- introduce Calm; use to hear nearby enemies and limit item abuse before death
- let AI actors manage items and share them with party members
- completely revamp item manipulation UI
- add a command to cede control to AI
- separate actor inventory, 10-item actor equipment and shared party stash
- vi movement keys (hjklyubn) are now disabled by default
- new movement keyset: laptop movement keys (uk8o79jl)

## [v0.2.12](https://github.com/LambdaHack/LambdaHack/compare/v0.2.10...v0.2.12)

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

## [v0.2.10](https://github.com/LambdaHack/LambdaHack/compare/v0.2.8...v0.2.10)

- screensaver game modes (AI vs AI)
- improved AI (can now climbs stairs, etc.)
- multiple, multi-floor staircases
- multiple savefiles
- configurable framerate and combat animations

## [v0.2.8](https://github.com/LambdaHack/LambdaHack/compare/v0.2.6.5...v0.2.8)

- cooperative and competitive multiplayer (shared-screen only in this version)
- overhauled searching
- rewritten engine code to have a single server that sends restricted game state updates to many fat clients, while a thin frontend layer multiplexes visuals from a subset of the clients

## [v0.2.6.5](https://github.com/LambdaHack/LambdaHack/compare/v0.2.6...v0.2.6.5)

- this is a minor release, primarily intended to fix the broken haddock documentation on Hackage
- changes since 0.2.6 are mostly unrelated to gameplay:
    - strictly typed config files split into UI and rules
    - a switch from Text to String throughout the codebase
    - use of the external library miniutter for English sentence generation

## [v0.2.6](https://github.com/LambdaHack/LambdaHack/compare/v0.2.1...v0.2.6)

- the Main Menu
- improved and configurable mode of squad combat

## [v0.2.1](https://github.com/LambdaHack/LambdaHack/compare/v0.2.0...v0.2.1)

- missiles flying for three turns (by an old kosmikus' idea)
- visual feedback for targeting
- animations of combat and individual monster moves

## [v0.2.0](https://github.com/LambdaHack/LambdaHack/compare/release-0.1.20110918...v0.2.6)

- the LambdaHack engine becomes a Haskell library
- the LambdaHack game depends on the engine library
