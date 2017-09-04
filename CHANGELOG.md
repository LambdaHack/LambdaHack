## [v0.6.1.0, aka 'Breaking one rule at a time'](https://github.com/LambdaHack/LambdaHack/compare/v0.6.0.0...v0.6.1.0)

- fix redrawing after window minimized and restored
- hack around vanishing texture on Windows
- hack around SDL backends not thread-safe on Windows
- the only breaking API change: specify font directory in game rules content
- let the game use its own fonts, not fonts from the sample game in library
- tweak some item creation to occur in character's pack, not on the ground
- slightly balance various content
- make sure the 'resolution' effect is not a drawback
- make artifact weapon rarities more regular
- avoid creating lit, open dungeon at the bottom, where foes have ranged weapons
- number scenarios in user descriptions
- correct, add and modify some in-game messages
- let player hear unseen summonings performed by other actors
- don't let actors hear blasts hitting walls, as opposed to hitting actors
- when moving item out of shared stash, reset its timeouts
- when ascending, shift timeouts of inventory as well
- when creating item not on the ground, discover it
- when dominating, auto-discover only if the item can't be discovered by use
- let henchmen take into account their targets, as described in PLAYING.md
- let only walkable tiles be explorable, for clear walls inside solid blocks
- move to API 2.0.0 of sdl2-ttf and depend on corrected sdl2 (builds on Windows)
- simplify code thanks to the new sdl2-ttf API
- tweak travis scripts and building docs in README

## [v0.6.0.0, aka 'Too much to tell'](https://github.com/LambdaHack/LambdaHack/compare/v0.5.0.0...v0.6.0.0)

- add and modify a lot of content: items, tiles, embedded items, scenarios
- improve AI: targeting, stealth, moving in groups, item use, fleeing, etc.
- make monsters more aggressive than animals
- tie scenarios into a loose, optional storyline
- add more level generators and more variety to room placement
- make stairs not walkable and use them by bumping
- align stair position on the levels they pass through
- introduce noctovision
- increase human vision to 12 so that normal speed missiles can be sidestepped
- tweak and document weapon damage calculation
- derive projectile damage mostly from their speed
- make heavy projectiles better vs armor but easier to sidestep
- improve hearing of unseen actions, actors and missiles impacts
- let some missiles lit up on impact
- make torches reusable flares and add blankets for dousing dynamic light
- add detection effects and use them in items and tiles
- make it possible to catch missiles, if not using weapons
- make it possible to wait 0.1 of a turn, at the cost of no bracing
- improve pathfinding, prefer less unknown, alterable and dark tiles on paths
- slow down actors when acting at the same time, for speed with large factions
- don't halve Calm at serious damage any more
- eliminate alternative FOV modes, for speed
- stop actors blocking FOV, for speed
- let actor move diagonally to and from doors, for speed
- improve blast (explosion) shapes visually and gameplay-wise
- add SDL2 frontend and deprecate GTK frontend
- add specialized square bitmap fonts and hack a scalable font
- use middle dot instead of period on the map (except in teletype frontend)
- add a browser frontend based on DOM, using ghcjs
- improve targeting UI, e.g., cycle among items on the map
- show an animation when actor teleports
- add character stats menu and stat description texts
- add item lore and organ lore menus
- add a command to sort item slots and perform the sort at startup
- add a single item manipulation menu and let it mark an item for later
- make history display a menu and improve display of individual messages
- display highscore dates according to the local timezone
- make the help screen a menu, execute actions directly from it
- rework the Main Menu
- rework special positions highlight in all frontends
- mark leader's target on the map (grey highlight)
- visually mark currently chosen menu item and grey out impossible items
- define mouse commands based on UI mode and screen area
- let the game be fully playable only with mouse, use mouse wheel
- pick menu items with mouse and with arrow keys
- add more sanity checks for content
- reorganize content in files to make rebasing on changed content easier
- rework keybinding definition machinery
- let clients, not the server, start frontends
- version savefiles and move them aside if versions don't match
- lots of bug fixes internal improvements and minor visual and text tweaks

## [v0.5.0.0, aka 'Halfway through space'](https://github.com/LambdaHack/LambdaHack/compare/v0.4.101.0...v0.5.0.0)

- let AI put excess items in shared stash and use them out of shared stash
- let UI multiple items pickup routine put items that don't fit into equipment into shared stash, if possible, not into inventory pack
- re-enable the ability to hear close, invisible foes
- add a few more AI and autonomous henchmen tactics (CTRL-T)
- keep difficulty setting over session restart
- change some game start keybindings
- replace the Duel game mode with the Raid game mode
- various bugfixes, minor improvements and balancing

## [v0.4.101.0, aka 'Officially fun'](https://github.com/LambdaHack/LambdaHack/compare/v0.4.100.0...v0.4.101.0)

- the game is now officially fun to play
- introduce unique boss monsters and unique artifact items
- add animals that heal the player
- let AI gang up, attempt stealth and react to player aggressiveness
- spawn actors fast and close to the enemy
- spawn actors less and less often on a given level, but with growing depth
- prefer weapons with effects, if recharged
- make the bracing melee bonus additive, not multiplicative
- let explosions buffet actors around
- make braced actors immune to translocation effects
- use mouse for movement, actor selection, aiming
- don't run straight with selected actors, but go-to cross-hair with them
- speed up default frame rate, slow down projectiles visually
- rework item manipulation UI
- you can pick up many items at once and it costs only one turn
- allow actors to apply and project from the shared stash
- reverse messages shown in player diary
- display actor organs and stats
- split highscore tables wrt game modes
- move score calculation formula to content
- don't keep the default/example config file commented out; was misleading
- I was naughty again and changed v0.5.0.0 of LambdaHack content API slightly
  one last time

## [v0.4.100.0, aka 'The last thaw'](https://github.com/LambdaHack/LambdaHack/compare/v0.4.99.0...v0.4.100.0)

- unexpectedly thaw and freeze again v0.5.0.0 of LambdaHack content API
- unexpectedly implement timeouts and temporary effects easily without FRP
- make a couple of skill levels meaningful and tweak skills of some actors
- make AI prefer exploration of easier levels
- permit overfull HP and Calm
- let non-projectile actors block view
- make colorful characters bold (if it resizes your fonts, turn off via colorIsBold = False in config file or --noColorIsBold on commandline)
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
