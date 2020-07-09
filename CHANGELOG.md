## [v0.10.0.0](https://github.com/LambdaHack/LambdaHack/compare/v0.9.5.0...v0.10.0.0)

## [v0.9.5.0](https://github.com/LambdaHack/LambdaHack/compare/v0.9.4.0...v0.9.5.0)

- Fix NumLock disabled in the browser
- In screen reader frontend, highlight active menu line with the cursor
- Clone the main main menu commands as map mode commands
- Add C-RMB and C-S-LMB as alternatives of MMB
- Announce prominently MMB binding for describing map positions
- Clean up the default config file, keeping compatibility
- Make scenario names longer and slighlty more informative
- Make Vi movement keys the default in addition to keypad and mouse
- Fix a bug where death prompt when autoplaying was capturing a keypress
- Let ESC from main menu return to insert coin mode, if applicable
- Make various small UI tweaks, especially to main menu and its submenu
- Let main menu lines have 35, not 30, characters
- Make the main menu ASCII art less intrusive (and easier for screen readers)
- Don't invalidate the score file due to game minor (only) version bump

## [v0.9.4.0](https://github.com/LambdaHack/LambdaHack/compare/v0.9.3.0...v0.9.4.0)

- In vty frontend highlight actors more
- Clean up actor highlighting
- Add yell/yawn to minimal command set, remove swerving the aiming line
- Invoke yell/yawn exclusively with '%', due tor Windows and terminal woes
- Move C-c command to C, not to mask C-c on console frontends
- Tweak and fix vty console frontends, for screen-readers
- React specially at gameover under certain special circumstances
- Simpliy assignSlot now that slots are auto-sorted
- Get rid of explicit item sorting; let lore and menu slots agree
- Make DetectExit non-modal
- Mark in a game end confirmation message that more treasure can be found
- Add a description to the escape embedded item
- Reword gameover text for raid scenario
- Be more verbose when confirming escape from the game
- Don't claim to summon, when not possible on this level
- Fix missing 'no longer poisoned' when applying antidote
- Don't ask confirmation for neutral (e.g., not IDed) items
- Fix 'you fall down; you stand on a sword'
- Prevent selecting regions via mouse down in web frontend
- Deselect item if player declines to apply or fling
- Hand-hold the player, warning if flung item is beneficial
- Hand-hold the player, warning if applied item is harmful
- Rewrite the condition in UI applyItem check
- Improve the lobable item skill failure message
- Let mouse buttons describe tiles, etc.
- Unblock S-MouseButton in SDL2 frontend
- Always describe things under mouse button
- Make the message when hitting dead foe more varied

## [v0.9.3.0, aka 'Velvet smoking jacket'](https://github.com/LambdaHack/LambdaHack/compare/v0.8.3.0...v0.9.3.0)

- Introduce message classes with configurable behaviour
- Create a new 16x16 font and use it everywhere; tweak smaller fonts
- Lock some levels or otherwise make ascending tricky
- Add cooldown to most melee weapons, display that in HUD, adjust AI
- Add per-scenario and per-outcome end-game messages in content
- Add duplicate and reroll item effects in preparation for crafting
- Add actor and item analytics as a preparation for XP gain quests
- Implement piercing projectiles that may share a tile with a big actor
- Increase the spawn speed now that monsters sleep a lot
- Introduce actors falling asleep and yelling
- Allow any level size and position
- Mention places when looking at tiles and add place lore menu
- Expand all kinds of content and rebalance
- Create and rework all item, cave and plot prose (Dan Keefe @Peritract)
- Make explosives in cramped spaces twice weaker
- Tweak player fling command
- Tweak equipping when equipment overfull
- Start cycling stores at equipment since that's the one mentioned in help
- Overhaul CI scripts
- Restructure and clean up codebase
- Extend balance debugging tools, using item and actor analytics, places, etc.
- Drop the gameplay option that first death means defeat
- Avoid idle-GC between slow keystrokes
- Put content data into a compact region to limit GC
- Remove the border around web frontend game screen; seems unneeded now
- Don't draw aiming line nor path in vty frontend
- Highlight xhair by cursor in vty frontend
- Highlight player by cursor in vty frontend
- Switch the default FPS to 24 for tradition's sake
- Highlight current high score
- Remove most stopPlayBack, now spurious, because message classes used
- Overhaul cabal file: define common options, split into internal libraries
- Fix confusion of nub and uniq
- Rename short wait to lurk and many lurks to heed
- Show a red message when HP or Calm dip too low or when foe appears
- Lose Calm and so alert AI even at weakest non-zero HP draining attacks
- Enable screenshots while in menus
- Rename config options related to fonts
- Recolour aiming line not to clash with the red crosshair
- Exchange the functions of yellow and red highlight
- Tweak all colours, in particular to differentiate blues/cyans
- Cap bright colours at 85 CIELAB Lightness at D65
- Normalize dark colours to be between 42 and 57 CIELAB Lightness at D65
- Get rid of colorIsBold option; KISS
- Tint white in alternating lines with different hue for long text readability
- Don't split lines at articles
- Set xhair to currently meleed foe to see his HP
- Display speed on HUD; tweak status lines in other ways
- Don't show description of leader target in HUD; TMI
- Help AI flee in a consistent direction over many turns
- Expose the save backup command, for browser games
- Don't display target info when item selected
- Let AI actors spawn even quite far from the player
- Auto-select all new team members, to help new players
- Replace O by zero on the map display; make zero distinct from O in all fonts
- Flesh out the initial ? prompt
- Add 'I' alias for pack-related commands, unless laptop key-scheme used
- Turn off movementLaptopKeys by default not to confuse new players
- Make sure AI attacks bosses even if distant and fleeing or non-moving
- Lower bonus HP at extreme difficulty
- Add a separate frame for each projectiles start
- Don't go modal at the frequent and weak hidden tile detection effect
- Make AI pick closest stairs more often
- Let apply-unskilled actors activate embedded items
- Don't boost damage by speed unless actor is projectile
- If everything else fails, let AI flee by opening doors
- Help AI actor prevent being dominated
- Make computing gameplay benefit to items more accurate
- Rename, clone and fine-tune effect Temporary
- Simplify code and content by getting rid of Recharging effect
- Let applying periodic items only produce the first effect
- Tweak item detection to help in skipping boring level portions and in stealth
- Invoke and display embedded items in the order specified in tile definitions
- Let lit trails illuminate colonnades
- Prevent an exploit for avoiding self-invoked firecrackers
- Don't let AI attempt summoning if not enough Calm
- Improve item label bracket codes in menus
- Pick randomly destination stairs if teleporting level
- Display the number of items in store
- Summarize value of player loot in shared stash menu's header
- Start history menu at the close-up of the last message
- Make fast-dying insects aggressive
- Overhaul game score DSL and particular scoring definitions in content
- Add and extend messages, e.g., tell if victim blocks and with what armor
- Extend and rework menu manipulation keys
- Remove specialized quaff, read and throw commands; KISS
- Split walls of text into more paragraphs and/or make them narrower
- Extend and update help and manual
- Don't let AI waste time looting distant lone projectiles
- Make Enum instances of Point and Vector contiguous, hackily
- Make dominated actor drop all his items, for ID and in case he defects ASAP
- Try to find a non-waiting action, if better AI leader can't be found
- Prevent summoning OoD actors
- Let animals eat food and add several foods
- Make Domination effect harder to activate
- Let only actors and items with SkOdor property leave smell and add perfumes
- Let spawning rate level out after a few dozen spawns
- Describe smell, if present in an inspected tile
- Let pushed actor fly after crashing a door open
- Show passing time and heard events even if no actors in the UI faction
- When movement impossible, describe the tile with SHIFT-direction
- Catch and steal projectiles when braced instead of when weaponless
- Let actors that are pushed perform any action in addition to movement
- Improve deduplication of messages
- When describing actor on map, tell if it has loot
- Represent being braced as having an organ; also add other pseudo-organs
- Overhaul hearing to facilitate triangulation based on sound cues
- Prefer to spawn aquatic actors on aquatic tiles
- Add swimming and flying skills and shallow water tile features
- Boost/drain skills via many new items
- Rework and extend skills and their effects as a preparation for XP rewards
- Enable specifying each side of outer cave fence separately
- Make definition of caves of a scenario more precise
- Specify more properties of levels in content
- Extend content validation
- Improve placement and fitting stairs and rooms on levels
- Don't hardwire level size
- Simplify game rules content
- Change the format of game client content
- Fix an arbitrary delay in killing dying actors
- Fix arbitrary branch of a corridor chosen when running
- Fix bush patches blocking off a level's corner
- Fix config file ignored at game reinit
- Fix running disturbed by flavours of walls
- Fix splitting lines one character too early
- Fix Calm drain from nearby foes occurring only every other turn
- Fix some AI looping movement, in particular when fleeing
- Fix running into own periodic explosions, e.g., from necklaces
- Fix 'she painfullies collide'
- Fix AI with vector targets unwilling to change them
- Fix crash when attempting to fling at a target on remote level
- Fix wrong timestamps in history
- Fix, again, various kinds of frames intruding between fadeout and fadein
- Fix wrong pluralization of some item names, compound and exceptions
- Fix disabled items benefit recalculation after item kind learned
- Fix in many ways too close initial faction and item positions
- Fix performance in many ways and places, particularly for JS translation
- Fix missing perception updates, causing missed AI actions concerning us
- Fix uninitialized sarenas, which was probably causing resume to change state
- Fix weak AI actors fleeing even if enemy can't melee
- Fix and optimize sifting free tiles for spawn/summon location
- Fix various cases of excessive summoning
- Fix recording of item first seen level
- Fix many problems with item descriptions and other messages
- Fix reporting of reduction and elimination of actor conditions
- Fix reading and interpreting old format config files
- Fix synced initial item timeouts and actor times, leading to artificial feel
- Fix actors erratically following their leader
- Fix lifts continuing as stars and the other way around
- Fix various 32bit overflows
- Fix other errors, probably not present or not visible in previous version

## [v0.8.3.0](https://github.com/LambdaHack/LambdaHack/compare/v0.8.1.2...v0.8.3.0)

- Add a hack to run SDL2 on the main thread, fixing the OS X crash
- Warn visually when impressed and Calm running low, risking domination
- Display actor as red when low Calm and impressed or when low HP
- Fix, complete and fine tune UI, AI and server skill and weapon checks
- Fix a bug where item aspects look different to clients than to the server
- Change the requirements for the main menu ASCII art

## [v0.8.1.2](https://github.com/LambdaHack/LambdaHack/compare/v0.8.1.1...v0.8.1.2)

- Fix typos detected by lintian
- Fix the code that runs in case of old async (bug introduced in v0.8.1.1)

## [v0.8.1.1](https://github.com/LambdaHack/LambdaHack/compare/v0.8.1.0...v0.8.1.1)

- no player-visible changes
- make it possible to compile with old async package
- rewrite copyright information according to Debian format
- make github display the correct main license

## [v0.8.1.0](https://github.com/LambdaHack/LambdaHack/compare/v0.8.0.0...v0.8.1.0)

- no player-visible changes
- significantly reduce RAM usage when compiling library
- update and extend CI

## [v0.8.0.0, aka 'Explosive dashboard'](https://github.com/LambdaHack/LambdaHack/compare/v0.7.1.0...v0.8.0.0)

- rework greying out menu items and permitting item application and projection
- rework history collection; merge message repetitions more aggressively
- display HP in red when below (configurable) warning threshold
- tweak AI: actors remember they are fleeing; better leader choice, etc.
- add to content specialized explosive projectiles; tune the effects
- calculate loot score component based on fraction of dungeon loot collected
- don't hardwire item price, but let it be specified in content
- let all valuables glitter in the dark to avoid complete level exploration
- teach AI to cure ailments and shake off impressions
- rework detection effects; add detection of items embedded in tiles
- automatically identify stolen items that only have minor effects
- let projectiles hit each other if fragile and substantial enough
- rework item kind identification code; change the way it's defined in content
- make more item kinds (including some traps) secret
- protect paralyzed actors with a stasis condition to avoid infinite paralysis
- implement dumping screenshots in SDL2 and create animated GIFs in Makefile
- generate most common consumables less often, but in depth-scaled bunches
- make pushed actors alter tiles and trigger effects of embedded items
- validate and cross-validate more content; reduce content creation boilerplate
- make summoning more varied and prevent chain-summoning
- add many ways to conditionally sequence effects
- create large, merged rooms more often
- generalize the terrain altering player command (C-c, mouse)
- let RET, SPACE and ESC clear pending messages, if any
- add dashboard with links to all menus and info screens
- scale some organ and trap power with level depth
- simplify level-scaled dice roll semantics
- change scaled dice notation 'dl' to 'dL' for readability in-game
- rebalance items and decrease dice variety to unclutter backpack
- colour-code beneficial and harmful conditions in menu and in HUD
- display item lore (also for organs, embedded items, explosions, etc.)
- display embedded item descriptions as if they were tile descriptions
- tweak blast visuals, lower particle counts, beautify their spread
- tweak projectile visuals, e.g., display an extra frame when projectile dies
- add intro screen and work on other ways to convey story
- simplify a lot of code, including a bit of game rules
- fix some bugs, tweak content, speed up some AI bottlenecks

## [v0.7.1.0, aka 'Ancient troubles'](https://github.com/LambdaHack/LambdaHack/compare/v0.7.0.0...v0.7.1.0)

- add amazing cave and item (actor, blast, organ) descriptions
- package for Windows as an installer and also as zip archives
- fix a crash from SDL frontend under some OpenGL drivers (no thread-safety)
- add WWW address to the Main Menu, for other sites that may run our JS blob

## [v0.7.0.0, aka 'The dice are cast'](https://github.com/LambdaHack/LambdaHack/compare/v0.6.2.0...v0.7.0.0)

- decouple tile searching from tile alteration
- refrain from identifying items that are not randomized
- switch away from incapacitated leader to let others revive him
- make rescue easier by not going into negative HP the first time
- fix crowd of friends on another level slowing even actors that melee
- fix missing report about items underneath an actor when changing levels
- API breakage: change the syntax of dice in content
- API addition: introduce cave descriptions
- keep all client states in the server and optimize communication with clients
- improve item choice for identification and item polymorphing
- reset embedded items when altering tile
- replace atomic command filtering with exception catching
- reimplement dice as symbolic expressions inducing multiple RNG calls
- switch to optparse-applicative and rewrite cli handling
- add stack and cabal new-build project files
- improve haddocks across the codebase

## [v0.6.2.0, aka 'Zoom out'](https://github.com/LambdaHack/LambdaHack/compare/v0.6.1.0...v0.6.2.0)

- make fireworks slower and so easier to spot
- make rattlesnake deeper but more common
- announce no effect of activation
- describe original and current faction of an actor
- highlight dominated actors
- mark organs with comma instead of percent and gems with dollar
- make the healing cave dangerous to prevent camping
- slightly balance various content
- by default move item the same as last time
- often spawn between heroes and stairs going deeper
- fix totalUsefulness computation for negative effects
- fix abandoning distant enemy target despite no alternatives
- fix slow pushing of actors
- fix a crash when many actors run towards stairs
- hotfix: Pass zoom keys through to the browser
- help players find the info about changing the font size
- depend on GHC >= 8.0 and new vector
- specialize client code already in SampleMonadClient.hs
- enable StrictData in all modules
- replace 'failure' with 'error' that now shows call stack

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
