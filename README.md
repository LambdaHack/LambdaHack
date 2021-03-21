LambdaHack
==========

[![Build Status](https://secure.travis-ci.org/LambdaHack/LambdaHack.svg?master)](https://travis-ci.org/LambdaHack/LambdaHack)
[![Hackage](https://img.shields.io/hackage/v/LambdaHack.svg)](https://hackage.haskell.org/package/LambdaHack)
[![Join the chat at Discord](https://img.shields.io/discord/688792755564052486.svg?label=chat%20on%20Discord&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.gg/87Ghnws)
[![Join the chat at Matrix](https://img.shields.io/matrix/lambdahack:mozilla.org.svg?label=chat%20on%20Matrix&logo=matrix&server_fqdn=mozilla.modular.im)](https://matrix.to/#/!HnbpAHMjOGHlYtrASl:mozilla.org?via=mozilla.modular.im)

LambdaHack is a Haskell[1] game engine library for ASCII roguelike[2]
games of arbitrary theme, size and complexity, with optional
tactical squad combat. It's packaged together with a sample
dungeon crawler in a quirky fantasy setting. The sample game can be
tried out in the browser at http://lambdahack.github.io.
The browser version runs fastest on Chrome, keyboard commands
and savefiles are supported only on recent enough versions
of browsers, but mouse should work everywhere.

As an example of the engine's capabilities, here is a showcase
of shooting down explosive projectiles. A couple were shot down close
enough to enemies to harm them. Others exploded closer to our party members
and took out of the air the projectiles that would otherwise harm them.

![gameplay screenshot](https://raw.githubusercontent.com/LambdaHack/media/master/screenshot/allureofthestars.com.shooting.down.explosives.gif)

This was a semi-automatic stealthy speedrun of the escape scenario
of the sample game, native binary, SDL2 frontend, single tiny
bitmap font. The enemy gang has a huge numerical and equipment
superiority. Our team loots the area on auto-pilot until the first foe
is spotted. Then they scout out enemy positions. Then hero 1 draws
enemies and unfortunately enemy fire as well, which is when he valiantly
shoots down explosives to avoid the worst damage. Then heroine 2 sneaks
behind enemy lines to reach the remaining treasure. That accomplished,
the captain signals retreat and leaves for the next area (the zoo).


Using the engine
----------------

To use the engine, you need to specify the content to be
procedurally generated. You specify what the game world
is made of (entities, their relations, physics and lore)
and the engine builds the world and runs it.
The library lets you compile a ready-to-play game binary,
using either the supplied or a custom-made main loop.
Several frontends are available (SDL2 is the default
for desktop and there is a JavaScript browser frontend)
and many other generic engine components are easily overridden,
but the fundamental source of flexibility lies
in the strict and type-safe separation of code from the content
and of clients (human and AI-controlled) from the server.

Please see the changelog file for recent improvements
and the issue tracker for short-term plans. Long term goals
include multiplayer tactical squad combat, in-game content
creation, auto-balancing and persistent content modification
based on player behaviour. Contributions are welcome.
Please offer feedback to mikolaj.konarski@funktory.com or, preferably,
on any of the public forums.

Other games known to use the LambdaHack library:

* Allure of the Stars[6], a near-future Sci-Fi game
* Space Privateers[8], an adventure game set in far future

Note: the engine and the sample game are bundled together in a single
Hackage[3] package released under the permissive `BSD3` license.
You are welcome to create your own games by forking and modifying
the single package, but please consider eventually splitting your changes
into a separate content-only package that depends on the upstream
engine library. This will help us exchange ideas and share improvements
to the common codebase. Alternatively, you can already start the development
in separation by cloning and rewriting Allure of the Stars[10]
and mix and merge with the sample LambdaHack game rules at will.
Note that the LambdaHack sample game derives from the Hack/Nethack visual
and narrative tradition[9], while Allure of the Stars uses the more free-form
Moria/Angband style (it also uses the AGPL license, and BSD3 + AGPL = AGPL,
so make sure you want to liberate your code and content to such an extent).


Installation of the sample game from binary archives
----------------------------------------------------

The game runs rather slowly in the browser (fastest on Chrome) and you are
limited to the square font for all purposes, though it's scalable.
Also, savefiles are prone to corruption on the browser,
e.g., when it's closed while the game is still saving progress
(which takes a long time). Hence, after trying out the game,
you may prefer to use a native binary for your architecture, if it exists.

Pre-compiled game binaries are available through the release page[11]
(and, for Windows, dev versions continuously from AppVeyor[18]).
To use a pre-compiled binary archive, unpack it and run the executable
in the unpacked directory or use program shortcuts from the installer,
if available. On Linux, make sure you have the SDL2 libraries installed
on your system (e.g., libsdl2-2.0-0 and libsdl2-ttf-2.0-0 on Ubuntu).
For Windows (XP no longer supported), the SDL2 and all other needed libraries
are included in the game's binary archive.


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
The default settings, the same that are built into the binary,
are on github at [GameDefinition/config.ui.default](https://github.com/LambdaHack/LambdaHack/blob/master/GameDefinition/config.ui.default).
When the game is run for the first time, or whenever the settings file
is deleted, the file is written to the default user data location,
which is `~/.Allure/` on Linux,
`C:\Users\<username>\AppData\Roaming\LambdaHack\`
(or `C:\Documents And Settings\user\Application Data\LambdaHack\`
or something else altogether) on Windows
and `Inspect/Application/Local Storage` under RMB menu
when run inside the Chrome browser.
If the user config file is outdated or corrupted, removing it guarantees
that the new default file would be put in its place.

Screen fonts and, consequently, window size can be changed by editing
the config file in the user data folder. The default bitmap font
`16x16xw.bdf` used for the game map covers most national characters
in the Latin alphabet (e.g. to give custom names to player characters)
and results in a game window of exactly 720p HD dimensions. The `8x8xb.fnt`
bitmap font results in a tiny window and covers latin-1 characters only.
The config file parameter `allFontsScale` permits further window size
adjustments, automatically switching to the scalable `16x16xw.woff`
version of the game map font. Config file option `chosenFontset` governs
not only the main game map font, but also the shape of the rectangular fonts,
if any, in which longer texts are overlaid over the map.

For high resolution displays and/or if fullscreen mode is requested
in the configuration file, `allFontsScale` needs to be set, e.g.,
to value 3 for 4K displays. Otherwise, the letters may be too small or,
in fullscreen or on retina displays in OS X, the screen may be automatically
scaled as a whole, not each letter separately, softening letter edges.

If you don't have a numeric keypad, you can use the left-hand movement
key setup (axwdqezc) or Vi editor keys (aka roguelike keys) or mouse.
If numeric keypad doesn't work, toggling the Num Lock key sometimes helps.
If running with the Shift key and keypad keys doesn't work,
try the Control key instead. The game is fully playable with mouse only,
as well as with keyboard only, but the most efficient combination
may be mouse for menus, go-to, inspecting the map, and aiming at distant
positions and keyboard for everything else.

If you are using a terminal frontend, e.g. the best supported vty frontend,
then numeric keypad (especially keypad `*` and `/`) may not work correctly,
depending on versions of libraries, terminfo and terminal emulators.
Toggling the Num Lock key may help or make issues worse. As a workaround,
in the vty frontend, numbers are used for movement, which sadly prevents
the number keys from selecting heroes. The commands that require pressing
Control and Shift together won't work either, but fortunately they are
not crucial to gameplay.

Some effort went into making the vty frontend usable with screen readers,
but without feedback it's hard to say how accessible that setup is.
As a side effect of screen reader support, there is no aiming line
nor path in vty frontend and some of map position highlighting
is performed using the terminal cursor. Screen readers may also work
better with animations turned off, using `--noAnim` or the corresponding
config file option.


Compilation of the library and sample game from source
------------------------------------------------------

The recommended frontend is based on SDL2, so you need the SDL2 libraries
for your OS. On Linux, remember to install the -dev versions as well,
e.g., libsdl2-dev and libsdl2-ttf-dev on Ubuntu Linux 16.04.
Other frontends are compiled similarly, but compilation to JavaScript
for the browser is more complicated and requires the ghcjs[15] compiler
and optionally the Google Closure Compiler[16].

The latest official version of the LambdaHack library can be downloaded,
compiled for SDL2 and installed automatically using the Cabal tool,
which is already a part of your OS distribution, or available within
The Haskell Platform[7]. Get the library from Hackage[3] as follows

    cabal update
    cabal run LambdaHack

For a newer snapshot, clone the source code from github[5]
and run `cabal run LambdaHack` from the main directory.
Alternatively, if you'd like to develop in this codebase,
the following speeds up the turn-around a lot

    cp cabal.project.local.development cabal.project.local
    cabal install cabal-plan

and then compile with

    cabal build .

and run the game with

    make play

There is a built-in black and white line terminal frontend, suitable
for teletype terminals or a keyboard and a printer (but it's going to use
a lot of paper, unless you disable animations with `--noAnim`). To compile
with one of the less rudimentary terminal frontends (in which case you are
on your own regarding font choice and color setup and you won't have
the spiffy colorful squares outlining special positions that exist in SDL2
frontend, but only crude cursor highlights), use Cabal flags, e.g,
to switch to the vty console frontend optimized for screen readers, run

    cabal run -fvty LambdaHack


Testing and debugging
---------------------

The [Makefile](https://github.com/LambdaHack/LambdaHack/blob/master/Makefile)
contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the teletype frontend are gathered in `make test`.
Of these, travis runs `test-travis` on each push to github.
Test commands with prefix `frontend` start AI vs. AI games
with the standard, user-friendly frontend.

Run `LambdaHack --help` to see a brief description of all debug options.
Of these, the `--sniff` option is very useful (though verbose
and initially cryptic), for displaying the traffic between clients
and the server. Some options in the config file may prove useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).

You can use HPC with the game as follows (details vary according
to HPC version).

    cabal clean
    cabal build --enable-coverage
    make test
    hpc report --hpcdir=dist/hpc/dyn/mix/LambdaHack --hpcdir=dist/hpc/dyn/mix/LambdaHack-xxx/ LambdaHack
    hpc markup --hpcdir=dist/hpc/dyn/mix/LambdaHack --hpcdir=dist/hpc/dyn/mix/LambdaHack-xxx/ LambdaHack

A quick manual playing session, after the automated tests, would be
in order as well, since the tests don't touch the topmost UI layer.
Note that a debug option of the form `--stopAfter*` is required to cleanly
terminate any automated test. This is needed to gather any HPC info,
because HPC requires a clean exit to save data files.


Coding style
------------

Stylish Haskell is used for slight auto-formatting at buffer save; see
[.stylish-haskell.yaml](https://github.com/LambdaHack/LambdaHack/blob/master/.stylish-haskell.yaml).
As defined in the file, indentation is 2 spaces wide and screen is
80-columns wide. Spaces are used, not tabs. Spurious whitespace avoided.
Spaces around arithmetic operators encouraged.
Generally, relax and try to stick to the style apparent in a file
you are editing. Put big formatting changes in separate commits.

Haddocks are provided for all module headers and for all functions and types
from major modules, in particular for the modules that are interfaces
for a whole directory of modules. Apart of that, only very important
functions and types are distinguished by having a haddock.
If minor ones have comments, they should not be haddocks
and they are permitted to describe implementation details and be out of date.
Prefer assertions instead of comments, unless too verbose.

The 'pointman' from game manual and UI is called 'leader' in the source code
and there are a few more mismatches, though the source code naming
and the UI naming should each be consistent in separation.
If the UI names stick, perhaps source code will be renamed at some point.

This codebase is an experiment in extensive use of states without lens.
So far, it works, doesn't result in much larger files or lots
of repetition and has the added benefits that newcomers don't need
to learn any lens library. Record punning, etc., definitely helps.


Further information
-------------------

For more information, visit the wiki[4]
and see [PLAYING.md](https://github.com/LambdaHack/LambdaHack/blob/master/GameDefinition/PLAYING.md),
[CREDITS](https://github.com/LambdaHack/LambdaHack/blob/master/CREDITS)
and [COPYLEFT](https://github.com/LambdaHack/LambdaHack/blob/master/COPYLEFT).

Have fun!



[1]: https://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: https://hackage.haskell.org/package/LambdaHack
[4]: https://github.com/LambdaHack/LambdaHack/wiki
[5]: https://github.com/LambdaHack/LambdaHack
[6]: http://allureofthestars.com
[7]: https://www.haskell.org/platform
[8]: https://github.com/tuturto/space-privateers
[9]: https://github.com/LambdaHack/LambdaHack/wiki/Sample-dungeon-crawler
[10]: https://github.com/AllureOfTheStars/Allure
[11]: https://github.com/LambdaHack/LambdaHack/releases
[15]: https://github.com/ghcjs/ghcjs
[16]: https://www.npmjs.com/package/google-closure-compiler
