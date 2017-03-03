LambdaHack
==========

[![Build Status](https://travis-ci.org/LambdaHack/LambdaHack.svg?branch=master)](https://travis-ci.org/LambdaHack/LambdaHack)
[![Hackage](https://img.shields.io/hackage/v/LambdaHack.svg)](https://hackage.haskell.org/package/LambdaHack)
[![Join the chat at https://gitter.im/LambdaHack/LambdaHack](https://badges.gitter.im/LambdaHack/LambdaHack.svg)](https://gitter.im/LambdaHack/LambdaHack?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

LambdaHack is a [Haskell] [1] game engine library for [roguelike] [2]
games of arbitrary theme, size and complexity. You specify the content
to be procedurally generated, including game rules and AI behaviour.
The library lets you compile a ready-to-play game binary, using either
the supplied or a custom-made main loop. Several frontends are available
(GTK is the default) and many other generic engine components
are easily overridden, but the fundamental source of flexibility lies
in the strict and type-safe separation of code and content and of clients
(human and AI-controlled) and server. Long-term goals for LambdaHack include
support for multiplayer tactical squad combat, in-game content creation,
auto-balancing and persistent content modification based on player behaviour.

The engine comes with a sample code for a little dungeon crawler,
called LambdaHack and described in [PLAYING.md](GameDefinition/PLAYING.md).

![gameplay screenshot](https://raw.githubusercontent.com/LambdaHack/media/master/screenshot/raid1.png)

Other games known to use the LambdaHack library:

* [Allure of the Stars] [6], a near-future Sci-Fi game
* [Space Privateers] [8], an adventure game set in far future

Note: the engine and the example game are bundled together in a single
[Hackage] [3] package released under the permissive `BSD3` license.
You are welcome to create your own games by forking and modifying
the single package, but please consider eventually splitting your changes
into a separate content-only package that depends on the upstream
engine library. This will help us exchange ideas and share improvements
to the common codebase. Alternatively, you can already start the development
in separation by cloning and rewriting [Allure of the Stars] [10]
or any other pure game content package and mix and merge with the example
LambdaHack game rules at will. Note that the LambdaHack sample game
derives from the [Hack/Nethack visual and narrative tradition] [9],
while Allure of the Stars uses the more free-form Moria/Angband style
(it also uses the `AGPL` license, and `BSD3 + AGPL = AGPL`,
so make sure you want to liberate your code and content to such an extent).


Installation from binary archives
---------------------------------

Pre-compiled game binaries for some platforms are available through
the [release page] [11] and from the [Nix Packages Collection] [12].
To manually install a binary archive, make sure you have the GTK
libraries suite on your system, unpack the LambdaHack archive
and run the executable in the unpacked directory.

On Windows, if you don't already have GTK installed (e.g., for the GIMP
picture editor) please download and run (with default settings)
the GTK installer from

http://sourceforge.net/projects/gtk-win/


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
A file with the default settings, the same as built into the binary, is in
[GameDefinition/config.ui.default](GameDefinition/config.ui.default).
When the game is run for the first time, the file is copied to the official
location, which is `~/.LambdaHack/config.ui.ini` on Linux and
`C:\Users\<username>\AppData\Roaming\LambdaHack\config.ui.ini`
(or `C:\Documents And Settings\user\Application Data\LambdaHack\config.ui.ini`
or something else altogether) on Windows.

Screen font can be changed and enlarged by editing the config file
at its official location or by Control-right-clicking on the game window.

If you use the numeric keypad, use the NumLock key on your keyboard
to toggle the game keyboard mode. With NumLock off, you walk with the numeric
keys and run with Shift (or Control) and the keys. This mode is probably
the best if you use mouse for running. When you turn NumLock on,
the reversed key setup enforces good playing habits by setting as the default
the run command (which automatically stops at threats, keeping you safe)
and requiring Shift (or Control) for the error-prone step by step walking.

If you don't have the numeric keypad, you can use laptop keys (uk8o79jl)
or you can enable the Vi keys (aka roguelike keys) in the config file.


Compilation from source
-----------------------

If you want to compile your own binaries from the source code,
use Cabal (already a part of your OS distribution, or available within
[The Haskell Platform] [7]), which also takes care of all the dependencies.

The recommended frontend for LambdaHack is based SDL2, so you need the SDL2
libraries for your OS. On Linux, remember to install the -dev versions as well,
e.g., libsdl2-dev and libsdl2-ttf-dev on Ubuntu Linux 16.04.

The latest official version of the LambdaHack library can be downloaded,
compiled and installed automatically by Cabal from [Hackage] [3] as follows

    cabal update
    cabal install LambdaHack

For a newer snapshot, download source from a development branch
at [github] [5] and run Cabal from the main directory

    cabal install

To compile with one of the rudimentary terminal frontends,
use Cabal flags, e.g,

    cabal install -fvty

To compile with GTK2 (deprecated but supported), you need GTK libraries
for your OS. On Windows follow [the same steps as for Wine] [13].
On OSX, if you encounter problems, you may want to
[compile the GTK libraries from sources] [14]. Invoke Cabal as follows

    cabal install -fgtk gtk2hs-buildtools .


Compatibility notes
-------------------

If you are using a terminal frontend, numeric keypad may not work
correctly depending on versions of the libraries, terminfo and terminal
emulators. The curses frontend is not fully supported due to the limitations
of the curses library. With the vty frontend started in an xterm,
Control-keypad keys for running seem to work OK, but on rxvt they do not.
The commands that require pressing Control and Shift together won't
work either, but fortunately they are not crucial to gameplay.
For movement, laptop (uk8o79jl) and Vi keys (hjklyubn, if enabled
in config.ui.ini) should work everywhere. GTK works fine, too, both
with numeric keypad and with mouse.


Testing and debugging
---------------------

The [Makefile](Makefile) contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the dumb `stdout` frontend are gathered in `make test`.
Of these, travis runs `test-travis-*` on each push to the repo.
Test commands with prefix `frontend` start AI vs. AI games
with the standard, user-friendly gtk frontend.

Run `LambdaHack --help` to see a brief description of all debug options.
Of these, `--sniffIn` and `--sniffOut` are very useful (though verbose
and initially cryptic), for monitoring the traffic between clients
and the server. Some options in the config file may prove useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).

You can use HPC with the game as follows (details vary according
to HPC version). A quick manual playing session
after the automated tests would be in order, as well, since the tests don't
touch the topmost UI layer.

    cabal clean
    cabal install --enable-coverage
    make test
    hpc report --hpcdir=dist/hpc/dyn/mix/LambdaHack --hpcdir=dist/hpc/dyn/mix/LambdaHack-xxx/ LambdaHack
    hpc markup --hpcdir=dist/hpc/dyn/mix/LambdaHack --hpcdir=dist/hpc/dyn/mix/LambdaHack-xxx/ LambdaHack

Note that debug option `--stopAfter` is required to cleanly terminate
any automated test. This is needed to gather any HPC info, because HPC
requires a clean exit to save data files.


Further information
-------------------

For more information, visit the [wiki] [4]
and see [PLAYING.md](GameDefinition/PLAYING.md), [CREDITS](CREDITS)
and [LICENSE](LICENSE).

Have fun!



[1]: http://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: http://hackage.haskell.org/package/LambdaHack
[4]: https://github.com/LambdaHack/LambdaHack/wiki
[5]: http://github.com/LambdaHack/LambdaHack
[6]: http://allureofthestars.com
[7]: http://www.haskell.org/platform
[8]: https://github.com/tuturto/space-privateers
[9]: https://github.com/LambdaHack/LambdaHack/wiki/Sample-dungeon-crawler

[10]: https://github.com/AllureOfTheStars/Allure
[11]: https://github.com/LambdaHack/LambdaHack/releases/latest
[12]: http://hydra.cryp.to/search?query=LambdaHack
[13]: http://www.haskell.org/haskellwiki/GHC_under_Wine#Code_that_uses_gtk2hs
[14]: http://www.edsko.net/2014/04/27/haskell-including-gtk-on-mavericks
