LambdaHack
==========

This is an alpha release of LambdaHack, a [Haskell] [1] game engine
library for [roguelike] [2] games of arbitrary theme, size and complexity.
When completed, it will let you specify content to be procedurally generated,
define the AI behaviour on top of the generic content-independent rules,
override any of the generic engine components and compile a ready-to-play
game binary, using either the supplied or a custom-made main loop.
Long-term goals for LambdaHack include support for tactical squad combat,
in-game content creation, auto-balancing and persistent content modification
based on player behaviour.

The engine comes with a sample code for a little dungeon crawler,
called LambdaHack and described in PLAYING.md. The engine and the example
game are bundled together in a single [Hackage] [3] package.
You are welcome to create your own game by modifying the sample game
and the engine code, but please consider eventually splitting your changes
into a separate Hackage package that depends on the upstream library,
to help us exchange ideas and share improvements to the common code.
There is at least one more game using the LambdaHack library on Hackage,
a near-future Sci-Fi game called [Allure of the Stars] [4].


Compilation and installation
----------------------------

The library is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the library
can be downloaded automatically by Cabal from [Hackage] [3] as follows

    cabal install LambdaHack

For a more current snapshot, download the source from [github] [5]
and run Cabal from the main directory

    cabal install

For the example game, the best frontend (keyboard support and colours)
is gtk, but if needed, you may compile the game binary with one
of the terminal frontends using Cabal flags, e.g,

    cabal install -fvty

To use a crude bot for testing the game, you have to compile with
the standard input/output frontend, as follows

    cabal install -fstd

and run the bot, for example, in the following way

    DumbBot 42 20000000 | LambdaHack > /tmp/log

You may wish to tweak the game configuration file to let the bot play longer,
e.g., by making the dungeon much deeper, as in the supplied config.bot.


Further information
-------------------

For more information, visit the wiki (!!!!!!!!!!!!!TODO!!!!!!!!!!)
at https://github.com/kosmikus/LambdaHack/wiki
and see the files PLAYING.md, CREDITS and LICENSE.

Have fun!



[1]: http://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: http://hackage.haskell.org/package/LambdaHack
[4]: http://hackage.haskell.org/package/Allure
[5]: http://github.com/kosmikus/LambdaHack
