Allure of the Stars
===================

This is an alpha prerelease of Allure of the Stars,
a near-future Sci-Fi [roguelike] [1] and tactical squad game.
Long term goals are high replayability and auto-balancing
through procedural content generation and persistent content
modification based on player behaviour.
The game is written in [Haskell] [2] and based
on the [LambdaHack roguelike engine] [3],


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the game can be downloaded
automatically by Cabal from [Hackage] [4] as follows

    cabal install Allure

For a more current snapshot, download the source from [github] [5]
and run Cabal from the main directory

    cabal install

The best frontend (keyboard support and colours) is gtk,
but if needed, you may try one of the terminal frontends with, e.g,

    cabal install -fvty


Savegame directory
------------------

If you don't want Allure of the Stars to write to the current directory,
create a personal savegame directory (on Linux it's ~/.Allure/).
and copy the scores file there. You may also want to
copy the configuration file src/config.default to
~/.Allure/config and modify it, but be careful changing
gameplay options --- they can easily unbalance or break the game.


Further information
-------------------

For more information, visit the wiki at https://github.com/Mikolaj/Allure/wiki
and see the files PLAYING.md, CREDITS and LICENSE.

Have fun!



[1]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[2]: http://www.haskell.org/
[3]: http://github.com/kosmikus/LambdaHack
[4]: http://hackage.haskell.org/package/Allure
[5]: http://github.com/Mikolaj/Allure
