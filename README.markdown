LambdaHack
==========

LambdaHack is a small [roguelike] [1] game written in [Haskell] [2].
It is getting more and more configurable and aims to become a flexible
rouguelike engine, suitable for large and small dungeon crawling games
of arbitrary themes. In particular, we try to keep the AI code independent
of particular monster, item and terrain definitions.


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the game can be downloaded
automatically by Cabal from [Hackage] [3] as follows

    cabal install LambdaHack

For a more current snapshot, download the source from [github] [4]
and run Cabal from the main directory

    cabal install

or you may try one of the terminal frontends with

    cabal install -fvty


Savegame directory
------------------

If you don't want LambdaHack to write to the current directory,
create a personal savegame directory (on Linux it's ~/.LambdaHack/).
and copy the scores file there. You may also want to
copy the configuration file src/config.default to
~/.LambdaHack/config and modify it, but be careful changing
gameplay options --- they can easily unbalance or break the game.


Further information
-------------------

See files PLAYING.markdown, DESIGN.markdown, CREDITS and LICENSE
for more information.



[1]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[2]: http://www.haskell.org/
[3]: http://hackage.haskell.org/package/LambdaHack
[4]: http://github.com/kosmikus/LambdaHack
