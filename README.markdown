Allure of the Stars
===================

Allure of the Stars is a [roguelike] [1] computer game in early development
that employs procedural content generation evolving in response
to player behaviour, with the goal of ensuring high replayability.
The game is written in [Haskell] [2] and based
on the versatile [LambdaHack engine] [3],


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the game can be downloaded
automatically by Cabal from [Hackage] [4] as follows

    cabal install Allure

For a more current snapshot, download the source from [github] [5]
and run Cabal from the main directory

    cabal install

or you may try one of the terminal frontends with

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

See files PLAYING.markdown, DESIGN.markdown, CREDITS and LICENSE
for more information.



[1]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[2]: http://www.haskell.org/
[3]: http://github.com/kosmikus/LambdaHack
[4]: http://hackage.haskell.org/package/Allure
[5]: http://github.com/Mikolaj/Allure
