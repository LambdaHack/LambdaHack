Playing LambdaHack
==================

Playing the game consist of walking around the dungeon and bumping
into things (doors, monsters, treasure). Once the few basic command keys
and on-screen symbols are learned, mastery and enjoyment of the game
is the matter of tactical skill and literary imagination.

To be honest, right now you need a lot of imagination, since the game
is very basic, though playable and winnable. Contributions welcome.


Dungeon
-------

The goal of the hero is to explore the dungeon from top to the very bottom
(and grab lots of shiny treasure and gear on the way).
The dungeon consists of 10 levels and each level consists of 80 by 21 tiles.
The basic tiles tiles are as follows.

               dungeon terrain type               on-screen symbol
               floor                              .
               wall (horizontal and vertical)     - and |
               corridor                           #
               stairs (up and down)               < and >
               closed door                        +
               rock                               blank

The game world is persistent, i.e., every time the hero visits a level
during one game, the level should look the same.


Keys
----

Below are the default key bindings in the game.

               key    command
               c      close a door
               d      drop an object
               i      display inventory
               o      open a door
               s      search for secret doors
               q      quaff a potion
               M      display previous messages
               S      save and quit the game
               Q      quit without saving
               .      wait
               ,      pick up an object
               :      target floors
               <      ascend a level
               >      descend a level
               TAB    cycle among heroes on the level
               0--9   select a hero anywhere in the dungeon

One of the ways of moving throughout the level is with the vi text editor keys
(also known as "Rogue-like keys").

               key    command
               k      up
               j      down
               h      left
               l      right
               y      up-left
               u      up-right
               b      down-left
               n      down-right

Pressing a capital letter corresponding to a direction key will have
the hero run in that direction until something interesting occurs.

It's also possible to move using the numerical keypad, with Shift for running
and the middle '5' key for resting. (If you are using the curses frontend,
numerical keypad may not work correctly for terminals with broken terminfo,
e.g., gnome terminal tends to have problems, while xterm works fine.)

There are also some debug and cheat keys. Use at your peril!

               key    command
               v      display the version of the game
               O      toggle "omniscience"
               I      display level meta-data
               R      toggle smell display
               T      toggle level generation sequence
               V      toggle field of vision display


Monsters
--------

The hero is not alone in the dungeon. Monsters roam the game world, too.
Monsters inhabit specific locations on the game map, and can be seen
if the tile they are on can be seen by the hero.
Every monster gets a turn per move of the hero. Monster moves
are restricted in the same way as hero moves, i.e., they cannot move
into obstacles like walls or rock. Some monsters
ignore the hero, others chase him only when they see him
and the especially dangerous kind is able to smell the hero.

When the hero moves into a monster or a monster bumps into the hero,
combat occurs. Whenever combat occurs, the attacked actor may lose some health.
If the hero dies, the game ends.


On Winning and Dying
--------------------

If you happen to die, you are free to start again from the first level
of the dungeon, but all your treasure is gone and the dungeon will look
different this time.

You win the game if you escape the dungeon alive with treasure and valuable
items --- the more the better!
