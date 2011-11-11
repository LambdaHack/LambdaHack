Playing Allure of the Stars
===========================

Playing Allure of the Stars involves walking around the dungeon
(err, a Solar System space-ship that incidentally closely resembles
a fantasy dungeon, in this pre-release version of the game),
alone or in a party of fearless adventurers, jumping between levels,
bumping into monsters, doors and walls, gathering magical treasure
and making creative use of it. The bloodthirsty monsters do the same,
intelligence allowing, while tirelessly chasing the noble heroes
by smell and night-sight.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
right now, but the game is already playable and winnable.
Contributions welcome.


Dungeon
-------

The goal of the hero is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.
The dungeon consists of 10 levels and each level consists of 80 by 21 tiles.
The basic tiles are as follows.

               dungeon terrain type               on-screen symbol
               floor                              .
               wall                               #
               stairs up                          <
               stairs down                        >
               open door                          '
               closed door                        +

The game world is persistent, i.e., every time the player visits a level
during a single game, the level layout is the same. Some items
aid in dungeon exploration, e.g., a ring of searching improves the speed
of finding hidden doors by heroes and monsters. The higher the magical
bonus displayed for this and other dungeon items, the more effective it is.
Only the best item carried in a hero's or monster's inventory counts.
You can throw the rest away, but beware that your adversaries may pick it up
and use it against you.


Keys
----

You move throughout the level using the numerical keypad or
the vi text editor keys (also known as "Rogue-like keys").

               7 8 9     y k u
                \|/       \|/
               4-5-6     h-.-l
                /|\       /|\
               1 2 3     b j n

Shift and a movement key make the hero run in the indicated direction,
until anything of interest is spotted. '5' and '.' skip a turn.
(Note that If you are using the curses or vty frontends,
numerical keypad may not work correctly depending on the versions
of curses, terminfo and terminal emulators. Vi keys should work regardless.)

Below are the basic default keys form common commands.

               key    command
               <      ascend a level
               >      descend a level
               ?      display help
               Q      quit without saving
               X      save and exit the game
               c      close a door
               d      drop an object
               g      get an object
               i      display inventory
               q      quaff a potion
               r      read a scroll
               a      aim a wand
               t      throw a weapon

Searching for secret doors and opening closed doors have no keys assigned.
Instead, bump into a wall to search and bump into a door to open.

To make a distance attack, you need to set your target first.
The targeting commands are listed below, together with all the other
less used commands.

               key    command
               ESC    cancel action
               RET    accept choice
               TAB    cycle among heroes on level
               0--9   select a hero anywhere in the dungeon (gtk only)
               *      target monster
               /      target location
               D      dump current configuration
               P      display previous messages
               V      display game version

There are also some debug and cheat keys. Use at your peril!

               key    command
               O      toggle "omniscience"
               I      inform about level meta-data
               R      rotate display modes


Monsters
--------

The hero is not alone in the dungeon. Monsters roam the dark caves
and crawl from damp holes day and night. While heroes pay attention
to all other party members and take moves sequentially, one after another,
monsters don't care about each other and all move at once,
sometimes brutally colliding by mistake.

When the hero bumps into a monster or a monster attacks the hero,
melee combat occurs. The best weapon carried by each opponent
is taken into account for calculating bonus damage. The total damage
the current hero can potentially inflict is displayed at the bottom
of the screen. The total damage potential of a monster may change
as it finds and picks up new weapons. Heroes and monsters running
into another (with the Shift key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape.

Throwing weapons at targets wounds them, consuming the weapon in the process.
You can target a monster with the '*' key from the top row or kepad.
You may throw any object in your possession
(press '*' for a non-standard choice) or on the floor (press '-'),
though only objects of a few kinds inflict any damage.
Whenever a monster or a hero hit points reach zero, the combatant dies.
When the last hero dies, the game ends.


On Winning and Dying
--------------------

You win the game if you escape the dungeon alive. Your score is
the sum of all gold you've plundered plus 100gp for each gem.
Only the loot in possession of the party members on level 1 counts
(the rest is considered MIA).

If all heroes die, your score is halved and only the treasure carried
by the last standing hero counts. You are free to start again
from the first level of the dungeon, but all your wealth and items
are gone and the dungeon and it's treasure look differently.
