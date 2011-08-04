Playing Allure of the Stars
===========================

Playing Allure of the Stars involves walking around the dungeon
(err, a Solar System space-ship that incidentally closely resembles
a fantasy dungeon, in this prerelease version of the game),
alone or in a party of fearless adventurers, jumping between levels,
bumping into monsters, doors and walls, gathering magical treasure
and making creative use of it. The bloodthirsty monsters do the same,
intelligence allowing, while tirelessly chasing the noble heroes
by smell and night-sight.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, you need a lot of imagination
right now, since the game is still quite basic, though playable and winnable.
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
               closed door                        +
               open door                          '

The game world is persistent, i. e., every time a hero visits a level
during a single game, the level layout looks the same. Some items
aid in dungeon exploration, e.g., a ring of searching improves the speed
of finding hidden doors by heroes and monsters. The higher the magical
bonus displayed for this and other dungeon items, the more effective it is.
Only the best item carried in a hero's or monster's inventory counts.
You can throw the rest away, but beware that your adversaries may pick it up
and use it against you.


Keys
----

Below are the basic default keys.

               key    command
               .      wait
               <      ascend a level
               >      descend a level
               ?      display help
               Q      quit without saving
               X      save and exit the game
               c      close a door
               d      drop an object
               g      get an object
               i      display inventory
               o      open a door (alternatively, you can bump into a door)
               q      quaff a potion
               r      read a scroll
               s      search for secret doors (or you can bump into a wall)

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

It is also possible to move using the numerical keypad, with Shift for running
and the middle '5' key for waiting. (If you are using the curses frontend,
numerical keypad may not work correctly for terminals with broken terminfo,
e.g., gnome terminal has problems, while xterm works fine.)

To make a distance attack, you need to set your target first.
The targeting commands are listed below, together with all the other
less common player commands.

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
               a      aim a wand
               t      throw a weapon

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
You can target a monster with the '*' key from the top row or numpad.
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
