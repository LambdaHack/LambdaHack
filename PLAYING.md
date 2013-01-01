Playing LambdaHack
==================

LambdaHack is a small dungeon crawler illustrating the roguelike game engine
library also called LambdaHack. Playing the game involves walking around
the dungeon, alone or in a party of fearless adventurers, setting up ambushes,
hiding in shadow, covering tracks, breaking through to deeper caves,
bumping into monsters, doors and walls, gathering magical treasure
and making creative use of it. The bloodthirsty monsters do the same,
intelligence allowing, while tirelessly chasing the elusive heroes
by smell and sight.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
for this simple game, but it's playable and winnable.


Dungeon
-------

The heroes are marked on the map with symbol '@' and with '1', '2', ..., '9'.
Their goal is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.
The dungeon consists of 10 levels and each level consists of 80 by 21 tiles.
The basic tiles are as follows.

               dungeon terrain type               on-screen symbol
               floor                              .
               corridor                           #
               wall (horizontal and vertical)     - and |
               pillar                             O
               stairs up                          <
               stairs down                        >
               open door                          | and -
               closed door                        +
               rock                               blank

The game world is persistent, i.e., every time the player visits a level
during a single game, the level layout is the same. Some items
aid in dungeon exploration, e.g., a ring of searching improves the speed
of finding hidden doors by heroes and monsters. The higher the ability
bonus displayed for this and other dungeon items, the more effective it is.
Only the best item carried in a hero's or monster's inventory counts.
You can throw the rest away, but beware that your adversaries may pick it up
and use it against your party.


Keys
----

You move throughout the level using the numerical keypad or
the vi text editor keys (also known as "Rogue-like keys").

               7 8 9     y k u
                \|/       \|/
               4-5-6     h-.-l
                /|\       /|\
               1 2 3     b j n

SHIFT (or CTRL) and a movement key make the selected hero run in the indicated
direction, until anything of interest is spotted. '5' and '.' use a turn
to brace for combat, which gives a chance to bposk blows next turn.
Melee, searching for secret doors and opening closed doors can be done
by bumping into a monster, a wall and a door, respectively.

Below are the default keys for major commands. Those of them that take
hero time are marked with a *.

               key     command
               <       ascend a level*
               >       descend a level*
               ?       display help
               R       restart game*
               S       save game
               X       save and exit*
               c       close a door*
               d       drop an object*
               g       get an object*
               i       display inventory
               o       open a door*
               q       quaff a potion*
               r       read a scroll*
               t       throw a dart*
               z       zap a wand*

To make a ranged attack, you need to set your target first, using
targeting mode. Note that the target, for the few commands that require any,
is indicated by the targeting cursor. The origin of a command
--- the  hero that performs it --- is unaffected by targeting. For example,
not the targeted door, but one adjacent to the selected hero is closed by him.

To avoid confusion, commands that take time are bposked when targeting
at a remote level (when the cursor is on a different level
than the selected hero). The targeting commands and all the less used
commands are listed below. None of them takes hero time.

               key       command
               ESC       cancel action
               RET       accept choice
               SPACE     clear messages
               TAB       cycle among heroes on level
               SHIFT-TAB cycle among heroes in the dungeon
               *         target monster
               +         swerve targeting line
               -         unswerve targeting line
               /         target position
               D         dump current configuration
               P         display previous messages
               [         target next shallower level
               ]         target next deeper level
               {         target 10 levels shallower
               }         target 10 levels deeper
               0--9      select a hero anywhere in the dungeon

There are also some debug and cheat keys, all entered with the CTRL
key modifier. Use at your own peril!

               key     command
               CTRL-a  toggle visible area display
               CTRL-o  toggle "omniscience" (effective next turn)
               CTRL-s  toggle smell display
               CTRL-v  cycle vision modes (effective next turn)


Monsters
--------

Heroes are not alone in the dungeon. Monsters roam the dark caves
and crawl from damp holes day and night. While heroes pay attention
to all other party members and take care to move one at a time,
monsters don't care about each other and all move at once,
sometimes brutally colliding by accident.

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
Target a monster with the '*' key from the top keyboard row or from keypad.
You may throw any object in your possession (press '?' to choose
an object and press it again for a non-standard choice) or on the floor
(press '-'). Only objects of a few kinds inflict any damage.
Whenever the monster's or hero's hit points reach zero, the combatant dies.
When the last hero dies, the game ends.


On Winning and Dying
--------------------

You win the game if you escape the dungeon alive. Your score is
the sum of all gold you've plundered plus 100 gold pieces for each gem.
Only the loot in possession of the party members on the current level
counts (the rest of the party is considered MIA).

If all heroes die, your score is halved and only the treasure carried
by the last standing hero counts. You are free to start again
from a different entrance to the dungeon, but all your previous wealth
is gone and fresh, undaunted enemies bar your way.
