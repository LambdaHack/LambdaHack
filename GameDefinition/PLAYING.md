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
The game also features multiplayer cooperative and competitive modes,
though only a shared-screen interface is provided at this time.


Dungeon
-------

The heroes are marked on the map with symbols '@' and '1' through '9'.
Their goal is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.
The dungeon of the campaign mode game consists of 10 levels and each level
consists of large number of tiles. The basic tiles are as follows.

               dungeon terrain type               on-screen symbol
               floor                              .
               corridor                           #
               wall (horizontal and vertical)     - and |
               rock or tree                       O
               cache                              &
               stairs up                          <
               stairs down                        >
               open door                          | and -
               closed door                        +
               bedrock                            blank

The game world is persistent, i.e., every time the player visits a level
during a single game, the level layout is the same.


Keys
----

You move throughout the level using the numerical keypad or
the vi text editor keys (also known as "Rogue-like keys").

               7 8 9     y k u
                \|/       \|/
               4-5-6     h-.-l
                /|\       /|\
               1 2 3     b j n

SHIFT (or CTRL) and a movement key make the party leader run in the indicated
direction, until anything of interest is spotted. The '5' and '.' keys take
a turn to brace for combat, which gives a chance to block blows
for the remainder of the turn. Melee, searching for secret doors
and opening closed doors can be done by bumping into a monster,
a wall and a door, respectively.

Below are the default keys for major commands.

               key       command
               ESC       cancel action or bring up the Main Menu
               RET       accept choice
               <         ascend a level
               >         descend a level
               ?         display help
               I         display inventory
               P         play back last keys 100 times
               c         close a door
               d         drop an object
               g         get an object
               o         open a door
               p         play back last keys
               q         quaff a potion
               r         read a scroll
               t         throw a dart
               z         zap a wand
               CTRL-p    play back last keys 1000 times

To make a ranged attack, you need to set your target first,
using targeting mode ('*' and '/' keys). The target, for the few
commands that require any, is indicated by the targeting cursor.
The targeting commands and all the less used commands are listed below.

               key       command
               TAB       cycle among party members on the level
               SHIFT-TAB cycle among party members in the dungeon
               *         target monster
               +         swerve targeting line
               -         unswerve targeting line
               /         target position
               =         select (or deselect) a party member
               D         display player diary
               S         mark smell
               T         mark suspect terrain
               V         mark visible area
               [         target next shallower level
               ]         target next deeper level
               _         deselect (or select) all on the level
               {         target 10 levels shallower
               }         target 10 levels deeper
               0--9      pick a new leader anywhere in the dungeon

Commands for saving and exiting the current game, starting a new game, etc.,
are listed in the Main Menu, brough up by the ESC key.
Some of the game modes are multiplayer or feature multiple computer
players (allied or not). The setup of the modes can be modified
via a configuration file. Game difficulty setting affects hitpoints
at birth for any actors of any UI-using player.

               key       command
               CTRL-x    save and exit
               CTRL-r    new campaign game
               CTRL-k    new skirmish game
               CTRL-p    new PvP game
               CTRL-o    new Coop game
               CTRL-e    new defense game
               CTRL-d    cycle next game difficulty

There are also some debug, testing and cheat options and game modes
that can be specified on the command line when starting the game server.
Use at your own peril! :) Of these, you may find the screensaver modes
the least spoilery and the most fun, e.g.:

    LambdaHack --newGame --noMore --maxFps 45 --savePrefix testCampaign --gameMode testCampaign


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
as it finds and picks up new weapons. Heroes and monsters running into
one another (with the Shift key) do not inflict damage, but change places.
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

You win the game if you escape the dungeon alive (or eliminate
all opposition, in some game modes). Your score is
the sum of all gold you've plundered plus 100 gold pieces for each gem.
Only the loot in possession of the party members on the current level
counts (the rest of the party is considered MIA).

If all heroes die, your score is halved and only the treasure carried
by the last standing hero counts. You are free to start again
from a different entrance to the dungeon, but all your previous wealth
is gone and fresh, undaunted enemies bar your way.
