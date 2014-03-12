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
for this rudimentary game, but it's playable and winnable.
The game also features experimental multiplayer cooperative and competitive
modes, but they are troublesome to play with the shared-screen
and shared-keyboard interface available at this time.
Contributions welcome.


Heroes
------

The heroes are marked on the map with symbols `@` and `1` through `9`.
Their goal is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.

The currently chosen party leader is highlighted on the screen
and his attributes are displayed at the bottommost status line,
which in its most complex form may look as follows.

    *@12 Adventurer D: 5d1+1 S: 20/50 H: 33/50 Target: basilisk  (path  7)

The line starts with the list of party members (unless only one member
resides on the currently displayed level) and the shortened name of the team.
After 'D' comes the damage of the leader's weapon, after 'C' his current
and maximum calm (composure, focus) and after 'H' his current and maximum
hit points (health). At the end, the personal target of the leader
is described and the length of the shortest path to the target.

The other status line relates to the dungeon and to the whole party.

    5  Tall cavern  [33% seen] Cursor: exact spot (71,12)  (path  5)

First comes the depth of the current level and its description.
Then the approximate percentage of its explorable tiles already
seen by the heroes. The cursor is the common target of the whole party,
directly manipulated with movement keys in the targeting mode.
The line ends with the length of the shortest path from the leader
to the cursor position.


Dungeon
-------

The dungeon of the campaign mode game consists of 10 levels and each level
consists of a large number of tiles. The basic tile kinds are as follows.

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


Commands
--------

You move throughout the level using the numerical keypad (left diagram)
or its compact laptop replacement (middle) or Vi text editor keys
(right, also known as "Rogue-like keys", which have to be enabled
in config.ui.ini).

                7 8 9          7 8 9          y k u
                 \|/            \|/            \|/
                4-5-6          u-i-o          h-.-l
                 /|\            /|\            /|\
                1 2 3          j k l          b j n

`SHIFT` (or `CTRL`) and a movement key make the current party leader
(and currently selected party members, if any) run in the indicated
direction, until anything of interest is spotted.
The '5', 'i' and '.' keys consume a turn and make you brace for combat,
which confers a chance to block blows for the remainder of the turn.
In targeting mode the same keys move the targeting cursor.

Melee, searching for secret doors and opening closed doors
can be done by bumping into a monster, a wall and a door, respectively.
Few commands other than movement are necessary for casual play.
Some are provided only as building blocks for more complex convenience
commands, e.g., the autoexplore command (key `X`) could be defined
by the player as a macro using `BACKSPACE`, `CTRL-?`, `;` and `P`.

Below are the remaining keys for movement and terrain alteration.

                keys           command
                <              ascend a level
                CTRL-<         ascend 10 levels
                >              descend a level
                CTRL->         descend 10 levels
                ;              make one step towards the target
                :              go to target for 100 steps
                CTRL-:         go to target for 10 steps
                x              explore the closest unknown spot
                X              autoexplore 100 times
                CTRL-X         autoexplore 10 times
                R              rest (wait 100 times)
                CTRL-R         rest (wait 10 times)
                c              close door

Inventory and items-related keys are as follows.

                keys           command
                I              display inventory
                g and ,        get an object
                d              drop an object
                E              display equipment
                w              wield/wear an object
                t              take off an object
                A              display all owned objects
                q              quaff potion
                r              read scroll
                s              sling missile
                z              zap wand

To make a ranged attack, as in the last few commands above,
you need to set your target first (however, initial target is set
automatically as soon as a monster comes into view). Once in targeting mode,
you can move the targeting cursor with arrow keys and switch focus
among enemies with `*` (or among friends, projectiles and enemies, depending
on targeting mode set by `/`). The details of the shared cursor position
and of the personal target are described at the bottom of the screen.
All targeting keys are listed below.

                keys           command
                KEYPAD_* and \ target enemy
                /              cycle targeting mode
                +              swerve targeting line
                -              unswerve targeting line
                BACKSPACE      clear target/cursor
                CTRL-?         target the closest unknown spot
                CTRL-I         target the closest item
                CTRL-{         target the closest stairs up
                CTRL-}         target the closest stairs down

Commands for automating the actions of one or more members of the team.

                keys           command
                =              select (or deselect) a party member
                _              deselect (or select) all on the level
                p              play back last keys
                P              play back last keys 100 times
                CTRL-p         play back last keys 1000 times
                CTRL-P         play back last keys 10 times
                '              start recording a macro
                CTRL-A         automate faction (ESC to retake control)

Assorted remaining keys and commands follow.

                keys           command
                ?              display help
                D              display player diary
                T              mark suspect terrain
                V              mark visible area
                S              mark smell
                TAB            cycle among party members on the level
                SHIFT-TAB      cycle among party members in the dungeon
                SPACE          clear messages
                ESC            cancel action
                RET            accept choice
                0--6           pick a new hero leader anywhere in the dungeon

Commands for saving and exiting the current game, starting a new game, etc.,
are listed in the Main Menu, brought up by the `ESC` key.
All but the campaign and skirmish game modes are experimental
and feature multiple human or computer players (allied or not).
Game difficulty setting affects hitpoints at birth for any actors
of any UI-using faction.

                keys           command
                CTRL-x         save and exit
                CTRL-a         new campaign game
                CTRL-k         new skirmish game
                CTRL-v         new PvP game
                CTRL-o         new Coop game
                CTRL-e         new defense game
                CTRL-d         cycle next game difficulty

There are also some debug, testing and cheat options and game modes
that can be specified on the command line when starting the game server.
Use at your own peril! :) Of these, you may find the screensaver modes
the least spoilery and the most fun, e.g.:

    LambdaHack --newGame --noMore --maxFps 45 --savePrefix test --automateAll --gameMode campaign --difficulty 1


Monsters
--------

Heroes are not alone in the dungeon. Monsters roam the dark caves
and crawl from damp holes day and night. While heroes pay attention
to all other party members and take care to move one at a time,
monsters don't care about each other and all move at once,
sometimes brutally colliding by accident.

When the hero bumps into a monster or a monster attacks the hero,
melee combat occurs. The best weapon equipped by each opponent
is taken into account for calculating bonus damage. The total damage
the current hero can potentially inflict is displayed at the bottom
of the screen. The total damage potential of a monster may change
as it finds and picks up new weapons. Heroes and monsters running into
one another (with the `SHIFT` key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape.

Slinging missiles at targets wounds them, consuming the weapon in the process.
You may sling any object in your possession (press `?` to choose
an object and press it again for a non-standard choice) or on the floor
(press `-`). Only objects of a few kinds inflict any damage.
Whenever the monster's or hero's hit points reach zero, the combatant dies.
When the last hero dies, the game ends.


On Winning and Dying
--------------------

You win the game if you escape the dungeon alive or eliminate all opposition.
If you escape the dungeon, your score is the sum of all gold
you've plundered, plus 100 gold pieces for each gem, minus 100 gold
for each hero you lost. If the game mode offers you no escape
and so your goal is to eliminate you foes completely, your score is
100 gold for each opponent killed, plus a bonus if you manage
to kill all quickly.

If all heroes die, your score is halved. You are free to start again
from a different entrance to the dungeon, but all your previous wealth
is gone and fresh, undaunted enemies bar your way.
