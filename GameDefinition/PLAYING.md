Playing LambdaHack
==================

LambdaHack is a small dungeon crawler illustrating the roguelike game engine
of the same name. Playing the game involves exploring spooky dungeons,
alone or in a party of fearless adventurers, setting up ambushes
for unwary creatures, hiding in shadows, bumping into unspeakable horrors,
hidden passages and gorgeous magical treasure and making creative use
of it all. The madness-inspiring abominations that multiply in the depths
perform the same feats, due to their aberrant, abstract hyper-intelligence,
while tirelessly chasing the elusive heroes by sight, sound and smell.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
for this rudimentary game, but it's playable and winnable.
Contributions are welcome.


Heroes
------

The heroes are marked on the map with symbols `@` and `1` through `9`.
Their goal is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.

The currently chosen party leader is highlighted on the screen
and his attributes are displayed at the bottommost status line,
which in its most complex form may look as follows.

    *@12 Adventurer  4d1+5% Calm: 20/60 HP: 33/50 Target: basilisk  [**___]

The line starts with the list of party members (unless only one member
resides on the currently displayed level) and the shortened name of the team.
Then comes the damage of the leader's weapon (but regardless of the figures,
each attack inflicts at least 1 damage), then his current and maximum
Calm (composure, focus, attentiveness), then his current and maximum
HP (hit points, health). At the end, the personal target of the leader
is described, in this case a basilisk monster, with hit points drawn as a bar.

The other status line describes the current location in relation to the party.

    5  Lofty hall   [33% seen] Cursor: exact spot (71,12)  p15 l10

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
The 'cursor' is the common target of the whole party,
directly manipulated with movement keys in the targeting mode.
At the end comes the length of the shortest path from the leader
to the cursor position and the straight-line distance between the two points.


Dungeon
-------

The dungeon of the campaign mode game consists of 10 levels and each level
consists of a large number of tiles. The basic tile kinds are as follows.

               dungeon terrain type               on-screen symbol
               ground                             .
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
during a single game, its layout is the same.


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

In targeting mode the keys above move the targeting cursor. In normal mode,
`SHIFT` (or `CTRL`) and a movement key make the current party leader
(and currently selected party members, if any) run in the indicated
direction, until anything of interest is spotted.
The '5', 'i' and '.' keys consume a turn and make you brace for combat,
which reduces any damage taken for a turn and makes it impossible
for foes to displace you. You displace enemies or friends by bumping
into them with SHIFT (or CTRL).

Melee, searching for secret doors, looting and opening closed doors
can be done by bumping into a monster, a wall and a door, respectively.
Few commands other than movement, 'g'etting an item from the floor,
'a'pplying an item and 'f'linging an item are necessary for casual play.
Some are provided only as specialized versions of more general commands
or as building blocks for more complex convenience commands,
e.g., the autoexplore command (key `X`) could be defined
by the player as a macro using `BACKSPACE`, `CTRL-?`, `;` and `V`.

Below are the remaining keys for terrain exploration and alteration.

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

Item-use related keys are as follows.

                keys           command
                E              describe equipment of the leader
                P              describe backpack inventory of the leader
                S              describe the shared party stash
                G              describe items on the ground
                A              describe all owned items
                g and ,        get an item
                d              drop an item
                e              equip an item
                p              pack an item into inventory backpack
                s              stash and share an item
                a              activate applicable item
                q              quaff potion
                r              read scroll
                f              fling projectable item
                t              throw missile
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

Here are the commands for automating the actions of one or more members
of the team.

                keys           command
                =              select (or deselect) a party member
                _              deselect (or select) all on the level
                v              voice again the recorded commands
                V              voice the recorded commands 100 times
                CTRL-v         voice the recorded commands 1000 times
                CTRL-V         voice the recorded commands 10 times
                '              start recording commands
                CTRL-A         automate faction (ESC to retake control)

Assorted remaining keys and commands follow.

                keys           command
                ?              display help
                D              display player diary
                T              mark suspect terrain
                Z              mark visible zone
                C              mark smell clues
                TAB            cycle among party members on the level
                SHIFT-TAB      cycle among all party members
                SPACE          clear messages
                ESC            cancel action, open Main Menu
                RET            accept choice
                0--6           pick a new hero leader anywhere in the dungeon

Commands for saving and exiting the current game, starting a new game, etc.,
are listed in the Main Menu, brought up by the `ESC` key.
Game difficulty setting affects hitpoints at birth for any actors
of any UI-using faction. For a person new to roguelikes, the Duel game mode
offers a gentle introduction. The subsequent game modes gradually introduce
squad combat, stealth, asymmetric battles and more game elements.

                keys           command
                CTRL-x         save and exit
                CTRL-u         new Duel game
                CTRL-k         new Skirmish game
                CTRL-m         new Ambush game
                CTRL-b         new Battle game
                CTRL-a         new Campaign game
                CTRL-d         cycle next game difficulty

There are also some debug, testing and cheat options and game modes
that can be specified on the command line when starting the game server.
Use at your own peril! :) Of these, you may find the screensaver game modes
the least spoilery and the most fun, e.g.:

    LambdaHack --savePrefix test --newGame --noMore --maxFps 60 --automateAll --gameMode campaign --difficulty 1

The `--automateAll` option strictly corresponds to the `CTRL-A` command,
but most of the debug options have no corresponding commands.


Monsters
--------

Heroes are not alone in the dungeon. Monstrosities, natural
and out of this world, roam the dark caves and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and all move at once, sometimes brutally colliding by accident.

When the hero bumps into a monster or a monster attacks the hero,
melee combat occurs. The best equipped weapon or the best fighting organ
of each opponent is taken into account for calculating damage.
The damage the current hero can potentially inflict is displayed
at the bottom of the screen, but the actual damage depends also
on the monster's armor. Heroes and monsters running into one another
(with the `SHIFT` key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape.

Slinging a missile at a target wounds it, consuming the weapon in the process.
You may propel any item in your equipment, inventory and on the ground
(press `?` to choose an item and press it again for a non-standard choice).
Only items of a few kinds inflict any damage, but some have other effects.
Whenever the monster's or hero's hit points reach zero, the combatant dies.
When the last hero dies, the game ends in defeat.


On Winning and Dying
--------------------

You win the game if you escape the dungeon alive or, in game modes with
no exit opportunity, if you eliminate all opposition. In the former case,
your score is based on the gold and precious gems you've plundered,
plus a bonus based on the number of heroes you lost. In the latter case,
your score is based on the number of turns you spent overcoming your foes
and, as a bonus, the number of enemies you've subdued.

If all your heroes fall, you are awarded a score for your valiant deeds,
but no winning bonus. When, invariably, a new overconfident party
of adventurers storms the dungeon, they start from a new entrance,
with no experience and no equipment, and new, undaunted enemies
bar their way.
