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
for this rudimentary set of scenarios, even though they are playable
and winnable. Contributions are welcome.


Heroes
------

The heroes are marked on the map with symbols `@` and `1` through `9`.
Their goal is to explore the dungeon, battle the horrors within,
gather as much gold and gems as possible, and escape to tell the tale.

The currently chosen party leader is highlighted on the screen
and his attributes are displayed at the bottommost status line,
which in its most complex form may look as follows.

    *@12 Adventurer  4d1+5% Calm: 20/60 HP: 33/50 Target: basilisk  [**___]

The line starts with the list of party members (unless there's only one member)
and the shortened name of the team. Clicking on the list selects heroes and
the selected run together when `SHIFT`-left mouse button is pressed.

Then comes the damage of the highest damage dice weapon the leader can use,
then his current and maximum Calm (composure, focus, attentiveness), then
his current and maximum HP (hit points, health). At the end, the personal
target of the leader is described, in this case a basilisk monster,
with hit points drawn as a bar. Weapon damage and other item stats
are displayed using the dice notation `XdY`, which means X rolls
of Y-sided dice. A variant denoted `XdsY` is additionally
scaled by the level depth in proportion to the maximal dungeon depth.
You can read more about combat resolution in section Monsters below.

The second status line describes the current level in relation
to the party.

    5  Lofty hall   [33% seen] X-hair: exact spot (71,12)  p15 l10

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
The 'X-hair' (meaning 'crosshair') is the common focus of the whole party,
denoted on the map by a white box and manipulated with movement keys
in aiming mode. At the end of the status line comes the length of the shortest
path from the leader to the crosshair position and the straight-line distance
between the two points.


Dungeon
-------

The dungeon of any particular scenario may consists of one or many
levels and each level consists of a large number of tiles.
The basic tile kinds are as follows.

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

You walk throughout a level using the left mouse button or the numeric
keypad (left diagram) or its compact laptop replacement (middle)
or Vi text editor keys (right, also known as "Rogue-like keys",
which have to be enabled in config.ui.ini).

                7 8 9          7 8 9          y k u
                 \|/            \|/            \|/
                4-5-6          u-i-o          h-.-l
                 /|\            /|\            /|\
                1 2 3          j k l          b j n

In aiming mode the same keys (or the middle and right mouse buttons)
move the crosshair (the white box). In normal mode, `SHIFT` (or `CTRL`)
and a movement key make the current party leader run in the indicated
direction, until anything of interest is spotted. The `5` keypad key
and the `i` and `.` keys consume a turn and make you brace for combat,
which reduces any damage taken for a turn and makes it impossible
for foes to displace you. You displace enemies or friends by bumping
into them with `SHIFT` (or `CTRL`).

Melee, searching for secret doors, looting and opening closed doors
can be done by bumping into a monster, a wall and a door, respectively.
Few commands other than movement, 'g'etting an item from the floor,
'a'pplying an item and 'f'linging an item are necessary for casual play.
Some are provided only as specialized versions of the more general
commands or as building blocks for more complex convenience macros,
e.g., the autoexplore command (key `X`) could be defined
by the player as a macro using `CTRL-?`, `:` and `V`.

The following minimal command set lets you accomplish almost anything
in the game, though not necessarily with the fewest number of keystrokes.
The full list of commands can be seen in the in-game help accessible
from the Main Menu.

        keys            command
        <               ascend a level
        >               descend a level
        c               close door
        E               manage equipment of the leader
        g and ,         get items
        a               apply consumable
        f               fling projectile
        +               swerve the aiming line
        D               display player diary
        T               toggle suspect terrain display
        SHIFT-TAB       cycle among all party members
        ESC             cancel action, open Main Menu

The only activity not possible with the commands above is the management
of non-leader party members. The defaults should usually suffice,
especially if your non-leader heroes can only melee or wait
and none have found the equipment that enables opportunity fire.
If there's a need, you can manually set party tactics with `CTRL-T`
and you can assign individual targets to party members
using the aiming and targeting commands listed below.

        keys            command
        KEYPAD_* and \  aim at an enemy
        KEYPAD_/ and |  cycle aiming styles
        +               swerve the aiming line
        -               unswerve the aiming line
        CTRL-?          set crosshair to the closest unknown spot
        CTRL-I          set crosshair to the closest item
        CTRL-{          set crosshair to the closest stairs up
        CTRL-}          set crosshair to the closest stairs down
        BACKSPACE       reset target/crosshair
        RET and INSERT  accept target/choice

For ranged attacks, setting the crosshair or individual targets
beforehand is not mandatory, because the crosshair is set automatically
as soon as a monster comes into view and can still be adjusted while
in the missile choice menu. However, if you want to assign persistent
personal targets or just inspect the level map closely, you can enter
the detailed aiming mode with the right mouse button or with
the `*` keypad key that selects enemies or the `/` keypad key that
marks a tile. You can move the aiming crosshair with direction keys
and assign a personal target to the leader with `RET`.
The details of the shared crosshair position and of the personal target
are described in the status lines at the bottom of the screen.

Commands for saving and exiting the current game, starting a new game, etc.,
are listed in the Main Menu, brought up by the `ESC` key.
Game difficulty setting affects hitpoints at birth for any actors
of any UI-using faction. For a person new to roguelikes, the Duel scenario
offers a gentle introduction. The subsequent game modes gradually introduce
squad combat, stealth, opportunity fire, asymmetric battles and more
gameplay elements.


Monsters
--------

Heroes are not alone in the dungeon. Monstrosities, natural
and out of this world, roam the dark caves and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and all move at once, sometimes brutally colliding by accident.

When the hero bumps into a monster or a monster attacks the hero,
melee combat occurs. Heroes and monsters running into one another
(with the `SHIFT` key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape. In some circumstances actors are immune to the displacing,
e.g., when both parties form a continuous front-line.

In melee combat, the best equipped weapon (or the best fighting organ)
of each opponent is taken into account for determining the damage
and any extra effects of the blow. If a recharged weapon with a non-trivial
effect is in the equipment, it is chosen for combat. Otherwise combat
involves the weapon with the highest raw damage dice (the same as displayed
at bottommost status line).

To determine the damage dealt, the outcome of the weapon's damage dice roll
is multiplied by the melee damage bonus (summed from the equipped items
of the attacker) minus the melee armor modifier of the defender.
Regardless of the calculation, each attack inflicts at least 1 damage.
The current leader's melee bonus, armor modifier and other detailed
stats can be viewed via the `!` command.

In ranged combat, the missile is assumed to be attacking the defender
in melee, using itself as the weapon, but the ranged damage bonus
and the ranged armor modifier are taken into account for calculations.
You may propel any item in your equipment, inventory pack and on the ground
(by default you are offered only the appropriate items; press `?`
to cycle item menu modes). Only items of a few kinds inflict any damage,
but some have other effects, beneficial, detrimental or mixed.

Whenever the monster's or hero's hit points reach zero, the combatant dies.
When the last hero dies, the scenario ends in defeat.


On Winning and Dying
--------------------

You win the scenario if you escape the dungeon alive or, in scenarios with
no exit locations, if you eliminate all opposition. In the former case,
your score is based on the gold and precious gems you've plundered.
In the latter case, your score is based on the number of turns you spent
overcoming your foes (the quicker the victory, the better; the slower
the demise, the better). Bonus points, based on the number of heroes
that were lost, are awarded if you win.

If all your heroes fall, you will invariably see a new foolhardy party
of adventurers clamoring to be led into the dungeon. They start
their conquest from a new entrance, with no experience and no equipment,
and new, undaunted enemies bar their way. Lead them wisely!
