Playing LambdaHack
==================

This file is temporarily out of date.
The following backstory blurb is a copy of the sample game intro screen.

 LambdaHack is a small dungeon crawler
 illustrating the roguelike game engine
 of the same name. Playing the game
 involves exploring spooky dungeons,
 alone or in a party of fearless explorers,
 avoiding and setting up ambushes,
 hiding in shadows from the gaze
 of unspeakable horrors, discovering secret
 passages and gorgeous magical treasure
 and making creative use of it all.

 The madness-inspiring abominations that
 multiply in the depths perform the same
 feats, due to their aberrant, abstract
 hyper-intelligence. They look out for
 any sign of weakness or indecision,
 ready to tirelessly chase the elusive
 heroes by sight, sound and smell.


What to expect
--------------

LambdaHack is a turn-based game. You issue a command.
Then you watch its results unfold on the screen, without you being able
to intervene. Then all settles down and you have as much time
as you want to inspect the battlefield and think about your next move.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
for this modest sample game, but it has its own distinct quirky mood
and is playable and winnable.

The game differs from classic roguelikes in a few ways:

* player manually controls each member of his squad, though often the best
  tactics is to scout with only one character (a classic roguelike feel)
  and let others guard important areas
* the game is turn-based, but with visibly high granularity --- projectiles
  fly gradually over time with varying speeds and can be sidestepped
  or shot down; less so explosions that are swarms of projectile particles
  (turn-based just the same)
* time passes and factions pursue their goals on a few levels simultaneously,
  while other levels are frozen (but all are persistent)
* the same laws of simulated nature apply to all factions and all actors,
  whether player-controlled or AI-controlled; e.g., the same field of view
  and pathfinding algorithms, skill checks, equipment and item use rules
* combat mechanics is deterministic; randomness comes from enemies
  and procedurally generated terrain only
* there's (almost) no HP regeneration; attrition ensures all past (silly)
  decisions matter; HP starts at around half max
* each character has 10 uniform equipment slots, which fill quickly given
  that most melee weapons have cooldowns
* each faction has a single shared inventory of unlimited size,
  which has a physical location on the map and so can be ransacked

If the game window is too large for your screen or you experience
other technical issues, please consult
[README.md](https://github.com/LambdaHack/LambdaHack/blob/master/README.md)
or describe your problem on gitter or the issue tracker.
Contributions of all kinds are welcome. Please offer feedback
to mikolaj.konarski@funktory.com or, preferably, at any of the public forums.


Starting your adventure
-----------------------

Commands for starting a new game, saving and exiting the current game,
configuring convenience settings and toggling AI control of the party
are listed in the main menu, brought up by the `ESC` key. Of the convenience
settings, the `suspect terrain` choice is of particular interest,
because it determines not only screen display of the level map,
but also whether suspect tiles are considered for auto-explore
and for the `C-?` command that marks the nearest unexplored position.

Game difficulty, from the game setup menu, determines hitpoints at birth:
difficulty below 5 multiplies hitpoints of player characters, difficulty
over 5 multiplies hitpoints of their enemies.
The "lone wolf" challenge mode reduces player's starting actors
to exactly one, though later on new heroes may join the party.
The "cold fish" challenge mode makes it impossible for player characters
to be healed by actors from other factions (this is a significant
restriction in the long crawl scenario).

The game scenarios, as ordered by their number, lead the player along
an optional story arc. The first two scenarios double as tutorials
that offer rudimentary preparation for the main game, the long crawl.
They gradually introduce exploration, stealth and melee combat,
helping the player develop his repertoire of squad formations
and move patterns, suitable for different tactical contexts.
When the player loses, it helps to scan the defeat message of the scenario
for hints of strategies known to work in the given tactical context.
Alternatively, the player may postpone reading these messages and instead
try to puzzle out the tactics himself --- this is not so hard, as there are
not so many moving parts to figure, at least in the short scenarios.

As soon as the player learns to navigate initial levels of the crawl game
and starts employing ranged combat, light sources and other means
of gaining or denying battlefield intel, and dies a lot, it makes sense
to return to the remaining short scenarios. They bring forth many extra
game features and tactics and prevent the player from missing half the fun
by trying to play the crawl just like a normal roguelike with spare
heroes (and often fail horribly). The extra scenarios continue
the plotline from the initial tutorial scenarios in the form of flashbacks
and eventually lead up to the events that start the main crawl adventure.
The training they provide is more synthetic in nature, drilling
a particular skill set, even as exotic as opportunity fire management,
a frantic race to the exit and big asymmetric battles. The challenge may be,
accordingly, quite tough.

The main scenario, the long crawl, is the only one that takes
place in a multi-floor setting, requiring lots of time to beat,
with focus on resource management, survival, gathering environment
clues and guessing and countering opponents' strategies.
The player has a choice of exploring a single level at a time or portions
of many levels along a single staircase. On some levels he may explore
and loot with a single scout eluding most opponents. On others he may be
forced to change the pace and perform a complete exterminatory sweep
involving his whole party. On yet others, his best course of action may be
to defend a key location until the first wave of attackers is broken.
The size of the game arena calls for strategic thinking, including
resource management, area denial and unavoidable temporary retreats
to other levels. Thus, the crawl game mode is the most replayable one,
but even the small scenarios can be refreshed by ramping up difficulty
settings and striving to beat a high score.


Exploring the world
-------------------

The map of any particular scenario consists of one or many levels
and each level has a large number of tiles with a particular
terrain kind on each. The game world is persistent, i.e., every time
the player visits a level during a single game, its layout is the same.

Letters and digits on the game screen are likely to represent actors.
On the other hand, terrain is depicted with non-letter and non-digit
characters and with zero `0`. Items lying on the ground are represented
similarly, though blocky solid symbols are likely to be non-passable
terrain and particularly unlikely to be items. In case of doubt,
one of the aiming commands (`/` and keypad `/`, with default keybinding)
cycles through all visible and remembered items on the level
and another (`*` and keypad `*`, with default keybinding) through all foes.
Also, pointing at a map position with `MMB` (middle mouse button) displays
a short description of its contents. The basic terrain kinds are as follows.

    terrain type                           on-screen symbol
    wall (horizontal and vertical)         - and |
    tree or rock or man-made column        0
    rubble                                 &
    bush, transparent obstacle             %
    trap, ice obstacle                     ^
    closed door                            +
    open door (horizontal and vertical)    | and -
    corridor                               #
    smoke or fog                           ;
    ground                                 .
    water                                  ~
    stairs or exit up                      <
    stairs or exit down                    >
    bedrock                                blank

Actors are marked with lower and upper case letters and with
characters `@` and `1` through `9` (but never `0`). Player-controlled
heroes are always bright white and by default they are selected
(e.g., to run together) so they have a blue highlight around their symbol.
If player manages to take control of animals or other actors, they retain
their letter and color, but gain a highlight as well.

So, for example, the following map shows a room with a closed door,
full of actors, connected by a corridor with a room with an open door,
a pillar, a staircase down and rubble that obscures one of the corners.
The lowest row of the larger room is full of items.

    ------       ------
    |@19.|       |....&&
    |r...+#######-...0.>&&|
    |Ra..|       |[?!,)$"=|
    ------       ----------


Leading your heroes
-------------------

The heroes are displayed on the map with bright white color (red if they are
extremely weakened) and symbols `@` and `1` through `9` (never `0`).
The currently chosen party pointman is highlighted on the map with yellow.
The easiest way to control your team is to run a short distance
with your pointman using Shift or LMB, switch the pointman with Tab, repeat.
In open terrain, if you keep consistent distance between teammates,
this resembles the leap frog infantry tactics. For best effects,
try to end each sprint behind a cover or concealment.

Pointman hero's attributes are displayed at the bottom-most status line which,
in its most complex form, looks as follows.

    *@12   2m/s Calm: 20/60 HP: 33/50 Pointman: Haskell Alvin   6d1+5% 4d1

The line starts with the list of party members, with the current pointman
highlighted in yellow. Most commands involve only the pointman, including
movement with keyboard or keypad or `LMB` (left mouse button).
If more heroes are selected (highlighted in blue), they run together
whenever `:` or `S-LMB` (while holding Shift) over map area is pressed,
though that's usually not a precise enough method of controlling a team.
Any sleeping hero is highlighted in green and can be woken up
by yelling with `%`, which also taunts or stresses nearby enemies.

Next on the bottom-most status line is the pointman's current and maximum
Calm (morale, composure, focus, attentiveness), then his current
and maximum HP (hit points, health). The colon after "Calm" turning
into a dot signifies that the pointman is in a position without ambient
illumination, making stealthy conduct easier. A brace sign instead
of a colon after "HP" means the pointman is braced for combat
(see section [Basic Commands](#basic-commands)).

In the second half of the bottom-most status line, the pointman's name
is shown. Then come damage dice of the pointman's melee weapons and pointman's
appendages, ordered by their power. The dice of the first recharged weapon,
the one that is going to be used now, is adorned with percentage
damage bonus collected from the whole equipment of the pointman.
If the dice are displayed with upper-case `D` instead of lower-case `d`,
the weapon has additional effects apart of the usual kinetic damage.
The nature of the effects can be appraised via the equipment outfit menu.

Weapon damage and other item properties are displayed using
the dice notation `xdy`, which denotes `x` rolls of `y`-sided dice.
A variant written `xdLy` is additionally scaled by the level depth
in proportion to the maximal level depth (at the first level it's
always one, then it grows up to full rolled value at the last level).
Section [Monsters](#monsters) below describes combat resolution in detail,
including the role of the percentage damage bonus.

The second, the upper status line describes the current level in relation
to the party.

    5  Lofty hall    [33% seen] X-hair: dire basilisk    [__**]

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
The `X-hair` (aiming crosshair) is the common focus of the whole party,
marked on the map with a red box and manipulated with mouse
or movement keys in aiming mode. In this example, the crosshair points
at a dire basilisk monster, with its hit points drawn as a bar.

Instead of a monster, the `X-hair` area may describe a position on the map,
a recently spotted item on the floor or an item in inventory selected
for further action or, if none are available, a summary of the team status.
For example, this form

    5  Lofty hall    [33% seen] X-hair: exact spot (71,12)    p15 l10

indicates that the party is aiming at an exact spot on the map.
At the end of the status line comes the length of the shortest
path from the pointman's position to the spot and the straight-line
distance between the two points, one that a flung projectile would travel
if there were no obstacles.


Moving and acting
-----------------

This section is a copy of the few initial bits of in-game help. The help
pages are automatically generated based on a game's keybinding content and
on overrides in the player's config file. The remaining in-game help screens,
not shown here, list all game commands grouped by categories in detail.
A text snapshot of the complete in-game help is in
[InGameHelp.txt](InGameHelp.txt).

Walk throughout a level with mouse or numeric keypad (left diagram below)
or the Vi editor keys (right) or with a compact laptop setup (middle) that
requires enabling in config.ui.ini. Run until disturbed with Shift or Control.
Go-to with LMB (left mouse button). Run collectively via S-LMB (holding Shift).

               7 8 9          7 8 9          y k u
                \|/            \|/            \|/
               4-5-6          u-i-o          h-.-l
                /|\            /|\            /|\
               1 2 3          j k l          b j n

In aiming mode, the same keys (and mouse) move the x-hair (aiming crosshair).
Press `KP_5` (`5` on keypad) to wait, bracing for impact, which reduces any
damage taken and prevents displacement by foes. Press `S-KP_5` or `C-KP_5`
(the same key with Shift or Control) to lurk 0.1 of a turn, without bracing.
Displace enemies by running into them with Shift/Control or S-LMB. Search,
open, descend and attack by bumping into walls, doors, stairs and enemies.
The best melee weapon is automatically chosen from your equipment
and from among your body parts.

The following commands, joined with the basic set above,
let you accomplish anything in the game, though
not necessarily with the fewest keystrokes. You can also
play the game exclusively with a mouse, or both mouse
and keyboard. (See the ending help screens for mouse commands.)
Lastly, you can select a command with arrows or mouse directly
from the help screen or the dashboard and execute it on the spot.

    keys         command
    E            manage equipment of the pointman
    g or ,       grab item(s)
    ESC          open main menu/finish aiming
    RET or INS   open dashboard/accept target
    SPACE        clear messages and show history
    S-TAB        cycle among all party members
    KP_* or !    cycle x-hair among enemies
    KP_/ or /    cycle x-hair among items
    c            close door
    %            yell/yawn

Screen area and UI mode (exploration/aiming) determine
mouse click effects. First, we give an overview
of effects of each button over the game map area.
The list includes not only left and right buttons, but also
the optional middle mouse button (MMB) and the mouse wheel,
which is also used over menus, to page-scroll them.
(For mice without RMB, one can use Control key with LMB and for mice
without MMB, one can use C-RMB or C-S-LMB.)

    keys         command
    LMB          go to pointer for 25 steps/fling at enemy
    S-LMB        run to pointer collectively for 25 steps/fling at enemy
    RMB or C-LMB start aiming at enemy under pointer
    S-RMB        open or close or alter at pointer
    MMB or C-RMB snap x-hair to floor under pointer
    WHEEL-UP     swerve the aiming line
    WHEEL-DN     unswerve the aiming line


Battling monsters
-----------------

The life of the heroes is full of dangers. Monstrosities, natural
and out of this world, roam the dark corridors and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and all move at once, sometimes brutally colliding by accident.

Monsters are depicted on the map with letters. Upper case letters
are unique monsters, often guardians of dungeon levels, and lower case
letters are the rabble. If there are humans not from our team,
they are marked with `@` and `1` through `9` in other colours than white.

When a hero walks and bumps into a monster or a monster attacks
the hero, melee combat occurs. Hero *running* into and displacing
a monster (with the `Shift` key and, in case of keypad movement,
also possibly a `Control` key), does not involve inflicting a damage,
but only causes an exchange of places. This gives the opponent
a free blow, but can improve the tactical situation or aid escape.
In some circumstances actors are immune to the displacing,
e.g., when both parties form a continuous front-line.

In melee combat, the best recharged equipped weapon (or the best fighting
organ that is not on cooldown) of each opponent is taken into account
for determining the damage and any extra effects of the blow.
To determine the damage dealt, the outcome of the weapon's damage dice roll
is multiplied by a percentage bonus. The bonus is calculated by taking
the damage bonus (summed from the equipped items of the attacker,
capped at 200%) minus the melee armor modifier of the defender
(capped at 200%, as well), with the outcome bounded between -99% and 99%,
which means that at least 1% of damage always gets through
and the damage is always lower than twice the dice roll.
The current pointman's melee bonus, armor modifier and other detailed
stats can be viewed in the skills menu, accessible via the `#` command.

In ranged combat, the projectile is assumed to be attacking the defender
in melee, using itself as the weapon, with the usual dice and damage bonus.
This time, the ranged armor skill of the defender is taken into account
and, additionally, the speed of the missile (based on shape and weight)
figures in the calculation. You may propel any item from your inventory
(by default you are offered only the appropriate items; press `+` to cycle
item menu modes). Only items of a few kinds inflict any damage, but some
have other effects, beneficial, detrimental or mixed.

In-game detailed item descriptions contain melee and ranged damage estimates.
They do not take into account damage from effects and, if bonuses are not
known, guesses are based on averages for the item kind in question.
The displayed figures are rounded, but the game internally keeps track
of minute fractions of HP in all calculcations.

The stress of combat drains Calm, gradually limiting viewing radius and,
if Calm reaches zero and the actor is sufficiently impressed by his foes,
making him defect and surrender unto their domination.
Whenever the monster's or hero's hit points reach zero,
the combatant is incapacitated and promptly dies.
When the last hero dies or is dominated, the scenario ends in defeat.


Attacking from a distance
-------------------------

For ranged attacks, setting the aiming crosshair beforehand is not mandatory,
because x-hair is set automatically as soon as a monster comes into view
and can still be adjusted for as long as the missile to fling is not chosen.
However, sometimes you want to examine the level map tile by tile
or assign persistent personal targets to party members.
The latter is essential in the rare cases when your non-pointmen
(non-pointman characters) can move autonomously or fire opportunistically
(via innate skills or rare equipment). Also, if your non-pointman is adjacent
to more than one enemy, setting his target makes him melee a particular foe.

You can enter the aiming mode with the `*` and keypad `*` keys that
select enemies or the `/` and keypad `/` keys that cycle among items
on the floor and mark a tile underneath an item. You can move x-hair
with direction keys and assign a personal target to the pointman
with a `RET` key (Return, Enter). The details of the shared x-hair mark
are displayed in a status line close to the bottom of the screen,
as explained in section [Heroes](#heroes) above.


Winning and dying
-----------------

You win a scenario if you escape the location alive (which may prove
difficult, because your foes tend to gradually build up the ambush squad
blocking your escape route) or, in scenarios with no exit locations,
if you eliminate all opposition. In the former case, your score
is based predominantly on the gold and precious gems you've plundered.
In the latter case, your score is most influenced by the number
of turns you spent overcoming your foes (the quicker the victory, the better;
the slower the demise, the better). Bonus points, affected by the number
of heroes lost, are awarded only if you win. The score is heavily
modified by the chosen game difficulty, but not by any other challenges
(which are, however, proudly displayed in the high score listing).

When all your heroes fall, you are going to invariably see a new foolhardy
party of adventurers clamoring to be led into the unknown perils. They start
their conquest from a new entrance, with no experience and no equipment,
and new undaunted enemies bar their way. Lead the new hopeful explorers
with wisdom and fortitude!
