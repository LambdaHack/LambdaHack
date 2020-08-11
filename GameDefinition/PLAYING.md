Playing LambdaHack
==================

The following backstory blurb is a copy of the sample game intro screen:

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
to intervene. Then the dust settles and you have as much time
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
* time passes and factions pursue their goals on a few levels
  simultaneously, while other levels are frozen (but all are persistent)
* the same laws of simulated world apply to all factions and all actors,
  whether player-controlled or AI-controlled; e.g., the same field of view
  calculation, skill checks, equipment limitations, rules for item
  and terrain use
* combat mechanics is deterministic; randomness comes only from AI
  decisions and procedurally generated terrain
* there's (almost) no HP regeneration; attrition ensures all past (silly)
  decisions matter; HP of every actor starts at around half max
* each character has 10 uniform equipment slots, which fill quickly given
  that most melee weapons have cooldowns
* each faction has a single shared inventory of unlimited size,
  which has a physical location on the map and so can be ransacked

If the game window is too large for your screen or you experience
other technical issues, please consult
[README.md](https://github.com/LambdaHack/LambdaHack/blob/master/README.md)
or describe your problem on gitter or the issue tracker.
Contributions of all kinds are welcome. Please offer feedback
to mikolaj.konarski@funktory.com or, preferably, on any of the public forums.


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
When the player loses, a defeat message for the scenario appears
with hints about strategies known to work in the given tactical context.
Alternatively, the player may postpone reading these messages and instead
try to puzzle out the tactics himself --- this is not so hard, as there are
not so many moving parts to figure, at least in the short scenarios.

As soon as the player learns to navigate initial levels of the crawl game
and starts employing ranged combat, light sources and other means
of gaining or denying battlefield intel, and dies a lot, it makes sense
to return to the remaining short scenarios. They bring forth many extra
game features and tactics and prevent the player from missing half the fun
by trying to play the crawl just like a normal roguelike with spare heroes.
The extra scenarios continue the plotline from the initial tutorial scenarios
in the form of flashbacks and eventually lead up to the events that start
the main crawl adventure. The training they provide has narrow focus,
drilling a particular skill set, even as exotic as opportunity fire
management, a frantic race to the exit and big asymmetric melee battle.
The challenge in the scenarios may be, accordingly, quite tough,
particularly at the higher difficulty levels.

The main scenario, the long crawl, is the only one that takes
place in a multi-floor setting, requiring lots of time to beat,
with focus on resource management and survival, including food gathering
and cooking, crafting, terrain transformation using tools, spotting
environment clues and guessing and countering opponents' strategies.
The player has a choice of exploring a single level at a time or portions
of many levels along a single staircase. On some levels he may explore
and loot with a single scout eluding most opponents. On others he may be
forced to change the pace and perform a complete exterminatory sweep
involving his whole party. On yet others, his best course of action may be
to defend a key location until the first wave of attackers is broken.
The large game arena calls for strategic thinking, including resource
management, area denial and unavoidable temporary retreats to other levels.
Thus, the crawl game mode is the most replayable one, but even the small
scenarios can be refreshed by striving to beat a high score and then
ramping up the difficulty settings.


Exploring the world
-------------------

The map of any particular scenario consists of one or many
levels and a level consists of a number of tiles with a particular
terrain kind on each. The game world is persistent, i.e., every time
the player visits a level during a single game, its layout is the same
(unless modified by other actors).

Letters and digits on the game screen are likely to represent actors.
On the other hand, terrain is depicted with non-letter and non-digit
characters and with zero `0`. Blocky solid symbols are likely to be
non-passable and/or not translucent terrain. White, cyan and green terrain
is usually inert, red is burning or trapped, blue activable or trapped,
magenta activable or searchable.

Items lying on the ground are represented with non-letter and non-digit
characters, just as terrain, though rarely with blocky symbols.
In case of doubt, one of the aiming commands (`/` and keypad `/`,
with default keybinding) cycles through all visible and remembered
items on the level and another (`*` and keypad `*`, with default keybinding)
through all foes. Also, pointing at a map position with `MMB`
(middle mouse button) displays a short description of its contents.
The same with `RMB` enters aiming mode, in which repeated pointing
at the same position increases detail level of the description,
the same as when pressing `SPACE`. The basic terrain kinds are as follows.

    terrain type                          on-screen symbol

    bush, transparent obstacle            %
    trap, ice obstacle                    ^

    wall (horizontal and vertical)        - and |
    bedrock                               blank
    tree, rock, man-made column           0
    rubble                                &
    stairs, exit up                       <
    stairs, exit down                     >
    closed door                           +

    open door (horizontal and vertical)   | and -
    corridor                              #
    ground                                .
    water, other fluid                    ~

    smoke, fog, open fire                 ;
    workshop, curtain, foliage            :

The four groups above, in turn, block movement but not view,
block both, block neither, block view but not movement.
Additionally, each tile, regardless if open and if translucent,
may be permanently lit with ambient light or not.

Actors are marked with lower and upper case letters and with
characters `@` and `1` through `9` (but never `0`). Player-controlled
heroes are always bright white and at game start they are selected
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
with your pointman using Shift-direction or LMB, switch the pointman
with Tab, repeat.

In open terrain, if you keep consistent distance between teammates,
this resembles the leap frog infantry tactics, in which the immobile
team members cover the movement of the others. For best effects,
try to end each sprint behind a cover or concealment (note that a thin pillar
is neither, but a single shadowed position may be enough to hide in the dark).

Pointman hero's attributes are displayed at the bottom-most status line which,
in its most complex form, looks as follows.

    *@12   2m/s Calm: 20/60 HP: 33/50 Pointman: Haskell Alvin   6d1+5% 4d1

The line starts with the list of party members, with the current pointman
highlighted in yellow. Most commands involve only the pointman, including
movement with keyboard or keypad or `LMB` (left mouse button).
If more heroes are selected (highlighted in blue), they run together
whenever `:` or `S-LMB` (LMB while holding down Shift) over map area
is pressed, though that's usually not a precise enough method
of controlling a team. Any sleeping hero is highlighted in green
and can be woken up by yelling with `%`, which also taunts
or stresses up nearby enemies.

Next on the bottom-most status line is the pointman's current and maximum
Calm (morale, composure, focus, attentiveness), then his current
and maximum HP (hit points, health). The colon after "Calm" turning
into a dot signifies that the pointman is in a position without ambient
illumination, making stealthy conduct easier. A brace sign instead
of a colon after "HP" means the pointman is braced for combat
(see section [Moving and acting](#Moving-and-acting)).

In the second half of the bottom-most status line, the pointman's name
is shown. Then come damage dice of the pointman's melee weapons and pointman's
appendages, ordered by their power. The dice of the first recharged weapon,
the one that is going to be used now, is adorned with percentage
damage bonus collected from the whole equipment of the pointman.
If the dice are displayed with upper-case `D` instead of lower-case `d`,
the weapon has additional effects apart of the usual kinetic damage
and any additional direct damage, such as wounding or burning.
The nature of the effects can be appraised via the equipment outfit menu.
The wounding and burning effects always just add their nominal value
to total damage, bypassing any melee skill and armor modifiers.

Weapon damage and other item properties are displayed using
the dice notation `xdy`, which denotes `x` rolls of `y`-sided dice.
A variant written `xdLy` is additionally scaled by the level depth
in proportion to the maximal level depth (at the first level it's
always one, then it grows up to full rolled value at the last level).
Section [Battling monsters](#Battling-monsters) below describes combat
resolution in detail, including the role of the percentage bonuses.

The second, the upper status line describes the current level in relation
to the party.

    5  Lofty hall    [33% seen] dire basilisk    [__**]

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
Then the common focus of the whole party, know as the aiming crosshair,
marked on the map with a red box and manipulated with mouse
or movement keys in aiming mode. In this example, the crosshair points
at a dire basilisk monster with its hit points drawn as a half-full bar.

Instead of a monster, the aiming crosshair status area may describe
a position on the map, a recently spotted item on the floor or an item
in inventory selected for further action or, if none are available,
a summary of the team composition. For example, this form

    5  Lofty hall    [33% seen] exact spot (71,12)    p15 l10

indicates that the party is aiming at an exact spot on the map.
At the end of this example status line comes the length of the shortest
path from the pointman's position to the spot in crosshair and
the straight-line distance between the two points, one that a flung
projectile would travel if there were no obstacles.


Moving and acting
-----------------

This section is a copy of the few initial bits of in-game help. The in-game
help pages are automatically generated based on a game's keybinding content
definitions and on overrides in the player's config file. The remaining
in-game help screens, not shown here, list all game commands grouped
by categories in detail. A text snapshot of the complete in-game help is in
[InGameHelp.txt](InGameHelp.txt).

Walk throughout a level with mouse or numeric keypad (right diagram below)
or the Vi editor keys (middle) or the left-hand movement keys (left). Run until
disturbed with Shift or Control. Go-to a position with LMB (left mouse button).

          q w e          y k u          7 8 9
           \|/            \|/            \|/
          a-s-d          h-.-l          4-5-6
           /|\            /|\            /|\
          z x c          b j n          1 2 3

In aiming mode, the same keys (and mouse) move the aiming crosshair.
Press `KP_5` (`5` on keypad) to wait, bracing for impact, which reduces any
damage taken and prevents displacement by foes. Press `S-KP_5` or `C-KP_5`
(the same key with Shift or Control) to lurk 0.1 of a turn, without bracing.

Displace enemies by running into them with Shift/Control or S-LMB. Search,
open, descend and attack by bumping into walls, doors, stairs and enemies.
The best, not on cooldown, melee weapon is automatically chosen from your
equipment and from among your body parts.

The following few commands, joined with the movement and running keys,
let you accomplish almost anything in the game, though not necessarily
with the fewest keystrokes. You can also play the game exclusively
with a mouse, or both mouse and keyboard (e.g., mouse for go-to
and terrain inspection and keyboard for everything else). Lastly,
you can select a command with arrows or mouse directly from the help
screen or the dashboard and execute it on the spot.

    keys         command
    I            manage the shared inventory stash
    g or ,       grab item(s)
    ESC          clear messages/open main menu/finish aiming
    RET or INS   open dashboard/accept target
    SPACE        clear messages/show history/cycle detail level
    TAB          cycle among all party members
    *            cycle crosshair among enemies
    /            cycle crosshair among items
    M            modify any admissible terrain
    %            yell or yawn and stop sleeping

Screen area and UI mode (exploration/aiming) determine mouse click
effects. Here we give an overview of effects of each button over
the game map area. The list includes not only left and right buttons,
but also the optional middle mouse button (MMB) and the mouse wheel,
which is also used over menus, to page-scroll them. For mice without RMB,
one can use Control key with LMB and for mice without MMB, one can use
C-RMB or C-S-LMB.

    keys         command (exploration/aiming)
    LMB          go to pointer for 25 steps/fling at enemy
    S-LMB        run to pointer collectively for 25 steps/fling at enemy
    RMB or C-LMB start aiming at enemy under pointer/cycle detail level
    S-RMB        modify terrain at pointer
    MMB or C-RMB snap crosshair to floor under pointer/cycle detail level
    WHEEL-UP     swerve the aiming line
    WHEEL-DN     unswerve the aiming line

Note that mouse is totally optional. Keyboard suffices, occasionally
requiring a key lookup for some obscure command in the help screens.


Battling monsters
-----------------

The life of the heroes is full of dangers. Monstrosities, natural
and out of this world, roam the dark corridors and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and crowd and stampede all at once, sometimes brutally colliding by accident.

Monsters are depicted on the map with letters. Upper case letters
are unique monsters, often guardians of special floors, resources
and keys to other areas. Lower case letters are the rabble.
If there are humans not from our team, they are marked
with `@` and `1` through `9` in other colours than white.

When a hero walks and bumps into a monster or a monster attacks
the hero, melee combat occurs. Hero *running* into and displacing
a monster (with the `Shift` key and, in case of keypad movement,
alternatively a `Control` key), does not involve inflicting a damage,
but only causes an exchange of places. This gives the opponent
a free blow, but can improve the tactical situation or aid escape.
In some circumstances actors are immune to the displacing,
e.g., when both parties form a continuous front-line.

In melee combat, the best recharged equipped weapon (including fighting
organs that are not on cooldown) of each opponent is taken into account
for determining the damage and any extra effects of the blow.
To determine the damage dealt, the outcome of the weapon's damage
dice roll (but not rolls for any additional direct damage effects,
such as wounding or burning, which nominal values are added at the end)
is multiplied by a percentage bonus. The bonus is calculated
by taking the damage bonus (summed from the equipped items and organs
and conditions of the attacker, capped at 200%) minus the melee
armor modifier of the defender (capped at 200%, as well). However,
at least 5% of damage always gets through, even if the bonus is nominally
below -95%, so excessively strong armor acts only as a buffer against
high melee skill of opponents.

The current pointman's melee bonus, armor modifier and other detailed
stats can be viewed in the skills menu, accessible via the `#` command,
which summarizes all the stats conferred by organs and conditions listed
in the organ menu, invoked by `@`.

In ranged combat, the projectile is assumed to be attacking the defender
in melee, using itself as the weapon, with the usual dice and damage bonus.
This time, the *ranged* armor skill of the defender is taken into account
and, additionally, the speed of the missile (based on shape and weight)
figures in the calculation. You may propel any item from your inventory
(by default you are offered only the appropriate items; press `+` to open
all choices). Only items of a few kinds inflict any damage, but some
have other effects, beneficial, detrimental or mixed.

In-game detailed item descriptions contain melee and ranged damage estimates.
They do not take into account damage from effects and, if bonuses are not
known, guesses are based on averages for the item kind in question.
The displayed figures are rounded, but the game internally keeps track
of minute fractions of HP for all actors in all calculations.

The combat stress drains Calm, gradually limiting viewing radius and,
if Calm reaches zero and the actor is sufficiently impressed by his foes,
making him defect and surrender unto their domination. Whenever the monster's
or hero's hit points reach zero, the combatant falls down and quickly
gets permanently incapacitated. When the last hero is disabled or dominated,
the scenario ends in defeat.


Attacking from a distance
-------------------------

For ranged attacks, setting the aiming crosshair beforehand is not mandatory,
because crosshair is set automatically as soon as a monster comes into view
and can still be adjusted for as long as the missile to fling is not chosen.
However, sometimes you want to examine the level map tile by tile
or assign persistent personal targets to party members.
The latter is essential in the rare cases when your henchmen
(non-pointman characters) can move autonomously or fire opportunistically
(via innate skills or rare equipment). Also, if your non-pointman character
is adjacent to more than one enemy, setting his target makes him melee
a particular foe.

You can enter the aiming mode with the `*` and keypad `*` keys that
select enemies or the `/` and keypad `/` keys that cycle among items
on the floor. You can move crosshair with direction keys and assign
a personal target to the pointman with a `RET` key (Return, Enter).
The details of the shared crosshair mark are displayed in a status
line close to the bottom of the screen, as explained in section
[Leading your heroes](#Leading-your-heroes) above. You cycle aiming
mode from foe to spot to vector with the ``\`` key, which is useful
when a monster vanishes but you still want to fling at its last
known position.


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
party of adventurers clamoring to be led into the unknown perils.
They start their conquest afresh, with no experience, no supplies
for survival and no equipment, and new undaunted enemies bar their way.
Lead the new hopeful explorers with wisdom and fortitude!


FAQ
---

- Q: Why does the percentage of explored tiles turns from 100% to 99%?

A: Apparently enemies transformed a tile from unexplorable terrain kind
to explorable. The new tile has never been seen by the player,
so the percentage is no longer at 100%.

- Q: Why when a single hero gets ambushed and is fighting at close quarters,
his distant teammates don't jointly come to his rescue.

A: The teammates wait for him to come back into the formation instead
so that they may assume a frontline and then melee their foes together.
The immobile heroes are assumed to be pinned to their positions
by fear and shock, but also by their imperative to hold formation,
so as to defend an important position or avoid running piecemeal
into a trap or into friendly fire or avoid breaking concealment
and revealing their position or leaving a vantage point from which
they can observe and relay enemy movement. For untrained teams,
optimal simultaneous synchronized squad movement is not feasible.
It would be, if all squaddies had cameras, with a few drones overhead
for best effect, and if a team of off-site coordinators would analyze
the situation and micromanage them all. This is not the case here.

- Q: Why is the noise I'm hearing "indistinct"?

A: That's because it's out of direct hearing range of each teammate
on the level, but ponderous enough to be perceived by all as vibrations
and echos from afar. Any other noise adjective tells that the noise
is heard by at least one of teammates and how far it is from the pointman
(who may or may not hear it directly, as indicated by Calm drop).

- Q: Why are there two 'weakened' conditions in the organ menu of my hero?

A: Each team has a different recipe for their weakness brew.
Consequently, multiple affliction by the concoction from a single team
prolongs a single malady, but affliction by concotions from many teams
causes concurrent ailments, with compounded effects, but independent
and short durations. The benefit of the mechanism is that
it's possible to tell the perpetrator team of the ailment.
The exceptions are the conditions that activate each turn, e.g., healing
(regeneration, various resistances that effectively cure each turn)
or wounding (poison). These are similar regardless of the team and so
the condition is always only prolonged.

- Q: Why is a harpoon in my shared inventory stash charging for hundreds
of turns?

A: This is an artifact of time running independently on each level.
Any ideas on how to improve this are welcome. A workaround is to drop and then
pick up the item on the level you want to use it. When picked up,
it gets recharged after, randomly, from one to two times the normal
cooldown period of the item and then recharges normally for as long
as it's used only on this level.
