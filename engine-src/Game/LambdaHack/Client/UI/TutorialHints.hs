module Game.LambdaHack.Client.UI.TutorialHints
  (TutorialHints (..)
  , renderTutorialHints
  ) where

import Data.Text (Text)
import Prelude

data TutorialHints =
    DamageOfDifferentKind
    | NewFloorNewOpportunity
    | CannotHarmYouInMelee
    | CaughtProjectile
    | HitsWithNoDirectDamage
    | TemporaryConditions
    | WokenUpActors
    | AvoidWalkingEnemies
    | AlotOfDamageFromOneSource
    | TerrainNotFullyKnown
    | OutOfSightEvents
    | HearingRadius
    | SwitchTeammat
    | MeleeEnemies
    | UseTerrainEffect
    | SwitchPointmanAndAvoidMeleeAlone
    | SwitchPointmanAndSoftenFoes
    deriving(Enum, Eq, Show)

-- | Generate the standard textual representation for the tutoiral hints
--
renderTutorialHints :: TutorialHints -> Text
renderTutorialHints = \case
  DamageOfDifferentKind ->
    "You took damage of a different kind than the normal piercing hit, which means your armor couldn't block any part of it. Normally, your HP (hit points, health) do not regenerate, so losing them is a big deal. Apply healing concoctions or take a long sleep to replenish your HP (but in this hectic environment not even uninterrupted resting that leads to sleep is easy)."
  NewFloorNewOpportunity ->
    "New floor is new opportunities, though the old level is still there and others may roam it after you left. Viewing all floors, without moving between them, can be done using the '<' and '>' keys."
  CannotHarmYouInMelee ->
    "This enemy can't harm you in melee. Left alone could it possibly be of some use?"
  CaughtProjectile ->
    "You managed to catch a projectile, thanks to being braced and hitting it exactly when it was at arm's reach. The obtained item has been put into the shared stash of your party."
  HitsWithNoDirectDamage ->
    "Some hits don't cause piercing, impact, burning nor any other direct damage. However, they can have other effects, bad, good or both."
  WokenUpActors ->
    "Woken up actors regain stats and skills, including sight radius and melee armor, over several turns."
  AvoidWalkingEnemies ->
    "To avoid waking enemies up, make sure they don't lose HP nor too much Calm through noises, particularly close ones. Beware, however, that they slowly regenerate HP as they sleep and eventually wake up at full HP."
  AlotOfDamageFromOneSource ->
    "You took a lot of damage from one source. If the danger persists, consider retreating towards your teammates or buffing up or an instant escape, if consumables permit."
  TemporaryConditions ->
     "Temporary conditions, especially the bad ones, pass quickly, usually after just a few turns. While active, they are listed in the '@' organ menu and the effects of most of them are seen in the '#' skill menu."
  TerrainNotFullyKnown ->
    "Solid terrain drawn in pink is not fully known until searched. This is usually done by bumping into it, which also triggers effects and transformations the terrain is capable of. Once revealed, the terrain can be inspected in aiming mode started with the '*' key or with mouse."
  OutOfSightEvents ->
    "Events out of your sight radius (as listed in the '#' skill menu) can sometimes be heard, depending on your hearing radius skill. Some, such as death shrieks, can always be heard regardless of skill and distance, including when they come from a different floor."
  HearingRadius ->
   "Enemies you can't see are sometimes heard yelling and emitting other noises. Whether you can hear them, depends on their distance and your hearing radius, as listed in the '#' skill menu."
  SwitchTeammat ->
   "You survive this mission, or die trying, as a team. After a few moves, feel free to switch the controlled teammate (marked on the map with the yellow box) using the Tab key to another party member (marked with a green box)."  -- assuming newbies don't remap their keys
  MeleeEnemies ->
    "Enemies are normally dealt with using melee (by bumping when adjacent) or ranged combat (by 'f'linging items at them)."
  UseTerrainEffect ->
    "Enemies can be dealt with not only via combat, but also with clever use of terrain effects, stealth (not emitting nor reflecting light) or hasty retreat (particularly when foes are asleep or drowsy)."
  SwitchPointmanAndAvoidMeleeAlone ->
    "When dealing with groups of enemies, remember than you fight as a team. Switch the pointman (marked on the map with the yellow box) using the Tab key until you move each teammate to a tactically advantageous position. Avoid meleeing alone."
  SwitchPointmanAndSoftenFoes ->
    "When dealing with groups of armed enemies, remember than you fight as a team. Switch the pointman (marked on the map with the yellow box) using the Tab key until you move each teammate to a tactically advantageous position. Retreat, if necessary to form a front line. Soften the foes with missiles, especially of exploding kind."
