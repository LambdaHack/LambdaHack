-- | Temporary pseudo-organ (condition) definitions.
module Content.ItemKindTemporary
  ( -- * Group name patterns
    pattern STRENGTHENED, pattern WEAKENED, pattern PROTECTED_FROM_MELEE, pattern PROTECTED_FROM_RANGED, pattern DEFENSELESS, pattern RESOLUTE, pattern HASTED, pattern SLOWED, pattern FAR_SIGHTED, pattern BLIND, pattern KEEN_SMELLING, pattern FOUL_SMELLING, pattern ROSE_SMELLING, pattern SHINY_EYED, pattern DEAFENED, pattern DEAF, pattern DRUNK, pattern FRENZIED, pattern IMMOBILE, pattern PACIFIED, pattern IRREPLACEABLE, pattern RETAINING, pattern IMPATIENT, pattern DISPOSSESSED, pattern WITHHOLDING, pattern PARSIMONIOUS, pattern MORE_MOBILE, pattern MORE_COMBATIVE, pattern MORE_DISPLACING, pattern MORE_MODIFYING, pattern MORE_PATIENT, pattern MORE_TIDY, pattern MORE_PROJECTING, pattern MORE_PRACTICAL, pattern REGENERATING, pattern POISONED, pattern SLOW_RESISTANT, pattern POISON_RESISTANT
  , -- * Content
    temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

pattern STRENGTHENED, WEAKENED, PROTECTED_FROM_MELEE, PROTECTED_FROM_RANGED, DEFENSELESS, RESOLUTE, HASTED, SLOWED, FAR_SIGHTED, BLIND, KEEN_SMELLING, FOUL_SMELLING, ROSE_SMELLING, SHINY_EYED, DEAFENED, DEAF, DRUNK, FRENZIED, IMMOBILE, PACIFIED, IRREPLACEABLE, RETAINING, IMPATIENT, DISPOSSESSED, WITHHOLDING, PARSIMONIOUS, MORE_MOBILE, MORE_COMBATIVE, MORE_DISPLACING, MORE_MODIFYING, MORE_PATIENT, MORE_TIDY, MORE_PROJECTING, MORE_PRACTICAL, REGENERATING, POISONED, SLOW_RESISTANT, POISON_RESISTANT :: GroupName ItemKind

pattern STRENGTHENED = "strengthened"
pattern WEAKENED = "weakened"
pattern PROTECTED_FROM_MELEE = "protected from melee"
pattern PROTECTED_FROM_RANGED = "protected from ranged"
pattern DEFENSELESS = "defenseless"
pattern RESOLUTE = "resolute"
pattern HASTED = "hasted"
pattern SLOWED = "slowed"
pattern FAR_SIGHTED = "far-sighted"
pattern BLIND = "blind"
pattern KEEN_SMELLING = "keen-smelling"
pattern FOUL_SMELLING = "foul-smelling"
pattern ROSE_SMELLING = "rose-smelling"
pattern SHINY_EYED = "shiny-eyed"
pattern DEAFENED = "deafened"
pattern DEAF = "deaf"
pattern DRUNK = "drunk"
pattern FRENZIED = "frenzied"
pattern IMMOBILE = "immobile"
pattern PACIFIED = "pacified"
pattern IRREPLACEABLE = "irreplaceable"
pattern RETAINING = "retaining"
pattern IMPATIENT = "impatient"
pattern DISPOSSESSED = "dispossessed"
pattern WITHHOLDING = "withholding"
pattern PARSIMONIOUS = "parsimonious"
pattern MORE_MOBILE = "more mobile"
pattern MORE_COMBATIVE = "more combative"
pattern MORE_DISPLACING = "more displacing"
pattern MORE_MODIFYING = "more modifying"
pattern MORE_PATIENT = "more patient"
pattern MORE_TIDY = "more tidy"
pattern MORE_PROJECTING = "more projecting"
pattern MORE_PRACTICAL = "more practical"
pattern REGENERATING = "regenerating"
pattern POISONED = "poisoned"
pattern SLOW_RESISTANT = "slow resistant"
pattern POISON_RESISTANT = "poison resistant"

-- * Content

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant :: ItemKind

-- * Condition definitions

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
-- The messages are needed also under @OnSmash@ to display when item removed
-- via @DropItem@ and not via natural periodic activation.
tmpAspects :: GroupName ItemKind -> [Aspect] -> ItemKind
tmpAspects grp aspects =
  let name = fromGroupName grp
  in ItemKind
    { isymbol  = '+'
    , iname    = name
    , ifreq    = [(grp, 1), (CONDITION, 1)]
    , iflavour = zipPlain [BrWhite]
    , icount   = 1
    , irarity  = [(1, 1)]
    , iverbHit = "affect"
    , iweight  = 0
    , idamage  = 0
    , iaspects = -- timeout is 0; activates and vanishes soon,
                 -- depending on initial timer setting
                 aspects
                 ++ [SetFlag Periodic, SetFlag Fragile, SetFlag Condition]
    , ieffects = [ OnSmash $ verbMsgLess name  -- announce partial neutralization
                 -- not spamming for normal periodic wear each turn
                 , OnSmash $ verbMsgNoLonger name  -- for forced neutralization
                 , verbMsgNoLonger name ]  -- for periodic wear of last copy
    , idesc    = ""  -- no description needed; powers are enough
    , ikit     = []
    }

tmpEffects :: GroupName ItemKind -> Dice -> [Effect] -> ItemKind
tmpEffects grp icount effects =
  let tmp = tmpAspects grp []
  in tmp { icount
         , ieffects = effects ++ ieffects tmp
         }

tmpStrengthened = tmpAspects STRENGTHENED [AddSkill SkHurtMelee 20]
tmpWeakened = tmpAspects WEAKENED
                         [AddSkill SkHurtMelee (-30)]  -- don't cancel out ^
tmpProtectedMelee = tmpAspects PROTECTED_FROM_MELEE
                               [AddSkill SkArmorMelee 50]
tmpProtectedRanged = tmpAspects PROTECTED_FROM_RANGED
                                [AddSkill SkArmorRanged 25]
tmpVulnerable = tmpAspects DEFENSELESS [ AddSkill SkArmorMelee (-50)
                                         , AddSkill SkArmorRanged (-25) ]
tmpResolute = tmpAspects RESOLUTE [AddSkill SkMaxCalm 60]
tmpFast20 = tmpAspects HASTED [AddSkill SkSpeed 20]
tmpSlow10 = tmpAspects SLOWED [AddSkill SkSpeed (-10)]
tmpFarSighted = tmpAspects FAR_SIGHTED [AddSkill SkSight 5]
tmpBlind = tmpAspects BLIND [AddSkill SkSight (-99)]
tmpKeenSmelling = tmpAspects KEEN_SMELLING [AddSkill SkSmell 2]
tmpFoulSmelling = tmpAspects FOUL_SMELLING [AddSkill SkOdor 2]
tmpRoseSmelling = tmpAspects ROSE_SMELLING [AddSkill SkOdor (-4)]
tmpNoctovision = tmpAspects SHINY_EYED [AddSkill SkNocto 2]
tmpDeafened = tmpAspects DEAFENED [AddSkill SkHearing (-10)]
tmpDeaf = tmpAspects DEAF [AddSkill SkHearing (-99)]
tmpDrunk = tmpAspects DRUNK [ AddSkill SkHurtMelee 30  -- fury
                              , AddSkill SkArmorMelee (-20)
                              , AddSkill SkArmorRanged (-20)
                              , AddSkill SkSight (-8)
                              ]
tmpBonusSkAggresion =
  tmpAspects FRENZIED [AddSkill SkAggression 5]

tmpNoSkMove =
  tmpAspects IMMOBILE [AddSkill SkMove (-99)]
tmpNoSkMelee =
  tmpAspects PACIFIED [AddSkill SkMelee (-99)]
tmpNoSkDisplace =
  tmpAspects IRREPLACEABLE [AddSkill SkDisplace (-99)]
tmpNoSkAlter =
  tmpAspects RETAINING [AddSkill SkAlter (-99)]
tmpNoSkWait =
  tmpAspects IMPATIENT [AddSkill SkWait (-99)]
tmpNoSkMoveItem =
  tmpAspects DISPOSSESSED [AddSkill SkMoveItem (-99)]
tmpNoSkProject =
  tmpAspects WITHHOLDING [AddSkill SkProject (-99)]
tmpNoSkApply =
  tmpAspects PARSIMONIOUS [AddSkill SkApply (-99)]

tmpBonusSkMove =
  tmpAspects MORE_MOBILE [AddSkill SkMove 5]
tmpBonusSkMelee =
  tmpAspects MORE_COMBATIVE [AddSkill SkMelee 5]
tmpBonusSkDisplace =
  tmpAspects MORE_DISPLACING [AddSkill SkDisplace 5]
tmpBonusSkAlter =
  tmpAspects MORE_MODIFYING [AddSkill SkAlter 5]
tmpBonusSkWait =
  tmpAspects MORE_PATIENT [AddSkill SkWait 5]
tmpBonusSkMoveItem =
  tmpAspects MORE_TIDY [AddSkill SkMoveItem 5]
tmpBonusSkProject =
  tmpAspects MORE_PROJECTING [AddSkill SkProject 8]
    -- TODO: 11, but let player control potion throwing by non-pointmen;
    -- beware also of capReinforced and other sources of the skill
tmpBonusSkApply =
  tmpAspects MORE_PRACTICAL [AddSkill SkApply 5]

tmpRegenerating =
  tmpEffects REGENERATING (4 + 1 `d` 2) [RefillHP 1]
tmpPoisoned =
  tmpEffects POISONED (4 + 1 `d` 2) [RefillHP (-1)]
tmpSlow10Resistant =
  tmpEffects POISON_RESISTANT (8 + 1 `d` 4)
             [DropItem 1 1 COrgan SLOWED]
tmpPoisonResistant =
  tmpEffects SLOW_RESISTANT (8 + 1 `d` 4)
             [DropItem 1 maxBound COrgan POISONED]
