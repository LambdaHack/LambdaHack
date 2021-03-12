-- | Temporary pseudo-organ (condition) definitions.
module Content.ItemKindTemporary
  ( -- * Group name patterns
    pattern S_IMMOBILE, pattern S_PACIFIED, pattern S_IRREPLACEABLE, pattern S_RETAINING, pattern S_IMPATIENT, pattern S_DISPOSSESSED, pattern S_WITHHOLDING, pattern S_PARSIMONIOUS
  , pattern S_MORE_MOBILE, pattern S_MORE_COMBATIVE, pattern S_MORE_DISPLACING, pattern S_MORE_MODIFYING, pattern S_MORE_PATIENT, pattern S_MORE_TIDY, pattern S_MORE_PROJECTING, pattern S_MORE_PRACTICAL
  , pattern S_STRENGTHENED, pattern S_WEAKENED, pattern S_PROTECTED_FROM_MELEE, pattern S_PROTECTED_FROM_RANGED, pattern S_DEFENSELESS, pattern S_RESOLUTE, pattern S_HASTED, pattern S_SLOWED, pattern S_FAR_SIGHTED, pattern S_BLIND, pattern S_KEEN_SMELLING, pattern S_FOUL_SMELLING, pattern S_ROSE_SMELLING, pattern S_RANGED_DEFLECTING, pattern S_MELEE_DEFLECTING, pattern S_SHINY_EYED, pattern S_DEAFENED, pattern S_DEAF, pattern S_DRUNK, pattern S_FRENZIED, pattern S_REGENERATING, pattern S_POISONED, pattern S_SLOW_RESISTANT, pattern S_POISON_RESISTANT
  , temporariesGNSingleton, noStatGN, bonusStatGN
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

noStatGN :: [GroupName ItemKind]
noStatGN =
       [S_IMMOBILE, S_PACIFIED, S_IRREPLACEABLE, S_RETAINING, S_IMPATIENT, S_DISPOSSESSED, S_WITHHOLDING, S_PARSIMONIOUS]

bonusStatGN :: [GroupName ItemKind]
bonusStatGN =
       [S_MORE_MOBILE, S_MORE_COMBATIVE, S_MORE_DISPLACING, S_MORE_MODIFYING, S_MORE_PATIENT, S_MORE_TIDY, S_MORE_PROJECTING, S_MORE_PRACTICAL]

temporariesGNSingleton :: [GroupName ItemKind]
temporariesGNSingleton =
       [S_STRENGTHENED, S_WEAKENED, S_PROTECTED_FROM_MELEE, S_PROTECTED_FROM_RANGED, S_DEFENSELESS, S_RESOLUTE, S_HASTED, S_SLOWED, S_FAR_SIGHTED, S_BLIND, S_KEEN_SMELLING, S_FOUL_SMELLING, S_ROSE_SMELLING, S_RANGED_DEFLECTING, S_MELEE_DEFLECTING, S_SHINY_EYED, S_DEAFENED, S_DEAF, S_DRUNK, S_FRENZIED, S_REGENERATING, S_POISONED, S_SLOW_RESISTANT, S_POISON_RESISTANT]
    ++ noStatGN ++ bonusStatGN

pattern S_IMMOBILE, S_PACIFIED, S_IRREPLACEABLE, S_RETAINING, S_IMPATIENT, S_DISPOSSESSED, S_WITHHOLDING, S_PARSIMONIOUS :: GroupName ItemKind

pattern S_MORE_MOBILE, S_MORE_COMBATIVE, S_MORE_DISPLACING, S_MORE_MODIFYING, S_MORE_PATIENT, S_MORE_TIDY, S_MORE_PROJECTING, S_MORE_PRACTICAL :: GroupName ItemKind

pattern S_STRENGTHENED, S_WEAKENED, S_PROTECTED_FROM_MELEE, S_PROTECTED_FROM_RANGED, S_DEFENSELESS, S_RESOLUTE, S_HASTED, S_SLOWED, S_FAR_SIGHTED, S_BLIND, S_KEEN_SMELLING, S_FOUL_SMELLING, S_ROSE_SMELLING, S_RANGED_DEFLECTING, S_MELEE_DEFLECTING, S_SHINY_EYED, S_DEAFENED, S_DEAF, S_DRUNK, S_FRENZIED, S_REGENERATING, S_POISONED, S_SLOW_RESISTANT, S_POISON_RESISTANT :: GroupName ItemKind

pattern S_STRENGTHENED = GroupName "strengthened"
pattern S_WEAKENED = GroupName "weakened"
pattern S_PROTECTED_FROM_MELEE = GroupName "protected from melee"
pattern S_PROTECTED_FROM_RANGED = GroupName "protected from ranged"
pattern S_DEFENSELESS = GroupName "defenseless"
pattern S_RESOLUTE = GroupName "resolute"
pattern S_HASTED = GroupName "hasted"
pattern S_SLOWED = GroupName "slowed"
pattern S_FAR_SIGHTED = GroupName "far-sighted"
pattern S_BLIND = GroupName "blind"
pattern S_KEEN_SMELLING = GroupName "keen-smelling"
pattern S_FOUL_SMELLING = GroupName "foul-smelling"
pattern S_ROSE_SMELLING = GroupName "rose-smelling"
pattern S_RANGED_DEFLECTING = GroupName "ranged-deflecting"
pattern S_MELEE_DEFLECTING = GroupName "melee-deflecting"
pattern S_SHINY_EYED = GroupName "shiny-eyed"
pattern S_DEAFENED = GroupName "deafened"
pattern S_DEAF = GroupName "deaf"
pattern S_DRUNK = GroupName "drunk"
pattern S_FRENZIED = GroupName "frenzied"
pattern S_REGENERATING = GroupName "regenerating"
pattern S_POISONED = GroupName "poisoned"
pattern S_SLOW_RESISTANT = GroupName "slow resistant"
pattern S_POISON_RESISTANT = GroupName "poison resistant"
pattern S_IMMOBILE = GroupName "immobile"
pattern S_PACIFIED = GroupName "pacified"
pattern S_IRREPLACEABLE = GroupName "irreplaceable"
pattern S_RETAINING = GroupName "retaining"
pattern S_IMPATIENT = GroupName "impatient"
pattern S_DISPOSSESSED = GroupName "dispossessed"
pattern S_WITHHOLDING = GroupName "withholding"
pattern S_PARSIMONIOUS = GroupName "parsimonious"
pattern S_MORE_MOBILE = GroupName "super-mobile"
pattern S_MORE_COMBATIVE = GroupName "super-combative"
pattern S_MORE_DISPLACING = GroupName "super-displacing"
pattern S_MORE_MODIFYING = GroupName "super-modifying"
pattern S_MORE_PATIENT = GroupName "super-patient"
pattern S_MORE_TIDY = GroupName "super-tidy"
pattern S_MORE_PROJECTING = GroupName "super-projecting"
pattern S_MORE_PRACTICAL = GroupName "super-practical"

-- * Content

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpDefenseless, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpRangedDeflecting, tmpMeleeDeflecting, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpDefenseless, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpRangedDeflecting, tmpMeleeDeflecting, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply :: ItemKind

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
-- The messages are needed also under @OnSmash@ to display when item removed
-- via @DropItem@ and not via natural periodic activation.
tmpAspects :: GroupName ItemKind -> [Aspect] -> ItemKind
tmpAspects grp aspects =
  let name = fromGroupName grp  -- @iname@ must match @ifreq@, see @myBadGrps@
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
    , ieffects = [ OnSmash $ verbMsgLess name
                   -- announce partial neutralization, but don't spam
                   -- about normal periodic wear each turn
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

tmpStrengthened = tmpAspects S_STRENGTHENED [AddSkill SkHurtMelee 20]
tmpWeakened = tmpAspects S_WEAKENED
                         [AddSkill SkHurtMelee (-30)]  -- don't cancel out ^
tmpProtectedMelee = tmpAspects S_PROTECTED_FROM_MELEE
                               [AddSkill SkArmorMelee 50]
tmpProtectedRanged = tmpAspects S_PROTECTED_FROM_RANGED
                                [AddSkill SkArmorRanged 25]
tmpDefenseless = tmpAspects S_DEFENSELESS [ AddSkill SkArmorMelee (-50)
                                          , AddSkill SkArmorRanged (-25) ]
tmpResolute = tmpAspects S_RESOLUTE [AddSkill SkMaxCalm 60]
tmpFast20 = tmpAspects S_HASTED [AddSkill SkSpeed 20]
tmpSlow10 = tmpAspects S_SLOWED [AddSkill SkSpeed (-10)]
tmpFarSighted = tmpAspects S_FAR_SIGHTED [AddSkill SkSight 5]
tmpBlind = tmpAspects S_BLIND [ AddSkill SkSight (-99)
                              , AddSkill SkArmorMelee (-30) ]
tmpKeenSmelling = tmpAspects S_KEEN_SMELLING [AddSkill SkSmell 2]
tmpFoulSmelling = tmpAspects S_FOUL_SMELLING [AddSkill SkOdor 2]
tmpRoseSmelling = tmpAspects S_ROSE_SMELLING [AddSkill SkOdor (-4)]
tmpRangedDeflecting =
  tmpAspects S_RANGED_DEFLECTING [AddSkill SkDeflectRanged 1]
tmpMeleeDeflecting =
  tmpAspects S_MELEE_DEFLECTING [AddSkill SkDeflectMelee 1]
tmpNoctovision = tmpAspects S_SHINY_EYED [AddSkill SkNocto 2]
tmpDeafened = tmpAspects S_DEAFENED [AddSkill SkHearing (-6)]
tmpDeaf = tmpAspects S_DEAF [ AddSkill SkHearing (-99)
                            , AddSkill SkArmorMelee (-30) ]
tmpDrunk = tmpAspects S_DRUNK [ AddSkill SkHurtMelee 30  -- fury
                              , AddSkill SkArmorRanged (-30)
                              , AddSkill SkSight (-8) ]

tmpBonusSkAggresion =
  tmpAspects S_FRENZIED [ AddSkill SkAggression 5
                        , AddSkill SkArmorMelee (-30) ]

tmpRegenerating =
  tmpEffects S_REGENERATING (4 + 1 `d` 2) [RefillHP 1]
tmpPoisoned =
  tmpEffects S_POISONED (3 + 1 `d` 2) [RefillHP (-1)]
tmpSlow10Resistant =
  tmpEffects S_SLOW_RESISTANT (8 + 1 `d` 4)
             [DropItem 1 1 COrgan S_SLOWED]
tmpPoisonResistant =
  tmpEffects S_POISON_RESISTANT (8 + 1 `d` 4)
             [DropItem 1 maxBound COrgan S_POISONED]

tmpNoSkMove =
  tmpAspects S_IMMOBILE [AddSkill SkMove (-99)]
tmpNoSkMelee =
  tmpAspects S_PACIFIED [AddSkill SkMelee (-99)]
tmpNoSkDisplace =
  tmpAspects S_IRREPLACEABLE [AddSkill SkDisplace (-99)]
tmpNoSkAlter =
  tmpAspects S_RETAINING [AddSkill SkAlter (-99)]
tmpNoSkWait =
  tmpAspects S_IMPATIENT [AddSkill SkWait (-99)]
tmpNoSkMoveItem =
  tmpAspects S_DISPOSSESSED [AddSkill SkMoveItem (-99)]
tmpNoSkProject =
  tmpAspects S_WITHHOLDING [AddSkill SkProject (-99)]
tmpNoSkApply =
  tmpAspects S_PARSIMONIOUS [AddSkill SkApply (-99)]

tmpBonusSkMove =
  tmpAspects S_MORE_MOBILE [AddSkill SkMove 5]
tmpBonusSkMelee =
  tmpAspects S_MORE_COMBATIVE [AddSkill SkMelee 5]
tmpBonusSkDisplace =
  tmpAspects S_MORE_DISPLACING [AddSkill SkDisplace 5]
tmpBonusSkAlter =
  tmpAspects S_MORE_MODIFYING [AddSkill SkAlter 5]
tmpBonusSkWait =
  tmpAspects S_MORE_PATIENT [AddSkill SkWait 5]
tmpBonusSkMoveItem =
  tmpAspects S_MORE_TIDY [AddSkill SkMoveItem 5]
tmpBonusSkProject =
  tmpAspects S_MORE_PROJECTING [AddSkill SkProject 8]
    -- TODO: 11, but let player control potion throwing by non-pointmen;
    -- beware also of capReinforced and other sources of the skill
tmpBonusSkApply =
  tmpAspects S_MORE_PRACTICAL [AddSkill SkApply 5]
