-- | Temporary pseudo-organ (condition) definitions.
module Content.ItemKindTemporary
  ( temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpFoulSmelling, tmpRoseSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpBonusSkAggresion, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant :: ItemKind

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
-- The messages are needed also under @OnSmash@ to display when item removed
-- via @DropItem@ and not via natural periodic activation.
tmpAspects :: Text -> [Aspect] -> ItemKind
tmpAspects name aspects = ItemKind
  { isymbol  = '+'
  , iname    = name
  , ifreq    = [(toGroupName name, 1), ("condition", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 0
  , idamage  = 0
  , iaspects = -- timeout is 0; activates and vanishes soon,
               -- depending on initial timer setting
               aspects ++ [SetFlag Periodic, SetFlag Fragile, SetFlag Condition]
  , ieffects = [ OnSmash $ verbMsgLess name  -- announce partial neutralization
               -- not spamming for normal periodic wear each turn
               , OnSmash $ verbMsgNoLonger name  -- for forced neutralization
               , verbMsgNoLonger name ]  -- for periodic wear of last copy
  , idesc    = ""  -- no description needed; powers are enough
  , ikit     = []
  }

tmpEffects :: Text -> Dice -> [Effect] -> ItemKind
tmpEffects name icount effects =
  let tmp = tmpAspects name []
  in tmp { icount
         , ieffects = effects ++ ieffects tmp
         }

tmpStrengthened = tmpAspects "strengthened" [AddSkill SkHurtMelee 20]
tmpWeakened = tmpAspects "weakened"
                         [AddSkill SkHurtMelee (-30)]  -- don't cancel out ^
tmpProtectedMelee = tmpAspects "protected from melee"
                               [AddSkill SkArmorMelee 50]
tmpProtectedRanged = tmpAspects "protected from ranged"
                                [AddSkill SkArmorRanged 25]
tmpVulnerable = tmpAspects "defenseless" [ AddSkill SkArmorMelee (-50)
                                         , AddSkill SkArmorRanged (-25) ]
tmpResolute = tmpAspects "resolute" [AddSkill SkMaxCalm 60]
tmpFast20 = tmpAspects "hasted" [AddSkill SkSpeed 20]
tmpSlow10 = tmpAspects "slowed" [AddSkill SkSpeed (-10)]
tmpFarSighted = tmpAspects "far-sighted" [AddSkill SkSight 5]
tmpBlind = tmpAspects "blind" [AddSkill SkSight (-99)]
tmpKeenSmelling = tmpAspects "keen-smelling" [AddSkill SkSmell 2]
tmpFoulSmelling = tmpAspects "foul-smelling" [AddSkill SkOdor 2]
tmpRoseSmelling = tmpAspects "rose-smelling" [AddSkill SkOdor (-4)]
tmpNoctovision = tmpAspects "shiny-eyed" [AddSkill SkNocto 2]
tmpDeafened = tmpAspects "deafened" [AddSkill SkHearing (-10)]
tmpDeaf = tmpAspects "deaf" [AddSkill SkHearing (-99)]
tmpDrunk = tmpAspects "drunk" [ AddSkill SkHurtMelee 30  -- fury
                              , AddSkill SkArmorMelee (-20)
                              , AddSkill SkArmorRanged (-20)
                              , AddSkill SkSight (-8)
                              ]
tmpBonusSkAggresion =
  tmpAspects "frenzied" [AddSkill SkAggression 5]

tmpNoSkMove =
  tmpAspects "immobile" [AddSkill SkMove (-99)]
tmpNoSkMelee =
  tmpAspects "pacified" [AddSkill SkMelee (-99)]
tmpNoSkDisplace =
  tmpAspects "irreplaceable" [AddSkill SkDisplace (-99)]
tmpNoSkAlter =
  tmpAspects "retaining" [AddSkill SkAlter (-99)]
tmpNoSkWait =
  tmpAspects "impatient" [AddSkill SkWait (-99)]
tmpNoSkMoveItem =
  tmpAspects "dispossessed" [AddSkill SkMoveItem (-99)]
tmpNoSkProject =
  tmpAspects "withholding" [AddSkill SkProject (-99)]
tmpNoSkApply =
  tmpAspects "parsimonious" [AddSkill SkApply (-99)]

tmpBonusSkMove =
  tmpAspects "more mobile" [AddSkill SkMove 5]
tmpBonusSkMelee =
  tmpAspects "more combative" [AddSkill SkMelee 5]
tmpBonusSkDisplace =
  tmpAspects "more displacing" [AddSkill SkDisplace 5]
tmpBonusSkAlter =
  tmpAspects "more modifying" [AddSkill SkAlter 5]
tmpBonusSkWait =
  tmpAspects "more patient" [AddSkill SkWait 5]
tmpBonusSkMoveItem =
  tmpAspects "more tidy" [AddSkill SkMoveItem 5]
tmpBonusSkProject =
  tmpAspects "more projecting" [AddSkill SkProject 8]
    -- TODO: 11, but let player control potion throwing by non-pointmen;
    -- beware also of capReinforced and other sources of the skill
tmpBonusSkApply =
  tmpAspects "more practical" [AddSkill SkApply 5]

tmpRegenerating =
  tmpEffects "regenerating" (4 + 1 `d` 2) [RefillHP 1]
tmpPoisoned =
  tmpEffects "poisoned" (4 + 1 `d` 2) [RefillHP (-1)]
tmpSlow10Resistant =
  tmpEffects "slow resistant" (8 + 1 `d` 4)
             [DropItem 1 1 COrgan "slowed"]
tmpPoisonResistant =
  tmpEffects "poison resistant" (8 + 1 `d` 4)
             [DropItem 1 maxBound COrgan "poisoned"]
