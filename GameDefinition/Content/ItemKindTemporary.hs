-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTemporary
  ( temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDeafened, tmpDeaf, tmpDrunk, tmpNoSkMove, tmpNoSkMelee, tmpNoSkDisplace, tmpNoSkAlter, tmpNoSkWait, tmpNoSkMoveItem, tmpNoSkProject, tmpNoSkApply, tmpBonusSkMove, tmpBonusSkMelee, tmpBonusSkDisplace, tmpBonusSkAlter, tmpBonusSkWait, tmpBonusSkMoveItem, tmpBonusSkProject, tmpBonusSkApply, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant :: ItemKind

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
tmpAspects2 :: (GroupName ItemKind) -> Text -> [Aspect] -> ItemKind
tmpAspects2 groupName name aspects = ItemKind
  { isymbol  = '+'
  , iname    = name
  , ifreq    = [(groupName, 1), ("condition", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 0
  , idamage  = 0
  , iaspects = -- timeout is 0; activates and vanishes soon,
               -- depending on initial timer setting
               aspects
               ++ [SetFlag Periodic, SetFlag Fragile, SetFlag Durable]
                    -- hack: destroy on drop
  , ieffects = [ Recharging $ tmpLess name
               , OnSmash $ tmpLess name ]
  , idesc    = ""  -- no description needed; stats are enough
  , ikit     = []
  }

tmpAspects :: Text -> [Aspect] -> ItemKind
tmpAspects name = tmpAspects2 (toGroupName name) name

tmpEffects :: Text -> Dice -> [Effect] -> ItemKind
tmpEffects name icount effects =
  let tmp = tmpAspects name []
  in tmp { icount
         , ieffects = effects
                      ++ [ Recharging $ tmpNoLonger name
                         , OnSmash $ tmpNoLonger name ]
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
tmpNoctovision = tmpAspects "shiny-eyed" [AddSkill SkNocto 2]
tmpDeafened = tmpAspects "deafened" [AddSkill SkHearing (-10)]
tmpDeaf = tmpAspects "deaf" [AddSkill SkHearing (-99)]
tmpDrunk = tmpAspects "drunk" [ AddSkill SkHurtMelee 30  -- fury
                              , AddSkill SkArmorMelee (-20)
                              , AddSkill SkArmorRanged (-20)
                              , AddSkill SkSight (-8)
                              ]

tmpNoSkMove =
  tmpAspects2 "no SkMove" "immobile" [AddSkill SkMove (-99)]
tmpNoSkMelee =
  tmpAspects2 "no SkMelee" "pacified" [AddSkill SkMelee (-99)]
tmpNoSkDisplace =
  tmpAspects2 "no SkDisplace" "irreplaceable" [AddSkill SkDisplace (-99)]
tmpNoSkAlter =
  tmpAspects2 "no SkAlter" "retaining" [AddSkill SkAlter (-99)]
tmpNoSkWait =
  tmpAspects2 "no SkWait" "impatient" [AddSkill SkWait (-99)]
tmpNoSkMoveItem =
  tmpAspects2 "no SkMoveItem" "dispossessed" [AddSkill SkMoveItem (-99)]
tmpNoSkProject =
  tmpAspects2 "no SkProject" "withholding" [AddSkill SkProject (-99)]
tmpNoSkApply =
  tmpAspects2 "no SkApply" "parsimonious" [AddSkill SkApply (-99)]

tmpBonusSkMove =
  tmpAspects2 "bonus SkMove" "more mobile" [AddSkill SkMove 5]
tmpBonusSkMelee =
  tmpAspects2 "bonus SkMelee" "more combative" [AddSkill SkMelee 5]
tmpBonusSkDisplace =
  tmpAspects2 "bonus SkDisplace" "more displacing" [AddSkill SkDisplace 5]
tmpBonusSkAlter =
  tmpAspects2 "bonus SkAlter" "more altering" [AddSkill SkAlter 5]
tmpBonusSkWait =
  tmpAspects2 "bonus SkWait" "more patient" [AddSkill SkWait 5]
tmpBonusSkMoveItem =
  tmpAspects2 "bonus SkMoveItem" "tidier" [AddSkill SkMoveItem 5]
tmpBonusSkProject =
  tmpAspects2 "bonus SkProject" "more projecting" [AddSkill SkProject 5]
tmpBonusSkApply =
  tmpAspects2 "bonus SkApply" "more practical" [AddSkill SkApply 5]

tmpRegenerating =
  tmpEffects "regenerating" (4 + 1 `d` 2) [Recharging (RefillHP 1)]
tmpPoisoned =
  tmpEffects "poisoned" (4 + 1 `d` 2) [Recharging (RefillHP (-1))]
tmpSlow10Resistant =
  tmpEffects "slow resistant" (8 + 1 `d` 4)
             [Recharging (DropItem 1 1 COrgan "slowed")]
tmpPoisonResistant =
  tmpEffects "poison resistant" (8 + 1 `d` 4)
             [Recharging (DropItem 1 maxBound COrgan "poisoned")]
