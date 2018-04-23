-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTemporary
  ( temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemAspect (Aspect (..))
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant :: ItemKind

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
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
               aspects
  , ieffects = [ Recharging $ tmpLess name
               , OnSmash $ tmpLess name ]
  , ifeature = [Periodic, Fragile, Durable]  -- hack: destroy on drop
  , idesc    = ""  -- no description needed; stats are enough
  , ikit     = []
  }

tmpEffects :: Text -> Dice -> [Effect] -> ItemKind
tmpEffects name icount effects =
  let tmp = tmpAspects name []
  in tmp { icount
         , ieffects = effects
                      ++ [ Recharging $ tmpNoLonger name
                         , OnSmash $ tmpNoLonger name ]
         }

tmpStrengthened = tmpAspects "strengthened" [AddHurtMelee 20]
tmpWeakened = tmpAspects "weakened" [AddHurtMelee (-30)]  -- don't cancel out ^
tmpProtectedMelee = tmpAspects "protected from melee" [AddArmorMelee 50]
tmpProtectedRanged = tmpAspects "protected from ranged" [AddArmorRanged 25]
tmpVulnerable = tmpAspects "defenseless" [ AddArmorMelee (-50)
                                         , AddArmorRanged (-25) ]
tmpResolute = tmpAspects "resolute" [AddMaxCalm 60]
tmpFast20 = tmpAspects "hasted" [AddSpeed 20]
tmpSlow10 = tmpAspects "slowed" [AddSpeed (-10)]
tmpFarSighted = tmpAspects "far-sighted" [AddSight 5]
tmpBlind = tmpAspects "blind" [AddSight (-99)]
tmpKeenSmelling = tmpAspects "keen-smelling" [AddSmell 2]
tmpNoctovision = tmpAspects "shiny-eyed" [AddNocto 2]
tmpDrunk = tmpAspects "drunk" [ AddHurtMelee 30  -- fury
                              , AddArmorMelee (-20)
                              , AddArmorRanged (-20)
                              , AddSight (-8)
                              ]

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
