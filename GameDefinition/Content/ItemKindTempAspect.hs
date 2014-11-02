-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTempAspect ( tempAspects ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

tempAspects :: [ItemKind]
tempAspects =
  [tmpStrengthened, tmpWeakened, tmpProtected, tmpPaintedRed, tmpFast20, tmpSlow10, tmpFarSighted, tmpKeenSmelling, tmpDrunk, tmpRegenerating, tmpPoisoned]

tmpStrengthened,    tmpWeakened, tmpProtected, tmpPaintedRed, tmpFast20, tmpSlow10, tmpFarSighted, tmpKeenSmelling, tmpDrunk, tmpRegenerating, tmpPoisoned :: ItemKind

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
tmpAs :: Text -> [Aspect Dice] -> ItemKind
tmpAs name aspects = ItemKind
  { isymbol  = '.'
  , iname    = name
  , ifreq    = [(toGroupName name, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 1
  , iaspects = [Periodic, Timeout 1]  -- activates and vanishes soon,
                                      -- depending on initial timer setting
               ++ aspects
  , ieffects = [Recharging (Temporary $ "be no longer" <+> name)]
  , ifeature = [Identified]
  , idesc    = ""
  , ikit     = []
  }

tmpStrengthened = tmpAs "strengthened" [AddHurtMelee 20]
tmpWeakened = tmpAs "weakened" [AddHurtMelee (-20)]
tmpProtected = tmpAs "protected" [ AddArmorMelee 30
                                 , AddArmorRanged 30 ]
tmpPaintedRed = tmpAs "painted red" [ AddArmorMelee (-30)
                                    , AddArmorRanged (-30) ]
tmpFast20 = tmpAs "fast 20" [AddSpeed 20]
tmpSlow10 = tmpAs "slow 10" [AddSpeed (-10)]
tmpFarSighted = tmpAs "far-sighted" [AddSight 5]
tmpKeenSmelling = tmpAs "keen-smelling" [AddSmell 1]
tmpDrunk = tmpAs "drunk" [ AddHurtMelee 30  -- fury
                         , AddArmorMelee (-20)
                         , AddArmorRanged (-20)
                         , AddSight (-7)
                         ]
tmpRegenerating =
  let tmp = tmpAs "regenerating" []
  in tmp { icount = 7 + d 5
         , ieffects = [Recharging (RefillHP 1)] ++ ieffects tmp
         }
tmpPoisoned =
  let tmp = tmpAs "poisoned" []
  in tmp { icount = 7 + d 5
         , ieffects = [Recharging (RefillHP (-1))] ++ ieffects tmp
         }
