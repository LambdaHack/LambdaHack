-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTempAspect ( tempAspects ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

tempAspects :: [ItemKind]
tempAspects =
  [tmpFast20, tmpDrunk]

tmpFast20,    tmpDrunk :: ItemKind

-- The @name@ will be used in item description, so it should be an adjetive
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
  , iaspects = [Periodic, Timeout 0]  -- activates and vanishes soon,
                                      -- depending on initial timer setting
               ++ aspects
  , ieffects = [Recharging (Temporary name)]
  , ifeature = [Identified]
  , idesc    = ""
  , ikit     = []
  }

tmpFast20 = tmpAs "fast 20" [AddSpeed 20]
tmpDrunk = tmpAs "drunk" [ AddHurtMelee 20  -- fury
                         , AddArmorMelee (-20)
                         , AddHurtRanged (-20)
                         , AddArmorRanged (-20)
                         , AddSight (-7)
                         ]
