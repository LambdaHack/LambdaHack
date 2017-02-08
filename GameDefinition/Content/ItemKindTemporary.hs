-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTemporary
  ( temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant, impressedMark1, impressedMark10]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpPoisoned, tmpSlow10Resistant, tmpPoisonResistant, impressedMark1, impressedMark10 :: ItemKind

tmpNoLonger :: Text -> Effect
tmpNoLonger name = Temporary $ "be no longer" <+> name

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
tmpAs :: Text -> [Aspect] -> ItemKind
tmpAs name aspects = ItemKind
  { isymbol  = '+'
  , iname    = name
  , ifreq    = [(toGroupName name, 1), ("temporary condition", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 0
  , idamage  = toDmg 0
  , iaspects = -- timeout is 0; activates and vanishes soon,
               -- depending on initial timer setting
               aspects
  , ieffects = [ Periodic
               , Recharging $ tmpNoLonger name
               , OnSmash $ tmpNoLonger name ]
  , ifeature = [Identified, Fragile, Durable]  -- hack: destroy on drop
  , idesc    = ""
  , ikit     = []
  }

tmpStrengthened = tmpAs "strengthened" [AddHurtMelee 20]
tmpWeakened = tmpAs "weakened" [AddHurtMelee (-20)]
tmpProtectedMelee = tmpAs "protected melee" [AddArmorMelee 50]
tmpProtectedRanged = tmpAs "protected ranged" [AddArmorRanged 25]
tmpVulnerable = tmpAs "defenseless" [ AddArmorMelee (-50)
                                    , AddArmorRanged (-25) ]
tmpResolute = tmpAs "resolute" [AddMaxCalm 60]
tmpFast20 = tmpAs "fast 20" [AddSpeed 20]
tmpSlow10 = tmpAs "slow 10" [AddSpeed (-10)]
tmpFarSighted = tmpAs "far-sighted" [AddSight 5]
tmpKeenSmelling = tmpAs "keen-smelling" [AddSmell 2]
tmpNoctovision = tmpAs "shiny-eyed" [AddNocto 2]
tmpDrunk = tmpAs "drunk" [ AddHurtMelee 30  -- fury
                         , AddArmorMelee (-20)
                         , AddArmorRanged (-20)
                         , AddSight (-8)
                         ]
tmpRegenerating =
  let tmp = tmpAs "regenerating" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (RefillHP 1) : ieffects tmp
         }
tmpPoisoned =
  let tmp = tmpAs "poisoned" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (RefillHP (-1)) : ieffects tmp
         }
tmpSlow10Resistant =
  let tmp = tmpAs "slow resistant" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (DropItem 1 COrgan "slow 10") : ieffects tmp
         }
tmpPoisonResistant =
  let tmp = tmpAs "poison resistant" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (DropItem maxBound COrgan "poisoned")
                      : ieffects tmp
         }
impressedMark1 =
  let tmp = tmpAs "impressed" []
  in tmp { isymbol = '!'
         , ifreq = [("mark impressed 1", 1), ("impressed", 1)]
         , ieffects = [OnSmash $ tmpNoLonger "impressed"]
         }
impressedMark10 =
  let tmp = tmpAs "impressed" []
  in tmp { isymbol = '!'
         , icount = 10
         , ifreq = [("mark impressed 10", 1), ("impressed", 1)]
         , ieffects = [OnSmash $ tmpNoLonger "impressed"]  -- not @Periodic@
         }
