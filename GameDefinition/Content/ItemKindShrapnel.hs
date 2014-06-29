-- | Shrapnel definitions.
module Content.ItemKindShrapnel ( shrapnels ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

shrapnels :: [ItemKind]
shrapnels =
  [fragrance, mist_healing, mist_wounding, burningOil2, burningOil3, burningOil4, explosionBlast10, glass_piece, smoke]

fragrance,    mist_healing, mist_wounding, burningOil2, burningOil3, burningOil4, explosionBlast10, glass_piece, smoke :: ItemKind

fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 15
  , irarity  = []
  , iverbHit = "engulf"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Impress]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mist_healing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 11
  , irarity  = []
  , iverbHit = "revitalize"
  , iweight  = 1
  , iaspects = [AddLight 1]
  , ieffects = [RefillHP 2]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mist_wounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 13
  , irarity  = []
  , iverbHit = "devitalize"
  , iweight  = 1
  , iaspects = []
  , ieffects = [RefillHP (-2)]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
burningOil2 = burningOil 2
burningOil3 = burningOil 3
burningOil4 = burningOil 4
explosionBlast10 = explosionBlast 10
glass_piece = ItemKind  -- when blowing up windows
  { isymbol  = '\''
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 17
  , irarity  = []
  , iverbHit = "cut"
  , iweight  = 10
  , iaspects = []
  , ieffects = [Hurt (1 * d 1)]
  , ifeature = [toLinger 20, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
smoke = ItemKind  -- when stuff burns out
  { isymbol  = '\''
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 19
  , irarity  = []
  , iverbHit = "choke"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 30
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '\''
  , iname    = "burning oil"
  , ifreq    = [("burning oil" <+> tshow n, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = intToDice (n * 6)
  , irarity  = []
  , iverbHit = "burn"
  , iweight  = 1
  , iaspects = [AddLight 2]
  , ieffects = [ Burn 1
               , Paralyze (intToDice n) ]  -- actors strain not to trip on oil
  , ifeature = [ toVelocity (min 100 $ n * 7)
               , Fragile, Identified ]
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }

explosionBlast :: Int -> ItemKind
explosionBlast n = ItemKind
  { isymbol  = '\''
  , iname    = "explosion blast"
  , ifreq    = [("explosion blast" <+> tshow n, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 12  -- strong, but few, so not always hits target
  , irarity  = []
  , iverbHit = "tear apart"
  , iweight  = 1
  , iaspects = [AddLight $ intToDice n]
  , ieffects = [Burn (n `div` 2), DropBestWeapon]
  , ifeature = [Fragile, toLinger 10, Identified]
  , idesc    = ""
  , ikit     = []
  }
