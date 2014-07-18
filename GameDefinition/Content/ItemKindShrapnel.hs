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
  [fragrance, pheromone, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, mistHealing, mistWounding, distortion, waste, burningOil2, burningOil3, burningOil4, explosionBlast10, explosionBlast20, glassPiece, smoke, boilingWater, glue]

fragrance,    pheromone, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, mistHealing, mistWounding, distortion, waste, burningOil2, burningOil3, burningOil4, explosionBlast10, explosionBlast20, glassPiece, smoke, boilingWater, glue :: ItemKind

-- * Parameterized shrapnel

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '*'
  , iname    = "burning oil"
  , ifreq    = [("burning oil" <+> tshow n, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = intToDice (n * 4)
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 1
  , iaspects = [AddLight 2]
  , ieffects = [ Burn (n `div` 2)
               , Paralyze (intToDice $ n `div` 2) ]  -- tripping on oil
  , ifeature = [ toVelocity (min 100 $ n * 7)
               , Fragile, Identified ]
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }
burningOil2 = burningOil 2
burningOil3 = burningOil 3
burningOil4 = burningOil 4
explosionBlast :: Int -> ItemKind
explosionBlast n = ItemKind
  { isymbol  = '*'
  , iname    = "explosion blast"
  , ifreq    = [("explosion blast" <+> tshow n, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 12  -- strong, but few, so not always hits target
  , irarity  = [(1, 1)]
  , iverbHit = "tear apart"
  , iweight  = 1
  , iaspects = [AddLight $ intToDice n]
  , ieffects = [RefillHP (- n `div` 2), DropBestWeapon]
  , ifeature = [Fragile, toLinger 10, Identified]
  , idesc    = ""
  , ikit     = []
  }
explosionBlast10 = explosionBlast 10
explosionBlast20 = explosionBlast 20
firecracker :: Int -> ItemKind
firecracker n = ItemKind
  { isymbol  = '*'
  , iname    = "firecracker"
  , ifreq    = [("firecracker" <+> tshow n, 1)]
  , iflavour = zipPlain [stdCol !! (n `mod` length stdCol)]
  , icount   = intToDice $ 2 * n
  , irarity  = [(1, 1)]
  , iverbHit = "crack"
  , iweight  = 1
  , iaspects = [AddLight $ intToDice $ n `div` 2]
  , ieffects = [Burn 1, Explode $ "firecracker" <+> tshow (n - 1)]
  , ifeature = [ ToThrow $ ThrowMod (n * 10) 20
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
firecracker7 = firecracker 7
firecracker6 = firecracker 6
firecracker5 = firecracker 5
firecracker4 = firecracker 4
firecracker3 = firecracker 3
firecracker2 = firecracker 2

-- * Assorted

fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [Magenta]
  , icount   = 15
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Impress]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '\''
  , iname    = "musky whiff"
  , ifreq    = [("pheromone", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Dominate]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistHealing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 11
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , iaspects = [AddLight 1]
  , ieffects = [RefillHP 2]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistWounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 13
  , irarity  = [(1, 1)]
  , iverbHit = "devitalize"
  , iweight  = 1
  , iaspects = []
  , ieffects = [RefillHP (-2)]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
distortion = ItemKind
  { isymbol  = 'v'
  , iname    = "vortex"
  , ifreq    = [("distortion", 1)]
  , iflavour = zipFancy [White]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Teleport $ 15 + d 10]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
waste = ItemKind
  { isymbol  = '*'
  , iname    = "waste"
  , ifreq    = [("waste", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 10
  , irarity  = [(1, 1)]
  , iverbHit = "splosh"
  , iweight  = 50
  , iaspects = []
  , ieffects = [RefillHP (-1)]
  , ifeature = [ ToThrow $ ThrowMod 28 50
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
glassPiece = ItemKind  -- when blowing up windows
  { isymbol  = '*'
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 17
  , irarity  = [(1, 1)]
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
  , ifreq    = [("smoke", 1), ("firecracker 1", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 19
  , irarity  = [(1, 1)]
  , iverbHit = "choke"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 21, Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
boilingWater = ItemKind
  { isymbol  = '*'
  , iname    = "boiling water"
  , ifreq    = [("boiling water", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 9
  , irarity  = [(1, 1)]
  , iverbHit = "boil"
  , iweight  = 5
  , iaspects = []
  , ieffects = [Burn 1]
  , ifeature = [toVelocity 50, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
glue = ItemKind
  { isymbol  = '*'
  , iname    = "hoof glue"
  , ifreq    = [("glue", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 14
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 20
  , iaspects = []
  , ieffects = [Paralyze (3 + d 3)]
  , ifeature = [toVelocity 40, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
