-- | Blast definitions.
module Content.ItemKindBlast
  ( blasts
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

blasts :: [ItemKind]
blasts =
  [burningOil2, burningOil3, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, waste, glassPiece, smoke, boilingWater, glue, spark, mistAntiSlow, mistAntidote, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessSpray, eyeDrop, smellyDroplet, eyeShine, whiskeySpray]

burningOil2,    burningOil3, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, waste, glassPiece, smoke, boilingWater, glue, spark, mistAntiSlow, mistAntidote, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessSpray, eyeDrop, smellyDroplet, eyeShine, whiskeySpray :: ItemKind

-- We take care (e.g., in burningOil below) that blasts are not faster
-- than 100% fastest natural speed, or some frames would be skipped,
-- which is a waste of prefectly good frames.

-- * Parameterized immediate effect blasts

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '*'
  , iname    = "burning oil"
  , ifreq    = [(toGroupName $ "burning oil" <+> tshow n, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = intToDice (n * 8)
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 2]
  , ieffects = [Burn 1, Paralyze 2]  -- tripping on oil
  , ifeature = [ toVelocity (min 100 $ n * 5)
               , Fragile, Identified ]
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }
burningOil2 = burningOil 2
burningOil3 = burningOil 3
explosionBlast :: Int -> ItemKind
explosionBlast n = ItemKind
  { isymbol  = '*'
  , iname    = "blast"
  , ifreq    = [(toGroupName $ "blast" <+> tshow n, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 16  -- strong and wide, but few, so not always hits target
  , irarity  = [(1, 1)]
  , iverbHit = "tear apart"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine $ intToDice $ min 10 n]
  , ieffects = [RefillHP (- n `div` 2)]
               ++ [PushActor (ThrowMod (100 * (n `div` 5)) 50)]
               ++ [DropItem COrgan "temporary conditions" | n >= 10]
  , ifeature = [toLinger 20, Fragile, Identified]  -- 4 steps, 1 turn
  , idesc    = ""
  , ikit     = []
  }
explosionBlast2 = explosionBlast 2
explosionBlast10 = explosionBlast 10
explosionBlast20 = explosionBlast 20
firecracker :: Int -> ItemKind
firecracker n = ItemKind
  { isymbol  = '*'
  , iname    = "firecracker"
  , ifreq    = [(toGroupName $ "firecracker" <+> tshow n, 1)]
  , iflavour = zipPlain [brightCol !! (n `mod` length brightCol)]
  , icount   = intToDice (n `div` 6) + d (n `div` 2)
  , irarity  = [(1, 1)]
  , iverbHit = "crack"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine $ intToDice $ n `div` 2]
  , ieffects = [ RefillCalm (-1) | n >= 5 ]
               ++ [ DropBestWeapon | n >= 5]
               ++ [ OnSmash (Explode $ toGroupName
                             $ "firecracker" <+> tshow (n - 1))
                  | n > 2 ]
  , ifeature = [ ToThrow $ ThrowMod (5 + 3 * n) (10 + 100 `div` n)
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

-- * Assorted immediate effect blasts

fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"  -- instant, fast fragrance
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [Magenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Impress]
  -- Linger 10, because sometimes it takes 2 turns due to starting just
  -- before actor turn's end (e.g., via a necklace).
  , ifeature = [ toLinger 10  -- 2 steps, 1 turn
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '\''
  , iname    = "musky whiff"  -- a kind of slow mist rather than fragrance
  , ifreq    = [("pheromone", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Impress, OverfillCalm (-20)]
  , ifeature = [ toVelocity 10  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistCalming = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("calming mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillCalm 2]
  , ifeature = [ toVelocity 10  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
odorDistressing = ItemKind
  { isymbol  = '\''
  , iname    = "distressing whiff"
  , ifreq    = [("distressing odor", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "distress"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OverfillCalm (-20)]
  , ifeature = [ toVelocity 10  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistHealing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"  -- powerful, so slow and narrow
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 1]
  , ieffects = [RefillHP 2]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistHealing2 = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist 2", 1)]
  , iflavour = zipFancy [White]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 2]
  , ieffects = [RefillHP 4]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistWounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "devitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillHP (-2)]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
distortion = ItemKind
  { isymbol  = 'v'
  , iname    = "vortex"
  , ifreq    = [("distortion", 1)]
  , iflavour = zipFancy [White]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Teleport $ 15 + d 10]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
waste = ItemKind
  { isymbol  = '*'
  , iname    = "waste"
  , ifreq    = [("waste", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "splosh"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillHP (-1)]
  , ifeature = [ ToThrow $ ThrowMod 40 25  -- 2 steps, one turn
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
glassPiece = ItemKind  -- when blowing up windows
  { isymbol  = '*'
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "cut"
  , iweight  = 10
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillHP (-1)]  -- high velocity, so can't do idamage
  , ifeature = [toLinger 20, Fragile, Identified]  -- 4 steps, 1 turn
  , idesc    = ""
  , ikit     = []
  }
smoke = ItemKind  -- when stuff burns out
  { isymbol  = '\''
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "choke"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 20, Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
boilingWater = ItemKind
  { isymbol  = '*'
  , iname    = "boiling water"
  , ifreq    = [("boiling water", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "boil"
  , iweight  = 5
  , idamage  = toDmg 0
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
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 20
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Paralyze (6 + 2 * d 3)]
  , ifeature = [toVelocity 40, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
spark = ItemKind
  { isymbol  = '\''
  , iname    = "spark"
  , ifreq    = [("spark", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 4]
  , ieffects = [Burn 1]
  , ifeature = [toLinger 10, Fragile, Identified]  -- 2 steps, 1 turn
  , idesc    = ""
  , ikit     = []
  }
mistAntiSlow = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("anti-slow mist", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [DropItem COrgan "slow 10"]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistAntidote = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("antidote mist", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cure"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [DropItem COrgan "poisoned"]
  , ifeature = [ toVelocity 5  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }

-- * Assorted temporary condition blasts

-- All have @toVelocity 10@, the slowest that travels at least 2 steps.

denseShower = ItemKind
  { isymbol  = '\''
  , iname    = "dense shower"
  , ifreq    = [("dense shower", 1)]
  , iflavour = zipFancy [Red]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "strengthen"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "strengthened" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
sparseShower = ItemKind
  { isymbol  = '\''
  , iname    = "sparse shower"
  , ifreq    = [("sparse shower", 1)]
  , iflavour = zipFancy [Blue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "weaken"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganGameTurn "weakened" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
protectingBalmMelee = ItemKind
  { isymbol  = '\''
  , iname    = "balm droplet"
  , ifreq    = [("protecting balm melee", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "protected melee" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
protectingBalmRanged = ItemKind
  { isymbol  = '\''
  , iname    = "balm droplet"
  , ifreq    = [("protecting balm ranged", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "protected ranged" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
vulnerabilityBalm = ItemKind
  { isymbol  = '?'
  , iname    = "PhD defense question"
  , ifreq    = [("PhD defense question", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "nag"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganGameTurn "defenseless" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
resolutionDust = ItemKind
  { isymbol  = '\''
  , iname    = "resolution dust"
  , ifreq    = [("resolution dust", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "calm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "resolute" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
hasteSpray = ItemKind
  { isymbol  = '\''
  , iname    = "haste spray"
  , ifreq    = [("haste spray", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "haste"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "fast 20" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
slownessSpray = ItemKind
  { isymbol  = '\''
  , iname    = "slowness spray"
  , ifreq    = [("slowness spray", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "slow"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganGameTurn "slow 10" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
eyeDrop = ItemKind
  { isymbol  = '\''
  , iname    = "eye drop"
  , ifreq    = [("eye drop", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "far-sighted" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
smellyDroplet = ItemKind
  { isymbol  = '\''
  , iname    = "smelly droplet"
  , ifreq    = [("smelly droplet", 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sensitize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "keen-smelling" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
eyeShine = ItemKind
  { isymbol  = '\''
  , iname    = "eye shine"
  , ifreq    = [("eye shine", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "smear"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "shiny-eyed" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
whiskeySpray = ItemKind
  { isymbol  = '\''
  , iname    = "whiskey spray"
  , ifreq    = [("whiskey spray", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "inebriate"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "drunk" (3 + d 3)]
  , ifeature = [toVelocity 10, Fragile, Identified]
  , idesc    = ""
  , ikit     = []
  }
