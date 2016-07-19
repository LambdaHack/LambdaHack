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
  [burningOil2, burningOil3, burningOil4, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, waste, glassPiece, smoke, boilingWater, glue, spark, mistAntiSlow, mistAntidote, mistStrength, mistWeakness, protectingBalm, vulnerabilityBalm, hasteSpray, slownessSpray, eyeDrop, eyeShine, smellyDroplet, whiskeySpray]

burningOil2,    burningOil3, burningOil4, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, waste, glassPiece, smoke, boilingWater, glue, spark, mistAntiSlow, mistAntidote, mistStrength, mistWeakness, protectingBalm, vulnerabilityBalm, hasteSpray, slownessSpray, eyeDrop, eyeShine, smellyDroplet, whiskeySpray :: ItemKind

-- * Parameterized immediate effect blasts

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '*'
  , iname    = "burning oil"
  , ifreq    = [(toGroupName $ "burning oil" <+> tshow n, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = intToDice (n * 5)
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 1
  , iaspects = [AddLight 2]
  , ieffects = [Burn 1, Paralyze 2]  -- tripping on oil
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
  , iname    = "blast"
  , ifreq    = [(toGroupName $ "blast" <+> tshow n, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 15  -- strong, but few, so not always hits target
  , irarity  = [(1, 1)]
  , iverbHit = "tear apart"
  , iweight  = 1
  , iaspects = [AddLight $ intToDice n]
  , ieffects = [RefillHP (- n `div` 2)]
               ++ [PushActor (ThrowMod (100 * (n `div` 5)) 50)]
               ++ [DropItem COrgan "temporary conditions" True | n >= 10]
  , ifeature = [Fragile, toLinger 20, Identified]
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
  , iaspects = [AddLight $ intToDice $ n `div` 2]
  , ieffects = [ RefillCalm (-1) | n >= 5 ]
               ++ [ DropBestWeapon | n >= 5]
               ++ [ OnSmash (Explode $ toGroupName
                             $ "firecracker" <+> tshow (n - 1))
                  | n > 2 ]
  , ifeature = [ ToThrow $ ThrowMod (10 + 3 * n) (10 + 100 `div` n)
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
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [Magenta]
  , icount   = 20
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Impress]
  -- Linger 10, because sometimes it takes 2 turns due to starting just
  -- before actor turn's end (e.g., via a necklace).
  , ifeature = [ ToThrow $ ThrowMod 28 10  -- 2 steps, one turn
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '\''
  , iname    = "musky whiff"
  , ifreq    = [("pheromone", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 18
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Impress, OverfillCalm (-20)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistCalming = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("calming mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 19
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1
  , iaspects = []
  , ieffects = [RefillCalm 2]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
odorDistressing = ItemKind
  { isymbol  = '\''
  , iname    = "distressing whiff"
  , ifreq    = [("distressing odor", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 10
  , irarity  = [(1, 1)]
  , iverbHit = "distress"
  , iweight  = 1
  , iaspects = []
  , ieffects = [OverfillCalm (-20)]
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
  , icount   = 9
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
mistHealing2 = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist 2", 1)]
  , iflavour = zipFancy [White]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , iaspects = [AddLight 2]
  , ieffects = [RefillHP 4]
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
  , icount   = 7
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
  , icount   = 6
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
  , icount   = 18
  , irarity  = [(1, 1)]
  , iverbHit = "splosh"
  , iweight  = 50
  , iaspects = []
  , ieffects = [RefillHP (-1)]
  , ifeature = [ ToThrow $ ThrowMod 28 10  -- 2 steps, one turn
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
glassPiece = ItemKind  -- when blowing up windows
  { isymbol  = '*'
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 18
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
  , ifreq    = [("smoke", 1)]
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
  , icount   = 21
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
  , icount   = 20
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 20
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
  , icount   = 17
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 1
  , iaspects = [AddLight 4]
  , ieffects = [Burn 1]
  , ifeature = [Fragile, toLinger 10, Identified]
  , idesc    = ""
  , ikit     = []
  }
mistAntiSlow = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("anti-slow mist", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 7
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 1
  , iaspects = []
  , ieffects = [DropItem COrgan "slow 10" True]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
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
  , iaspects = []
  , ieffects = [DropItem COrgan "poisoned" True]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }

-- * Assorted temporary condition blasts

mistStrength = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("strength mist", 1)]
  , iflavour = zipFancy [Red]
  , icount   = 6
  , irarity  = [(1, 1)]
  , iverbHit = "strengthen"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "strengthened" (3 + d 3)]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
mistWeakness = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("weakness mist", 1)]
  , iflavour = zipFancy [Blue]
  , icount   = 5
  , irarity  = [(1, 1)]
  , iverbHit = "weaken"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganGameTurn "weakened" (3 + d 3)]
  , ifeature = [ toVelocity 7  -- the slowest that gets anywhere (1 step only)
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
protectingBalm = ItemKind
  { isymbol  = '\''
  , iname    = "balm droplet"
  , ifreq    = [("protecting balm", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 13
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "protected" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
vulnerabilityBalm = ItemKind
  { isymbol  = '\''
  , iname    = "PhD defense question"
  , ifreq    = [("PhD defense question", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 14
  , irarity  = [(1, 1)]
  , iverbHit = "nag"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganGameTurn "defenseless" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
hasteSpray = ItemKind
  { isymbol  = '\''
  , iname    = "haste spray"
  , ifreq    = [("haste spray", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 15
  , irarity  = [(1, 1)]
  , iverbHit = "haste"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "fast 20" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
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
  , iaspects = []
  , ieffects = [toOrganGameTurn "slow 10" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
eyeDrop = ItemKind
  { isymbol  = '\''
  , iname    = "eye drop"
  , ifreq    = [("eye drop", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 17
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "far-sighted" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
eyeShine = ItemKind
  { isymbol  = '\''
  , iname    = "eye shine"
  , ifreq    = [("eye shine", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "smear"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "shiny-eyed" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
smellyDroplet = ItemKind
  { isymbol  = '\''
  , iname    = "smelly droplet"
  , ifreq    = [("smelly droplet", 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 18
  , irarity  = [(1, 1)]
  , iverbHit = "sensitize"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "keen-smelling" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
whiskeySpray = ItemKind
  { isymbol  = '\''
  , iname    = "whiskey spray"
  , ifreq    = [("whiskey spray", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 19
  , irarity  = [(1, 1)]
  , iverbHit = "inebriate"
  , iweight  = 1
  , iaspects = []
  , ieffects = [toOrganActorTurn "drunk" (3 + d 3)]
  , ifeature = [ toVelocity 13  -- the slowest that travels at least 2 steps
               , Fragile, Identified ]
  , idesc    = ""
  , ikit     = []
  }
