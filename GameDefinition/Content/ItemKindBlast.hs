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
  [burningOil2, burningOil3, burningOil4, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, glassPiece, smoke, boilingWater, glue, singleSpark, spark, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, waste, youthSprinkle, poisonCloud, mistAntiSlow, mistAntidote]

burningOil2,    burningOil3, burningOil4, explosionBlast2, explosionBlast10, explosionBlast20, firecracker2, firecracker3, firecracker4, firecracker5, firecracker6, firecracker7, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, glassPiece, smoke, boilingWater, glue, singleSpark, spark, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, waste, youthSprinkle, poisonCloud, mistAntiSlow, mistAntidote :: ItemKind

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
  , iverbHit = "sear"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 2]
  , ieffects = [Burn 1, Paralyze 2]  -- tripping on oil
  , ifeature = [ toVelocity (min 100 $ n `div` 2 * 10)
               , Fragile, Identified ]
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }
burningOil2 = burningOil 2  -- 2 steps, 2 turns
burningOil3 = burningOil 3  -- 3 steps, 2 turns
burningOil4 = burningOil 4  -- 4 steps, 2 turns
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
               ++ [PushActor (ThrowMod (100 * (n `div` 5)) 50)| n >= 10]
               ++ [DropItem 1 maxBound COrgan "temporary condition" | n >= 10]
               ++ [DropItem 1 maxBound COrgan "impressed" | n >= 10]  -- shock
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
  , icount   = intToDice (n `div` 6) + 1 `d` (n `div` 2)
  , irarity  = [(1, 1)]
  , iverbHit = "crack"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine $ intToDice $ n `div` 2]
  , ieffects = [ RefillCalm (3 - n) | n >= 5 ]
               ++ [ DropBestWeapon | n >= 5]
               ++ [ OnSmash $ Explode
                    $ toGroupName $ "firecracker" <+> tshow (n - 1)
                  | n > 2 ]
  , ifeature = [toVelocity 5, Fragile, Identified]
  , idesc    = "Scraps of burnt paper, covering little pockets of black powder, buffeted by colorful explosions."
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
  { isymbol  = '`'
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
  , ifeature = [toLinger 10, Fragile, Identified]  -- 2 steps, 1 turn
  , idesc    = "A pleasant scent."
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '`'
  , iname    = "musky whiff"  -- a kind of mist rather than fragrance
  , ifreq    = [("pheromone", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Impress, RefillCalm (-10)]
  , ifeature = [toVelocity 10, Fragile, Identified]  -- 2 steps, 2 turns
  , idesc    = "A sharp, strong scent."
  , ikit     = []
  }
mistCalming = ItemKind  -- unused
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("calming mist", 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillCalm 2]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "A soothing, gentle cloud."
  , ikit     = []
  }
odorDistressing = ItemKind
  { isymbol  = '`'
  , iname    = "distressing whiff"
  , ifreq    = [("distressing odor", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "distress"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillCalm (-20)]
  , ifeature = [toLinger 10, Fragile, Identified]  -- 2 steps, 1 turn
  , idesc    = "It turns the stomach."
  , ikit     = []
  }
mistHealing = ItemKind
  { isymbol  = '`'
  , iname    = "mist"  -- powerful, so slow and narrow
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 1]
  , ieffects = [RefillHP 2]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "It fills the air with light and life."
  , ikit     = []
  }
mistHealing2 = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("healing mist 2", 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 2]
  , ieffects = [RefillHP 4]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "At its touch, wounds close and bruises fade."
  , ikit     = []
  }
mistWounding = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "devitalize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillHP (-2)]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "The air itself stings and itches."
  , ikit     = []
  }
distortion = ItemKind
  { isymbol  = 'v'
  , iname    = "vortex"
  , ifreq    = [("distortion", 1)]
  , iflavour = zipFancy [White]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Teleport $ 15 + 1 `d` 10]
  , ifeature = [toLinger 10, Fragile, Identified]  -- 2 steps, 1 turn
  , idesc    = "The air shifts oddly, as though light is being warped."
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
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [RefillHP (-1)]  -- high velocity, so can't do idamage
  , ifeature = [toLinger 20, Fragile, Identified]  -- 4 steps, 1 turn
  , idesc    = "Swift, sharp edges."
  , ikit     = []
  }
smoke = ItemKind  -- when stuff burns out  -- unused
  { isymbol  = '`'
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "choke"  -- or obscure
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [toVelocity 20, Fragile, Identified]  -- 4 steps, 2 turns
  , idesc    = "Twirling clouds of grey smoke."
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
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Burn 1]
  , ifeature = [toVelocity 30, Fragile, Identified]  -- 6 steps, 2 turns
  , idesc    = "It bubbles and hisses."
  , ikit     = []
  }
glue = ItemKind
  { isymbol  = '*'
  , iname    = "hoof glue"
  , ifreq    = [("glue", 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Paralyze 10]
  , ifeature = [toVelocity 20, Fragile, Identified]  -- 4 steps, 2 turns
  , idesc    = "Thick and clinging."
  , ikit     = []
  }
singleSpark = ItemKind
  { isymbol  = '`'
  , iname    = "single spark"
  , ifreq    = [("single spark", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "spark"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 4]
  , ieffects = []
  , ifeature = [toLinger 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "A glowing ember."
  , ikit     = []
  }
spark = ItemKind
  { isymbol  = '`'
  , iname    = "spark"
  , ifreq    = [("spark", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "scorch"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = [AddShine 4]
  , ieffects = [Burn 1]
  , ifeature = [toLinger 10, Fragile, Identified]  -- 2 steps, 1 turn
  , idesc    = "A flash of fire."
  , ikit     = []
  }

-- * Temporary condition blasts strictly matching the aspects

-- Almost all have @toLinger 10@, that travels 2 steps in 1 turn.
-- These are very fast projectiles, not getting into the way of big
-- actors and not burdening the engine for long.

denseShower = ItemKind
  { isymbol  = '`'
  , iname    = "dense shower"
  , ifreq    = [("dense shower", 1)]
  , iflavour = zipFancy [Green]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "strengthen"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "strengthened" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A thick rain of droplets."
  , ikit     = []
  }
sparseShower = ItemKind
  { isymbol  = '`'
  , iname    = "sparse shower"
  , ifreq    = [("sparse shower", 1)]
  , iflavour = zipFancy [Red]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "weaken"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganGameTurn "weakened" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Light droplets that cling to clothing."
  , ikit     = []
  }
protectingBalmMelee = ItemKind
  { isymbol  = '`'
  , iname    = "balm droplet"
  , ifreq    = [("melee protective balm", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "protected from melee" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A thick ointment that hardens the skin."
  , ikit     = []
  }
protectingBalmRanged = ItemKind
  { isymbol  = '`'
  , iname    = "balm droplet"
  , ifreq    = [("ranged protective balm", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "protected from ranged" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Grease that protects from flying death."
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
  , ieffects = [toOrganGameTurn "defenseless" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Only the most learned make use of this."
  , ikit     = []
  }
resolutionDust = ItemKind
  { isymbol  = '`'
  , iname    = "resolution dust"
  , ifreq    = [("resolution dust", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "calm"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "resolute" (3 + 1 `d` 3)]
                 -- short enough duration that @calmEnough@ not a big problem
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A handful of honest earth, to strengthen the soul."
  , ikit     = []
  }
hasteSpray = ItemKind
  { isymbol  = '`'
  , iname    = "haste spray"
  , ifreq    = [("haste spray", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "haste"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "hasted" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A quick spurt."
  , ikit     = []
  }
slownessMist = ItemKind
  { isymbol  = '`'
  , iname    = "slowness mist"
  , ifreq    = [("slowness mist", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "slow"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganGameTurn "slowed" (3 + 1 `d` 3)]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn, mist
  , idesc    = "Clammy fog, making each movement an effort."
  , ikit     = []
  }
eyeDrop = ItemKind
  { isymbol  = '`'
  , iname    = "eye drop"
  , ifreq    = [("eye drop", 1)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "far-sighted" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Not to be taken orally."
  , ikit     = []
  }
ironFiling = ItemKind
  { isymbol  = '`'
  , iname    = "iron filing"
  , ifreq    = [("iron filing", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "blind"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "blind" (10 + 1 `d` 10)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A shaving of bright metal."
  , ikit     = []
  }
smellyDroplet = ItemKind
  { isymbol  = '`'
  , iname    = "smelly droplet"
  , ifreq    = [("smelly droplet", 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sensitize"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "keen-smelling" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "A viscous lump that stains the skin."
  , ikit     = []
  }
eyeShine = ItemKind
  { isymbol  = '`'
  , iname    = "eye shine"
  , ifreq    = [("eye shine", 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "smear"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "shiny-eyed" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "They almost glow in the dark."
  , ikit     = []
  }

-- * Assorted temporary condition blasts or related (also, matching flasks)

whiskeySpray = ItemKind
  { isymbol  = '`'
  , iname    = "whiskey spray"
  , ifreq    = [("whiskey spray", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "inebriate"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganActorTurn "drunk" (3 + 1 `d` 3)]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "It burns in the best way."
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
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Burn 1]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Sodden and foul-smelling."
  , ikit     = []
  }
youthSprinkle = ItemKind
  { isymbol  = '`'
  , iname    = "youth sprinkle"
  , ifreq    = [("youth sprinkle", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sprinkle"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganNone "regenerating"]
  , ifeature = [toLinger 10, Fragile, Identified]
  , idesc    = "Bright and smelling of the Spring."
  , ikit     = []
  }
poisonCloud = ItemKind
  { isymbol  = '`'
  , iname    = "poison cloud"
  , ifreq    = [("poison cloud", 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "poison"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [toOrganNone "poisoned"]
  , ifeature = [toVelocity 10, Fragile, Identified]  -- 2 steps, 2 turns
  , idesc    = "Choking gas that stings the eyes."
  , ikit     = []
  }
mistAntiSlow = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("anti-slow mist", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [DropItem 1 1 COrgan "slowed"]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "A cleansing rain."
  , ikit     = []
  }
mistAntidote = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("antidote mist", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cure"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [DropItem 1 maxBound COrgan "poisoned"]
  , ifeature = [toVelocity 5, Fragile, Identified]  -- 1 step, 1 turn
  , idesc    = "Washes away death's dew."
  , ikit     = []
  }
