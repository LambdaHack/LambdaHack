-- | Blast definitions.
module Content.ItemKindBlast
  ( blasts
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

blasts :: [ItemKind]
blasts =
  [burningOil2, burningOil3, burningOil4, firecracker1, firecracker2, firecracker3, firecracker4, firecracker5, spreadFragmentation, spreadFragmentation8, focusedFragmentation, spreadConcussion, spreadConcussion8, focusedConcussion, spreadFlash, spreadFlash8, focusedFlash, singleSpark, glassPiece, focusedGlass,fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, smoke, boilingWater, glue, waste, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, youthSprinkle, poisonCloud, mistAntiSlow, mistAntidote]

burningOil2,    burningOil3, burningOil4, firecracker1, firecracker2, firecracker3, firecracker4, firecracker5, spreadFragmentation, spreadFragmentation8, focusedFragmentation, spreadConcussion, spreadConcussion8, focusedConcussion, spreadFlash, spreadFlash8, focusedFlash, singleSpark, glassPiece, focusedGlass,fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, smoke, boilingWater, glue, waste, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, vulnerabilityBalm, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, youthSprinkle, poisonCloud, mistAntiSlow, mistAntidote :: ItemKind

-- We take care (e.g., in burningOil below) that blasts are not faster
-- than 100% fastest natural speed, or some frames would be skipped,
-- which is a waste of prefectly good frames.

-- * Parameterized blasts

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '*'
  , iname    = "burning oil"
  , ifreq    = [(toGroupName $ "burning oil" <+> tshow n, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = intToDice (n * 8)
  , irarity  = [(1, 1)]
  , iverbHit = "sear"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity (min 100 $ n `div` 2 * 10)
               , Fragile, Blast
               , AddAbility AbShine 2 ]
  , ieffects = [ Burn 1
               , toOrganBad "slowed" (2 + 1 `d` 2) ]  -- tripping on oil
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }
burningOil2 = burningOil 2  -- 2 steps, 2 turns
burningOil3 = burningOil 3  -- 3 steps, 2 turns
burningOil4 = burningOil 4  -- 4 steps, 2 turns
firecracker :: Int -> ItemKind
firecracker n = ItemKind
  { isymbol  = '*'
  , iname    = "firecracker"
  , ifreq    = [(toGroupName $ if n == 5
                               then "firecracker"
                               else "firecracker" <+> tshow n, 1)]
  , iflavour = zipPlain [brightCol !! ((n + 2) `mod` length brightCol)]
  , icount   = if n <= 3 then 1 `d` min 2 n else 2 + 1 `d` 2
  , irarity  = [(1, 1)]
  , iverbHit = if n >= 4 then "singe" else "crack"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5, Fragile, Blast
               , AddAbility AbShine $ intToDice $ 1 + n `div` 2 ]
  , ieffects = [if n >= 4 then Burn 1 else RefillCalm (-2)]
               ++ [DropBestWeapon | n >= 4]
               ++ [ OnSmash $ Explode
                    $ toGroupName $ "firecracker" <+> tshow (n - 1)
                  | n >= 2 ]
  , idesc    = "Scraps of burnt paper, covering little pockets of black powder, buffeted by colorful explosions."
  , ikit     = []
  }
firecracker5 = firecracker 5
firecracker4 = firecracker 4
firecracker3 = firecracker 3
firecracker2 = firecracker 2
firecracker1 = firecracker 1

-- * Focused blasts

spreadFragmentation = ItemKind
  { isymbol  = '*'
  , iname    = "fragmentation burst"
  , ifreq    = [("violent fragmentation", 1)]
  , iflavour = zipPlain [Red]  -- flying shards; some fire and smoke
  , icount   = 16  -- strong but few, so not always hits target
  , irarity  = [(1, 1)]
  , iverbHit = "tear apart"
  , iweight  = 1
  , idamage  = 3 `d` 1  -- deadly and adjacent actor hit by 2 on average;
                        -- however, moderate armour blocks completely
  , iaspects = [ toLinger 20, Lobable, Fragile, Blast  -- 4 steps, 1 turn
               , AddAbility AbShine 3, AddAbility AbHurtMelee $ -12 * 5 ]
  , ieffects = [DropItem 1 maxBound COrgan "condition"]
  , idesc    = ""
  , ikit     = []
  }
spreadFragmentation8 = spreadFragmentation
  { iname    = "fragmentation burst"
  , ifreq    = [("fragmentation", 1)]
  , icount   = 8
  , iaspects = [ toLinger 10, Lobable, Fragile, Blast  -- 2 steps, 1 turn
               , AddAbility AbShine 3, AddAbility AbHurtMelee $ -12 * 5 ]
      -- smaller radius, so worse for area effect, but twice the direct damage
  }
focusedFragmentation = ItemKind
  { isymbol  = '`'
  , iname    = "deflagration ignition"  -- black powder
  , ifreq    = [("focused fragmentation", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4  -- 32 in total vs 16; on average 4 hits
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 0, Fragile, Blast]  -- 0 steps, 1 turn
      -- when the target position is occupied, the explosion starts one step
      -- away, hence we set range to 0 steps, to limit dispersal
  , ieffects = [OnSmash $ Explode "fragmentation"]
  , idesc    = ""
  , ikit     = []
  }
spreadConcussion = ItemKind
  { isymbol  = '*'
  , iname    = "concussion blast"
  , ifreq    = [("violent concussion", 1)]
  , iflavour = zipPlain [Magenta]  -- mosty shock wave; some fire and smoke
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "shock"
  , iweight  = 1
  , idamage  = 1 `d` 1  -- only air pressure, so not as deadly as fragmentation,
                        -- but armour can't block completely that easily
  , iaspects = [ toLinger 20, Lobable, Fragile, Blast  -- 4 steps, 1 turn
               , AddAbility AbShine 3, AddAbility AbHurtMelee $ -8 * 5 ]
      -- outdoors it has short range, but we only model indoors in the game;
      -- it's much faster than black powder shock wave, but we are beyond
      -- human-noticeable speed differences on short distances anyway
  , ieffects = [ DropItem maxBound 1 CEqp "misc armor"
               , PushActor (ThrowMod 400 25)  -- 1 step, fast; after DropItem
                   -- this produces spam for braced actors; too bad
               , DropItem 1 maxBound COrgan "condition" ]
  , idesc    = ""
  , ikit     = []
  }
spreadConcussion8 = spreadConcussion
  { iname    = "concussion blast"
  , ifreq    = [("concussion", 1)]
  , icount   = 8
  , iaspects = [ toLinger 10, Lobable, Fragile, Blast  -- 2 steps, 1 turn
               , AddAbility AbShine 3, AddAbility AbHurtMelee $ -8 * 5 ]
  }
focusedConcussion = ItemKind
  { isymbol  = '`'
  , iname    = "detonation ignition"  -- nitroglycerine
  , ifreq    = [("focused concussion", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 0, Fragile, Blast]  -- 0 steps, 1 turn
  , ieffects = [OnSmash $ Explode "concussion"]
  , idesc    = ""
  , ikit     = []
  }
spreadFlash = ItemKind
  { isymbol  = '`'
  , iname    = "magnesium flash"
  , ifreq    = [("violent flash", 1)]
  , iflavour = zipPlain [BrWhite]  -- very brigh flash
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "dazzle"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 20, Fragile, Blast  -- 4 steps, 1 turn
               , AddAbility AbShine 5 ]
  , ieffects = [toOrganBad "blind" 10, toOrganBad "weakened" 30]
                 -- Wikipedia says: blind for five seconds and afterimage
                 -- for much longer, harming aim
  , idesc    = "A flash of fire."
  , ikit     = []
  }
spreadFlash8 = spreadFlash
  { iname    = "spark"
  , ifreq    = [("spark", 1)]
  , icount   = 8
  , iverbHit = "blind"
  , iaspects = [ toLinger 10, Fragile, Blast  -- 2 steps, 1 turn
               , AddAbility AbShine 5 ]
  }
focusedFlash = ItemKind
  { isymbol  = '`'
  , iname    = "magnesium ignition"
  , ifreq    = [("focused flash", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 0, Fragile, Blast]  -- 0 steps, 1 turn
  , ieffects = [OnSmash $ Explode "spark"]
  , idesc    = ""
  , ikit     = []
  }
singleSpark = spreadFlash
  { iname    = "single spark"
  , ifreq    = [("single spark", 1)]
  , icount   = 1
  , iverbHit = "spark"
  , iaspects = [ toLinger 5, Fragile, Blast  -- 1 step, 1 turn
               , AddAbility AbShine 3 ]
  , ieffects = []
  , idesc    = "A glowing ember."
  , ikit     = []
  }
glassPiece = ItemKind
  { isymbol  = '*'
  , iname    = "glass piece"
  , ifreq    = [("glass hail", 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cut"
  , iweight  = 1
  , idamage  = 1 `d` 1
  , iaspects = [ toLinger 20, Fragile, Blast  -- 4 steps, 1 turn
               , AddAbility AbHurtMelee $ -15 * 5 ]
                 -- brittle, not too dense; armor blocks
  , ieffects = [RefillHP (-1)]
  , idesc    = "Swift, sharp edges."
  , ikit     = []
  }
focusedGlass = glassPiece  -- when blowing up windows
  { ifreq    = [("focused glass hail", 1)]
  , icount   = 4
  , iaspects = [ toLinger 0, Fragile, Blast  -- 0 steps, 1 turn
               , AddAbility AbHurtMelee $ -15 * 5 ]
  , ieffects = [RefillHP (-1), OnSmash $ Explode "glass hail"]
  }

-- * Assorted immediate effect blasts

fragrance = ItemKind
  { isymbol  = '`'
  , iname    = "fragrance"  -- instant, fast fragrance
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]  -- 2 steps, 1 turn
  , ieffects = [Impress]
  -- Linger 10, because sometimes it takes 2 turns due to starting just
  -- before actor turn's end (e.g., via a necklace).
  , idesc    = "A pleasant scent."
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '`'
  , iname    = "musky whiff"  -- a kind of mist rather than fragrance
  , ifreq    = [("pheromone", 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 10, Fragile, Blast]  -- 2 steps, 2 turns
  , ieffects = [Impress, RefillCalm (-10)]
  , idesc    = "A sharp, strong scent."
  , ikit     = []
  }
mistCalming = ItemKind  -- unused
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("calming mist", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 5, Fragile, Blast]  -- 1 step, 1 turn
  , ieffects = [RefillCalm 2]
  , idesc    = "A soothing, gentle cloud."
  , ikit     = []
  }
odorDistressing = ItemKind
  { isymbol  = '`'
  , iname    = "distressing whiff"
  , ifreq    = [("distressing odor", 1)]
  , iflavour = zipFancy [BrRed]  -- salmon
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "distress"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]  -- 2 steps, 1 turn
  , ieffects = [RefillCalm (-20)]
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
  , idamage  = 0
  , iaspects = [ toVelocity 5, Fragile, Blast  -- 1 step, 1 turn
               , AddAbility AbShine 1 ]
  , ieffects = [RefillHP 2]
  , idesc    = "It fills the air with light and life."
  , ikit     = []
  }
mistHealing2 = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("healing mist 2", 1)]
  , iflavour = zipPlain [Green]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5, Fragile, Blast  -- 1 step, 1 turn
               , AddAbility AbShine 2 ]
  , ieffects = [RefillHP 4]
  , idesc    = "At its touch, wounds close and bruises fade."
  , ikit     = []
  }
mistWounding = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "devitalize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 5, Fragile, Blast]  -- 1 step, 1 turn
  , ieffects = [RefillHP (-2)]
  , idesc    = "The air itself stings and itches."
  , ikit     = []
  }
distortion = ItemKind
  { isymbol  = 'v'
  , iname    = "vortex"
  , ifreq    = [("distortion", 1)]
  , iflavour = zipPlain [White]
  , icount   = 8  -- braced are immune to Teleport; avoid failure messages
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Lobable, Fragile, Blast]  -- 2 steps, 1 turn
  , ieffects = [Teleport $ 15 + 1 `d` 10]
  , idesc    = "The air shifts oddly, as though light is being warped."
  , ikit     = []
  }
smoke = ItemKind  -- when stuff burns out  -- unused
  { isymbol  = '`'
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "choke"  -- or "obscure"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 20, Fragile, Blast]  -- 4 steps, 2 turns
  , ieffects = []
  , idesc    = "Twirling clouds of grey smoke."
  , ikit     = []
  }
boilingWater = ItemKind
  { isymbol  = '*'
  , iname    = "boiling water"
  , ifreq    = [("boiling water", 1)]
  , iflavour = zipPlain [White]
  , icount   = 18
  , irarity  = [(1, 1)]
  , iverbHit = "boil"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 30, Fragile, Blast]  -- 6 steps, 2 turns
  , ieffects = [Burn 1]
  , idesc    = "It bubbles and hisses."
  , ikit     = []
  }
glue = ItemKind
  { isymbol  = '*'
  , iname    = "hoof glue"
  , ifreq    = [("glue", 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 8  -- Paralyze doesn't stack; avoid failure messages
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 20, Fragile, Blast]  -- 4 steps, 2 turns
  , ieffects = [Paralyze 10]
  , idesc    = "Thick and clinging."
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
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [Burn 1]
  , idesc    = "Sodden and foul-smelling."
  , ikit     = []
  }

-- * Temporary condition blasts

-- Almost all have @toLinger 10@, that travels 2 steps in 1 turn.
-- These are very fast projectiles, not getting into the way of big
-- actors and not burdening the engine for long.
-- A few are slower 'mists'.

denseShower = ItemKind
  { isymbol  = '`'
  , iname    = "dense shower"
  , ifreq    = [("dense shower", 1)]
  , iflavour = zipFancy [Green]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "strengthen"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "strengthened" (3 + 1 `d` 3)]
  , idesc    = "A thick rain of droplets."
  , ikit     = []
  }
sparseShower = ItemKind
  { isymbol  = '`'
  , iname    = "sparse shower"
  , ifreq    = [("sparse shower", 1)]
  , iflavour = zipFancy [Red]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "weaken"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganBad "weakened" (3 + 1 `d` 3)]
  , idesc    = "Light droplets that cling to clothing."
  , ikit     = []
  }
protectingBalmMelee = ItemKind
  { isymbol  = '`'
  , iname    = "balm droplet"
  , ifreq    = [("melee protective balm", 1)]
  , iflavour = zipFancy [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "protected from melee" (3 + 1 `d` 3)]
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
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "protected from ranged" (3 + 1 `d` 3)]
  , idesc    = "Grease that protects from flying death."
  , ikit     = []
  }
vulnerabilityBalm = ItemKind
  { isymbol  = '?'
  , iname    = "PhD defense question"
  , ifreq    = [("PhD defense question", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "nag"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganBad "defenseless" (3 + 1 `d` 3)]
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
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "resolute" (3 + 1 `d` 3)]
                 -- short enough duration that @calmEnough@ not a big problem
  , idesc    = "A handful of honest earth, to strengthen the soul."
  , ikit     = []
  }
hasteSpray = ItemKind
  { isymbol  = '`'
  , iname    = "haste spray"
  , ifreq    = [("haste spray", 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "haste"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "hasted" (3 + 1 `d` 3)]
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
  , iweight  = 0
  , idamage  = 0
  , iaspects = [toVelocity 5, Fragile, Blast]
                 -- 1 step, 1 turn, mist, slow
  , ieffects = [toOrganBad "slowed" (3 + 1 `d` 3)]
  , idesc    = "Clammy fog, making each movement an effort."
  , ikit     = []
  }
eyeDrop = ItemKind
  { isymbol  = '`'
  , iname    = "eye drop"
  , ifreq    = [("eye drop", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "far-sighted" (3 + 1 `d` 3)]
  , idesc    = "Not to be taken orally."
  , ikit     = []
  }
ironFiling = ItemKind
  { isymbol  = '`'
  , iname    = "iron filing"
  , ifreq    = [("iron filing", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "blind"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganBad "blind" (10 + 1 `d` 10)]
  , idesc    = "A shaving of bright metal."
  , ikit     = []
  }
smellyDroplet = ItemKind
  { isymbol  = '`'
  , iname    = "smelly droplet"
  , ifreq    = [("smelly droplet", 1)]
  , iflavour = zipFancy [Blue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sensitize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "keen-smelling" (3 + 1 `d` 3)]
  , idesc    = "A viscous lump that stains the skin."
  , ikit     = []
  }
eyeShine = ItemKind
  { isymbol  = '`'
  , iname    = "eye shine"
  , ifreq    = [("eye shine", 1)]
  , iflavour = zipFancy [Cyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "smear"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "shiny-eyed" (3 + 1 `d` 3)]
  , idesc    = "They almost glow in the dark."
  , ikit     = []
  }
whiskeySpray = ItemKind
  { isymbol  = '`'
  , iname    = "whiskey spray"
  , ifreq    = [("whiskey spray", 1)]
  , iflavour = zipFancy [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "inebriate"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganGood "drunk" (3 + 1 `d` 3)]
  , idesc    = "It burns in the best way."
  , ikit     = []
  }
youthSprinkle = ItemKind
  { isymbol  = '`'
  , iname    = "youth sprinkle"
  , ifreq    = [("youth sprinkle", 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sprinkle"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, Fragile, Blast]
  , ieffects = [toOrganNoTimer "regenerating"]
  , idesc    = "Bright and smelling of the Spring."
  , ikit     = []
  }
poisonCloud = ItemKind
  { isymbol  = '`'
  , iname    = "poison cloud"
  , ifreq    = [("poison cloud", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "poison"
  , iweight  = 0
  , idamage  = 0
  , iaspects = [toVelocity 10, Fragile, Blast]  -- 2 steps, 2 turns
  , ieffects = [toOrganNoTimer "poisoned"]
  , idesc    = "Choking gas that stings the eyes."
  , ikit     = []
  }
mistAntiSlow = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("anti-slow mist", 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 5, Fragile, Blast]  -- 1 step, 1 turn
  , ieffects = [DropItem 1 1 COrgan "slowed"]
  , idesc    = "A cleansing rain."
  , ikit     = []
  }
mistAntidote = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [("antidote mist", 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cure"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toVelocity 5, Fragile, Blast]  -- 1 step, 1 turn
  , ieffects = [DropItem 1 maxBound COrgan "poisoned"]
  , idesc    = "Washes away death's dew."
  , ikit     = []
  }
