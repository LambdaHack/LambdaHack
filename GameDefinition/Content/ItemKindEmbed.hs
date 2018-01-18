-- | Definitions of items embedded in map tiles.
module Content.ItemKindEmbed
  ( embeds
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

embeds :: [ItemKind]
embeds =
  [scratchOnWall, obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, doorwayTrap,  stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit]

scratchOnWall,    obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, doorwayTrap,  stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit :: ItemKind

-- Make sure very few walls are substantially useful, e.g., caches,
-- and none that are secret. Otherwise the player will spend a lot of time
-- bumping walls, which is boring compare to fights or dialogues
-- and ever worse, the player will bump all secret walls, wasting time
-- and foregoing the fun of guessing how to find entrance to a disjoint part
-- of the level by bumping the least number of secret walls.
scratchOnWall = ItemKind
  { isymbol  = '?'
  , iname    = "scratch on wall"
  , ifreq    = [("scratch on wall", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scratch"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Temporary "start making sense of the scratches", DetectHidden 3]
  , ifeature = [Identified, Durable]
  , idesc    = "A seemingly random series of scratches, carved deep into the wall."
  , ikit     = []
  }
obscenePictogram = ItemKind
  { isymbol  = '*'
  , iname    = "obscene pictogram"
  , ifreq    = [("obscene pictogram", 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "infuriate"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [Timeout 7]
  , ieffects = [ Recharging $ Temporary "enter destructive rage at the sight of an obscene pictogram"
               , Recharging $ RefillCalm (-20)
               , Recharging $ OneOf
                   [ toOrganActorTurn "strengthened" (3 + 1 `d` 3)
                   , CreateItem CInv "sandstone rock" timerNone ] ]
  , ifeature = [Identified, Durable]
  , idesc    = "It's not even anatomically possible."
  , ikit     = []
  }
subtleFresco = ItemKind
  { isymbol  = '*'
  , iname    = "subtle fresco"
  , ifreq    = [("subtle fresco", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [Timeout 7]
  , ieffects = [ Temporary "feel refreshed by the subtle fresco"
               , RefillCalm 2
               , Recharging $ toOrganActorTurn "far-sighted" (3 + 1 `d` 3)
               , Recharging $ toOrganActorTurn "keen-smelling" (3 + 1 `d` 3) ]
  , ifeature = [Identified, Durable]
  , idesc    = "Expensive yet tasteful."
  , ikit     = []
  }
treasureCache = stairsUp
  { isymbol  = 'O'
  , iname    = "treasure cache"
  , ifreq    = [("treasure cache", 1)]
  , iflavour = zipPlain [BrBlue]
  , ieffects = [CreateItem CGround "useful" timerNone]
  , idesc    = "Glittering gold, just waiting to be taken."
  }
treasureCacheTrap = ItemKind
  { isymbol  = '^'
  , iname    = "treasure cache trap"
  , ifreq    = [("treasure cache trap", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ toOrganNone "poisoned", Explode "glue"
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1) ]]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "It's a trap!"
  , ikit     = []
  }
signboardExit = ItemKind
  { isymbol  = '?'
  , iname    = "signboard with exits"
  , ifreq    = [("signboard", 80)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "whack"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [DetectExit 100]
  , ifeature = [Identified, Durable]
  , idesc    = "A battered sign, carved by unknown hands."
  , ikit     = []
  }
signboardMap = signboardExit
  { iname    = "signboard with a map"
  , ifreq    = [("signboard", 20)]
  , ieffects = [Detect 10]
  , idesc    = ""
  }
fireSmall = ItemKind
  { isymbol  = '%'
  , iname    = "small fire"
  , ifreq    = [("small fire", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Burn 1, Explode "single spark"]
  , ifeature = [Identified, Durable]
  , idesc    = "A few small logs, burning brightly."
  , ikit     = []
  }
fireBig = fireSmall
  { isymbol  = 'O'
  , iname    = "big fire"
  , ifreq    = [("big fire", 1)]
  , ieffects = [ Burn 2, Explode "spark"
               , CreateItem CInv "wooden torch" timerNone ]
  , ifeature = [Identified, Durable]
  , idesc    = "Glowing with light and warmth."
  , ikit     = []
  }
frost = ItemKind
  { isymbol  = '^'
  , iname    = "frost"
  , ifreq    = [("frost", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , RefillCalm 20  -- cold reason
               , PushActor (ThrowMod 100 50) ]  -- slippery ice, 1 step, slow
  , ifeature = [Identified, Durable]
  , idesc    = "Intricate patterns of shining ice."
  , ikit     = []
  }
rubble = ItemKind
  { isymbol  = '&'
  , iname    = "rubble"
  , ifreq    = [("rubble", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ Explode "glass piece", Explode "waste"
                      , Summon "animal" $ 1 `dL` 2, toOrganNone "poisoned"
                      , CreateItem CGround "useful" timerNone
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1) ]]
  , ifeature = [Identified, Durable]
  , idesc    = "Broken chunks of rock and glass."
  , ikit     = []
  }
doorwayTrap = ItemKind
  { isymbol  = '^'
  , iname    = "doorway trap"
  , ifreq    = [("doorway trap", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cripple"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ toOrganActorTurn "blind" $ (2 + 1 `dL` 3) * 10
                      , toOrganActorTurn "slowed" $ (2 + 1 `dL` 3) * 10
                      , toOrganActorTurn "weakened" $ (2 + 1 `dL` 3) * 10 ]]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
stairsUp = ItemKind
  { isymbol  = '<'
  , iname    = "staircase up"
  , ifreq    = [("staircase up", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"  -- the verb is only used when the item hits,
                        -- not when it's applied otherwise, e.g., from tile
  , iweight  = 100000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Ascend True]
  , ifeature = [Identified, Durable]
  , idesc    = "Stairs that rise towards escape."
  , ikit     = []
  }
stairsDown = stairsUp
  { isymbol  = '>'
  , iname    = "staircase down"
  , ifreq    = [("staircase down", 1)]
  , ieffects = [Ascend False]
  , idesc    = ""
  }
escape = stairsUp
  { isymbol  = 'E'
  , iname    = "escape"
  , ifreq    = [("escape", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffects = [Escape]
  , idesc    = ""
  }
staircaseTrapUp = ItemKind
  { isymbol  = '^'
  , iname    = "staircase trap"
  , ifreq    = [("staircase trap up", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "buffet"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [ Temporary "be caught in an updraft"
               , Teleport $ 3 + 1 `dL` 10 ]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "A hidden spring, to help the unwary soar."
  , ikit     = []
  }
-- Needs to be separate from staircaseTrapUp, to make sure the item is
-- registered after up staircase (not only after down staircase)
-- so that effects are invoked in the proper order and, e.g., teleport works.
staircaseTrapDown = staircaseTrapUp
  { ifreq    = [("staircase trap down", 1)]
  , iverbHit = "open up under"
  , ieffects = [ Temporary "tumble down the stairwell"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
pulpit = ItemKind
  { isymbol  = '?'
  , iname    = "pulpit"
  , ifreq    = [("pulpit", 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "ask"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [ CreateItem CGround "any scroll" timerNone
               , toOrganGameTurn "defenseless" $ (2 + 1 `dL` 3) * 10
               , Explode "PhD defense question" ]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "A dark wood stand, where strange priests once preached."
  , ikit     = []
  }
