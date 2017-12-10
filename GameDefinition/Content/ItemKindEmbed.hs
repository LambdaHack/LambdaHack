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
  [stairsUp, stairsDown, escape, terrainCache, terrainCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, staircaseTrapUp, staircaseTrapDown, doorwayTrap, obscenePictograms, subtleFresco, scratchOnWall, pulpit]

stairsUp,    stairsDown, escape, terrainCache, terrainCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, staircaseTrapUp, staircaseTrapDown, doorwayTrap, obscenePictograms, subtleFresco, scratchOnWall, pulpit :: ItemKind

stairsUp = ItemKind
  { isymbol  = '<'
  , iname    = "staircase up"
  , ifreq    = [("staircase up", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"
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
  }
escape = stairsUp
  { iname    = "escape"
  , ifreq    = [("escape", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffects = [Escape]
  }
terrainCache = stairsUp
  { isymbol  = 'O'
  , iname    = "treasure cache"
  , ifreq    = [("terrain cache", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffects = [CreateItem CGround "useful" TimerNone]
  }
terrainCacheTrap = ItemKind
  { isymbol  = '^'
  , iname    = "treasure cache trap"
  , ifreq    = [("terrain cache trap", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "trap"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ toOrganNone "poisoned", Explode "glue"
                      , ELabel "", ELabel "", ELabel ""
                      , ELabel "", ELabel "", ELabel ""
                      , ELabel "", ELabel "" ]]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "Glittering gold, just waiting to be taken."
  , ikit     = []
  }
signboardExit = ItemKind
  { isymbol  = 'O'
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
  { iname    = "signboard with map"
  , ifreq    = [("signboard", 20)]
  , ieffects = [Detect 10]
  }
fireSmall = ItemKind
  { isymbol  = '&'
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
               , CreateItem CInv "wooden torch" TimerNone ]
  , ifeature = [Identified, Durable]
  , idesc    = "Glowing with light and warmth."
  , ikit     = []
  }
frost = ItemKind
  { isymbol  = '%'
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
               , PushActor (ThrowMod 200 50) ]  -- slippery ice
  , ifeature = [Identified, Durable]
  , idesc    = "Intricate patterns of shining ice."
  , ikit     = []
  }
rubble = ItemKind
  { isymbol  = ':'
  , iname    = "rubble"
  , ifreq    = [("rubble", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ Explode "glass piece", Explode "waste"
                      , Summon "animal" 1, toOrganNone "poisoned"
                      , CreateItem CGround "useful" TimerNone
                      , ELabel "", ELabel "", ELabel ""
                      , ELabel "", ELabel "", ELabel "" ]]
  , ifeature = [Identified, Durable]
  , idesc    = "Broken chunks of rock and glass."
  , ikit     = []
  }
staircaseTrapUp = ItemKind
  { isymbol  = '^'
  , iname    = "staircase trap"
  , ifreq    = [("staircase trap up", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Temporary "be caught in an updraft", Teleport 5]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "A hidden spring, to help the unwary soar."
  , ikit     = []
  }
-- Needs to be separate from staircaseTrapUp, to make sure the item is
-- registered after up staircase (not only after down staircase)
-- so that effects are invoked in the proper order and, e.g., teleport works.
staircaseTrapDown = staircaseTrapUp
  { ifreq    = [("staircase trap down", 1)]
  , ieffects = [ Temporary "tumble down the stairwell"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
doorwayTrap = ItemKind
  { isymbol  = '^'
  , iname    = "doorway trap"
  , ifreq    = [("doorway trap", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "trap"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [OneOf [ toOrganActorTurn "blind" (20 + 1 `d` 5)
                      , toOrganActorTurn "slowed" (20 + 1 `d` 5)
                      , toOrganActorTurn "weakened" (20 + 1 `d` 5) ]]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
obscenePictograms = ItemKind
  { isymbol  = '*'
  , iname    = "obscene pictograms"
  , ifreq    = [("obscene pictograms", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "infuriate"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [Timeout 7]
  , ieffects = [ Temporary "enter destructive rage at the sight of obscene pictograms"
               , RefillCalm (-20)
               , Recharging $ OneOf
                   [ toOrganActorTurn "strengthened" (3 + 1 `d` 3)
                   , CreateItem CInv "sandstone rock" TimerNone ] ]
  , ifeature = [Identified, Durable]
  , idesc    = "They aren't even anatomically possible."
  , ikit     = []
  }
subtleFresco = ItemKind
  { isymbol  = '*'
  , iname    = "subtle fresco"
  , ifreq    = [("subtle fresco", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = ""
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
scratchOnWall = ItemKind
  { isymbol  = '*'
  , iname    = "scratch on wall"
  , ifreq    = [("scratch on wall", 1)]
  , iflavour = zipPlain [BrBlue]
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
pulpit = ItemKind
  { isymbol  = 'O'
  , iname    = "pulpit"
  , ifreq    = [("pulpit", 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "ask"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [ CreateItem CGround "any scroll" TimerNone
               , toOrganGameTurn "defenseless" (20 + 1 `d` 5)
               , Explode "PhD defense question" ]
  , ifeature = [Identified]  -- not Durable, springs at most once
  , idesc    = "A dark wood stand, where strange priests once preached."
  , ikit     = []
  }
