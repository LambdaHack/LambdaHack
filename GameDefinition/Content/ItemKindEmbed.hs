-- | Definitions of items embedded in map tiles.
module Content.ItemKindEmbed
  ( embeds
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Flavour
import Game.LambdaHack.Content.ItemKind

embeds :: [ItemKind]
embeds =
  [scratchOnWall, obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardEmbed, signboardMerchandise, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit, shallowWater, straightPath, frozenGround]

scratchOnWall,    obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardEmbed, signboardMerchandise, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit, shallowWater, straightPath, frozenGround :: ItemKind

-- Make sure very few walls are substantially useful, e.g., caches,
-- and none that are secret. Otherwise the player will spend a lot of time
-- bumping walls, which is boring compared to fights or dialogues
-- and ever worse, the player will bump all secret walls, wasting time
-- and foregoing the fun of guessing how to find entrance to a disjoint part
-- of the level by bumping the least number of secret walls.
scratchOnWall = ItemKind
  { isymbol  = '?'
  , iname    = "claw mark"
  , ifreq    = [("scratch on wall", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scratch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ VerbMsg "start making sense of the scratches"
               , Detect DetectHidden 3 ]
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
  , idamage  = 0
  , iaspects = [Timeout 7, SetFlag Durable]
  , ieffects = [ VerbMsg "enter destructive rage at the sight of an obscene pictogram"
               , RefillCalm (-20)
               , OneOf [ toOrganGood "strengthened" (3 + 1 `d` 2)
                       , CreateItem CInv "sandstone rock" timerNone ] ]
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
  , idamage  = 0
  , iaspects = [Timeout 7, SetFlag Durable]
  , ieffects = [ VerbMsg "feel refreshed by the subtle fresco"
               , toOrganGood "far-sighted" (3 + 1 `d` 2)
               , toOrganGood "keen-smelling" (3 + 1 `d` 2) ]
                 -- hearing gets a boost through bracing, so no need here
  , idesc    = "Expensive yet tasteful."
  , ikit     = []
  }
treasureCache = ItemKind
  { isymbol  = '0'
  , iname    = "treasure cache"
  , ifreq    = [("treasure cache", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [CreateItem CGround "common item" timerNone]
  , idesc    = "Glittering treasure, just waiting to be taken."
  , ikit     = []
  }
treasureCacheTrap = ItemKind
  { isymbol  = '^'
  , iname    = "cache trap"
  , ifreq    = [("treasure cache trap", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [OneOf [ toOrganBad "blind" (10 + 1 `d` 10)
                      , RefillCalm (-99)
                      , Explode "focused concussion"
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1) ]]
  , idesc    = "It's a trap!"
  , ikit     = []
  }
signboardExit = ItemKind
  { isymbol  = '?'
  , iname    = "inscription"
  , ifreq    = [("signboard", 50)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "whack"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [Detect DetectExit 100]
  , idesc    = "Crude big arrows hastily carved by unknown hands."
  , ikit     = []
  }
signboardEmbed = signboardExit
  { iname    = "notice"
  , ifreq    = [("signboard", 50)]
  , ieffects = [Detect DetectEmbed 12]
  , idesc    = "The battered poster is untitled and unsigned."
  }
signboardMerchandise = signboardExit
  { iname    = "treasure map"
  , ifreq    = [("signboard", 50)]
  , ieffects = [Detect DetectLoot 20]
  , idesc    = "In equal parts cryptic and promising."
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
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [Burn 1, Explode "single spark"]
  , idesc    = "A few small logs, burning brightly."
  , ikit     = []
  }
fireBig = fireSmall
  { isymbol  = '0'
  , iname    = "big fire"
  , ifreq    = [("big fire", 1)]
  , ieffects = [ Burn 2
               , CreateItem CInv "wooden torch" timerNone
               , Explode "spark" ]
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
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , RefillCalm 20  -- cold reason
               , PushActor (ThrowMod 400 10 1) ]  -- slippery ice
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
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [OneOf [ Explode "focused glass hail"
                      , Summon "mobile animal" $ 1 `dL` 2
                      , toOrganNoTimer "poisoned"
                      , CreateItem CGround "common item" timerNone
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1) ]]
  , idesc    = "Broken chunks of rock and glass."
  , ikit     = []
  }
doorwayTrapTemplate = ItemKind
  { isymbol  = '+'
  , iname    = "doorway trap"
  , ifreq    = [("doorway trap unknown", 1)]
  , iflavour = zipPlain brightCol
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cripple"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [HideAs "doorway trap unknown"]
      -- not Durable, springs at most once
  , ieffects = []
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
doorwayTrap1 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 50)]
  , ieffects = [toOrganBad "blind" $ (1 `dL` 4) * 5]
  -- , idesc    = ""
  }
doorwayTrap2 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 25)]
  , ieffects = [toOrganBad "slowed" $ (1 `dL` 4) * 10]
  -- , idesc    = ""
  }
doorwayTrap3 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 25)]
  , ieffects = [toOrganBad "weakened" $ (1 `dL` 4) * 10 ]
  -- , idesc    = ""
  }
stairsUp = ItemKind
  { isymbol  = '<'
  , iname    = "flight"
  , ifreq    = [("staircase up", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"  -- the verb is only used when the item hits,
                        -- not when it's applied otherwise, e.g., from tile
  , iweight  = 100000
  , idamage  = 0
  , iaspects = [ELabel "of steps", SetFlag Durable]
  , ieffects = [Ascend True]
  , idesc    = "Stairs that rise towards escape."
  , ikit     = []
  }
stairsDown = stairsUp
  { isymbol  = '>'
  , ifreq    = [("staircase down", 1)]
  , ieffects = [Ascend False]
  , idesc    = ""
  }
escape = stairsUp
  { isymbol  = 'E'
  , iname    = "way"
  , ifreq    = [("escape", 1)]
  , iflavour = zipPlain [BrYellow]
  , iaspects = [SetFlag Durable]
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
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ VerbMsg "be caught in an updraft"
               , Teleport $ 3 + 1 `dL` 10 ]
  , idesc    = "A hidden spring, to help the unwary soar."
  , ikit     = []
  }
-- Needs to be separate from staircaseTrapUp, to make sure the item is
-- registered after up staircase (not only after down staircase)
-- so that effects are invoked in the proper order and, e.g., teleport works.
staircaseTrapDown = staircaseTrapUp
  { ifreq    = [("staircase trap down", 1)]
  , iverbHit = "open up under"
  , ieffects = [ VerbMsg "tumble down the stairwell"
               , toOrganGood "drunk" (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
pulpit = ItemKind
  { isymbol  = '?'
  , iname    = "lectern"
  , ifreq    = [("pulpit", 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "ask"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ OneOf [ CreateItem CGround "any scroll" timerNone
                       , Detect DetectAll 20
                       , toOrganBad "defenseless" $ (1 `dL` 6) * 10
                       , toOrganGood "drunk" (20 + 1 `d` 5) ]
               , Explode "PhD defense question" ]
  , idesc    = "A dark wood stand, where strange priests once preached."
  , ikit     = []
  }
shallowWater = ItemKind
  { isymbol  = '~'
  , iname    = "shallow water"
  , ifreq    = [("shallow water", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "impede"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ParalyzeInWater 2]
  , idesc    = ""
  , ikit     = []
  }
straightPath = ItemKind
  { isymbol  = '.'
  , iname    = "straight path"
  , ifreq    = [("straight path", 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [InsertMove 2]
  , idesc    = ""
  , ikit     = []
  }
frozenGround = ItemKind
  { isymbol  = '.'
  , iname    = "shade"
  , ifreq    = [("frozen ground", 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 50  -- very thick ice and refreezes
  , irarity  = [(1, 1)]
  , iverbHit = "betray"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ELabel "of ice"]
                 -- no Durable or some items would be impossible to pick up
  , ieffects = [PushActor (ThrowMod 400 10 1)]
                  -- the high speed represents gliding rather than flying
                  -- and so no need to lift actor's weight off the ground;
                  -- low linger comes from abrupt halt over normal surface
  , idesc    = ""
  , ikit     = []
  }
