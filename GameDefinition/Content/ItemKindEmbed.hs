-- | Definitions of items embedded in map tiles.
module Content.ItemKindEmbed
  ( -- * Group name patterns
    pattern SCRATCH_ON_WALL, pattern OBSCENE_PICTOGRAM, pattern SUBTLE_FRESCO, pattern TREASURE_CACHE, pattern TREASURE_CACHE_TRAP, pattern SIGNAGE, pattern SMALL_FIRE, pattern BIG_FIRE, pattern FROST, pattern RUBBLE, pattern DOORWAY_TRAP_UNKNOWN, pattern DOORWAY_TRAP, pattern STAIRS_UP, pattern STAIRS_DOWN, pattern ESCAPE, pattern STAIRS_TRAP_UP, pattern STAIRS_TRAP_DOWN, pattern LECTERN, pattern SHALLOW_WATER, pattern STRAIGHT_PATH, pattern FROZEN_GROUND
  , embedsGN
  , -- * Content
    embeds
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindTemporary
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

embedsGN :: [GroupName ItemKind]
embedsGN =
       [SCRATCH_ON_WALL, OBSCENE_PICTOGRAM, SUBTLE_FRESCO, TREASURE_CACHE, TREASURE_CACHE_TRAP, SIGNAGE, SMALL_FIRE, BIG_FIRE, FROST, RUBBLE, DOORWAY_TRAP_UNKNOWN, DOORWAY_TRAP, STAIRS_UP, STAIRS_DOWN, ESCAPE, STAIRS_TRAP_UP, STAIRS_TRAP_DOWN, LECTERN, SHALLOW_WATER, STRAIGHT_PATH, FROZEN_GROUND]

pattern SCRATCH_ON_WALL, OBSCENE_PICTOGRAM, SUBTLE_FRESCO, TREASURE_CACHE, TREASURE_CACHE_TRAP, SIGNAGE, SMALL_FIRE, BIG_FIRE, FROST, RUBBLE, DOORWAY_TRAP_UNKNOWN, DOORWAY_TRAP, STAIRS_UP, STAIRS_DOWN, ESCAPE, STAIRS_TRAP_UP, STAIRS_TRAP_DOWN, LECTERN, SHALLOW_WATER, STRAIGHT_PATH, FROZEN_GROUND :: GroupName ItemKind

pattern SCRATCH_ON_WALL = GroupName "scratch on wall"
pattern OBSCENE_PICTOGRAM = GroupName "obscene pictogram"
pattern SUBTLE_FRESCO = GroupName "subtle fresco"
pattern TREASURE_CACHE = GroupName "treasure cache"
pattern TREASURE_CACHE_TRAP = GroupName "treasure cache trap"
pattern SIGNAGE = GroupName "signage"
pattern SMALL_FIRE = GroupName "small fire"
pattern BIG_FIRE = GroupName "big fire"
pattern FROST = GroupName "frozen mass"
pattern RUBBLE = GroupName "rubble"
pattern DOORWAY_TRAP_UNKNOWN = GroupName "doorway trap unknown"
pattern DOORWAY_TRAP = GroupName "doorway trap"
pattern STAIRS_UP = GroupName "stairs up"
pattern STAIRS_DOWN = GroupName "stairs down"
pattern ESCAPE = GroupName "escape"
pattern STAIRS_TRAP_UP = GroupName "stairs trap up"
pattern STAIRS_TRAP_DOWN = GroupName "stairs trap down"
pattern LECTERN = GroupName "lectern"
pattern SHALLOW_WATER = GroupName "shallow water"
pattern STRAIGHT_PATH = GroupName "straight path"
pattern FROZEN_GROUND = GroupName "frozen ground"

-- * Content

embeds :: [ItemKind]
embeds =
  [scratchOnWall, obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signageExit, signageEmbed, signageMerchandise, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, stairsTrapUp, stairsTrapDown, lectern, shallowWater, straightPath, frozenGround]

scratchOnWall,    obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signageExit, signageEmbed, signageMerchandise, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, stairsTrapUp, stairsTrapDown, lectern, shallowWater, straightPath, frozenGround :: ItemKind


-- Make sure very few walls are substantially useful, e.g., caches,
-- and none that are secret. Otherwise the player will spend a lot of time
-- bumping walls, which is boring compared to fights or dialogues
-- and ever worse, the player will bump all secret walls, wasting time
-- and foregoing the fun of guessing how to find entrance to a disjoint part
-- of the level by bumping the least number of secret walls.
scratchOnWall = ItemKind
  { isymbol  = toContentSymbol '?'
  , iname    = "claw mark"
  , ifreq    = [(SCRATCH_ON_WALL, 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scratch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ VerbMsg "start making sense of the scratches" "."
               , Detect DetectHidden 3 ]
  , idesc    = "A seemingly random series of scratches, carved deep into the wall."
  , ikit     = []
  }
obscenePictogram = ItemKind
  { isymbol  = toContentSymbol '*'
  , iname    = "obscene pictogram"
  , ifreq    = [(OBSCENE_PICTOGRAM, 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "infuriate"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 7, SetFlag Durable]
  , ieffects = [ VerbMsg "enter destructive rage at the sight of an obscene pictogram" "."
               , RefillCalm (-20)
               , OneOf [ toOrganGood S_STRENGTHENED (3 + 1 `d` 2)
                       , CreateItem Nothing CGround S_SANDSTONE_ROCK timerNone ]
               ]
  , idesc    = "It's not even anatomically possible."
  , ikit     = []
  }
subtleFresco = ItemKind
  { isymbol  = toContentSymbol '*'
  , iname    = "subtle fresco"
  , ifreq    = [(SUBTLE_FRESCO, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 7, SetFlag Durable]
  , ieffects = [ VerbMsg "feel refreshed by the subtle fresco" "."
               , toOrganGood S_FAR_SIGHTED (3 + 1 `d` 2)
               , toOrganGood S_KEEN_SMELLING (3 + 1 `d` 2) ]
                 -- hearing gets a boost through bracing, so no need here
  , idesc    = "Expensive yet tasteful."
  , ikit     = []
  }
treasureCache = ItemKind
  { isymbol  = toContentSymbol 'o'
  , iname    = "treasure cache"
  , ifreq    = [(TREASURE_CACHE, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [CreateItem Nothing CGround COMMON_ITEM timerNone]
  , idesc    = "Glittering treasure, just waiting to be taken."
  , ikit     = []
  }
reliefMsg :: Effect
reliefMsg = VerbMsg "sigh with relief when nothing explodes in your face!" ""
treasureCacheTrap = ItemKind
  { isymbol  = toContentSymbol '^'
  , iname    = "cache trap"
  , ifreq    = [(TREASURE_CACHE_TRAP, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [OneOf [ toOrganBad S_BLIND (10 + 1 `d` 10)
                      , RefillCalm (-99)
                      , Explode S_FOCUSED_CONCUSSION
                      , reliefMsg, reliefMsg ]]
  , idesc    = "It's a trap!"
  , ikit     = []
  }
signageExit = ItemKind
  { isymbol  = toContentSymbol '?'
  , iname    = "inscription"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [BrGreen]
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
signageEmbed = signageExit
  { iname    = "notice"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [Cyan]
  , ieffects = [Detect DetectEmbed 12]
  , idesc    = "The battered poster is untitled and unsigned."
  }
signageMerchandise = signageExit
  { iname    = "treasure map"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [BrCyan]
  , ieffects = [Detect DetectLoot 20]
  , idesc    = "In equal parts cryptic and promising."
  }
fireSmall = ItemKind
  { isymbol  = toContentSymbol 'o'
  , iname    = "small fire"
  , ifreq    = [(SMALL_FIRE, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [Burn 1, Explode S_SINGLE_SPARK]
  , idesc    = "A few small logs, burning brightly."
  , ikit     = []
  }
fireBig = fireSmall
  { isymbol  = toContentSymbol '0'
  , iname    = "big fire"
  , ifreq    = [(BIG_FIRE, 1)]
  , iflavour = zipPlain [Red]
  , ieffects = [ Burn 2
               , CreateItem Nothing CGround S_WOODEN_TORCH timerNone
               , Explode S_SPARK ]
  , idesc    = "Glowing with light and warmth."
  , ikit     = []
  }
frost = ItemKind
  { isymbol  = toContentSymbol '^'
  , iname    = "frost"
  , ifreq    = [(FROST, 1)]
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
  { isymbol  = toContentSymbol '&'
  , iname    = "rubble"
  , ifreq    = [(RUBBLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [OneOf [ Explode S_FOCUSED_GLASS_HAIL
                      , Summon MOBILE_ANIMAL $ 1 `dL` 2
                      , toOrganNoTimer S_POISONED
                      , CreateItem Nothing CGround ANY_ARROW timerNone
                      , CreateItem Nothing CGround STARTING_WEAPON timerNone
                      , reliefMsg, reliefMsg, reliefMsg
                      , reliefMsg, reliefMsg, reliefMsg ]]
  , idesc    = "Broken chunks of rock and glass."
  , ikit     = []
  }
doorwayTrapTemplate = ItemKind
  { isymbol  = toContentSymbol '+'
  , iname    = "doorway trap"
  , ifreq    = [(DOORWAY_TRAP_UNKNOWN, 1), (DOORWAY_TRAP, 0)]
      -- the void group needed to pick the item for tile triggering
      -- even when not yet identified
  , iflavour = zipPlain brightCol
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cripple"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [PresentAs DOORWAY_TRAP_UNKNOWN]
      -- not Durable, springs at most once
  , ieffects = []
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
doorwayTrap1 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 50)]
  , ieffects = [toOrganBad S_BLIND $ (1 `dL` 4) * 5]
  -- , idesc    = ""
  }
doorwayTrap2 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 25)]
  , ieffects = [toOrganBad S_SLOWED $ (1 `dL` 4) * 10]
  -- , idesc    = ""
  }
doorwayTrap3 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 25)]
  , ieffects = [toOrganBad S_WEAKENED $ (1 `dL` 4) * 10 ]
  -- , idesc    = ""
  }
stairsUp = ItemKind
  { isymbol  = toContentSymbol '<'
  , iname    = "flight"
  , ifreq    = [(STAIRS_UP, 1)]
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
  { isymbol  = toContentSymbol '>'
  , ifreq    = [(STAIRS_DOWN, 1)]
  , ieffects = [Ascend False]
  , idesc    = ""
  }
escape = stairsUp
  { isymbol  = toContentSymbol '>'
  , iname    = "way"
  , ifreq    = [(ESCAPE, 1)]
  , iflavour = zipPlain [BrGreen]
  , iaspects = [SetFlag Durable]
  , ieffects = [Escape]
  , idesc    = "May this nightmare have an end?"
                 -- generic escape, so the text should be too;
                 -- for moon outdoors, spaceship, everywhere
  }
stairsTrapUp = ItemKind
  { isymbol  = toContentSymbol '^'
  , iname    = "staircase trap"
  , ifreq    = [(STAIRS_TRAP_UP, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "buffet"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ VerbMsgFail "be caught in an updraft" "."
               , Teleport $ 3 + 1 `dL` 10 ]
  , idesc    = "A hidden spring, to help the unwary soar."
  , ikit     = []
  }
-- Needs to be separate from stairsTrapUp, to make sure the item is
-- registered after up stairs (not only after down stairs)
-- so that effects are invoked in the proper order and, e.g., teleport works.
stairsTrapDown = stairsTrapUp
  { ifreq    = [(STAIRS_TRAP_DOWN, 1)]
  , iflavour = zipPlain [Red]
  , iverbHit = "open up under"
  , ieffects = [ VerbMsgFail "tumble down the stairwell" "."
               , toOrganGood S_DRUNK (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
lectern = ItemKind
  { isymbol  = toContentSymbol '?'
  , iname    = "lectern"
  , ifreq    = [(LECTERN, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "ask"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ OneOf [ CreateItem Nothing CGround ANY_SCROLL timerNone
                       , Detect DetectAll 20
                       , toOrganBad S_DEFENSELESS $ (1 `dL` 6) * 10
                       , toOrganGood S_DRUNK (20 + 1 `d` 5) ]
               , Explode S_DEFENSELESSNESS_RUNOUT ]
  , idesc    = "A dark wood stand, where strange priests once preached."
  , ikit     = []
  }
shallowWater = ItemKind
  { isymbol  = toContentSymbol '~'
  , iname    = "shallow water"
  , ifreq    = [(SHALLOW_WATER, 1)]
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
  { isymbol  = toContentSymbol '.'
  , iname    = "straight path"
  , ifreq    = [(STRAIGHT_PATH, 1)]
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
  { isymbol  = toContentSymbol '.'
  , iname    = "shade"
  , ifreq    = [(FROZEN_GROUND, 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 10  -- very thick ice and refreezes, but not too large and boring
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
