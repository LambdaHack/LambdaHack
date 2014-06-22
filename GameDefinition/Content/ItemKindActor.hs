-- | Actor definitions.
module Content.ItemKindActor ( actors ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [projectile]

projectile :: ItemKind

projectile = ItemKind  -- includes homing missiles
  { isymbol  = '*'
  , iname    = "projectile"
  , ifreq    = [("projectile", 1)]  -- Does not appear randomly in the dungeon
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , iverbApply   = "ERROR, please report: iverbApply"
  , iverbProject = "ERROR, please report: iverbProject"
  , iweight  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable]
  , idesc    = ""
  , ikit     = []
  }
