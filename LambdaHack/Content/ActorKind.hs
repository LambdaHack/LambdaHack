-- | Monsters and heroes for LambdaHack.
module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Random

cdefs :: Content.CDefs ActorKind
cdefs = Content.CDefs
  { getSymbol = asymbol
  , getName = aname
  , getFreq = afreq
  , validate = avalidate
  , content =
      [hero, projectile, eye, fastEye, nose]
  }
hero,        projectile, eye, fastEye, nose :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "hero"
  , afreq   = [("hero", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite  -- Heroes white, monsters colorful.
  , ahp     = RollDice 60 1
  , aspeed  = 20
  , asight  = True
  , asmell  = False
  , aiq     = 13  -- Can see hidden doors, when he is under alien control.
  , aregen  = 5000
  }

projectile = ActorKind  -- includes homing missiles
  { asymbol = '*'
  , aname   = "projectile"
  , afreq   = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite
  , ahp     = RollDice 0 0
  , aspeed  = 0
  , asight  = False
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  }

eye = ActorKind
  { asymbol = 'e'
  , aname   = "reducible eye"
  , afreq   = [("monster", 60), ("summon", 50)]
  , acolor  = BrRed
  , ahp     = RollDice 3 4
  , aspeed  = 20
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = 1000
  }
fastEye = ActorKind
  { asymbol = 'e'
  , aname   = "super-fast eye"
  , afreq   = [("monster", 15)]
  , acolor  = BrBlue
  , ahp     = RollDice 1 4
  , aspeed  = 40
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 50  -- Regenerates fast (at max HP most of the time!).
  }
nose = ActorKind
  { asymbol = 'n'
  , aname   = "point-free nose"
  , afreq   = [("monster", 20), ("summon", 100)]
  , acolor  = Green
  , ahp     = RollDice 7 2
  , aspeed  = 18
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 1000
  }
