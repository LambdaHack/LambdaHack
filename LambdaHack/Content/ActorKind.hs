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
      [hero, eye, fastEye, nose]
  }
hero,        eye, fastEye, nose :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "hero"
  , afreq   = [("hero", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite  -- Heroes white, monsters colorful.
  , ahp     = RollDice 50 1
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 13  -- Can see hidden doors, when he is under alien control.
  , aregen  = 1500
  }

eye = ActorKind
  { asymbol = 'e'
  , aname   = "reducible eye"
  , afreq   = [("monster", 60), ("summon", 50)]
  , acolor  = BrRed
  , ahp     = RollDice 1 12  -- Falls in 1--4 unarmed rounds.
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = 1500
  }
fastEye = ActorKind
  { asymbol = 'e'
  , aname   = "super-fast eye"
  , afreq   = [("monster", 10)]
  , acolor  = BrBlue
  , ahp     = RollDice 1 6  -- Falls in 1--2 unarmed rounds.
  , aspeed  = 4
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 1500
  }
nose = ActorKind
  { asymbol = 'n'
  , aname   = "point-free nose"
  , afreq   = [("monster", 20), ("summon", 100)]
  , acolor  = Green
  , ahp     = RollDice 6 2
  , aspeed  = 11
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 1500
  }
