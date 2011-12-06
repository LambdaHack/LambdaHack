module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.ActorKind

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
  { ahp     = (50, 1)
  , aspeed  = 10
  , asymbol = '@'
  , aname   = "hero"
  , acolor  = BrWhite  -- Heroes white, monsters colorful.
  , asight  = True
  , asmell  = False
  , aiq     = 13  -- Can see secret doors, when he is under alien control.
  , aregen  = 1500
  , afreq   = 0  -- Does not appear randomly in the dungeon.

  }

eye = ActorKind
  { ahp     = (1, 12)  -- falls in 1--4 unarmed rounds
  , aspeed  = 10
  , asymbol = 'e'
  , acolor  = BrRed
  , aname   = "the reducible eye"
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = 1500
  , afreq   = 6
  }
fastEye = ActorKind
  { ahp     = (1, 6)  -- falls in 1--2 unarmed rounds
  , aspeed  = 4
  , asymbol = 'e'
  , acolor  = BrBlue
  , aname   = "the super-fast eye"
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 1500
  , afreq   = 1
  }
nose = ActorKind
  { ahp     = (6, 2)  -- 2--5 and in 1 round of the strongest sword
  , aspeed  = 11
  , asymbol = 'n'
  , acolor  = Green
  , aname   = "the point-free nose"
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 1500
  , afreq   = 2
  }
