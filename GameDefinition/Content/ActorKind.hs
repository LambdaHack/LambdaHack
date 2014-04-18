-- | Monsters and heroes for LambdaHack.
module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind

cdefs :: ContentDef ActorKind
cdefs = ContentDef
  { getSymbol = asymbol
  , getName = aname
  , getFreq = afreq
  , validate = validateActorKind
  , content =
      [hero, projectile, eye, fastEye, nose, armadillo, gilaMonster, komodoDragon, hyena, alligator]
  }
hero,        projectile, eye, fastEye, nose, armadillo, gilaMonster, komodoDragon, hyena, alligator :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "civilian"  -- modified, except in the civilian faction
  , afreq   = [("hero", 1)]
  , acolor  = BrBlack  -- modified, except in the civilian faction
  , ahp     = 50
  , acalm   = 50
  , aspeed  = toSpeed 2
  , asight  = True
  , asmell  = False
  , aiq     = 15
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }

projectile = ActorKind  -- includes homing missiles
  { asymbol = '*'
  , aname   = "projectile"
  , afreq   = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite
  , ahp     = 0
  , acalm   = 0
  , aspeed  = toSpeed 0
  , asight  = False
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = []
  }

eye = ActorKind
  { asymbol = 'e'
  , aname   = "reducible eye"
  , afreq   = [("monster", 60), ("horror", 60)]
  , acolor  = BrRed
  , ahp     = 7 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 2
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }
fastEye = ActorKind
  { asymbol = 'e'
  , aname   = "super-fast eye"
  , afreq   = [("monster", 15), ("horror", 15)]
  , acolor  = BrBlue
  , ahp     = d 6
  , acalm   = 50
  , aspeed  = toSpeed 4
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 10  -- Regenerates fast (at max HP most of the time!).
  , acanDo  = [minBound..maxBound]
  }
nose = ActorKind
  { asymbol = 'n'
  , aname   = "point-free nose"
  , afreq   = [("monster", 20), ("horror", 20)]
  , acolor  = Green
  , ahp     = 17 * d 2
  , acalm   = 50
  , aspeed  = toSpeed 1.8
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }
armadillo = ActorKind
  { asymbol = 'a'
  , aname   = "giant armadillo"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Brown
  , ahp     = 17 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.8
  , asight  = True
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }
gilaMonster = ActorKind
  { asymbol = 'g'
  , aname   = "Gila monster"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = BrYellow
  , ahp     = 5 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.5
  , asight  = True
  , asmell  = False
  , aiq     = 0
  , aregen  = 20
  , acanDo  = [minBound..maxBound]
  }
komodoDragon = ActorKind
  { asymbol = 'd'
  , aname   = "Komodo dragon"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Blue
  , ahp     = 17 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 2.5
  , asight  = True
  , asmell  = False
  , aiq     = 0
  , aregen  = 40
  , acanDo  = [minBound..maxBound]
  }
hyena = ActorKind
  { asymbol = 'h'
  , aname   = "spotted hyena"
  , afreq   = [("animal", 20), ("horror", 20)]
  , acolor  = Red
  , ahp     = 7 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 3.5
  , asight  = True
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }
alligator = ActorKind
  { asymbol = 'a'
  , aname   = "alligator"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Blue
  , ahp     = 27 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.7
  , asight  = True
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = [minBound..maxBound]
  }
