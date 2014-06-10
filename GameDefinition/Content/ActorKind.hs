-- | Monsters and heroes for LambdaHack.
module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind

cdefs :: ContentDef ActorKind
cdefs = ContentDef
  { getSymbol = asymbol
  , getName = aname
  , getFreq = afreq
  , validate = validateActorKind
  , content =
      [warrior, adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, armadillo, gilaMonster, komodoDragon, hyena, alligator]
  }
warrior,        adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, armadillo, gilaMonster, komodoDragon, hyena, alligator :: ActorKind

warrior = ActorKind
  { asymbol = '@'
  , aname   = "warrior"  -- modified if in hero faction
  , afreq   = [("hero", 1), ("civilian", 1)]
  , acolor  = BrBlack  -- modified if in hero faction
  , ahp     = 50
  , acalm   = 50
  , aspeed  = toSpeed 2
  , aiq     = 15
  , acanDo  = [minBound..maxBound]
  , aitems  = [("fist", CBody), ("foot", CBody), ("eye 12", CBody)]
  }
adventurer = warrior
  { aname   = "adventurer" }
blacksmith = warrior
  { aname   = "blacksmith"  }
forester = warrior
  { aname   = "forester"  }

clerk = warrior
  { aname   = "clerk"
  , afreq   = [("civilian", 1)] }
hairdresser = clerk
  { aname   = "hairdresser" }
lawyer = clerk
  { aname   = "lawyer" }
peddler = clerk
  { aname   = "peddler" }
taxCollector = clerk
  { aname   = "tax collector" }

projectile = ActorKind  -- includes homing missiles
  { asymbol = '*'
  , aname   = "projectile"
  , afreq   = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite
  , ahp     = 0
  , acalm   = 0
  , aspeed  = toSpeed 0
  , aiq     = 0
  , acanDo  = []
  , aitems  = []
  }

eye = ActorKind
  { asymbol = 'e'
  , aname   = "reducible eye"
  , afreq   = [("monster", 60), ("horror", 60)]
  , acolor  = BrRed
  , ahp     = 7 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 2
  , aiq     = 8
  , acanDo  = [minBound..maxBound]
  , aitems  = [("lash", CBody), ("tentacle", CBody), ("pupil", CBody)]
  }
fastEye = ActorKind
  { asymbol = 'e'
  , aname   = "super-fast eye"
  , afreq   = [("monster", 15), ("horror", 15)]
  , acolor  = BrBlue
  , ahp     = d 6
  , acalm   = 50
  , aspeed  = toSpeed 4
  , aiq     = 12
  , acanDo  = [minBound..maxBound]
  , aitems  = [ ("lash", CBody), ("tentacle", CBody), ("tentacle", CBody)
              , ("speed gland 5", CBody), ("pupil", CBody) ]
  }
nose = ActorKind
  { asymbol = 'n'
  , aname   = "point-free nose"
  , afreq   = [("monster", 20), ("horror", 20)]
  , acolor  = Green
  , ahp     = 17 * d 2
  , acalm   = 50
  , aspeed  = toSpeed 1.8
  , aiq     = 0
  , acanDo  = [minBound..maxBound]
  , aitems  = [("nose tip", CBody), ("lip", CBody), ("nostril", CBody)]
  }

armadillo = ActorKind
  { asymbol = 'a'
  , aname   = "giant armadillo"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Brown
  , ahp     = 8 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.8
  , aiq     = 2
  , acanDo  = [minBound..maxBound]
  , aitems  = [ ("claw", CBody), ("snout", CBody), ("armored skin", CBody)
              , ("eye 3", CBody), ("nostril", CBody) ]
  }
gilaMonster = ActorKind
  { asymbol = 'g'
  , aname   = "Gila monster"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = BrYellow
  , ahp     = 4 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.5
  , aiq     = 0
  , acanDo  = [minBound..maxBound]
  , aitems  = [ ("venom tooth", CBody), ("small claw", CBody)
              , ("speed gland 1", CBody)
              , ("eye 12", CBody), ("nostril", CBody) ]
  }
komodoDragon = ActorKind  -- bad hearing
  { asymbol = 'd'
  , aname   = "Komodo dragon"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Blue
  , ahp     = 10 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 2.5
  , aiq     = 0
  , acanDo  = [minBound..maxBound]
  , aitems  = [ ("large tail", CBody), ("jaw", CBody), ("small claw", CBody)
              , ("speed gland 2", CBody), ("armored skin", CBody)
              , ("eye 6", CBody), ("nostril", CBody) ]
  }
hyena = ActorKind
  { asymbol = 'h'
  , aname   = "spotted hyena"
  , afreq   = [("animal", 20), ("horror", 20)]
  , acolor  = Red
  , ahp     = 7 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 3.5
  , aiq     = 5
  , acanDo  = [minBound..maxBound]
  , aitems  = [("jaw", CBody), ("eye 12", CBody), ("nostril", CBody)]
  }
alligator = ActorKind
  { asymbol = 'a'
  , aname   = "alligator"
  , afreq   = [("animal", 10), ("horror", 10)]
  , acolor  = Blue
  , ahp     = 12 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 1.7
  , aiq     = 0
  , acanDo  = [minBound..maxBound]
  , aitems  = [ ("large jaw", CBody), ("large tail", CBody), ("claw", CBody)
              , ("armored skin", CBody), ("eye 12", CBody) ]
  }
