-- | Monsters and heroes for LambdaHack.
module Content.ActorKind ( cdefs ) where

import Data.List

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
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
      [warrior, adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, thornbush]
  }
warrior,        adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, thornbush :: ActorKind

warrior = ActorKind
  { asymbol  = '@'
  , aname    = "warrior"  -- modified if in hero faction
  , afreq    = [("hero", 1), ("civilian", 1)]
  , acolor   = BrBlack  -- modified if in hero faction
  , amaxHP   = 50
  , amaxCalm = 50
  , aspeed   = toSpeed 2
  , aAbility = [minBound..maxBound]
  , aitems   = [("fist", CBody), ("foot", CBody), ("eye 12", CBody)]
  }
adventurer = warrior
  { aname    = "adventurer" }
blacksmith = warrior
  { aname    = "blacksmith"  }
forester = warrior
  { aname    = "forester"  }

clerk = warrior
  { aname    = "clerk"
  , afreq    = [("civilian", 1)] }
hairdresser = clerk
  { aname    = "hairdresser" }
lawyer = clerk
  { aname    = "lawyer" }
peddler = clerk
  { aname    = "peddler" }
taxCollector = clerk
  { aname    = "tax collector" }

projectile = ActorKind  -- includes homing missiles
  { asymbol  = '*'
  , aname    = "projectile"
  , afreq    = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor   = BrWhite
  , amaxHP   = 0
  , amaxCalm = 0
  , aspeed   = toSpeed 0
  , aAbility = []
  , aitems   = []
  }

eye = ActorKind
  { asymbol  = 'e'
  , aname    = "reducible eye"
  , afreq    = [("monster", 60), ("horror", 60)]
  , acolor   = BrRed
  , amaxHP   = 25
  , amaxCalm = 50
  , aspeed   = toSpeed 2
  , aAbility = [minBound..maxBound]
  , aitems   = [("lash", CBody), ("tentacle", CBody), ("pupil", CBody)]
  }
fastEye = ActorKind
  { asymbol  = 'e'
  , aname    = "super-fast eye"
  , afreq    = [("monster", 15), ("horror", 15)]
  , acolor   = BrBlue
  , amaxHP   = 6
  , amaxCalm = 50
  , aspeed   = toSpeed 4
  , aAbility = [minBound..maxBound]
  , aitems   = [ ("lash", CBody), ("tentacle", CBody), ("tentacle", CBody)
               , ("speed gland 5", CBody), ("pupil", CBody) ]
  }
nose = ActorKind
  { asymbol  = 'n'
  , aname    = "point-free nose"
  , afreq    = [("monster", 20), ("horror", 20)]
  , acolor   = Green
  , amaxHP   = 35
  , amaxCalm = 50
  , aspeed   = toSpeed 1.8
  , aAbility = [minBound..maxBound]
  , aitems   = [("nose tip", CBody), ("lip", CBody), ("nostril", CBody)]
  }
elbow = ActorKind
  { asymbol  = 'e'
  , aname    = "ground elbow"
  , afreq    = [("monster", 10), ("horror", 20)]
  , acolor   = Magenta
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = toSpeed 1.5
  , aAbility = delete AbMelee [minBound..maxBound]
  , aitems   = [ ("eye 15", CBody), ("armored skin", CBody)
               , ("speed gland 2", CBody)
               , ("any scroll", CEqp), ("any scroll", CEqp)
               , ("any scroll", CEqp)
               , ("any arrow", CEqp), ("any arrow", CEqp), ("any arrow", CEqp) ]
  }

armadillo = ActorKind
  { asymbol  = 'a'
  , aname    = "giant armadillo"
  , afreq    = [("animal", 10), ("horror", 10)]
  , acolor   = Brown
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = toSpeed 1.8
  , aAbility = delete AbTrigger [minBound..maxBound]
  , aitems   = [ ("claw", CBody), ("snout", CBody), ("armored skin", CBody)
               , ("eye 3", CBody), ("nostril", CBody) ]
  }
gilaMonster = ActorKind
  { asymbol  = 'g'
  , aname    = "Gila monster"
  , afreq    = [("animal", 10), ("horror", 10)]
  , acolor   = BrYellow
  , amaxHP   = 15
  , amaxCalm = 50
  , aspeed   = toSpeed 1.5
  , aAbility = delete AbTrigger [minBound..maxBound]
  , aitems   = [ ("venom tooth", CBody), ("small claw", CBody)
               , ("speed gland 1", CBody)
               , ("eye 12", CBody), ("nostril", CBody) ]
  }
komodoDragon = ActorKind  -- bad hearing
  { asymbol  = 'd'
  , aname    = "Komodo dragon"
  , afreq    = [("animal", 10), ("horror", 10)]
  , acolor   = Blue
  , amaxHP   = 40
  , amaxCalm = 50
  , aspeed   = toSpeed 2.5
  , aAbility = [minBound..maxBound]
  , aitems   = [ ("large tail", CBody), ("jaw", CBody), ("small claw", CBody)
               , ("speed gland 2", CBody), ("armored skin", CBody)
               , ("eye 6", CBody), ("nostril", CBody) ]
  }
hyena = ActorKind
  { asymbol  = 'h'
  , aname    = "spotted hyena"
  , afreq    = [("animal", 20), ("horror", 20)]
  , acolor   = Red
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = toSpeed 3.5
  , aAbility = [minBound..maxBound]
  , aitems   = [("jaw", CBody), ("eye 12", CBody), ("nostril", CBody)]
  }
alligator = ActorKind
  { asymbol  = 'a'
  , aname    = "alligator"
  , afreq    = [("animal", 10), ("horror", 10)]
  , acolor   = Blue
  , amaxHP   = 50
  , amaxCalm = 50
  , aspeed   = toSpeed 1.7
  , aAbility = [minBound..maxBound]
  , aitems   = [ ("large jaw", CBody), ("large tail", CBody), ("claw", CBody)
               , ("armored skin", CBody), ("eye 12", CBody) ]
  }
thornbush = ActorKind
  { asymbol  = 't'
  , aname    = "thornbush"
  , afreq    = [("animal", 10), ("horror", 10)]
  , acolor   = Brown
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = toSpeed 2
  , aAbility = [AbWait, AbMelee]
  , aitems   = [ ("thorn", CBody), ("armored skin", CBody) ]
  }
