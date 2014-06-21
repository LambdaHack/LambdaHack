-- | Monsters and heroes for LambdaHack.
module Content.ActorKind ( cdefs ) where

import Data.List

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
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
  , acolor   = BrBlack  -- modified if in hero faction
  , amaxHP   = 50
  , amaxCalm = 50
  , aspeed   = 20
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 3  -- no via eyes, but feel, hearing, etc.
  , asmell   = 0
  , aitems   = [("fist", CBody), ("foot", CBody), ("eye 9", CBody)]
  , afreq    = [("hero", 1), ("civilian", 1)]
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
  , acolor   = BrWhite
  , amaxHP   = 0
  , amaxCalm = 0
  , aspeed   = 0
  , aAbility = []
  , aArmor   = 0
  , asight   = 0
  , asmell   = 0
  , aitems   = []
  , afreq    = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  }

eye = ActorKind
  { asymbol  = 'e'
  , aname    = "reducible eye"
  , acolor   = BrRed
  , amaxHP   = 25
  , amaxCalm = 50
  , aspeed   = 20
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 12
  , asmell   = 0
  , aitems   = [("lash", CBody), ("pupil", CBody)]
  , afreq    = [("monster", 60), ("horror", 60)]
  }
fastEye = ActorKind
  { asymbol  = 'e'
  , aname    = "super-fast eye"
  , acolor   = BrBlue
  , amaxHP   = 6
  , amaxCalm = 50
  , aspeed   = 40
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 12
  , asmell   = 0
  , aitems   = [ ("lash", CBody), ("tentacle", CBody), ("tentacle", CBody)
               , ("speed gland 5", CBody), ("pupil", CBody) ]
  , afreq    = [("monster", 15), ("horror", 15)]
  }
nose = ActorKind
  { asymbol  = 'n'
  , aname    = "point-free nose"
  , acolor   = Green
  , amaxHP   = 35
  , amaxCalm = 50
  , aspeed   = 18
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 0  -- depends solely on smell
  , asmell   = 3
  , aitems   = [("nose tip", CBody), ("lip", CBody)]
  , afreq    = [("monster", 20), ("horror", 20)]
  }
elbow = ActorKind
  { asymbol  = 'e'
  , aname    = "ground elbow"
  , acolor   = Magenta
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = 15
  , aAbility = delete AbMelee [minBound..maxBound]
  , aArmor   = 0
  , asight   = 4  -- can always shoot
  , asmell   = 0
  , aitems   = [ ("eye 12", CBody), ("armored skin", CBody)
               , ("speed gland 2", CBody)
               , ("any scroll", CInv), ("any scroll", CInv)
               , ("any scroll", CInv)
               , ("any arrow", CInv), ("any arrow", CInv), ("any arrow", CInv) ]
  , afreq    = [("monster", 10), ("horror", 20)]
  }

armadillo = ActorKind
  { asymbol  = 'a'
  , aname    = "giant armadillo"
  , acolor   = Brown
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = 18
  , aAbility = delete AbTrigger [minBound..maxBound]
  , aArmor   = 0
  , asight   = 3
  , asmell   = 0
  , aitems   = [ ("claw", CBody), ("snout", CBody), ("armored skin", CBody)
               , ("nostril", CBody) ]
  , afreq    = [("animal", 10), ("horror", 10)]
  }
gilaMonster = ActorKind
  { asymbol  = 'g'
  , aname    = "Gila monster"
  , acolor   = BrYellow
  , amaxHP   = 15
  , amaxCalm = 50
  , aspeed   = 15
  , aAbility = delete AbTrigger [minBound..maxBound]
  , aArmor   = 0
  , asight   = 3
  , asmell   = 0
  , aitems   = [ ("venom tooth", CBody), ("small claw", CBody)
               , ("speed gland 1", CBody)
               , ("eye 9", CBody), ("nostril", CBody) ]
  , afreq    = [("animal", 10), ("horror", 10)]
  }
komodoDragon = ActorKind  -- bad hearing
  { asymbol  = 'd'
  , aname    = "Komodo dragon"
  , acolor   = Blue
  , amaxHP   = 40
  , amaxCalm = 50
  , aspeed   = 25
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 3
  , asmell   = 0
  , aitems   = [ ("large tail", CBody), ("jaw", CBody), ("small claw", CBody)
               , ("speed gland 2", CBody), ("armored skin", CBody)
               , ("eye 3", CBody), ("nostril", CBody) ]
  , afreq    = [("animal", 10), ("horror", 10)]
  }
hyena = ActorKind
  { asymbol  = 'h'
  , aname    = "spotted hyena"
  , acolor   = Red
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = 35
  , aAbility = [minBound..maxBound]
  , aArmor   = 0
  , asight   = 3
  , asmell   = 0
  , aitems   = [("jaw", CBody), ("eye 9", CBody), ("nostril", CBody)]
  , afreq    = [("animal", 20), ("horror", 20)]
  }
alligator = ActorKind
  { asymbol  = 'a'
  , aname    = "alligator"
  , acolor   = Blue
  , amaxHP   = 50
  , amaxCalm = 50
  , aspeed   = 17
  , aAbility = [minBound..maxBound]
  , aArmor   = 0  -- TODO: add more, when it's not a drawback
  , asight   = 3
  , asmell   = 0
  , aitems   = [ ("large jaw", CBody), ("large tail", CBody), ("claw", CBody)
               , ("armored skin", CBody), ("eye 9", CBody) ]
  , afreq    = [("animal", 10), ("horror", 10)]
  }
thornbush = ActorKind
  { asymbol  = 't'
  , aname    = "thornbush"
  , acolor   = Brown
  , amaxHP   = 30
  , amaxCalm = 50
  , aspeed   = 20
  , aAbility = [AbWait, AbMelee]
  , aArmor   = 50
  , asight   = 0
  , asmell   = 0
  , aitems   = [("thorn", CBody)]
  , afreq    = [("animal", 10), ("horror", 10)]
  }
