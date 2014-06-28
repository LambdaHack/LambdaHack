-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor ( actors ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, thornbush]

warrior,    adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, projectile, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, thornbush :: ItemKind

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "warrior"  -- modified if in hero faction
  , ifreq    = [("hero", 1), ("civilian", 1)]
  , iflavour = zipPlain [BrBlack]  -- modified if in hero faction
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 50, AddMaxCalm 50, AddSpeed 20
               , AddSight 3 ]  -- no via eyes, but feel, hearing, etc.
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("fist", COrgan), ("foot", COrgan), ("eye 4", COrgan)]
  }
adventurer = warrior
  { iname    = "adventurer" }
blacksmith = warrior
  { iname    = "blacksmith" }
forester = warrior
  { iname    = "forester" }

clerk = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 1)] }
hairdresser = clerk
  { iname    = "hairdresser" }
lawyer = clerk
  { iname    = "lawyer" }
peddler = clerk
  { iname    = "peddler" }
taxCollector = clerk
  { iname    = "tax collector" }

projectile = ItemKind  -- includes homing missiles
  { isymbol  = '*'
  , iname    = "projectile"
  , ifreq    = [("projectile", 1)]  -- Does not appear randomly in the dungeon
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = []
  }

eye = ItemKind
  { isymbol  = 'e'
  , iname    = "reducible eye"
  , ifreq    = [("monster", 60), ("horror", 60)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 25, AddMaxCalm 50, AddSpeed 20
               , AddSight 4 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("lash", COrgan), ("pupil", COrgan)]
  }
fastEye = ItemKind
  { isymbol  = 'e'
  , iname    = "super-fast eye"
  , ifreq    = [("monster", 15), ("horror", 15)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 6, AddMaxCalm 50, AddSpeed 30
               , AddSight 4 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("lash", COrgan), ("tentacle", COrgan), ("tentacle", COrgan)
               , ("speed gland 5", COrgan), ("pupil", COrgan) ]
  }
nose = ItemKind
  { isymbol  = 'n'
  , iname    = "point-free nose"
  , ifreq    = [("monster", 20), ("horror", 20)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 35, AddMaxCalm 50, AddSpeed 18
               , AddSight 0, AddSmell 3 ]  -- depends solely on smell
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("nose tip", COrgan), ("lip", COrgan)]
  }
elbow = ItemKind
  { isymbol  = 'e'
  , iname    = "ground elbow"
  , ifreq    = [("monster", 10), ("horror", 20)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 50, AddSpeed 10
               , AddSkills $ EM.singleton AbMelee (-1)
               , AddSight 15 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("armored skin", COrgan), ("speed gland 2", COrgan)
               , ("any scroll", CInv), ("any scroll", CInv)
               , ("any scroll", CInv)
               , ("any arrow", CInv), ("any arrow", CInv), ("any arrow", CInv) ]
  }

armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 10), ("horror", 10)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 50, AddSpeed 18
               , AddSkills $ EM.singleton AbTrigger (-1)
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("claw", COrgan), ("snout", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 10), ("horror", 10)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 50, AddSpeed 15
               , AddSkills $ EM.singleton AbTrigger (-1)
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing
  { isymbol  = 'd'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 10), ("horror", 10)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 50, AddSpeed 20
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan), ("small claw", COrgan)
               , ("speed gland 2", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [("animal", 20), ("horror", 20)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 50, AddSpeed 35
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("jaw", COrgan), ("eye 4", COrgan), ("nostril", COrgan)]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 10), ("horror", 10)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 50, AddMaxCalm 50, AddSpeed 17
               -- TODO: add innate armor, when it's not a drawback
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 4", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 10), ("horror", 10)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 50, AddSpeed 20
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1..])
               , AddArmorMelee 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("thorn", COrgan)]
  }
