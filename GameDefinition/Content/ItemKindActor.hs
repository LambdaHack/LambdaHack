-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor ( actors ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, adventurer, blacksmith, forester, scientist, soldier, sniper, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, torsor, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, hornetSwarm, thornbush, geyser]

warrior,    adventurer, blacksmith, forester, scientist, soldier, sniper, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, torsor, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, hornetSwarm, thornbush, geyser :: ItemKind

-- * Hunams

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "warrior"  -- modified if in hero faction
  , ifreq    = [("hero", 100), ("civilian", 100)]
  , iflavour = zipPlain [BrBlack]  -- modified if in hero faction
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 60  -- partially from clothes and assumed first aid
               , AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("fist", COrgan), ("foot", COrgan), ("eye 5", COrgan)]
  }
adventurer = warrior
  { iname    = "adventurer" }
blacksmith = warrior
  { iname    = "blacksmith" }
forester = warrior
  { iname    = "forester" }
scientist = warrior
  { iname    = "scientist" }

soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier", 100)]
  , ikit     = ikit warrior ++ [("starting weapon", CEqp)]
  }
sniper = warrior
  { iname    = "sniper"
  , ifreq    = [("sniper", 100)]
  , ikit     = ikit warrior
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("any arrow", CInv), ("any arrow", CInv)
                  , ("any arrow", CInv), ("any arrow", CInv)
                  , ("flask", CInv), ("light source", CInv)
                  , ("light source", CInv), ("light source", CInv) ]
  }

clerk = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 100)] }
hairdresser = clerk
  { iname    = "hairdresser" }
lawyer = clerk
  { iname    = "lawyer" }
peddler = clerk
  { iname    = "peddler" }
taxCollector = clerk
  { iname    = "tax collector" }

-- * Monsters

eye = ItemKind
  { isymbol  = 'e'
  , iname    = "reducible eye"
  , ifreq    = [("monster", 100), ("horror", 100)]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [("lash", COrgan), ("pupil", COrgan)]
  }
fastEye = ItemKind
  { isymbol  = 'j'
  , iname    = "injective jaw"
  , ifreq    = [("monster", 100), ("horror", 100)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(1, 3), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Hungers but never eats. Bites but never swallows. Burrows its own image through, but never carries anything back."  -- rather weak: not about injective objects, but puny, concrete, injective functions  --- where's the madness in that?
  , ikit     = [ ("tooth", COrgan), ("speed gland 10", COrgan)
               , ("lip", COrgan), ("lip", COrgan), ("vision 8", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'n'
  , iname    = "point-free nose"
  , ifreq    = [("monster", 100), ("horror", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(1, 8), (3, 0), (4, 5), (10, 5)]  -- ensure explorers at lvl 3
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.fromList [(AbProject, -1), (AbApply, -1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "No mouth, yet it devours everything around, constantly sniffing itself inward; pure movement structure, no constant point to focus one's maddened gaze on."
  , ikit     = [("nose tip", COrgan), ("lip", COrgan), ("nostril", COrgan)]
  }
elbow = ItemKind
  { isymbol  = 'e'
  , iname    = "commutative elbow"
  , ifreq    = [("monster", 100), ("horror", 100)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(6, 1), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 8, AddMaxCalm 90, AddSpeed 26
               , AddSkills
                 $ EM.fromList [(AbProject, 1), (AbApply, 1), (AbMelee, -1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An arm strung like a bow. A few edges, but none keen enough. A few points, but none piercing. Deadly objects zip out of the void."
  , ikit     = [ ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("vision 14", COrgan)
               , ("any arrow", CInv), ("any arrow", CInv)
               , ("any arrow", CInv), ("any arrow", CInv) ]
  }
torsor = ItemKind
  { isymbol  = 'T'
  , iname    = "The Forgetful Torsor"
  , ifreq    = [("monster", 100), ("horror", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(9, 0), (10, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 100, AddMaxCalm 100, AddSpeed 10
               , AddSkills $ EM.fromList
                   [(AbProject, 1), (AbApply, 1), (AbTrigger, -1)] ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A principal homogeneous manifold, that acts freely and with enormous force, but whose stabilizers are trivial, making it rather helpless without a support group."
  , ikit     = [ ("right torsion", COrgan),  ("left torsion", COrgan)
               , ("pupil", COrgan), ("pupil", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv) ]
  }
-- "ground x" --- for immovable monster that can only tele or prob travel
-- pullback
-- skeletal

-- * Animals

-- They need rather strong melee, because they don't use items.
-- Unless/until they level up.

animalSkillMalus :: Skills
animalSkillMalus =
  EM.fromList $ zip [AbDisplace, AbMoveItem, AbProject, AbApply] [-1, -1..]

armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 35, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("claw", COrgan), ("snout", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan), ("eye 2", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 5", COrgan), ("nostril", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 25, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom fang", COrgan)
               , ("eye 5", COrgan), ("nostril", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 16
               , AddSkills animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan), ("small claw", COrgan)
               , ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(4, 6), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 30
               , AddSkills animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("jaw", COrgan), ("eye 5", COrgan), ("nostril", COrgan)]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 17
               , AddSkills animalSkillMalus ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 5", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(2, 0), (3, 1000), (4, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 70, AddMaxCalm 60, AddSpeed 25
               , AddSkills $ EM.insert AbTrigger (-1) animalSkillMalus ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan), ("eye 2", COrgan)
               , ("horn", COrgan), ("snout", COrgan) ]
  }

-- * Non-animal animals

hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("sting", COrgan), ("vision 4", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 'b'
  , iname    = "thornbush"
  , ifreq    = [("animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 2), (10, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..]) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("thorn", COrgan), ("armored skin", COrgan)]
  }
geyser = ItemKind
  { isymbol  = 'g'
  , iname    = "geyser"
  , ifreq    = [("animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(5, 2), (10, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 5
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 80, AddArmorRanged 80 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("vent", COrgan), ("fissure", COrgan)]
  }
