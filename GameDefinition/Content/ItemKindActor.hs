-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( actors
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush,
   geyserBoiling, geyserArsenic, geyserSulfur]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush,
   geyserBoiling, geyserArsenic, geyserSulfur :: ItemKind

-- * Hunams
warrior = ItemKind
  { isymbol  = '@'
  , iname    = "warrior"  -- modified if initial actors in hero faction
  , ifreq    = [("hero", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 80  -- partially from clothes and assumed first aid
               , AddMaxCalm 70, AddSpeed 20, AddNocto 2
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A hardened veteran of combat."
  , ikit     = [ ("fist", COrgan), ("foot", COrgan), ("eye 6", COrgan)
               , ("sapient brain", COrgan) ]
  }
warrior2 = warrior
  { iname    = "adventurer"
  -- , idesc    = ""
  }
warrior3 = warrior
  { iname    = "blacksmith"
  -- , idesc    = ""
   }
warrior4 = warrior
  { iname    = "forester"
  -- , idesc    = ""
  }
warrior5 = warrior
  { iname    = "scientist"
  -- , idesc    = ""
  }

scout = warrior
  { iname    = "scout"
  , ifreq    = [("scout hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("armor ranged", CEqp)
                  , ("add nocto 1", CInv) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [("ranger hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("weak arrow", CInv), ("armor ranged", CEqp)]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [("escapist hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("weak arrow", CInv)  -- mostly for probing
                  , ("armor ranged", CEqp)
                  , ("flask", CInv)
                  , ("light source", CInv)
                  , ("blanket", CInv) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [("ambusher hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior  -- dark and numerous, so more kit without exploring
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("light source", CEqp), ("wooden torch", CInv)
                  , ("weak arrow", CInv), ("any arrow", CSha), ("flask", CSha) ]
  -- , idesc    = ""
  }
soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("starting weapon", CEqp)]
  -- , idesc    = ""
  }

civilian = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrBlack]
  -- , idesc    = ""
  }
civilian2 = civilian
  { iname    = "hairdresser"
  -- , idesc    = ""
  }
civilian3 = civilian
  { iname    = "lawyer"
  -- , idesc    = ""
  }
civilian4 = civilian
  { iname    = "peddler"
  -- , idesc    = ""
  }
civilian5 = civilian
  { iname    = "tax collector"
  -- , idesc    = ""
  }

-- * Monsters

-- They have bright colours, because they are not natural.

eye = ItemKind
  { isymbol  = 'e'
  , iname    = "reducible eye"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 10) ]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 16, AddMaxCalm 70, AddSpeed 20, AddNocto 2
               , AddAggression 1
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [ ("lash", COrgan), ("pupil", COrgan)
               , ("sapient brain", COrgan) ]
  }
fastEye = ItemKind
  { isymbol  = 'j'
  , iname    = "injective jaw"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 5, AddMaxCalm 70, AddSpeed 30, AddNocto 2
               , AddAggression 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Hungers but never eats. Bites but never swallows. Burrows its own image through, but never carries anything back."  -- rather weak: not about injective objects, but puny, concrete, injective functions  --- where's the madness in that?
  , ikit     = [ ("tooth", COrgan), ("speed gland 10", COrgan)
               , ("lip", COrgan), ("vision 6", COrgan)
               , ("sapient brain", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'n'
  , iname    = "point-free nose"
  , ifreq    = [("monster", 100), ("mobile", 1), ("mobile monster", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(1, 5), (4, 2), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 30, AddSpeed 18, AddNocto 2
               , AddAggression 1
               , AddAbility AbProject (-1), AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "No mouth, yet it devours everything around, constantly sniffing itself inward; pure movement structure, no constant point to focus one's maddened gaze on."
  , ikit     = [ ("nose tip", COrgan), ("lip", COrgan), ("nostril", COrgan)
               , ("sapient brain", COrgan) ]
  }
elbow = ItemKind
  { isymbol  = 'e'
  , iname    = "commutative elbow"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(7, 1), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 8, AddMaxCalm 80, AddSpeed 21, AddNocto 2
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2, AddAbility AbMelee (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An arm strung like a bow. A few edges, but none keen enough. A few points, but none piercing. Deadly objects zip out of the void."
  , ikit     = [ ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("vision 16", COrgan)
               , ("any arrow", CSha), ("any arrow", CInv)
               , ("weak arrow", CInv), ("weak arrow", CInv)
               , ("sapient brain", COrgan) ]
  }
torsor = ItemKind
  { isymbol  = 'T'
  , iname    = "The Forgetful Torsor"
  , ifreq    = [("monster", 100), ("mobile", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(9, 0), (10, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 300, AddMaxCalm 100, AddSpeed 10, AddNocto 2
               , AddAggression 3, AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 1]  -- can't exit the gated level, the boss
  , ieffects = [Unique]
  , ifeature = [Durable, Identified]
  , idesc    = "A principal homogeneous manifold, that acts freely and with enormous force, but whose stabilizers are trivial, making it rather helpless without a support group."
  , ikit     = [ ("right torsion", COrgan), ("left torsion", COrgan)
               , ("pupil", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv)
               , ("sapient brain", COrgan) ]
  }
-- "ground x" --- for immovable monster that can only tele or prob travel
-- pullback
-- skeletal

-- * Animals

-- They need rather strong melee, because they don't use items.
-- Unless/until they level up.

-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller and slower hyena
  { isymbol  = 'j'
  , iname    = "golden jackal"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 50) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 70, AddSpeed 24, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ ("small jaw", COrgan), ("eye 6", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
  , iname    = "griffon vulture"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 30) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 80, AddSpeed 22, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
      -- Animals don't have leader, usually, so even if only one of level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Random double moves,
      -- on either side, are just too frustrating.
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan), ("eye 7", COrgan)
               , ("animal brain", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 22, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("nostril", COrgan), ("eye 3", COrgan)
               , ("animal brain", COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 20, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ ("hooked claw", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan), ("eye 3", COrgan)
               , ("animal brain", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 50, AddSpeed 18, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 25, AddMaxCalm 60, AddSpeed 16, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ ("venom fang", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing; regeneration makes it very powerful
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 41, AddMaxCalm 60, AddSpeed 18, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Larger and more aggressive than any other lizard."
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan)
               , ("hooked claw", COrgan), ("speed gland 4", COrgan)
               , ("armored skin", COrgan), ("eye 3", COrgan)
               , ("nostril", COrgan), ("animal brain", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 70, AddSpeed 32, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ ("jaw", COrgan), ("eye 6", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(8, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 41, AddMaxCalm 70, AddSpeed 18, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An armored predator from the dawn of time."
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan)
               , ("small claw", COrgan)
               , ("armored skin", COrgan), ("eye 6", COrgan)
               , ("animal brain", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(2, 0), (3, 1000000), (4, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 90, AddMaxCalm 60, AddSpeed 27, AddNocto 2
               , AddAggression 2
               , AddAbility AbAlter (-1) ]  -- can't switch levels, a miniboss
  , ieffects = [Unique]
  , ifeature = [Durable, Identified]
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan), ("eye 2", COrgan)
               , ("horn", COrgan), ("snout", COrgan)
               , ("animal brain", COrgan) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = 'b'
  , iname    = "bee swarm"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 8, AddMaxCalm 60
               , AddSpeed 30, AddNocto 2  -- armor in sting
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ ("bee sting", COrgan), ("vision 6", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 8, AddMaxCalm 70, AddSpeed 30, AddNocto 2
               , AddAbility AbAlter (-2)  -- can't use stairs nor doors
               , AddArmorMelee 80, AddArmorRanged 40 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ ("sting", COrgan), ("vision 8", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 20), ("immobile animal", 30)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 22, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [("thorn", COrgan), ("armored skin", COrgan)]
  }
geyserBoiling = ItemKind
  { isymbol  = 'g'
  , iname    = "geyser"
  , ifreq    = [("animal", 30), ("immobile animal", 60)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 3), (5, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 10, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1
               , AddArmorMelee 40, AddArmorRanged 20 ]  -- hard material
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A jet of acidic water, hot enough to melt flesh."
  , ikit     = [("boiling vent", COrgan), ("boiling fissure", COrgan)]
  }
geyserArsenic = ItemKind
  { isymbol  = 'g'
  , iname    = "arsenic geyser"
  , ifreq    = [("animal", 10), ("immobile animal", 40)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 10), (5, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 22
               , AddNocto 2, AddShine 3
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The sharp scent betrays the poison within the spray."
  , ikit     = [("arsenic vent", COrgan), ("arsenic fissure", COrgan)]
  }
geyserSulfur = ItemKind
  { isymbol  = 'g'
  , iname    = "sulfur geyser"
  , ifreq    = [("animal", 10), ("immobile animal", 120)]
  , iflavour = zipPlain [BrYellow]  -- exception, animal with bright color
  , icount   = 1
  , irarity  = [(1, 10), (5, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 22
               , AddNocto 2, AddShine 3
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The pool boils and bubbles, stinking of rotten eggs. Despite the smell, these waters purify and strengthen."
  , ikit     = [("sulfur vent", COrgan), ("sulfur fissure", COrgan)]
  }
