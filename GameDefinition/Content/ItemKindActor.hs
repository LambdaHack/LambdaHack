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
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- LH-specific
  ++ [geyserBoiling, geyserArsenic, geyserSulfur]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- LH-specific
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 80  -- partially from clothes and first aid
               , AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20
               , AddSkill SkNocto 2
               , AddSkill SkProject 2
               , AddSkill SkApply 1
               , AddSkill SkAlter 2
               , AddSkill SkOdor 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = ""
  -- , idesc    = "A hardened veteran of combat."
  , ikit     = [ ("fist", COrgan), ("foot", COrgan)
               , ("eye 6", COrgan), ("ear 4", COrgan)
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
  , ikit     = ikit warrior
               ++ [ ("armor ranged", CEqp)
                  , ("weak arrow", CInv) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [("escapist hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("armor ranged", CEqp)
                  , ("weak arrow", CInv)  -- mostly for probing
                  , ("light source", CInv)
                  , ("wooden torch", CInv)
                  , ("blanket", CInv) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [("ambusher hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior  -- dark and numerous, so more kit without exploring
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("any arrow", CSha)
                  , ("weak arrow", CInv)
                  , ("explosive", CSha)
                  , ("light source", CEqp)
                  , ("wooden torch", CInv) ]
  -- , idesc    = ""
  }
soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("starting weapon", CEqp)
                  , ("explosive", CSha) ]
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 16, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2, AddSkill SkApply 1, AddSkill SkAlter 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [ ("lash", COrgan), ("pupil", COrgan)
               , ("sapient brain", COrgan) ]  -- no hearing, it's all eyes
  }
fastEye = ItemKind
  { isymbol  = 'j'
  , iname    = "injective jaw"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(5, 6), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 5, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkAlter 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Hungers but never eats. Bites but never swallows. Burrows its own image through, but never carries anything back."  -- rather weak: not about injective objects, but puny, concrete, injective functions  --- where's the madness in that?
  , ikit     = [ ("tooth", COrgan), ("speed gland 10", COrgan)
               , ("lip", COrgan), ("vision 6", COrgan), ("ear 4", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1), AddSkill SkAlter 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "No mouth, yet it devours everything around, constantly sniffing itself inward; pure movement structure, no constant point to focus one's maddened gaze on."
  , ikit     = [ ("nostril", COrgan), ("nose tip", COrgan), ("lip", COrgan)
               , ("sapient brain", COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = 'e'
  , iname    = "commutative elbow"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(6, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 21, AddSkill SkNocto 2
               , AddSkill SkProject 2, AddSkill SkApply 1
               , AddSkill SkAlter 2, AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An arm strung like a bow. A few edges, but none keen enough. A few points, but none piercing. Deadly objects zip out of the void."
  , ikit     = [ ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("vision 16", COrgan), ("ear 10", COrgan)
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
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 300, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 10, AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2, AddSkill SkApply 1
               , AddSkill SkAlter 1  -- can't exit the gated level, the boss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A principal homogeneous manifold, that acts freely and with enormous force, but whose stabilizers are trivial, making it rather helpless without a support group."
  , ikit     = [ ("right torsion", COrgan), ("left torsion", COrgan)
               , ("pupil", COrgan), ("ear 9", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 24, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ ("small jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 9", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one of level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Random double moves,
      -- on either side, are just too frustrating.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan)
               , ("eye 7", COrgan), ("ear 10", COrgan)
               , ("animal brain", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 7), (10, 1)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("eye 3", COrgan), ("ear 7", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ ("hooked claw", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan), ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 50
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 5", COrgan)
               , ("animal brain", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(6, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 25, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ ("venom fang", COrgan), ("rattle", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ ("jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 9", COrgan)
               , ("animal brain", COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Red]  -- speedy, so red
  , icount   = 1
  , irarity  = [(9, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds at its lesser cousins."
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan)
               , ("hooked claw", COrgan), ("speed gland 4", COrgan)
               , ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 4", COrgan)
               , ("animal brain", COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 45, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkSwimming 100  -- swims better than walks
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An armored predator from the dawn of time. You better not get within its reach."
  , ikit     = [ ("huge tail", COrgan), ("large jaw", COrgan)
               , ("small claw", COrgan), ("armored skin", COrgan)
               , ("eye 6", COrgan), ("ear 9", COrgan)
               , ("animal brain", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(2, 0), (3, 1000), (4, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 90, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 27, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-1)  -- can't switch levels, a miniboss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan)
               , ("eye 2", COrgan), ("ear 7", COrgan)
               , ("rhino horn", COrgan), ("snout", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ ("bee sting", COrgan), ("vision 6", COrgan), ("ear 6", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 8, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ ("sting", COrgan), ("vision 8", COrgan), ("ear 7", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 18), ("immobile animal", 30)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 14)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [("thorn", COrgan), ("armored skin", COrgan)]
  }
geyserBoiling = ItemKind
  { isymbol  = 'g'
  , iname    = "geyser"
  , ifreq    = [("animal", 25), ("immobile animal", 60)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 3), (5, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 11, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A jet of acidic water, hot enough to melt flesh."
  , ikit     = [("boiling vent", COrgan), ("boiling fissure", COrgan)]
  }
geyserArsenic = ItemKind
  { isymbol  = 'g'
  , iname    = "arsenic geyser"
  , ifreq    = [("animal", 8), ("immobile animal", 40)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 10), (5, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The sharp scent betrays the poison within the spray."
  , ikit     = [("arsenic vent", COrgan), ("arsenic fissure", COrgan)]
  }
geyserSulfur = ItemKind
  { isymbol  = 'g'
  , iname    = "sulfur geyser"
  , ifreq    = [("animal", 8), ("immobile animal", 120)]
  , iflavour = zipPlain [BrYellow]  -- exception, animal with bright color
  , icount   = 1
  , irarity  = [(1, 10), (5, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The pool boils and bubbles, stinking of rotten eggs. Despite the smell, these waters purify and strengthen."
  , ikit     = [("sulfur vent", COrgan), ("sulfur fissure", COrgan)]
  }
