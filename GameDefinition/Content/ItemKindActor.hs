-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( -- * Group name patterns
    pattern S_WOODEN_TORCH, pattern S_SANDSTONE_ROCK
  , pattern HERO, pattern SCOUT_HERO, pattern RANGER_HERO, pattern ESCAPIST_HERO, pattern AMBUSHER_HERO, pattern BRAWLER_HERO, pattern SOLDIER_HERO, pattern CIVILIAN, pattern MONSTER, pattern MOBILE_MONSTER, pattern SCOUT_MONSTER, pattern ANIMAL, pattern MOBILE_ANIMAL, pattern IMMOBILE_ANIMAL, pattern INSECT, pattern GEOPHENOMENON
  , pattern ADD_SIGHT, pattern ARMOR_RANGED, pattern ADD_NOCTO_1, pattern WEAK_ARROW, pattern LIGHT_ATTENUATOR, pattern FIREPROOF_CLOTH, pattern RING_OF_OPPORTUNITY_SNIPER, pattern ANY_ARROW, pattern STARTING_ARMOR, pattern STARTING_WEAPON, pattern GEM
  , actorsGN, actorsGNSingleton
  , -- * Content
    actors
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindOrgan
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

actorsGNSingleton :: [GroupName ItemKind]
actorsGNSingleton =
       [S_WOODEN_TORCH, S_SANDSTONE_ROCK]

pattern S_WOODEN_TORCH, S_SANDSTONE_ROCK :: GroupName ItemKind

actorsGN :: [GroupName ItemKind]
actorsGN =
       [HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, INSECT, GEOPHENOMENON]
    ++ [ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_ATTENUATOR, FIREPROOF_CLOTH, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_ARMOR, STARTING_WEAPON, GEM]

pattern HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, INSECT, GEOPHENOMENON :: GroupName ItemKind

pattern ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_ATTENUATOR, FIREPROOF_CLOTH, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_ARMOR, STARTING_WEAPON, GEM :: GroupName ItemKind

pattern HERO = GroupName "adventurer"
pattern SCOUT_HERO = GroupName "scout"
pattern RANGER_HERO = GroupName "ranger"
pattern ESCAPIST_HERO = GroupName "escapist"
pattern AMBUSHER_HERO = GroupName "ambusher"
pattern BRAWLER_HERO = GroupName "brawler"
pattern SOLDIER_HERO = GroupName "soldier"
pattern CIVILIAN = GroupName "civilian"
pattern MONSTER = GroupName "monstrosity"
pattern MOBILE_MONSTER = GroupName "mobile monstrosity"
pattern SCOUT_MONSTER = GroupName "scout monstrosity"
pattern ANIMAL = GroupName "animal"
pattern MOBILE_ANIMAL = GroupName "mobile animal"
pattern IMMOBILE_ANIMAL = GroupName "immobile animal"
pattern INSECT = GroupName "insect"
pattern GEOPHENOMENON = GroupName "geological phenomenon"

pattern S_WOODEN_TORCH = GroupName "wooden torch"
pattern S_SANDSTONE_ROCK = GroupName "sandstone rock"

pattern ADD_SIGHT = GroupName "sight improvement"
pattern ARMOR_RANGED = GroupName "ranged armor"
pattern ADD_NOCTO_1 = GroupName "noctovision improvement"
pattern WEAK_ARROW = GroupName "weak arrow"
pattern LIGHT_ATTENUATOR = GroupName "light attenuator"
pattern FIREPROOF_CLOTH = GroupName "fireproof cloth"
pattern RING_OF_OPPORTUNITY_SNIPER = GroupName "ring of sniper"
pattern ANY_ARROW = GroupName "arrow"
pattern STARTING_ARMOR = GroupName "starting armor"
pattern STARTING_WEAPON = GroupName "starting weapon"
pattern GEM = GroupName "gem"

-- * Content

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, brawler, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- LH-specific
  ++ [geyserBoiling, geyserArsenic, geyserSulfur]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, brawler, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- LH-specific
geyserBoiling, geyserArsenic, geyserSulfur :: ItemKind

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

humanOrgans :: [(GroupName ItemKind, CStore)]
humanOrgans = [ (S_FIST, COrgan), (S_FOOT, COrgan)
              , (S_EYE_6, COrgan), (S_EAR_3, COrgan)
              , (S_SAPIENT_BRAIN, COrgan) ]
warrior = ItemKind
  { isymbol  = toContentSymbol '@'
  , iname    = "warrior"  -- modified if initial actors in hero faction
  , ifreq    = [(HERO, 100), (MOBILE, 1)]
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
               , AddSkill SkWait 1  -- can lurk
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 2  -- can even apply periodic items
               , AddSkill SkOdor 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = ""  -- "A hardened veteran of combat."
  , ikit     = humanOrgans
               ++ [(S_SANDSTONE_ROCK, CStash)]
  }
warrior2 = warrior
  { iname    = "adventurer"
  , ikit     = humanOrgans
               ++ [(COMMON_ITEM, CStash)]
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
  , ifreq    = [(SCOUT_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (ADD_NOCTO_1, CStash) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [(RANGER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [(ESCAPIST_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (ADD_SIGHT, CEqp)
                  , (STARTING_ARMOR, CEqp)
                  , (WEAK_ARROW, CStash)  -- mostly for probing
                  , (LIGHT_ATTENUATOR, CStash)
                  , (S_WOODEN_TORCH, CStash)
                  , (FIREPROOF_CLOTH, CStash) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [(AMBUSHER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- dark and numerous, so more kit without exploring
               ++ [ (RING_OF_OPPORTUNITY_SNIPER, CEqp)
                  , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
                  , (WEAK_ARROW, CStash)
                  , (EXPLOSIVE, CStash)
                  , (LIGHT_ATTENUATOR, CEqp)
                  , (S_WOODEN_TORCH, CStash) ]
  -- , idesc    = ""
  }
brawler = warrior
  { iname    = "brawler"
  , ifreq    = [(BRAWLER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [(STARTING_WEAPON, CEqp)]
  -- , idesc    = ""
  }
soldier = brawler
  { iname    = "soldier"
  , ifreq    = [(SOLDIER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit brawler
               ++ [(EXPLOSIVE, CStash)]
  -- , idesc    = ""
  }

civilian = warrior
  { iname    = "clerk"
  , ifreq    = [(CIVILIAN, 100), (MOBILE, 1)]
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
  { isymbol  = toContentSymbol 'e'
  , iname    = "reducible eye"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 10) ]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(3, 0), (4, 10), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 16, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [ (S_LASH, COrgan), (S_PUPIL, COrgan)  -- at least one non-timed
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no hearing, it's all eyes
  }
fastEye = ItemKind
  { isymbol  = toContentSymbol 'j'
  , iname    = "injective jaw"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3, 0), (4, 6), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 5, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Hungers but never eats. Bites but never swallows. Burrows its own image through, but never carries anything back."  -- rather weak: not about injective objects, but puny, concrete, injective functions  --- where's the madness in that?
  , ikit     = [ (S_TOOTH, COrgan), (S_LIP, COrgan)  -- at least one non-timed
               , (S_SPEED_GLAND_10, COrgan)
               , (S_VISION_6, COrgan), (S_EAR_3, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = toContentSymbol 'n'
  , iname    = "point-free nose"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1), (MOBILE_MONSTER, 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(3, 0), (4, 5), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1)  -- can't project
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "No mouth, yet it devours everything around, constantly sniffing itself inward; pure movement structure, no constant point to focus one's maddened gaze on."
  , ikit     = [ (S_TIP, COrgan), (S_LIP, COrgan)  -- at least one non-timed
               , (S_NOSTRIL, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = toContentSymbol 'e'
  , iname    = "commutative elbow"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 0), (4, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An arm strung like a bow. A few edges, but none keen enough. A few points, but none piercing. Deadly objects zip out of the void."
  , ikit     = [ (S_SPEED_GLAND_5, COrgan), (S_BARK, COrgan)
               , (S_VISION_12, COrgan), (S_EAR_8, COrgan)
                   -- too powerful to get stronger sight
               , (S_SAPIENT_BRAIN, COrgan)
               , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
               , (WEAK_ARROW, CStash), (WEAK_ARROW, CStash) ]
  }
torsor = ItemKind
  { isymbol  = toContentSymbol 'T'
  , iname    = "The Forgetful Torsor"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(9, 0), (10, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 300, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkAlter (-1)  -- can't exit the gated level; a boss,
                                        -- but can dig rubble, ice
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A principal homogeneous manifold, that acts freely and with enormous force, but whose stabilizers are trivial, making it rather helpless without a support group."
  , ikit     = [ (S_RIGHT_TORSION, COrgan), (S_LEFT_TORSION, COrgan)
               , (S_PUPIL, COrgan)
               , (S_TENTACLE, COrgan)  -- low timeout, so rarely a stall
               , (S_EAR_8, COrgan)
               , (S_SAPIENT_BRAIN, COrgan)
               , (GEM, CStash), (GEM, CStash)
               , (GEM, CStash), (GEM, CStash) ]
  }
-- "ground x" --- for immovable monster that can only tele or prob travel
-- pullback
-- skeletal

-- * Animals

-- They need rather strong melee, because they don't use items.
-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller, slower and nosy hyena
  { isymbol  = toContentSymbol 'j'
  , iname    = "golden jackal"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 50) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 24, AddSkill SkNocto 2
               , AddSkill SkAggression 2  -- scout
               , AddSkill SkDisplace 1  -- scout
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ (S_SMALL_JAW, COrgan)
               , (S_EYE_6, COrgan), (S_NOSTRIL, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
griffonVulture = ItemKind  -- keep it boring and weak, because it summons
  { isymbol  = toContentSymbol 'v'
  , iname    = "griffon vulture"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 30) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 3), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 80
                   -- enough Calm to summon twice only if not attacked at all;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one on level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ (S_SCREECHING_BEAK, COrgan)  -- in reality it grunts and hisses
               , (S_SMALL_CLAW, COrgan)
               , (S_EYE_8, COrgan), (S_EAR_8, COrgan)
                   -- can't shoot, so strong sight is OK
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
skunk = ItemKind
  { isymbol  = toContentSymbol 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 8), (5, 1)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 13, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ (S_SCENT_GLAND, COrgan)
               , (S_SMALL_CLAW, COrgan), (S_SNOUT, COrgan)
               , (S_EYE_3, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = toContentSymbol 'a'
  , iname    = "giant armadillo"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 54000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkHurtMelee (-70)  -- quite harmless rolled in a ball
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ (S_HOOKED_CLAW, COrgan), (S_SNOUT, COrgan)
               , (S_ARMORED_SKIN, COrgan), (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = toContentSymbol 'g'
  , iname    = "Gila monster"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 50
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ (S_VENOM_TOOTH, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = toContentSymbol 's'
  , iname    = "rattlesnake"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 7), (20, 10)]  -- common among late spawns
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 28, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 2  -- often discharged. so flees anyway
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ (S_VENOM_FANG, COrgan)  -- when discharged, it's weaponless
               , (S_RATLLE, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
hyena = ItemKind
  { isymbol  = toContentSymbol 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 5), (20, 10)]
      -- gets summoned often, so low base rarity, except among late spawns
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 23, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ (S_JAW, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_EYE_6, COrgan), (S_NOSTRIL, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = toContentSymbol 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [BrRed]  -- speedy, so bright red
  , icount   = 1
  , irarity  = [(9, 0), (10, 11), (20, 20)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60  -- regens
               , AddSkill SkSpeed 17, AddSkill SkNocto 2
               , AddSkill SkHurtMelee 60  -- great fighter with low cooldowns
               , AddSkill SkAggression 1  -- match the description
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds as its lesser cousins."
  , ikit     = [ (S_LARGE_TAIL, COrgan), (S_JAW, COrgan)
               , (S_LIP, COrgan), (S_FOOT, COrgan)
               , (S_SPEED_GLAND_5, COrgan), (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_3, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
alligator = ItemKind  -- late, slow, deadly semi-tank with some armor;
                      -- too deadly to get more HP; bombs the only recourse
  { isymbol  = toContentSymbol 'a'
  , iname    = "alligator"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 12), (20, 10), (40, 40)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 55, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkSwimming 100  -- swims better than walks
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An armored predator from the dawn of time. You better not get within its reach."
  , ikit     = [ (S_HUGE_TAIL, COrgan)  -- the special trick, breaking frontline
               , (S_LARGE_JAW, COrgan)
               , (S_SMALL_CLAW, COrgan)
               , (S_ARMORED_SKIN, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = toContentSymbol 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(2, 0), (3, 1000), (4, 0)]  -- an early unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 90, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 27, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-1)  -- can't use hard stairs nor dig;
                                        -- a weak miniboss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ (S_RHINO_HORN, COrgan), (S_SNOUT, COrgan)
               , (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = toContentSymbol 'b'
  , iname    = "bee swarm"
  , ifreq    = [(ANIMAL, 100), (INSECT, 50), (MOBILE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ (S_BEE_STING, COrgan)  -- weaponless when it's used up
               , (S_VISION_6, COrgan), (S_EAR_6, COrgan)
               , (S_INSECT_MORTALITY, COrgan), (S_ANIMAL_BRAIN, COrgan) ]
  }
hornetSwarm = ItemKind  -- kind of tank with armor, but short-lived
  { isymbol  = toContentSymbol 'h'
  , iname    = "hornet swarm"
  , ifreq    = [(ANIMAL, 100), (INSECT, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 4), (20, 10)]
      -- should be many, because die after a time
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkHurtMelee 50
               , AddSkill SkMaxHP 10, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ (S_STING, COrgan)  -- when on cooldown, it's weaponless
               , (S_VISION_6, COrgan), (S_EAR_6, COrgan)
               , (S_INSECT_MORTALITY, COrgan), (S_ANIMAL_BRAIN, COrgan) ]
  }
thornbush = ItemKind  -- the wimpiest kind of early tank
  { isymbol  = toContentSymbol 't'
  , iname    = "thornbush"
  , ifreq    = [(ANIMAL, 20), (IMMOBILE_ANIMAL, 20)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 13)]
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [ (S_THORN, COrgan)  -- after all run out, it's weaponless
               , (S_BARK, COrgan) ]
  }
geyserBoiling = ItemKind
  { isymbol  = toContentSymbol 'g'
  , iname    = "geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 30), (GEOPHENOMENON, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 11, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A jet of acidic water, hot enough to melt flesh."
  , ikit     = [(S_BOILING_VENT, COrgan), (S_BOILING_FISSURE, COrgan)]
  }
geyserArsenic = ItemKind
  { isymbol  = toContentSymbol 'g'
  , iname    = "arsenic geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 40), (GEOPHENOMENON, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The sharp scent betrays the poison within the spray."
  , ikit     = [(S_ARSENIC_VENT, COrgan), (S_ARSENIC_FISSURE, COrgan)]
  }
geyserSulfur = ItemKind
  { isymbol  = toContentSymbol 'g'
  , iname    = "sulfur geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 120), (GEOPHENOMENON, 1)]
  , iflavour = zipPlain [BrYellow]  -- exception, animal with bright color
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The pool boils and bubbles, stinking of rotten eggs. Despite the smell, these waters purify and strengthen."
  , ikit     = [(S_SULFUR_VENT, COrgan), (S_SULFUR_FISSURE, COrgan)]
  }
