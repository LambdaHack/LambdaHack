-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( -- * Group name patterns
    pattern HERO, pattern SCOUT_HERO, pattern RANGER_HERO, pattern ESCAPIST_HERO, pattern AMBUSHER_HERO, pattern BRAWLER_HERO, pattern SOLDIER_HERO, pattern CIVILIAN, pattern MONSTER, pattern MOBILE_MONSTER, pattern SCOUT_MONSTER, pattern ANIMAL, pattern MOBILE_ANIMAL, pattern IMMOBILE_ANIMAL
  , pattern ADD_SIGHT, pattern ARMOR_RANGED, pattern ADD_NOCTO_1, pattern WEAK_ARROW, pattern LIGHT_MANIPULATION, pattern WOODEN_TORCH, pattern BLANKET, pattern RING_OF_OPPORTUNITY_SNIPER, pattern ANY_ARROW, pattern STARTING_WEAPON, pattern GEM
  , -- * Content
    actors, actorsGN
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindOrgan
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

actorsGN :: [GroupName ItemKind]
actorsGN =
       [HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL]
    ++ [ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_MANIPULATION, WOODEN_TORCH, BLANKET, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_WEAPON, GEM]

pattern HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL :: GroupName ItemKind

pattern ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_MANIPULATION, WOODEN_TORCH, BLANKET, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_WEAPON, GEM :: GroupName ItemKind

pattern HERO = GroupName "hero"
pattern SCOUT_HERO = GroupName "scout hero"
pattern RANGER_HERO = GroupName "ranger hero"
pattern ESCAPIST_HERO = GroupName "escapist hero"
pattern AMBUSHER_HERO = GroupName "ambusher hero"
pattern BRAWLER_HERO = GroupName "brawler hero"
pattern SOLDIER_HERO = GroupName "soldier hero"
pattern CIVILIAN = GroupName "civilian"
pattern MONSTER = GroupName "monster"
pattern MOBILE_MONSTER = GroupName "mobile monster"
pattern SCOUT_MONSTER = GroupName "scout monster"
pattern ANIMAL = GroupName "animal"
pattern MOBILE_ANIMAL = GroupName "mobile animal"
pattern IMMOBILE_ANIMAL = GroupName "immobile animal"

pattern ADD_SIGHT = GroupName "add sight"
pattern ARMOR_RANGED = GroupName "armor ranged"
pattern ADD_NOCTO_1 = GroupName "add nocto 1"
pattern WEAK_ARROW = GroupName "weak arrow"
pattern LIGHT_MANIPULATION = GroupName "light manipulation"
pattern WOODEN_TORCH = GroupName "wooden torch"
pattern BLANKET = GroupName "blanket"
pattern RING_OF_OPPORTUNITY_SNIPER = GroupName "ring of opportunity sniper"
pattern ANY_ARROW = GroupName "any arrow"
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

warrior = ItemKind
  { isymbol  = '@'
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
  , ikit     = [ (FIST, COrgan), (FOOT, COrgan)
               , (EYE_6, COrgan), (EAR_3, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]
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
  , ifreq    = [(SCOUT_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (ADD_NOCTO_1, CStash) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [(RANGER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior
               ++ [ (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [(ESCAPIST_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash)  -- mostly for probing
                  , (LIGHT_MANIPULATION, CStash)
                  , (WOODEN_TORCH, CStash)
                  , (BLANKET, CStash) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [(AMBUSHER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior  -- dark and numerous, so more kit without exploring
               ++ [ (RING_OF_OPPORTUNITY_SNIPER, CEqp)
                  , (ANY_ARROW, CStash)
                  , (WEAK_ARROW, CStash)
                  , (EXPLOSIVE, CStash)
                  , (LIGHT_MANIPULATION, CEqp)
                  , (WOODEN_TORCH, CStash) ]
  -- , idesc    = ""
  }
brawler = warrior
  { iname    = "brawler"
  , ifreq    = [(BRAWLER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior
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
  { isymbol  = 'e'
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
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Under your stare, it reduces to the bits that define its essence. Under introspection, the bits slow down and solidify into an arbitrary form again. It must be huge inside, for holographic principle to manifest so overtly."  -- holographic principle is an anachronism for XIX or most of XX century, but "the cosmological scale effects" is too weak
  , ikit     = [ (LASH, COrgan), (PUPIL, COrgan)  -- at least one non-timed
               , (SAPIENT_BRAIN, COrgan) ]  -- no hearing, it's all eyes
  }
fastEye = ItemKind
  { isymbol  = 'j'
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
  , ikit     = [ (TOOTH, COrgan), (LIP, COrgan)
               , (SPEED_GLAND_10, COrgan)
               , (VISION_6, COrgan), (EAR_3, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'n'
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
  , ikit     = [ (NOSE_TIP, COrgan), (LIP, COrgan)
               , (NOSTRIL, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = 'e'
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
  , ikit     = [ (SPEED_GLAND_5, COrgan), (BARK, COrgan)
               , (VISION_12, COrgan), (EAR_8, COrgan)
                   -- too powerful to get stronger sight
               , (SAPIENT_BRAIN, COrgan)
               , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
               , (WEAK_ARROW, CStash), (WEAK_ARROW, CStash) ]
  }
torsor = ItemKind
  { isymbol  = 'T'
  , iname    = "Forgetful Torsor"
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
  , ikit     = [ (RIGHT_TORSION, COrgan), (LEFT_TORSION, COrgan)
               , (PUPIL, COrgan), (TENTACLE, COrgan)
               , (EAR_8, COrgan)
               , (SAPIENT_BRAIN, COrgan)
               , (GEM, CStash), (GEM, CStash)
               , (GEM, CStash), (GEM, CStash) ]
  }
-- "ground x" --- for immovable monster that can only tele or prob travel
-- pullback
-- skeletal

-- * Animals

-- They need rather strong melee, because they don't use items.
-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller and slower hyena
  { isymbol  = 'j'
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
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ (SMALL_JAW, COrgan)
               , (EYE_6, COrgan), (NOSTRIL, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
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
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one on level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Common random double moves,
      -- on either side, are just too bothersome.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ (SCREECHING_BEAK, COrgan)  -- in reality it grunts and hisses
               , (SMALL_CLAW, COrgan)
               , (EYE_8, COrgan), (EAR_8, COrgan)
                   -- can't shoot, so strong sight is OK
               , (ANIMAL_BRAIN, COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
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
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ (SCENT_GLAND, COrgan)
               , (SMALL_CLAW, COrgan), (SNOUT, COrgan)
               , (EYE_3, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 13, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ (HOOKED_CLAW, COrgan), (SNOUT, COrgan)
               , (ARMORED_SKIN, COrgan), (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
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
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ (VENOM_TOOTH, COrgan), (SMALL_CLAW, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 28, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ (VENOM_FANG, COrgan)  -- when on cooldown, it's weaponless
               , (RATLLE, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 5)]  -- gets summoned often, so low base rarity
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 23, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ (JAW, COrgan)
               , (EYE_6, COrgan), (NOSTRIL, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [BrRed]  -- speedy, so bright red
  , icount   = 1
  , irarity  = [(9, 0), (10, 11)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 17, AddSkill SkNocto 2
               , AddSkill SkAggression 1  -- match the description
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds as its lesser cousins."
  , ikit     = [ (LARGE_TAIL, COrgan), (JAW, COrgan)
               , (HOOKED_CLAW, COrgan)
               , (SPEED_GLAND_5, COrgan), (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_3, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 55, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkSwimming 100  -- swims better than walks
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An armored predator from the dawn of time. You better not get within its reach."
  , ikit     = [ (HUGE_TAIL, COrgan), (LARGE_JAW, COrgan)
               , (SMALL_CLAW, COrgan)
               , (ARMORED_SKIN, COrgan)
               , (EYE_6, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "Maddened Rhinoceros"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
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
               , AddSkill SkAlter (-1)  -- can't use normal stairs nor dig;
                                        -- a weak miniboss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ (RHINO_HORN, COrgan), (SNOUT, COrgan)
               , (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = 'b'
  , iname    = "bee swarm"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ (BEE_STING, COrgan)  -- weaponless when it's used up
               , (VISION_6, COrgan), (EAR_6, COrgan)
               , (INSECT_MORTALITY, COrgan), (ANIMAL_BRAIN, COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 4)]  -- should be many, because die after a time
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 8, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ (STING, COrgan)  -- when on cooldown, it's weaponless
               , (VISION_6, COrgan), (EAR_6, COrgan)
               , (INSECT_MORTALITY, COrgan), (ANIMAL_BRAIN, COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [(ANIMAL, 20), (IMMOBILE_ANIMAL, 20)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 13)]
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [ (THORN, COrgan)  -- after all run out, it's weaponless
               , (BARK, COrgan) ]
  }
geyserBoiling = ItemKind
  { isymbol  = 'g'
  , iname    = "geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 30)]
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
  , ikit     = [(BOILING_VENT, COrgan), (BOILING_FISSURE, COrgan)]
  }
geyserArsenic = ItemKind
  { isymbol  = 'g'
  , iname    = "arsenic geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 40)]
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
  , ikit     = [(ARSENIC_VENT, COrgan), (ARSENIC_FISSURE, COrgan)]
  }
geyserSulfur = ItemKind
  { isymbol  = 'g'
  , iname    = "sulfur geyser"
  , ifreq    = [(ANIMAL, 8), (IMMOBILE_ANIMAL, 120)]
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
  , ikit     = [(SULFUR_VENT, COrgan), (SULFUR_FISSURE, COrgan)]
  }
