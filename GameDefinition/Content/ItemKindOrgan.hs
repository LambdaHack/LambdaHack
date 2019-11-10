-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( -- * Group name patterns
    pattern S_FIST, pattern S_FOOT, pattern S_HOOKED_CLAW, pattern S_SMALL_CLAW, pattern S_SNOUT, pattern S_SMALL_JAW, pattern S_JAW, pattern S_LARGE_JAW, pattern S_ANTLER, pattern S_HORN, pattern S_RHINO_HORN, pattern S_TENTACLE, pattern S_THORN, pattern S_BOILING_FISSURE, pattern S_ARSENIC_FISSURE, pattern S_SULFUR_FISSURE, pattern S_BEE_STING, pattern S_STING, pattern S_VENOM_TOOTH, pattern S_VENOM_FANG, pattern S_SCREECHING_BEAK, pattern S_LARGE_TAIL, pattern S_HUGE_TAIL, pattern S_ARMORED_SKIN, pattern S_BARK, pattern S_NOSTRIL, pattern S_RATLLE, pattern S_INSECT_MORTALITY, pattern S_SAPIENT_BRAIN, pattern S_ANIMAL_BRAIN, pattern S_SCENT_GLAND, pattern S_BOILING_VENT, pattern S_ARSENIC_VENT, pattern S_SULFUR_VENT, pattern S_EYE_3, pattern S_EYE_6, pattern S_EYE_8, pattern S_VISION_6, pattern S_VISION_12, pattern S_VISION_16, pattern S_EAR_3, pattern S_EAR_6, pattern S_EAR_8, pattern S_SPEED_GLAND_5, pattern S_SPEED_GLAND_10, pattern S_TOOTH, pattern S_LASH, pattern S_NOSE_TIP, pattern S_LIP, pattern S_RIGHT_TORSION, pattern S_LEFT_TORSION, pattern S_PUPIL
  , pattern SCAVENGER
  , -- * Content
    organs, organsGNSingleton, organsGN
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindBlast
import Content.ItemKindTemporary
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

organsGNSingleton :: [GroupName ItemKind]
organsGNSingleton =
       [S_FIST, S_FOOT, S_HOOKED_CLAW, S_SMALL_CLAW, S_SNOUT, S_SMALL_JAW, S_JAW, S_LARGE_JAW, S_ANTLER, S_HORN, S_RHINO_HORN, S_TENTACLE, S_THORN, S_BOILING_FISSURE, S_ARSENIC_FISSURE, S_SULFUR_FISSURE, S_BEE_STING, S_STING, S_VENOM_TOOTH, S_VENOM_FANG, S_SCREECHING_BEAK, S_LARGE_TAIL, S_HUGE_TAIL, S_ARMORED_SKIN, S_BARK, S_NOSTRIL, S_RATLLE, S_INSECT_MORTALITY, S_SAPIENT_BRAIN, S_ANIMAL_BRAIN, S_SCENT_GLAND, S_BOILING_VENT, S_ARSENIC_VENT, S_SULFUR_VENT, S_EYE_3, S_EYE_6, S_EYE_8, S_VISION_6, S_VISION_12, S_VISION_16, S_EAR_3, S_EAR_6, S_EAR_8, S_SPEED_GLAND_5, S_SPEED_GLAND_10, S_TOOTH, S_LASH, S_NOSE_TIP, S_LIP, S_RIGHT_TORSION, S_LEFT_TORSION, S_PUPIL]

pattern S_FIST, S_FOOT, S_HOOKED_CLAW, S_SMALL_CLAW, S_SNOUT, S_SMALL_JAW, S_JAW, S_LARGE_JAW, S_ANTLER, S_HORN, S_RHINO_HORN, S_TENTACLE, S_THORN, S_BOILING_FISSURE, S_ARSENIC_FISSURE, S_SULFUR_FISSURE, S_BEE_STING, S_STING, S_VENOM_TOOTH, S_VENOM_FANG, S_SCREECHING_BEAK, S_LARGE_TAIL, S_HUGE_TAIL, S_ARMORED_SKIN, S_BARK, S_NOSTRIL, S_RATLLE, S_INSECT_MORTALITY, S_SAPIENT_BRAIN, S_ANIMAL_BRAIN, S_SCENT_GLAND, S_BOILING_VENT, S_ARSENIC_VENT, S_SULFUR_VENT, S_EYE_3, S_EYE_6, S_EYE_8, S_VISION_6, S_VISION_12, S_VISION_16, S_EAR_3, S_EAR_6, S_EAR_8, S_SPEED_GLAND_5, S_SPEED_GLAND_10, S_TOOTH, S_LASH, S_NOSE_TIP, S_LIP, S_RIGHT_TORSION, S_LEFT_TORSION, S_PUPIL :: GroupName ItemKind

organsGN :: [GroupName ItemKind]
organsGN =
       [SCAVENGER]

pattern SCAVENGER :: GroupName ItemKind

pattern S_FIST = GroupName "fist"
pattern S_FOOT = GroupName "foot"
pattern S_HOOKED_CLAW = GroupName "hooked claw"
pattern S_SMALL_CLAW = GroupName "small claw"
pattern S_SNOUT = GroupName "snout"
pattern S_SMALL_JAW = GroupName "small jaw"
pattern S_JAW = GroupName "jaw"
pattern S_LARGE_JAW = GroupName "large jaw"
pattern S_ANTLER = GroupName "antler"
pattern S_HORN = GroupName "horn"
pattern S_RHINO_HORN = GroupName "rhino horn"
pattern S_TENTACLE = GroupName "tentacle"
pattern S_THORN = GroupName "thorn"
pattern S_BOILING_FISSURE = GroupName "boiling fissure"
pattern S_ARSENIC_FISSURE = GroupName "arsenic fissure"
pattern S_SULFUR_FISSURE = GroupName "sulfur fissure"
pattern S_BEE_STING = GroupName "bee sting"
pattern S_STING = GroupName "sting"
pattern S_VENOM_TOOTH = GroupName "venom tooth"
pattern S_VENOM_FANG = GroupName "venom fang"
pattern S_SCREECHING_BEAK = GroupName "screeching beak"
pattern S_LARGE_TAIL = GroupName "large tail"
pattern S_HUGE_TAIL = GroupName "huge tail"
pattern S_ARMORED_SKIN = GroupName "armored skin"
pattern S_BARK = GroupName "bark"
pattern S_NOSTRIL = GroupName "nostril"
pattern S_RATLLE = GroupName "rattle"
pattern S_INSECT_MORTALITY = GroupName "insect mortality"
pattern S_SAPIENT_BRAIN = GroupName "sapient brain"
pattern S_ANIMAL_BRAIN = GroupName "animal brain"
pattern S_SCENT_GLAND = GroupName "scent gland"
pattern S_BOILING_VENT = GroupName "boiling vent"
pattern S_ARSENIC_VENT = GroupName "arsenic vent"
pattern S_SULFUR_VENT = GroupName "sulfur vent"
pattern S_EYE_3 = GroupName "eye 3"
pattern S_EYE_6 = GroupName "eye 6"
pattern S_EYE_8 = GroupName "eye 8"
pattern S_VISION_6 = GroupName "vision 6"
pattern S_VISION_12 = GroupName "vision 12"
pattern S_VISION_16 = GroupName "vision 16"
pattern S_EAR_3 = GroupName "ear 3"
pattern S_EAR_6 = GroupName "ear 6"
pattern S_EAR_8 = GroupName "ear 8"
pattern S_SPEED_GLAND_5 = GroupName "speed gland 5"
pattern S_SPEED_GLAND_10 = GroupName "speed gland 10"

-- * LH-specific
pattern S_TOOTH = GroupName "tooth"
pattern S_LASH = GroupName "lash"
pattern S_NOSE_TIP = GroupName "nose tip"
pattern S_LIP = GroupName "lip"
pattern S_RIGHT_TORSION = GroupName "right torsion"
pattern S_LEFT_TORSION = GroupName "left torsion"
pattern S_PUPIL = GroupName "pupil"

pattern SCAVENGER = GroupName "scavenger"

-- * Content

organs :: [ItemKind]
organs =
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed]
  -- LH-specific
  ++ [tooth, lash, noseTip, lip, torsionRight, torsionLeft, pupil]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed :: ItemKind
-- LH-specific
tooth, lash, noseTip, lip, torsionRight, torsionLeft, pupil :: ItemKind

-- Weapons

-- * Human weapon organs

fist = ItemKind
  { isymbol  = ','
  , iname    = "fist"
  , ifreq    = [(S_FIST, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [(S_FOOT, 1)]
  , iverbHit = "kick"
  , idamage  = 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
                 -- great example of tutorial hints inside a flavourful text
  }

-- * Other weapon organs

hookedClaw = fist
  { iname    = "hooked claw"
  , ifreq    = [(S_HOOKED_CLAW, 1)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = 2 `d` 1
  , iaspects = Timeout (12 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad S_SLOWED 2]
  , idesc    = "A curved talon."
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [(S_SMALL_CLAW, 1)]
  , iverbHit = "slash"
  , idamage  = 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [(S_SNOUT, 1)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [(S_SMALL_JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [(S_JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 5 `d` 1
  , idesc    = "Delivers a powerful bite."
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [(S_LARGE_JAW, 1)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = 10 `d` 1
  , iaspects = [Timeout $ 2 + 1 `d` 2]  -- no effect, but limit raw damage
               ++ iaspects fist
  , idesc    = "Enough to swallow anything in a single gulp."
  }
antler = fist
  { iname    = "antler"
  , ifreq    = [(S_ANTLER, 1)]
  , icount   = 2
  , iverbHit = "ram"
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ 3 + (1 `d` 3) * 3
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
  , idesc    = ""
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [(S_HORN, 1)]
  , icount   = 2
  , iverbHit = "impale"
  , idamage  = 5 `d` 1
  , iaspects = [ AddSkill SkHurtMelee 10
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , idesc    = "Sharp and long, for defence or attack."
  }
rhinoHorn = fist
  { iname    = "ugly horn"  -- made of keratin, unlike real horns
  , ifreq    = [(S_RHINO_HORN, 1)]
  , icount   = 1  -- single, unlike real horns
  , iverbHit = "gore"
  , idamage  = 5 `d` 1
  , iaspects = [Timeout 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [Impress, Yell]  -- the owner is a mid-boss, after all
  , idesc    = "Very solid, considering it has the same composition as fingernails."
  }
tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [(S_TENTACLE, 1)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = 4 `d` 1
  , idesc    = "Damp and dextrous."
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [(S_THORN, 1)]
  , icount   = 2 + 1 `d` 3
  , iverbHit = "puncture"
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , ieffects = [VerbNoLonger "be not so thorny any more"]
  , idesc    = "Sharp yet brittle."
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [(S_BOILING_FISSURE, 1)]
  , icount   = 5 + 1 `d` 5
  , iverbHit = "hiss at"
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee 20  -- decreasing as count decreases
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [ DropItem 1 1 COrgan CONDITION  -- useful; limited
               , VerbNoLonger "widen the crack, releasing pressure" ]
  , idesc    = "A deep crack to the underworld."
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_ARSENIC_FISSURE, 1)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [ toOrganBad S_PARSIMONIOUS (5 + 1 `d` 3)
               -- weaken/poison, impacting intellectual abilities first
               , VerbNoLonger "stop exuding stupefying vapours" ]
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_SULFUR_FISSURE, 1)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0  -- heal not via (negative) idamage, for armour would block it
  , iaspects = SetFlag Benign : iaspects boilingFissure
  , ieffects = [ RefillHP 5
               , VerbNoLonger "run out of the healing fumes" ]
  , idesc    = ""
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [(S_BEE_STING, 1)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 200, AddSkill SkArmorRanged 45
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [Paralyze 6, RefillHP 4]
                 -- no special message when runs out, because it's 1 copy
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [(S_STING, 1)]
  , icount   = 1
  , iverbHit = "inject"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 4, AddSkill SkHurtMelee 40]
               ++ iaspects fist
  , ieffects = [toOrganBad S_RETAINING (3 + 1 `d` 3)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [(S_VENOM_TOOTH, 1)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad S_SLOWED (3 + 1 `d` 3)]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [(S_VENOM_FANG, 1)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 0
  , iaspects = Timeout (10 - 1 `dL` 5)
               : iaspects fist
  , ieffects = [toOrganNoTimer S_POISONED]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [(S_SCREECHING_BEAK, 1)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 3 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Summon SCAVENGER $ 1 `dL` 3]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [(S_LARGE_TAIL, 1)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = 7 `d` 1
  , iaspects = [Timeout $ 2 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "Almost as long as the trunk."
  }
hugeTail = largeTail
  { iname    = "huge tail"
  , ifreq    = [(S_HUGE_TAIL, 1)]
  , iverbHit = "upend"
  , iaspects = [Timeout $ 3 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [PushActor (ThrowMod 400 50 1)]  -- 2 steps, fast
  , idesc    = "Slow but immensely heavy."
  }

-- Non-weapons

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = ','
  , iname    = "armored skin"
  , ifreq    = [(S_ARMORED_SKIN, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Homemade armour is just as good."  -- hmm, it may get confused with leather armor jackets, etc.
  , ikit     = []
  }
bark = armoredSkin
  { iname    = "bark"
  , ifreq    = [(S_BARK, 1)]
  , idesc    = ""
  }

-- * Sense organs

eye :: Int -> GroupName ItemKind -> ItemKind
eye n grp = armoredSkin
  { iname    = "eye"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = "A piercing stare."
  }
eye3 = eye 3 S_EYE_3
eye6 = eye 6 S_EYE_6
eye8 = eye 8 S_EYE_8
vision :: Int -> GroupName ItemKind -> ItemKind
vision n grp = armoredSkin
  { iname    = "vision"
  , ifreq    = [(grp, 1)]
  , iverbHit = "visualize"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
vision6 = vision 6 S_VISION_6
vision12 = vision 12 S_VISION_12
vision16 = vision 16 S_VISION_16
nostril = armoredSkin
  { iname    = "nostril"
  , ifreq    = [(S_NOSTRIL, 1)]
  , icount   = 2
  , iverbHit = "snuff"
  , iaspects = [ AddSkill SkSmell 1  -- times 2, from icount
               , SetFlag Durable ]
  , idesc    = ""
  }
ear :: Int -> GroupName ItemKind -> ItemKind
ear n grp = armoredSkin
  { iname    = "ear"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "overhear"
  , iaspects = [ AddSkill SkHearing (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
ear3 = ear 3 S_EAR_3
ear6 = ear 6 S_EAR_6
ear8 = ear 8 S_EAR_8

-- * Assorted

rattleOrgan = armoredSkin
  { iname    = "rattle"
  , ifreq    = [(S_RATLLE, 1)]
  , iverbHit = "announce"
  , iaspects = [ Timeout $ 10 + (1 `d` 3) * 10  -- long, to limit spam
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Yell, RefillCalm 5]
  , idesc    = ""
  }
insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [(S_INSECT_MORTALITY, 1)]
  , iverbHit = "age"
  , iaspects = [ AddSkill SkAggression 2  -- try to attack before you die
               , Timeout $ 30 + (1 `d` 3) * 10  -- die very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP (-1), Yell]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [(S_SAPIENT_BRAIN, 1)]
  , iverbHit = "outbrain"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 4]  -- can use all stairs; dig rubble, ice
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               ++ [AddSkill SkApply 1]  -- can use most items, not just foods
               ++ [SetFlag Durable]
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [(S_ANIMAL_BRAIN, 1)]
  , iverbHit = "blank"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 2]  -- can use normal stairs; can't dig
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               -- No @SkApply@ bonus, so can only apply foods. Note, however,
               -- that AI doesn't risk applying unIded items, so in early
               -- game animals won't eat anything.
               ++ [AddSkill SkDisplace (-1)]  -- no melee tactics
               ++ [AddSkill SkMoveItem (-1)]  -- no item gathering
               ++ [AddSkill SkProject (-1)]  -- nor item flinging
               ++ [SetFlag Durable]
  , idesc    = ""
  }
speedGland :: Int -> GroupName ItemKind -> ItemKind
speedGland n grp = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(grp, 1)]
  , iverbHit = "spit at"
  , iaspects = [ AddSkill SkSpeed $ intToDice n
               , Timeout $ intToDice (100 `div` n)
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 1]
  , idesc    = ""
  }
speedGland5 = speedGland 5 S_SPEED_GLAND_5
speedGland10 = speedGland 10 S_SPEED_GLAND_10
scentGland = armoredSkin
  { iname    = "scent gland"
  , ifreq    = [(S_SCENT_GLAND, 1)]
  , icount   = 10 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [ Timeout $ (1 `d` 3) * 10
               , SetFlag Periodic, SetFlag Fragile ]  -- not Durable
  , ieffects = [ VerbNoLonger "look spent"
               , ApplyPerfume
               , Explode S_DISTRESSING_ODOR ]
                   -- keep explosion at the end to avoid the ambiguity of
                   -- "of ([foo explosion] of [bar])"
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(S_BOILING_VENT, 1)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_BOILING_WATER]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(S_ARSENIC_VENT, 1)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_SPARSE_SHOWER]
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(S_SULFUR_VENT, 1)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_DENSE_SHOWER]
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = 'H'  -- '+' reserved for conditions
  , iname    = "bonus HP"
  , ifreq    = [(S_BONUS_HP, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddSkill SkMaxHP 1]
  , idesc    = "Growing up in a privileged background gave you the training and the discrete garment accessories that improve your posture and resilience."
  }
braced = armoredSkin
  { isymbol  = 'B'
  , iname    = "braced"
  , ifreq    = [(S_BRACED, 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "brace"
  , iweight  = 0
  , iaspects = [ AddSkill SkArmorMelee 50, AddSkill SkArmorRanged 25
               , AddSkill SkHearing 10
               , SetFlag Condition ] -- hack: display as condition
  , idesc    = "Apart of increased resilience to attacks, being braced protects from displacement by foes and other forms of forced translocation, e.g., pushing or pulling."
  }
asleep = armoredSkin
  { isymbol  = 'S'
  , iname    = "asleep"
  , ifreq    = [(S_ASLEEP, 1)]
  , iflavour = zipPlain [BrGreen]  -- regenerates HP (very slowly)
  , icount   = 5
  , iverbHit = "slay"
  , iweight  = 0
  , iaspects = [AddSkill sk (-1) | sk <- [SkMove .. SkApply]]
               ++ [ AddSkill SkMelee 1, AddSkill SkAlter 1, AddSkill SkWait 1
                  , AddSkill SkSight (-3), AddSkill SkArmorMelee (-10)
                  , SetFlag Condition ]  -- hack: display as condition
  , idesc    = "Sleep helps to regain health, albeit extremely slowly. Being asleep makes you vulnerable, with gradually diminishing effects as the slumber wears off over several turns. Any non-idle action, not only combat but even yawning or stretching removes a sizable portion of the sleepiness."
  }
impressed = armoredSkin
  { isymbol  = 'I'
  , iname    = "impressed"
  , ifreq    = [(S_IMPRESSED, 1), (CONDITION, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)  -- to help player notice on HUD
                                          -- and to count as bad condition
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all copies gone
               , SetFlag Condition ]  -- this is really a condition,
                                      -- just not a timed condition
  , ieffects = [ OnSmash $ verbMsgLess "impressed"
               , OnSmash $ verbMsgNoLonger "impressed" ]
                   -- not periodic, so no wear each turn, so only @OnSmash@
  , idesc    = "Being impressed by one's adversary sounds like fun, but on battlefield it equals treason. Almost. Throw in depleted battle calm and it leads to mindless desertion outright."
  }

-- * LH-specific

tooth = fist
  { iname    = "tooth"
  , ifreq    = [(S_TOOTH, 1)]
  , icount   = 3
  , iverbHit = "nail"
  , idamage  = 2 `d` 1
  , idesc    = ""
  }
lash = fist
  { iname    = "lash"
  , ifreq    = [(S_LASH, 1)]
  , icount   = 1
  , iverbHit = "lash"
  , idamage  = 3 `d` 1
  , idesc    = ""
  }
noseTip = fist
  { iname    = "tip"
  , ifreq    = [(S_NOSE_TIP, 1)]
  , icount   = 1
  , iverbHit = "poke"
  , idamage  = 2 `d` 1
  , idesc    = ""
  }
lip = fist
  { iname    = "lip"
  , ifreq    = [(S_LIP, 1)]
  , icount   = 1
  , iverbHit = "lap"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (3 + 1 `d` 2)
               : iaspects fist
  , ieffects = [toOrganBad S_WEAKENED (2 + 1 `dL` 3)]
  , idesc    = ""
  }
torsionRight = fist
  { iname    = "right torsion"
  , ifreq    = [(S_RIGHT_TORSION, 1)]
  , icount   = 1
  , iverbHit = "twist"
  , idamage  = 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [toOrganBad S_SLOWED (3 + 1 `d` 3)]
  , idesc    = ""
  }
torsionLeft = fist
  { iname    = "left torsion"
  , ifreq    = [(S_LEFT_TORSION, 1)]
  , icount   = 1
  , iverbHit = "untwist"
  , idamage  = 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [toOrganBad S_WEAKENED (3 + 1 `d` 3)]
  , idesc    = ""
  }
pupil = fist
  { iname    = "pupil"
  , ifreq    = [(S_PUPIL, 1)]
  , icount   = 1
  , iverbHit = "gaze at"
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkSight 12, Timeout 12]
               ++ iaspects fist
  , ieffects = [DropItem 1 maxBound COrgan CONDITION, RefillCalm (-10)]
                 -- can be useful for the player, but Calm drain is a risk
  , idesc    = ""
  }
