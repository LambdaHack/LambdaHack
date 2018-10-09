-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( organs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, ear4, ear5, ear6, ear7, ear8, ear9, ear10, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed]
  -- LH-specific
  ++ [tooth, lash, noseTip, lip, torsionRight, torsionLeft, pupil]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, ear4, ear5, ear6, ear7, ear8, ear9, ear10, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed :: ItemKind
-- LH-specific
tooth, lash, noseTip, lip, torsionRight, torsionLeft, pupil :: ItemKind

-- Weapons

-- * Human weapon organs

fist = ItemKind
  { isymbol  = ','
  , iname    = "fist"
  , ifreq    = [("fist", 100)]
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
  , ifreq    = [("foot", 50)]
  , iverbHit = "kick"
  , idamage  = 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
  }

-- * Other weapon organs

hookedClaw = fist
  { iname    = "hooked claw"
  , ifreq    = [("hooked claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = 2 `d` 1
  , iaspects = Timeout (12 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Recharging (toOrganBad "slowed" 2)]
  , idesc    = "A curved talon."
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , iverbHit = "slash"
  , idamage  = 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [("small jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 5 `d` 1
  , idesc    = "Delivers a powerful bite."
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = 10 `d` 1
  , idesc    = "Enough to swallow anything in a single gulp."
  }
antler = fist
  { iname    = "antler"
  , ifreq    = [("antler", 100)]
  , icount   = 2
  , iverbHit = "ram"
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ 3 + (1 `d` 3) * 3
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , ieffects = [Recharging (PushActor (ThrowMod 100 50 1))]  -- 1 step, slow
  , idesc    = ""
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [("horn", 100)]
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
  , ifreq    = [("rhino horn", 20)]
  , icount   = 1  -- single, unlike real horns
  , iverbHit = "impale"
  , idamage  = 5 `d` 1
  , iaspects = [Timeout 7, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [Recharging Impress, Recharging Yell]
                 -- the owner is a mid-boss, after all
  , idesc    = "Very solid, considering it has the same composition as fingernails."
  }
tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = 4 `d` 1
  , idesc    = "Damp and dextrous."
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 2 + 1 `d` 3
  , iverbHit = "impale"
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , idesc    = "Sharp yet brittle."
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [("boiling fissure", 100)]
  , icount   = 5 + 1 `d` 5
  , iverbHit = "hiss at"
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee 20  -- decreasing as count decreases
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [DropItem 1 1 COrgan "condition"]  -- useful; limited
  , idesc    = "A deep crack to the underworld."
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("arsenic fissure", 100)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [toOrganBad "parsimonious" (5 + 1 `d` 3)]
               -- weaken/poison, impacting intellectual abilities first
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("sulfur fissure", 100)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0  -- heal not via (negative) idamage, for armour would block it
  , ieffects = [RefillHP 5]
  , idesc    = ""
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [("bee sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 200, AddSkill SkArmorRanged 45
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [Paralyze 6, RefillHP 4]
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 4, AddSkill SkHurtMelee 40]
               ++ iaspects fist
  , ieffects = [Recharging (toOrganBad "retaining" (10 + 1 `d` 10))]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Recharging (toOrganBad "slowed" (3 + 1 `d` 3))]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 0
  , iaspects = Timeout (10 - 1 `dL` 4)
               : iaspects fist
  , ieffects = [Recharging (toOrganNoTimer "poisoned")]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 2 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Recharging $ Summon "scavenger" $ 1 `dL` 3]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = 7 `d` 1
  , iaspects = [Timeout $ 2 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [Recharging (PushActor (ThrowMod 200 50 1))]  -- 1 step, fast
  , idesc    = "Almost as long as the trunk."
  }
hugeTail = largeTail
  { iname    = "huge tail"
  , ifreq    = [("huge tail", 50)]
  , iaspects = [Timeout $ 3 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [Recharging (PushActor (ThrowMod 400 50 1))]  -- 2 steps, fast
  , idesc    = "Slow but immensely heavy."
  }

-- Non-weapons

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = ','
  , iname    = "armored skin"
  , ifreq    = [("armored skin", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Homemade armour is just as good."
  , ikit     = []
  }

-- * Sense organs

eye :: Int -> ItemKind
eye n = armoredSkin
  { iname    = "eye"
  , ifreq    = [(toGroupName $ "eye" <+> tshow n, 100)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = "A piercing stare."
  }
eye2 = eye 2
eye3 = eye 3
eye4 = eye 4
eye5 = eye 5
eye6 = eye 6
eye7 = eye 7
eye8 = eye 8
vision :: Int -> ItemKind
vision n = armoredSkin
  { iname    = "vision"
  , ifreq    = [(toGroupName $ "vision" <+> tshow n, 100)]
  , iverbHit = "visualize"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
vision4 = vision 4
vision5 = vision 5
vision6 = vision 6
vision7 = vision 7
vision8 = vision 8
vision10 = vision 10
vision12 = vision 12
vision14 = vision 14
vision16 = vision 16
nostril = armoredSkin
  { iname    = "nostril"
  , ifreq    = [("nostril", 100)]
  , icount   = 2
  , iverbHit = "snuff"
  , iaspects = [ AddSkill SkSmell 1  -- times 2, from icount
               , SetFlag Durable ]
  , idesc    = ""
  }
ear   :: Int -> ItemKind
ear n = armoredSkin
  { iname    = "ear"
  , ifreq    = [(toGroupName $ "ear" <+> tshow n, 100)]
  , icount   = 2
  , iverbHit = "overhear"
  , iaspects = [ AddSkill SkHearing (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
ear4 = ear 4
ear5 = ear 5
ear6 = ear 6
ear7 = ear 7
ear8 = ear 8
ear9 = ear 9
ear10 = ear 10

-- * Assorted

rattleOrgan = armoredSkin
  { iname    = "rattle"
  , ifreq    = [("rattle", 100)]
  , iverbHit = "announce"
  , iaspects = [ Timeout $ 10 + (1 `d` 2) * 10 + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Recharging Yell, RefillCalm 5]
  , idesc    = ""
  }
insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [("insect mortality", 100)]
  , iverbHit = "age"
  , iaspects = [ Timeout $ 30 + (1 `d` 2) * 10 + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Recharging (RefillHP (-1)), Recharging Yell]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [("sapient brain", 100)]
  , iverbHit = "outbrain"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkWait 3]  -- can brace and sleep and lurk
               ++ [AddSkill SkAlter 2]  -- can use stairs
               ++ [AddSkill SkApply 1]  -- can use most items, not just foods
               ++ [SetFlag Durable]
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [("animal brain", 100)]
  , iverbHit = "blank"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               ++ [AddSkill SkAlter 2]  -- can use stairs
               ++ [ AddSkill sk (-1)
                  | sk <- [SkDisplace, SkMoveItem, SkProject] ]
               ++ [SetFlag Durable]
  , idesc    = ""
  }
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , iverbHit = "spit at"
  , iaspects = [ AddSkill SkSpeed $ intToDice n
               , Timeout $ intToDice (100 `div` n) + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Recharging (RefillHP 1), Recharging Yell]
  , idesc    = ""
  }
speedGland2 = speedGland 2
speedGland4 = speedGland 4
speedGland6 = speedGland 6
speedGland8 = speedGland 8
speedGland10 = speedGland 10
scentGland = armoredSkin
  { iname    = "scent gland"
  , ifreq    = [("scent gland", 100)]
  , icount   = 2 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [ Timeout $ (1 `d` 3) * 10 + 1 `d` 3
               , SetFlag Periodic ]  -- not Durable
  , ieffects = [ Recharging (Temporary "look spent")
               , Recharging (Explode "distressing odor")
               , Recharging ApplyPerfume ]
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 5 + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [ Recharging (Explode "boiling water")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("arsenic vent", 100)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 5 + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [ Recharging (Explode "sparse shower")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("sulfur vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 5 + 1 `d` 3
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [ Recharging (Explode "dense shower")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = 'H'  -- '+' reserved for conditions
  , iname    = "bonus HP"
  , ifreq    = [("bonus HP", 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxHP 1
               , SetFlag Durable ]
  , idesc    = "Increased training and connections in the right places give this actor augmented internal organs, much more resilient to damage."
  }
braced = armoredSkin
  { isymbol  = 'B'
  , iname    = "braced"
  , ifreq    = [("braced", 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "brace"
  , iweight  = 0
  , iaspects = [ AddSkill SkArmorMelee 50, AddSkill SkArmorRanged 25
               , AddSkill SkHearing 10
               , SetFlag Fragile, SetFlag Durable ]
                   -- hack: display as condition
  , idesc    = "Apart of increased resilience to attacks, being braced against attacks protects from displacement by foes and other forms of forced translocation, e.g., being pushed or pulled."
  }
asleep = armoredSkin
  { isymbol  = 'S'
  , iname    = "asleep"
  , ifreq    = [("asleep", 1)]
  , iflavour = zipPlain [BrGreen]  -- regenerates HP (very slowly)
  , icount   = 5
  , iverbHit = "slay"
  , iweight  = 0
  , iaspects = [AddSkill sk (-2) | sk <- [SkMove .. SkApply]]
               ++ [ AddSkill SkMelee 2, AddSkill SkWait 2
                  , AddSkill SkSight (-3), AddSkill SkArmorMelee (-10)
                  , SetFlag Fragile, SetFlag Durable ]
                      -- hack: display as condition
  , idesc    = "Sleep helps regain health, albeit extremely slowly. Being asleep makes an actor vulnerable, with gradually diminishing effects as the slumber wears off over several turns. Any non-idle action, not only combat but even yawning or stretching removes a sizable portion of the sleepiness."
  }
impressed = armoredSkin
  { isymbol  = 'I'
  , iname    = "impressed"
  , ifreq    = [("impressed", 1), ("condition", 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)
                 -- to help player notice on main screen
                 -- and to count as bad condition
               , SetFlag Fragile, SetFlag Durable ]
                   -- hack: destroy on drop
  , ieffects = [OnSmash $ tmpNoLonger "impressed"]  -- not Periodic
  , idesc    = "Being impressed by one's adversary sounds like fun, but on battlefield it equals treason. Almost. Throw in depleted battle calm and it leads to mindless desertion outright."
  }

-- * LH-specific

tooth = fist
  { iname    = "tooth"
  , ifreq    = [("tooth", 20)]
  , icount   = 3
  , iverbHit = "nail"
  , idamage  = 2 `d` 1
  , idesc    = ""
  }
lash = fist
  { iname    = "lash"
  , ifreq    = [("lash", 100)]
  , icount   = 1
  , iverbHit = "lash"
  , idamage  = 3 `d` 1
  , idesc    = ""
  }
noseTip = fist
  { iname    = "tip"
  , ifreq    = [("nose tip", 50)]
  , icount   = 1
  , iverbHit = "poke"
  , idamage  = 2 `d` 1
  , idesc    = ""
  }
lip = fist
  { iname    = "lip"
  , ifreq    = [("lip", 10)]
  , icount   = 1
  , iverbHit = "lap"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (3 + 1 `d` 2)
               : iaspects fist
  , ieffects = [Recharging (toOrganBad "weakened" (2 + 1 `dL` 3))]
  , idesc    = ""
  }
torsionRight = fist
  { iname    = "right torsion"
  , ifreq    = [("right torsion", 100)]
  , icount   = 1
  , iverbHit = "twist"
  , idamage  = 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [Recharging (toOrganBad "slowed" (3 + 1 `d` 3))]
  , idesc    = ""
  }
torsionLeft = fist
  { iname    = "left torsion"
  , ifreq    = [("left torsion", 100)]
  , icount   = 1
  , iverbHit = "twist"
  , idamage  = 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [Recharging (toOrganBad "weakened" (3 + 1 `d` 3))]
  , idesc    = ""
  }
pupil = fist
  { iname    = "pupil"
  , ifreq    = [("pupil", 100)]
  , icount   = 1
  , iverbHit = "gaze at"
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkSight 12, Timeout 12]
               ++ iaspects fist
  , ieffects = [ Recharging (DropItem 1 maxBound COrgan "condition")
               , Recharging $ RefillCalm (-10)
               ]
  , idesc    = ""
  }
