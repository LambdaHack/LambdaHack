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
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, impressed]
  -- LH-specific
  ++ [tooth, lash, noseTip, lip, torsionRight, torsionLeft, pupil]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, impressed :: ItemKind
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
  , idamage  = toDmg $ 4 `d` 1
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Meleeable]
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [("foot", 50)]
  , iverbHit = "kick"
  , idamage  = toDmg $ 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
  }

-- * Universal weapon organs

hookedClaw = fist
  { iname    = "hooked claw"
  , ifreq    = [("hooked claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [Timeout $ 8 - 1 `dL` 3]
  , ieffects = [Recharging (toOrganGameTurn "slowed" 2)]
  , idesc    = "A curved talon."
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , iverbHit = "slash"
  , idamage  = toDmg $ 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [("small jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = toDmg $ 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = toDmg $ 5 `d` 1
  , idesc    = "Delivers a powerful bite."
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = toDmg $ 10 `d` 1
  , idesc    = "Enough to swallow anything in a single gulp."
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [("horn", 20)]
  , icount   = 2
  , iverbHit = "impale"
  , idamage  = toDmg $ 6 `d` 1
  , iaspects = [AddHurtMelee 20]
  , idesc    = "Sharp and solid, for defence or attack."
  }

-- * Special weapon organs

tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = toDmg $ 4 `d` 1
  , idesc    = "Damp and dextrous."
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 2 + 1 `d` 3
  , iverbHit = "impale"
  , idamage  = toDmg $ 1 `d` 3
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = "Sharp yet brittle."
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [("boiling fissure", 100)]
  , icount   = 5 + 1 `d` 5
  , iverbHit = "hiss at"
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee 20]  -- decreasing as count decreases
  , ieffects = [InsertMove $ 1 `d` 3]
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = "A deep crack to the underworld."
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("arsenic fissure", 100)]
  , icount   = 3 + 1 `d` 3
  , idamage  = toDmg $ 2 `d` 1
  , ieffects = [toOrganGameTurn "weakened" (2 + 1 `dL` 3)]
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("sulfur fissure", 100)]
  , icount   = 2 + 1 `d` 2
  , idamage  = toDmg 0
  , ieffects = [RefillHP 5]
  , idesc    = ""
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [("bee sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = toDmg 0
  , iaspects = [AddArmorMelee 90, AddArmorRanged 45]
  , ieffects = [Paralyze 6, RefillHP 4]
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [Timeout $ 8 - 1 `dL` 3, AddHurtMelee 40]
  , ieffects = [Recharging (Paralyze 4)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [Timeout $ 6 - 1 `dL` 3]
  , ieffects = [Recharging (toOrganGameTurn "slowed" (3 + 1 `d` 3))]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [Timeout $ 8 - 1 `dL` 3]
  , ieffects = [Recharging (toOrganNone "poisoned")]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [Timeout $ 6 - 1 `dL` 3]
  , ieffects = [Recharging $ Summon "scavenger" 1]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = toDmg $ 6 `d` 1
  , iaspects = [Timeout $ 1 + 1 `d` 3, AddHurtMelee 20]
  , ieffects = [Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "Slow but heavy."
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
  , idamage  = toDmg 0
  , iaspects = [AddArmorMelee 30, AddArmorRanged 15]
  , ieffects = []
  , ifeature = [Durable, Identified]
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
  , iaspects = [AddSight (intToDice n)]
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
  , iaspects = [AddSight (intToDice n)]
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
  , iaspects = [AddSmell 1]  -- times 2, from icount
  , idesc    = ""
  }

-- * Assorted

insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [("insect mortality", 100)]
  , iverbHit = "age"
  , iaspects = [Timeout $ 30 + (1 `d` 2) * 10]
  , ieffects = [Periodic, Recharging (RefillHP (-1))]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [("sapient brain", 100)]
  , iverbHit = "outbrain"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbAlter 2]  -- can use stairs
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [("animal brain", 100)]
  , iverbHit = "blank"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbAlter 2]  -- can use stairs
               ++ [ AddAbility ab (-1)
                  | ab <- [AbDisplace, AbMoveItem, AbProject, AbApply] ]
  , idesc    = ""
  }
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , iverbHit = "spit at"
  , iaspects = [ AddSpeed $ intToDice n
               , Timeout $ intToDice $ 100 `div` n ]
  , ieffects = [Periodic, Recharging (RefillHP 1)]
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
  , iaspects = [Timeout $ (1 `d` 3) * 10]
  , ieffects = [ Periodic
               , Recharging (Explode "distressing odor")
               , Recharging ApplyPerfume ]
  , ifeature = [Identified]  -- not Durable
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Periodic
               , Recharging (Explode "boiling water")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("arsenic vent", 100)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Periodic
               , Recharging (Explode "sparse shower")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("sulfur vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Periodic
               , Recharging (Explode "dense shower")
               , Recharging (RefillHP 2) ]
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = 'H'  -- '+' reserved for temporary conditions
  , iname    = "bonus HP"
  , iflavour = zipPlain [BrBlue]
  , ifreq    = [("bonus HP", 1)]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddMaxHP 1]
  , idesc    = ""
  }
impressed = armoredSkin
  { isymbol  = '!'
  , iname    = "impressed"
  , iflavour = zipPlain [BrRed]
  , ifreq    = [("impressed", 1)]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [AddMaxCalm (-1)]  -- to help player notice on main screen
  , ieffects = [OnSmash $ tmpNoLonger "impressed"]  -- not @Periodic@
  , ifeature = [Identified, Fragile, Durable]  -- hack: destroy on drop
  , idesc    = ""
  }

-- * LH-specific

tooth = fist
  { iname    = "tooth"
  , ifreq    = [("tooth", 20)]
  , icount   = 3
  , iverbHit = "nail"
  , idamage  = toDmg $ 2 `d` 1
  , idesc    = ""
  }
lash = fist
  { iname    = "lash"
  , ifreq    = [("lash", 100)]
  , icount   = 1
  , iverbHit = "lash"
  , idamage  = toDmg $ 3 `d` 1
  , idesc    = ""
  }
noseTip = fist
  { iname    = "tip"
  , ifreq    = [("nose tip", 50)]
  , icount   = 1
  , iverbHit = "poke"
  , idamage  = toDmg $ 2 `d` 1
  , idesc    = ""
  }
lip = fist
  { iname    = "lip"
  , ifreq    = [("lip", 10)]
  , icount   = 1
  , iverbHit = "lap"
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [Timeout $ 3 + 1 `d` 2]
  , ieffects = [Recharging (toOrganGameTurn "weakened" (2 + 1 `dL` 3))]
  , idesc    = ""
  }
torsionRight = fist
  { iname    = "right torsion"
  , ifreq    = [("right torsion", 100)]
  , icount   = 1
  , iverbHit = "twist"
  , idamage  = toDmg $ 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddHurtMelee 20]
  , ieffects = [Recharging (toOrganGameTurn "slowed" (3 + 1 `d` 3))]
  , idesc    = ""
  }
torsionLeft = fist
  { iname    = "left torsion"
  , ifreq    = [("left torsion", 100)]
  , icount   = 1
  , iverbHit = "twist"
  , idamage  = toDmg $ 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddHurtMelee 20]
  , ieffects = [Recharging (toOrganGameTurn "weakened" (3 + 1 `d` 3))]
  , idesc    = ""
  }
pupil = fist
  { iname    = "pupil"
  , ifreq    = [("pupil", 100)]
  , icount   = 1
  , iverbHit = "gaze at"
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddSight 12, Timeout 7]
  , ieffects = [ Recharging (DropItem 1 maxBound COrgan "temporary condition")
               , Recharging $ RefillCalm (-10)
               ]
  , idesc    = ""
  }
