-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( organs
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

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
                 -- great example of tutorial hints inside a flavourful text
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
  , ieffects = [toOrganBad "slowed" 2]
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
  , iaspects = [Timeout $ 2 + 1 `d` 2]  -- no effect, but limit raw damage
               ++ iaspects fist
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
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
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
  , ifreq    = [("rhino horn", 100)]
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
  , iverbHit = "puncture"
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , ieffects = [VerbNoLonger "be not so thorny any more"]
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
  , ieffects = [ DropItem 1 1 COrgan "condition"  -- useful; limited
               , VerbNoLonger "widen the crack, releasing pressure" ]
  , idesc    = "A deep crack to the underworld."
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("arsenic fissure", 100)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [ toOrganBad "parsimonious" (5 + 1 `d` 3)
               -- weaken/poison, impacting intellectual abilities first
               , VerbNoLonger "stop exuding stupefying vapours" ]
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("sulfur fissure", 100)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0  -- heal not via (negative) idamage, for armour would block it
  , iaspects = SetFlag Benign : iaspects boilingFissure
  , ieffects = [ RefillHP 5
               , VerbNoLonger "run out of the healing fumes" ]
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
                 -- no special message when runs out, because it's 1 copy
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "inject"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 4, AddSkill SkHurtMelee 40]
               ++ iaspects fist
  , ieffects = [toOrganBad "retaining" (3 + 1 `d` 3)]
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
  , ieffects = [toOrganBad "slowed" (3 + 1 `d` 3)]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 0
  , iaspects = Timeout (10 - 1 `dL` 5)
               : iaspects fist
  , ieffects = [toOrganNoTimer "poisoned"]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 3 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Summon "scavenger" $ 1 `dL` 3]
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
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "Almost as long as the trunk."
  }
hugeTail = largeTail
  { iname    = "huge tail"
  , ifreq    = [("huge tail", 50)]
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
  , idesc    = "Homemade armour is just as good."  -- hmm, it may get confused with leather armor jackets, etc.
  , ikit     = []
  }
bark = armoredSkin
  { iname    = "bark"
  , ifreq    = [("bark", 100)]
  , idesc    = ""
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
eye3 = eye 3
eye6 = eye 6
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
vision6 = vision 6
vision12 = vision 12
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
ear3 = ear 3
ear6 = ear 6
ear8 = ear 8

-- * Assorted

rattleOrgan = armoredSkin
  { iname    = "rattle"
  , ifreq    = [("rattle", 100)]
  , iverbHit = "announce"
  , iaspects = [ Timeout $ 10 + (1 `d` 3) * 10  -- long, to limit spam
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Yell, RefillCalm 5]
  , idesc    = ""
  }
insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [("insect mortality", 100)]
  , iverbHit = "age"
  , iaspects = [ AddSkill SkAggression 2  -- try to attack before you die
               , Timeout $ 30 + (1 `d` 3) * 10  -- die very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP (-1), Yell]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [("sapient brain", 100)]
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
  , ifreq    = [("animal brain", 100)]
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
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , iverbHit = "spit at"
  , iaspects = [ AddSkill SkSpeed $ intToDice n
               , Timeout $ intToDice (100 `div` n)
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 1]
  , idesc    = ""
  }
speedGland5 = speedGland 5
speedGland10 = speedGland 10
scentGland = armoredSkin
  { iname    = "scent gland"
  , ifreq    = [("scent gland", 100)]
  , icount   = 10 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [ Timeout $ (1 `d` 3) * 10
               , SetFlag Periodic, SetFlag Fragile ]  -- not Durable
  , ieffects = [ VerbNoLonger "look spent"
               , ApplyPerfume
               , Explode "distressing odor" ]
                   -- keep explosion at the end to avoid the ambiguity of
                   -- "of ([foo explosion] of [bar])"
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode "boiling water"]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("arsenic vent", 100)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode "sparse shower"]
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("sulfur vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode "dense shower"]
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
  , iaspects = [AddSkill SkMaxHP 1]
  , idesc    = "Growing up in a privileged background gave you the training and the discrete garment accessories that improve your posture and resilience."
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
               , SetFlag Condition ] -- hack: display as condition
  , idesc    = "Apart of increased resilience to attacks, being braced protects from displacement by foes and other forms of forced translocation, e.g., pushing or pulling."
  }
asleep = armoredSkin
  { isymbol  = 'S'
  , iname    = "asleep"
  , ifreq    = [("asleep", 1)]
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
  , ifreq    = [("impressed", 1), ("condition", 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)  -- to help player notice on HUD
                                          -- and to count as bad condition
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all impressions gone
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
  , ieffects = [toOrganBad "weakened" (2 + 1 `dL` 3)]
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
  , ieffects = [toOrganBad "slowed" (3 + 1 `d` 3)]
  , idesc    = ""
  }
torsionLeft = fist
  { iname    = "left torsion"
  , ifreq    = [("left torsion", 100)]
  , icount   = 1
  , iverbHit = "untwist"
  , idamage  = 13 `d` 1
  , iaspects = [Timeout $ 5 + 1 `d` 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [toOrganBad "weakened" (3 + 1 `d` 3)]
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
  , ieffects = [DropItem 1 maxBound COrgan "condition", RefillCalm (-10)]
                 -- can be useful for the player, but Calm drain is a risk
  , idesc    = ""
  }
