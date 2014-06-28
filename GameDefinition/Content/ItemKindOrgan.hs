-- | Organ definitions.
module Content.ItemKindOrgan ( organs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, tentacle, lash, noseTip, lip, claw, smallClaw, snout, venomTooth, venomFang, largeTail, jaw, largeJaw, pupil, armoredSkin, speedGland1, speedGland2, speedGland3, speedGland4, speedGland5, eye2, eye3, eye4, eye5, nostril, thorn]

fist,    foot, tentacle, lash, noseTip, lip, claw, smallClaw, snout, venomTooth, venomFang, largeTail, jaw, largeJaw, pupil, armoredSkin, speedGland1, speedGland2, speedGland3, speedGland4, speedGland5, eye2, eye3, eye4, eye5, nostril, thorn :: ItemKind

fist = ItemKind
  { isymbol  = '%'
  , iname    = "fist"
  , ifreq    = [("fist", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 2
  , iverbHit = "punch"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (5 * d 1) 0]
  , ifeature = [Durable, EqpSlot EqpSlotWeapon "", Identified]
  , idesc    = ""
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [("foot", 50)]
  , icount   = 2
  , iverbHit = "kick"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
lash = fist
  { iname    = "lash"
  , ifreq    = [("lash", 100)]
  , icount   = 1
  , iverbHit = "lash"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
noseTip = fist
  { iname    = "nose tip"
  , ifreq    = [("nose tip", 50)]
  , icount   = 1
  , iverbHit = "poke"
  , ieffects = [Hurt (2 * d 1) 0]
  , idesc    = ""
  }
lip = fist
  { iname    = "lip"
  , ifreq    = [("lip", 10)]
  , icount   = 2
  , iverbHit = "lap"
  , ieffects = [Hurt (2 * d 1) 0]  -- TODO: decrease Hurt, but use
  , idesc    = ""
  }
claw = fist
  { iname    = "claw"
  , ifreq    = [("claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "slash"
  , ieffects = [Hurt (7 * d 1) 0]
  , idesc    = ""
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , icount   = 2
  , iverbHit = "slash"
  , ieffects = [Hurt (3 * d 1) 0]
  , idesc    = ""
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , iverbHit = "bite"
  , ieffects = [Hurt (2 * d 1) 0]
  , idesc    = ""
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , ieffects = [Hurt (3 * d 1) 0, Paralyze 3]
  , idesc    = ""
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , ieffects = [Hurt (3 * d 1) 12]
  , idesc    = ""
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , ieffects = [Hurt (9 * d 1) 0, PushActor (ThrowMod 400 25)]
  , idesc    = ""
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , ieffects = [Hurt (15 * d 1) 0]
  , idesc    = ""
  }
pupil = fist
  { iname    = "pupil"
  , ifreq    = [("pupil", 100)]
  , icount   = 1
  , iverbHit = "gaze at"
  , iaspects = [AddSight 7]
  , ieffects = [Hurt (5 * d 1) 0, Paralyze 1]  -- TODO: decrease Hurt, but use
  , idesc    = ""
  }
armoredSkin = fist
  { iname    = "armored skin"
  , ifreq    = [("armored skin", 100)]
  , icount   = 1
  , iverbHit = "bash"
  , iaspects = [AddArmorMelee 50]
  , ieffects = []
  , ifeature = [Identified]
  , idesc    = ""
  }
speedGland1 = speedGland 1
speedGland2 = speedGland 2
speedGland3 = speedGland 3
speedGland4 = speedGland 4
speedGland5 = speedGland 5
eye2 = eye 2
eye3 = eye 3
eye4 = eye 4
eye5 = eye 5
nostril = fist
  { iname    = "nostril"
  , ifreq    = [("nostril", 100)]
  , icount   = 2
  , iverbHit = "snuff"
  , iaspects = [AddSmell 1]
  , ieffects = []
  , ifeature = [Identified]
  , idesc    = ""
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 7
  , iverbHit = "impale"
  , ieffects = [Hurt (3 * d 1) 0]
  , idesc    = ""
  }

speedGland :: Int -> ItemKind
speedGland n = fist
  { iname    = "speed gland"
  , ifreq    = [("speed gland" <+> tshow n, 100)]
  , icount   = 1
  , iverbHit = "spit at"
  , iaspects = [AddSpeed 2, Periodic (intToDice $ 2 * n)]
  , ieffects = [RefillHP 1]
  , ifeature = [Identified]
  , idesc    = ""
  }

eye :: Int -> ItemKind
eye n = fist
  { iname    = "eye"
  , ifreq    = [("eye" <+> tshow n, 100)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [AddSight (intToDice n)]
  , ieffects = []
  , ifeature = [Identified]
  , idesc    = ""
  }
