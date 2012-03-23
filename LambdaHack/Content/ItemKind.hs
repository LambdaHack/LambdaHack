-- | Weapons and treasure for LambdaHack.
module Content.ItemKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import Game.LambdaHack.Random
import Game.LambdaHack.Content.ItemKind

cdefs :: Content.CDefs ItemKind
cdefs = Content.CDefs
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = ivalidate
  , content =
      [amulet, dart, gem1, gem2, gem3, gold, harpoon, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle, weight]
  }
amulet,        dart, gem1, gem2, gem3, gold, harpoon, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle, weight :: ItemKind

gem, potion, scroll :: ItemKind  -- generic templates

-- rollDeep (aDb, xDy) = rollDice aDb + lvl * rollDice xDy / depth

amulet = ItemKind
  { isymbol  = '"'
  , iname    = "amulet"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy [BrGreen]
  , ieffect  = Regeneration
  , icount   = intToDeep 1
  , ipower   = (RollDice 2 3, RollDice 1 10)
  , iverbApply   = "tear down"
  , iverbProject = "cast"
  , iweight  = 30
  , itoThrow = -50  -- not dense enough
  }
dart = ItemKind
  { isymbol  = '|'
  , iname    = "dart"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Cyan]
  , ieffect  = Wound (RollDice 1 1)
  , icount   = (RollDice 3 3, RollDice 0 0)
  , ipower   = intToDeep 0
  , iverbApply   = "snap"
  , iverbProject = "hurl"
  , iweight  = 50
  , itoThrow = 0  -- a cheap dart
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("dng", 20)]       -- x3, but rare on shallow levels
  , iflavour = zipPlain brightCol  -- natural, so not fancy
  , ieffect  = NoEffect
  , icount   = intToDeep 0
  , ipower   = intToDeep 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , itoThrow = 0
  }
gem1 = gem
  { icount   = (RollDice 0 0, RollDice 1 1)  -- appears on lvl 1
  }
gem2 = gem
  { icount   = (RollDice 0 0, RollDice 1 2)  -- appears halfway
  }
gem3 = gem
  { icount   = (RollDice 0 0, RollDice 1 3)  -- appears on max depth
  }
gold = ItemKind
  { isymbol  = '$'
  , iname    = "gold piece"
  , ifreq    = [("dng", 80)]
  , iflavour = zipPlain [BrYellow]
  , ieffect  = NoEffect
  , icount   = (RollDice 0 0, RollDice 10 10)
  , ipower   = intToDeep 0
  , iverbApply   = "grind"
  , iverbProject = "toss"
  , iweight  = 31
  , itoThrow = 0
  }
harpoon = ItemKind
  { isymbol  = '|'
  , iname    = "harpoon"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Brown]
  , ieffect  = Wound (RollDice 1 2)
  , icount   = (RollDice 0 0, RollDice 2 2)
  , ipower   = (RollDice 1 1, RollDice 2 2)
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 4000
  , itoThrow = 0  -- cheap but deadly
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy stdCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  , iweight  = 200
  , itoThrow = -50  -- oily, bad grip
  }
potion1 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = ApplyPerfume
  }
potion2 = potion
  { ieffect  = Heal
  , ipower   = (RollDice 5 1, RollDice 0 0)
  }
potion3 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = Wound (RollDice 0 0)
  , ipower   = (RollDice 5 1, RollDice 0 0)
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("dng", 10)]
  , iflavour = zipPlain [White]
  , ieffect  = Searching
  , icount   = intToDeep 1
  , ipower   = (RollDice 1 6, RollDice 3 2)
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , itoThrow = 0
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "decipher"
  , iverbProject = "lob"
  , iweight  = 50
  , itoThrow = -75  -- bad shape, even rolled up
  }
scroll1 = scroll
  { ieffect  = SummonFriend
  }
scroll2 = scroll
  { ifreq    = [("dng", 3)]
  , ieffect  = SummonEnemy
  }
scroll3 = scroll
  { ieffect  = Descend
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sword"
  , ifreq    = [("dng", 60)]
  , iflavour = zipPlain [BrCyan]
  , ieffect  = Wound (RollDice 3 1)
  , icount   = intToDeep 1
  , ipower   = (RollDice 1 2, RollDice 4 2)
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 2000
  , itoThrow = -50  -- ensuring it hits with the tip costs speed
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy [BrRed]
  , ieffect  = Dominate
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , itoThrow = 25  -- magic
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = [("unarmed", 100)]
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  }
foot = sword
  { isymbol  = '@'
  , iname    = "foot"
  , ifreq    = [("unarmed", 50)]
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("monstrous", 100)]
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  }
weight = sword
  { isymbol  = '@'
  , iname    = "power jump"
  , ifreq    = [("weight", 100)]
  , ieffect  = Wound (RollDice 99 99)
  , ipower   = (RollDice 1 99, RollDice 0 0)
  , iverbApply   = "squash"
  , iverbProject = "ERROR, please report: iverbProject weight"
  }
