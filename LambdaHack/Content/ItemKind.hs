{-# LANGUAGE OverloadedStrings #-}
-- | Weapons and treasure for LambdaHack.
module Content.ItemKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = ivalidate
  , content =
      [amulet, dart, gem1, gem2, gem3, currency, harpoon, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle]
  }
amulet,        dart, gem1, gem2, gem3, currency, harpoon, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle :: ItemKind

gem, potion, scroll, wand :: ItemKind  -- generic templates

-- rollDeep (aDb, xDy) = rollDice aDb + lvl * rollDice xDy / depth

amulet = ItemKind
  { isymbol  = '"'
  , iname    = "amulet"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy [BrGreen]
  , ieffect  = Regeneration (RollDice 2 3, RollDice 1 10)
  , icount   = intToDeep 1
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
  , ieffect  = Hurt (RollDice 1 1) (RollDice 1 2, RollDice 1 2)
  , icount   = (RollDice 3 3, RollDice 0 0)
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
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold piece"
  , ifreq    = [("dng", 50), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffect  = NoEffect
  , icount   = (RollDice 0 0, RollDice 10 10)
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
  , ieffect  = Hurt (RollDice 1 2) (RollDice 1 2, RollDice 2 2)
  , icount   = (RollDice 0 0, RollDice 2 2)
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
  { ieffect  = Heal 5
  }
potion3 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = Heal (-5)
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = []  -- [("dng", 10)]  -- TODO: make it useful
  , iflavour = zipPlain [White]
  , ieffect  = Searching (RollDice 1 6, RollDice 3 2)
  , icount   = intToDeep 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , itoThrow = 0
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("dng", 4)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , iverbApply   = "decipher"
  , iverbProject = "lob"
  , iweight  = 50
  , itoThrow = -75  -- bad shape, even rolled up
  }
scroll1 = scroll
  { ieffect  = SummonFriend 1
  , ifreq    = [("dng", 2)]
  }
scroll2 = scroll
  { ieffect  = SpawnMonster 1
  }
scroll3 = scroll
  { ieffect  = Descend 1
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sword"
  , ifreq    = [("dng", 60)]
  , iflavour = zipPlain [BrCyan]
  , ieffect  = Hurt (RollDice 3 1) (RollDice 1 2, RollDice 4 2)
  , icount   = intToDeep 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 2000
  , itoThrow = -50  -- ensuring it hits with the tip costs speed
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = [("dng", 10)]
  , iflavour = zipFancy brightCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , itoThrow = 25  -- magic
  }
wand1 = wand
  { ieffect  = Dominate
  }
wand2 = wand
  { ifreq    = [("dng", 3)]
  , ieffect  = Heal (-25)
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = [("hth", 1), ("unarmed", 100)]
  , ieffect  = Hurt (RollDice 3 1) (intToDeep 0)
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  }
foot = sword
  { isymbol  = '@'
  , iname    = "foot"
  , ifreq    = [("hth", 1), ("unarmed", 50)]
  , ieffect  = Hurt (RollDice 3 1) (intToDeep 0)
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("hth", 1), ("monstrous", 100)]
  , ieffect  = Hurt (RollDice 3 1) (intToDeep 0)
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  }
