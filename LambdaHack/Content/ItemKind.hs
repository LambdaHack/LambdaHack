-- | Weapons and treasure for LambdaHack.
module Content.ItemKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
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
      [amulet, dart, gem1, gem2, gem3, gem4, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle]
  }
amulet,        dart, gem1, gem2, gem3, gem4, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle :: ItemKind

gem, potion, scroll :: ItemKind  -- generic templates

-- rollDeep (aDb, xDy) = rollDice aDb + lvl * rollDice xDy / depth

amulet = ItemKind
  { isymbol  = '"'
  , iname    = "amulet"
  , ifreq    = [("dng", 10)]
  , iflavour = zipFancy [BrGreen]
  , ieffect  = Regeneration
  , icount   = intToDeep 1
  , ipower   = (RollDice 2 1, RollDice 2 2)
  , iverbApply   = "tear down"
  , iverbProject = "throw"
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
  , iverbProject = "throw"
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("dng", 20)]  -- x4, but rare on shallow levels
  , iflavour = zipPlain brightCol  -- natural, so not fancy
  , ieffect  = NoEffect
  , icount   = intToDeep 0
  , ipower   = intToDeep 0
  , iverbApply   = "crush"
  , iverbProject = "throw"
  }
gem1 = gem
  { icount   = (RollDice 1 1, RollDice 0 0)  -- appears on lvl 1
  }
gem2 = gem
  { icount   = (RollDice 0 0, RollDice 2 1)  -- appears halfway, doubled on max
  }
gem3 = gem
  { icount   = (RollDice 0 0, RollDice 1 1)  -- appears on max depth
  }
gem4 = gem
  { icount   = (RollDice 0 0, RollDice 1 1)  -- appears on max depth
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
  , iverbProject = "throw"
  }
javelin = ItemKind
  { isymbol  = '|'
  , iname    = "javelin"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Brown]
  , ieffect  = Wound (RollDice 1 1)
  , icount   = (RollDice 0 0, RollDice 2 2)
  , ipower   = (RollDice 1 7, RollDice 0 0)
  , iverbApply   = "break up"
  , iverbProject = "throw"
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("dng", 10)]
  , iflavour = zipFancy stdCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  }
potion1 = potion
  { ieffect  = ApplyPerfume
  }
potion2 = potion
  { ieffect  = Heal
  , ipower   = (RollDice 10 1, RollDice 0 0)
  }
potion3 = potion
  { ieffect  = Wound (RollDice 0 0)
  , ipower   = (RollDice 10 1, RollDice 0 0)
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("dng", 10)]
  , iflavour = zipPlain [White]
  , ieffect  = Searching
  , icount   = intToDeep 1
  , ipower   = (RollDice 1 1, RollDice 2 2)
  , iverbApply   = "squeeze down"
  , iverbProject = "throw"
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("dng", 10)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "decipher"
  , iverbProject = "throw"
  }
scroll1 = scroll
  { ieffect  = SummonFriend
  , ifreq    = [("dng", 20)]
  }
scroll2 = scroll
  { ieffect  = SummonEnemy
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
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = [("dng", 10)]
  , iflavour = zipFancy [BrRed]
  , ieffect  = Dominate
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "snap"
  , iverbProject = "zap"
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
