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
      [amulet, dart, gem1, gem2, gem3, gem4, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, sword, fist, wand]
  }
amulet,        dart, gem1, gem2, gem3, gem4, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, sword, fist, wand :: ItemKind

gem, potion, scroll :: ItemKind  -- generic templates

-- rollQuad (a, b, x, y) = a * roll b + (lvl * x * roll y) / 10

amulet = ItemKind
  { isymbol  = '"'
  , iflavour = [(BrGreen, True)]
  , iname    = "amulet"
  , ieffect  = Regeneration
  , icount   = intToQuad 1
  , ifreq    = 10
  , ipower   = (2, 1, 2, 2)
  , iverbApply   = "tear down"
  , iverbProject = "throw"
  }
dart = ItemKind
  { isymbol  = '|'
  , iflavour = [(Cyan, False)]
  , iname    = "dart"
  , ieffect  = Wound (1, 1)
  , icount   = (3, 3, 0, 0)
  , ifreq    = 30
  , ipower   = intToQuad 0
  , iverbApply   = "snap"
  , iverbProject = "throw"
  }
gem = ItemKind
  { isymbol  = '*'
  , iflavour = zipPlain brightCol  -- natural, so not fancy
  , iname    = "gem"
  , ieffect  = NoEffect
  , icount   = intToQuad 0
  , ifreq    = 20  -- x4, but rare on shallow levels
  , ipower   = intToQuad 0
  , iverbApply   = "crush"
  , iverbProject = "throw"
  }
gem1 = gem
  { icount   = (1, 1, 0, 0)  -- appears on lvl 1
  }
gem2 = gem
  { icount   = (0, 0, 2, 1)  -- appears on lvl 5, doubled on lvl 10
  }
gem3 = gem
  { icount   = (0, 0, 1, 1)  -- appears on lvl 10
  }
gem4 = gem
  { icount   = (0, 0, 1, 1)  -- appears on lvl 10
  }
gold = ItemKind
  { isymbol  = '$'
  , iflavour = [(BrYellow, False)]
  , iname    = "gold piece"
  , ieffect  = NoEffect
  , icount   = (0, 0, 10, 10)
  , ifreq    = 80
  , ipower   = intToQuad 0
  , iverbApply   = "grind"
  , iverbProject = "throw"
  }
javelin = ItemKind
  { isymbol  = '|'
  , iflavour = [(Yellow, False)]
  , iname    = "javelin"
  , ieffect  = Wound (1, 1)
  , icount   = (0, 0, 2, 2)
  , ifreq    = 30
  , ipower   = (1, 7, 0, 0)
  , iverbApply   = "break up"
  , iverbProject = "throw"
  }
potion = ItemKind
  { isymbol  = '!'
  , iflavour = zipFancy stdCol
  , iname    = "potion"
  , ieffect  = NoEffect
  , icount   = intToQuad 1
  , ifreq    = 10
  , ipower   = intToQuad 0
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  }
potion1 = potion
  { ieffect  = ApplyPerfume
  }
potion2 = potion
  { ieffect  = Heal
  , ipower   = (10, 1, 0, 0)
  }
potion3 = potion
  { ieffect  = Wound (0, 0)
  , ipower   = (10, 1, 0, 0)
  }
ring = ItemKind
  { isymbol  = '='
  , iflavour = [(White, False)]
  , iname    = "ring"
  , ieffect  = Searching
  , icount   = intToQuad 1
  , ifreq    = 10
  , ipower   = (1, 1, 2, 2)
  , iverbApply   = "squeeze down"
  , iverbProject = "throw"
  }
scroll = ItemKind
  { isymbol  = '?'
  , iflavour = zipFancy darkCol  -- arcane and old
  , iname    = "scroll"
  , ieffect  = NoEffect
  , icount   = intToQuad 1
  , ifreq    = 10
  , ipower   = intToQuad 0
  , iverbApply   = "decipher"
  , iverbProject = "throw"
  }
scroll1 = scroll
  { ieffect  = SummonFriend
  , ifreq    = 20
  }
scroll2 = scroll
  { ieffect  = SummonEnemy
  }
sword = ItemKind
  { isymbol  = ')'
  , iflavour = [(BrCyan, False)]
  , iname    = "sword"
  , ieffect  = Wound (3, 1)
  , icount   = intToQuad 1
  , ifreq    = 60
  , ipower   = (1, 2, 4, 2)
  , iverbApply   = "splinter"
  , iverbProject = "heave"
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = 0  -- Does not appear randomly in the dungeon.
  , iverbApply   = "ERROR, please report: iverbApply fist"
  , iverbProject = "ERROR, please report: iverbProject fist"
  }
wand = ItemKind
  { isymbol  = '/'
  , iflavour = [(BrRed, True)]
  , iname    = "wand"
  , ieffect  = Dominate
  , icount   = intToQuad 1
  , ifreq    = 10
  , ipower   = intToQuad 0
  , iverbApply   = "snap"
  , iverbProject = "zap"
  }
