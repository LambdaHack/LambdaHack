-- | Weapons and treasure for LambdaHack.
module Content.ItemKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemFeature
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = validateItemKind
  , content =
      [amulet, brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, oilLamp, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, scroll4, shield, sword, wand1, wand2, woodenTorch, fist, foot, tentacle, lash, noseTip, lip, claw, smallClaw, snout, venomTooth, venomFang, largeTail, jaw, largeJaw, fragrance, mist_healing, mist_wounding, burningOil2, burningOil3, burningOil4, explosionBlast10, glass_piece, smoke]
  }
amulet,        brassLantern, dart, dart100, gem1, gem2, gem3, currency, harpoon, oilLamp, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, scroll4, shield, sword, wand1, wand2, woodenTorch, fist, foot, tentacle, lash, noseTip, lip, claw, smallClaw, snout, venomTooth, venomFang, largeTail, jaw, largeJaw, fragrance, mist_healing, mist_wounding, burningOil2, burningOil3, burningOil4, explosionBlast10, glass_piece, smoke :: ItemKind

gem, potion, scroll, wand :: ItemKind  -- generic templates

amulet = ItemKind
  { isymbol  = '"'
  , iname    = "amulet"
  , ifreq    = [("useful", 6)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , iverbApply   = "tear down"
  , iverbProject = "cast"
  , iweight  = 30
  , itoThrow = -50  -- not dense enough
  , iaspects = [Regeneration (2 * d 3 + dl 10)]
  , ieffects = []
  , ifeature = []
  , idesc    = "A necklace of dried herbs and healing berries."
  }
brassLantern = ItemKind
  { isymbol  = '('
  , iname    = "brass lantern"
  , ifreq    = [("useful", 2)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , iverbApply   = "douse"
  , iverbProject = "heave"
  , iweight  = 2400
  , itoThrow = -30  -- hard to throw so that it opens and burns
  , iaspects = [Light 4]
  , ieffects = [Burn 4]
  , ifeature = [Explode "burning oil 4"]
  , idesc    = "Very bright and quite heavy brass lantern."
  }
dart = ItemKind
  { isymbol  = '|'
  , iname    = "dart"
  , ifreq    = [("useful", 20)]
  , iflavour = zipPlain [Cyan]
  , icount   = 3 * d 3
  , iverbApply   = "snap"
  , iverbProject = "hurl"
  , iweight  = 50
  , itoThrow = 0  -- a cheap dart
  , iaspects = []
  , ieffects = [Hurt (d 2) (d 3 + dl 3)]
  , ifeature = []
  , idesc    = "Little, but sharp and sturdy."
  }
dart100 = ItemKind
  { isymbol  = '|'
  , iname    = "fine dart"
  , ifreq    = [("useful", 20)]
  , iflavour = zipPlain [BrRed]
  , icount   = 3 * d 3
  , iverbApply   = "snap"
  , iverbProject = "hurl"
  , iweight  = 50
  , itoThrow = 100
  , iaspects = []
  , ieffects = [Hurt (d 1) (d 2 + dl 2)]
  , ifeature = []
  , idesc    = "Subtly balanced for throws of great speed."
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("treasure", 20)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain brightCol  -- natural, so not fancy
  , icount   = 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , itoThrow = 0
  , iaspects = []
  , ieffects = []
  , ifeature = []
  , idesc    = "Precious, though useless. Worth around 100 gold."
  }
gem1 = gem
  { icount   = dl 1  -- appears on max depth
  }
gem2 = gem
  { icount   = dl 2  -- appears halfway
  }
gem3 = gem
  { icount   = dl 3  -- appears early
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold piece"
  , ifreq    = [("treasure", 20), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 * dl 10  -- appears on lvl 2
  , iverbApply   = "grind"
  , iverbProject = "toss"
  , iweight  = 31
  , itoThrow = 0
  , iaspects = []
  , ieffects = []
  , ifeature = []
  , idesc    = "Reliably valuable in every civilized place."
  }
harpoon = ItemKind
  { isymbol  = '|'
  , iname    = "harpoon"
  , ifreq    = [("useful", 25)]
  , iflavour = zipPlain [Brown]
  , icount   = 2 * dl 2
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 4000
  , itoThrow = 0  -- cheap but deadly
  , iaspects = []
  , ieffects = [Hurt (3 * d 1) (d 2 + 2 * dl 2)]
  , ifeature = []
  , idesc    = "A long, well balanced rod, with a cruel, barbed head."
  }
oilLamp = ItemKind
  { isymbol  = '('
  , iname    = "oil lamp"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , iverbApply   = "douse"
  , iverbProject = "lob"
  , iweight  = 1000
  , itoThrow = -30  -- hard not to spill the oil while throwing
  , iaspects = [Light 3]
  , ieffects = [Burn 3]
  , ifeature = [Explode "burning oil 3"]
  , idesc    = "A clay lamp full of plant oil feeding a thick wick."
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "potion"
  , ifreq    = [("useful", 10)]
  , iflavour = zipFancy stdCol
  , icount   = 1
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  , iweight  = 200
  , itoThrow = -50  -- oily, bad grip
  , iaspects = []
  , ieffects = []
  , ifeature = [Consumable]
  , idesc    = "A flask of bubbly, slightly oily liquid of a suspect color."
  }
potion1 = potion
  { ieffects = [ApplyPerfume]
  , ifeature = ifeature potion ++ [Explode "fragrance"]
  }
potion2 = potion
  { ieffects = [Heal 5]
  , ifeature = ifeature potion ++ [Explode "healing mist"]
  }
potion3 = potion
  { ieffects = [Blast 10]
  , ifeature = ifeature potion ++ [Explode "explosion blast 10"]
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 6)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , itoThrow = 0
  , iaspects = [Steadfastness (d 2 + 2 * dl 2)]
  , ieffects = []
  , ifeature = []
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with the reminder of purpose."
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "scroll"
  , ifreq    = [("useful", 4)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , icount   = 1
  , iverbApply   = "decipher"
  , iverbProject = "lob"
  , iweight  = 50
  , itoThrow = -75  -- bad shape, even rolled up
  , iaspects = []
  , ieffects = []
  , ifeature = [Consumable]
  , idesc    = "A haphazardly scribbled piece of parchment. May contain directions or a secret call sign."
  }
scroll1 = scroll
  { ifreq    = [("useful", 2)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { ieffects = [Summon 1]
  }
scroll3 = scroll
  { ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { ifreq    = [("useful", 1)]
  , ieffects = [Dominate]
  }
shield = ItemKind
  { isymbol  = ']'
  , iname    = "shield"
  , ifreq    = [("useful", 5)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , iverbApply   = "bash"
  , iverbProject = "push"
  , iweight  = 3000
  , itoThrow = -80  -- unwieldy to throw and blunt
  , iaspects = [ArmorMelee 50]
  , ieffects = []
  , ifeature = []
  , idesc    = "Large and unwieldy. Absorbs the precentage of melee damage, both dealt and sustained."
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sword"
  , ifreq    = [("useful", 40)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 2000
  , itoThrow = -60  -- ensuring it hits with the tip costs speed
  , iaspects = []
  , ieffects = [Hurt (5 * d 1) (d 2 + 4 * dl 2)]
  , ifeature = []
  , idesc    = "A standard heavy weapon. Does not penetrate very effectively, but hard to block."
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "wand"
  , ifreq    = []  -- TODO: add charges, etc.  -- [("useful", 2)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , itoThrow = 25  -- magic
  , iaspects = []
  , ieffects = []
  , ifeature = [Fragile]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  }
wand1 = wand
  { ieffects = [NoEffect]
  }
wand2 = wand
  { ieffects = [NoEffect]
  }
woodenTorch = ItemKind
  { isymbol  = '('
  , iname    = "wooden torch"
  , ifreq    = [("useful", 10)]
  , iflavour = zipPlain [Brown]
  , icount   = d 3
  , iverbApply   = "douse"
  , iverbProject = "fling"
  , iweight  = 1200
  , itoThrow = 0
  , iaspects = [Light 2]
  , ieffects = [Burn 2]
  , ifeature = []
  , idesc    = "A heavy wooden torch, burning with a weak fire."
  }
fist = sword
  { isymbol  = '%'
  , iname    = "fist"
  , ifreq    = [("fist", 100)]
  , icount   = 2
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
foot = fist
  { isymbol  = '%'
  , iname    = "foot"
  , ifreq    = [("foot", 50)]
  , icount   = 2
  , iverbApply   = "kick"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
tentacle = fist
  { isymbol  = '%'
  , iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbApply   = "slap"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
lash = fist
  { isymbol  = '%'
  , iname    = "lash"
  , ifreq    = [("lash", 100)]
  , icount   = 1
  , iverbApply   = "lash"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
noseTip = fist
  { isymbol  = '%'
  , iname    = "nose tip"
  , ifreq    = [("nose tip", 50)]
  , icount   = 1
  , iverbApply   = "poke"
  , ieffects = [Hurt (2 * d 1) 0]
  , idesc    = ""
  }
lip = fist
  { isymbol  = '%'
  , iname    = "lip"
  , ifreq    = [("lip", 10)]
  , icount   = 2
  , iverbApply   = "lap"
  , ieffects = [Hurt (1 * d 1) 0]
  , idesc    = ""
  }
claw = fist
  { isymbol  = '%'
  , iname    = "claw"
  , ifreq    = [("claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbApply   = "slash"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
smallClaw = fist
  { isymbol  = '%'
  , iname    = "small claw"
  , ifreq    = [("small claw", 10)]
  , icount   = 2
  , iverbApply   = "slash"
  , ieffects = [Hurt (2 * d 1) 0]
  , idesc    = ""
  }
snout = fist
  { isymbol  = '%'
  , iname    = "snout"
  , ifreq    = [("snout", 10)]
  , iverbApply   = "bite"
  , ieffects = [Hurt (2 * d 1) 0]
  , idesc    = ""
  }
venomTooth = fist
  { isymbol  = '%'
  , iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbApply   = "bite"
  , ieffects = [Hurt (3 * d 1) 7]
  , idesc    = ""
  }
venomFang = fist
  { isymbol  = '%'
  , iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbApply   = "bite"
  , ieffects = [Hurt (3 * d 1) 12]
  , idesc    = ""
  }
largeTail = fist
  { isymbol  = '%'
  , iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbApply   = "knock"
  , ieffects = [Hurt (7 * d 1) 0]
  , idesc    = ""
  }
jaw = fist
  { isymbol  = '%'
  , iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbApply   = "rip"
  , ieffects = [Hurt (5 * d 1) 0]
  , idesc    = ""
  }
largeJaw = fist
  { isymbol  = '%'
  , iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbApply   = "crush"
  , ieffects = [Hurt (10 * d 1) 0]
  , idesc    = ""
  }
fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 15
  , iverbApply   = "smell"
  , iverbProject = "exude"
  , iweight  = 1
  , itoThrow = -87  -- the slowest that travels at least 2 steps
  , iaspects = []
  , ieffects = [Impress]
  , ifeature = [Fragile]
  , idesc    = ""
  }
mist_healing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 11
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -93  -- the slowest that gets anywhere (1 step only)
  , iaspects = []
  , ieffects = [Heal 2]
  , ifeature = [Fragile]
  , idesc    = ""
  }
mist_wounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 13
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -93
  , iaspects = []
  , ieffects = [Heal (-2)]
  , ifeature = [Fragile]
  , idesc    = ""
  }
burningOil2 = burningOil 2
burningOil3 = burningOil 3
burningOil4 = burningOil 4
explosionBlast10 = explosionBlast 10
glass_piece = ItemKind  -- when blowing up windows
  { isymbol  = '\''
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 17
  , iverbApply   = "grate"
  , iverbProject = "toss"
  , iweight  = 10
  , itoThrow = 0
  , iaspects = []
  , ieffects = [Hurt (d 1) 0]
  , ifeature = [Fragile, Linger 20]
  , idesc    = ""
  }
smoke = ItemKind  -- when stuff burns out
  { isymbol  = '\''
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 19
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -70
  , iaspects = []
  , ieffects = []
  , ifeature = [Fragile]
  , idesc    = ""
  }

burningOil :: Int -> ItemKind
burningOil n = ItemKind
  { isymbol  = '\''
  , iname    = "burning oil"
  , ifreq    = [("burning oil" <+> tshow n, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = intToDice (n * 6)
  , iverbApply   = "smear"
  , iverbProject = "spit"
  , iweight  = 1
  , itoThrow = min 0 $ n * 7 - 100
  , iaspects = []
  , ieffects = [Burn 1]
  , ifeature = [Fragile]
  , idesc    = "Sticky oil, burning brightly."
  }

explosionBlast :: Int -> ItemKind
explosionBlast n = ItemKind
  { isymbol  = '\''
  , iname    = "explosion blast"
  , ifreq    = [("explosion blast" <+> tshow n, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 12  -- strong, but few, so not always hits target
  , iverbApply   = "blast"
  , iverbProject = "give off"
  , iweight  = 1
  , itoThrow = 0
  , iaspects = []
  , ieffects = [Burn n]
  , ifeature = [Fragile, Linger 10]
  , idesc    = ""
  }
