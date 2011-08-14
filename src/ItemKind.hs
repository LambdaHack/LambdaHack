module ItemKind where

import qualified Data.List as L
import qualified Data.IntMap as IM

import Color
import Effect
import Random

data ItemKind = ItemKind
  { jsymbol  :: !Char      -- ^ map symbol
  , jflavour :: [Flavour]  -- ^ possible flavours
  , jname    :: String     -- ^ item group name
  , jeffect  :: Effect     -- ^ the effect when activated
  , jcount   :: RollQuad   -- ^ created in that quantify
  , jfreq    :: !Int       -- ^ created that often
  , jpower   :: RollQuad   -- ^ created with that power
  }
  deriving (Show, Eq, Ord)

type Flavour = (Color, Bool)  -- the flag tells to use fancy color names

zipPlain cs = L.zip cs (repeat False)
zipFancy cs = L.zip cs (repeat True)
darkCol    = [Red .. Cyan]
brightCol  = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol     = darkCol ++ brightCol
stdFlav    = zipPlain stdCol ++ zipFancy stdCol

flavourToName :: Flavour -> String
flavourToName (c, False) = colorToName c
flavourToName (c, True) = colorToName' c

flavourToColor :: Flavour -> Color
flavourToColor (c, _) = c

dungeonLoot :: IM.IntMap ItemKind
dungeonLoot = IM.fromDistinctAscList (L.zip [0..] loot)

getIK ik = dungeonLoot IM.! ik

loot :: [ItemKind]
loot =
  [amulet,
   dart,
   gem1, gem2, gem3, gem4,
   gold,
   potion1, potion2, potion3,
   ring,
   scroll1, scroll2,
   sword,
   wand]

-- rollQuad (a, b, x, y) = a * d b + (lvl * x * d y) / 10

amulet, dart, gem, gem1, gem2, gem3, gem4, gold :: ItemKind
potion, potion1, potion2, potion3 :: ItemKind
ring, scroll, scroll1, scroll2, sword, wand :: ItemKind
amulet = ItemKind
  { jsymbol  = '"'
  , jflavour = [(BrGreen, True)]
  , jname    = "amulet"
  , jeffect  = Regneration
  , jcount   = intToQuad 1
  , jfreq    = 10
  , jpower   = (2, 1, 2, 2)
  }
dart = ItemKind
  { jsymbol  = ')'
  , jflavour = [(Yellow, False)]
  , jname    = "dart"
  , jeffect  = Wound (1, 1)
  , jcount   = (3, 3, 0, 0)
  , jfreq    = 30
  , jpower   = intToQuad 0
  }
gem = ItemKind
  { jsymbol  = '*'
  , jflavour = zipPlain brightCol  -- natural, so not fancy
  , jname    = "gem"
  , jeffect  = NoEffect
  , jcount   = intToQuad 0
  , jfreq    = 20  -- x4, but rare on shallow levels
  , jpower   = intToQuad 0
  }
gem1 = gem
  { jcount   = (1, 1, 0, 0)  -- appears on lvl 1
  }
gem2 = gem
  { jcount   = (0, 0, 2, 1)  -- appears on lvl 5, doubled on lvl 10
  }
gem3 = gem
  { jcount   = (0, 0, 1, 1)  -- appears on lvl 10
  }
gem4 = gem
  { jcount   = (0, 0, 1, 1)  -- appears on lvl 10
  }
gold = ItemKind
  { jsymbol  = '$'
  , jflavour = [(BrYellow, False)]
  , jname    = "gold piece"
  , jeffect  = NoEffect
  , jcount   = (0, 0, 10, 10)
  , jfreq    = 80
  , jpower   = intToQuad 0
  }
potion = ItemKind
  { jsymbol  = '!'
  , jflavour = zipFancy stdCol
  , jname    = "potion"
  , jeffect  = NoEffect
  , jcount   = intToQuad 1
  , jfreq    = 10
  , jpower   = intToQuad 0
  }
potion1 = potion
  { jeffect  = ApplyPerfume
  }
potion2 = potion
  { jeffect  = Heal
  , jpower   = (10, 1, 0, 0)
  }
potion3 = potion
  { jeffect  = Wound (0, 0)
  , jpower   = (10, 1, 0, 0)
  }
ring = ItemKind
  { jsymbol  = '='
  , jflavour = [(White, False)]
  , jname    = "ring"
  , jeffect  = Searching
  , jcount   = intToQuad 1
  , jfreq    = 10
  , jpower   = (1, 1, 2, 2)
  }
scroll = ItemKind
  { jsymbol  = '?'
  , jflavour = zipFancy darkCol  -- arcane and old
  , jname    = "scroll"
  , jeffect  = NoEffect
  , jcount   = intToQuad 1
  , jfreq    = 10
  , jpower   = intToQuad 0
  }
scroll1 = scroll
  { jeffect  = SummonFriend
  , jfreq    = 20
  }
scroll2 = scroll
  { jeffect  = SummonEnemy
  }
sword = ItemKind
  { jsymbol  = ')'
  , jflavour = [(BrCyan, False)]
  , jname    = "sword"
  , jeffect  = Wound (3, 1)
  , jcount   = intToQuad 1
  , jfreq    = 60
  , jpower   = (1, 2, 4, 2)
  }
wand = ItemKind
  { jsymbol  = '/'
  , jflavour = [(BrRed, True)]
  , jname    = "wand"
  , jeffect  = Dominate
  , jcount   = intToQuad 1
  , jfreq    = 10
  , jpower   = intToQuad 0
  }
