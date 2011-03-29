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

-- rollQuad (a, b, x, y) = a + (b * lvl)/10 + d(x + (y * lvl)/10)
rollZero = (0, 0, 0, 0)
rollOne  = (1, 0, 0, 0)

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
   potion_water, potion_healing, potion_wounding,
   ring,
   scroll1, scroll2,
   sword,
   wand_domination]

amulet, dart, gem, gem1, gem2, gem3, gold :: ItemKind
potion, potion_water, potion_healing, potion_wounding :: ItemKind
ring, scroll, scroll1, scroll2, sword :: ItemKind
wand, wand_domination :: ItemKind
amulet = ItemKind
  { jsymbol  = '"'
  , jflavour = [(BrWhite, True)]
  , jname    = "amulet"
  , jeffect  = NoEffect
  , jcount   = rollOne
  , jfreq    = 20
  , jpower   = rollZero
  }
dart = ItemKind
  { jsymbol  = ')'
  , jflavour = [(Yellow, False)]
  , jname    = "dart"
  , jeffect  = Wound (1, 1)
  , jcount   = (3, 0, 6, 0)
  , jfreq    = 30
  , jpower   = rollZero
  }
gem = ItemKind
  { jsymbol  = '*'
  , jflavour = zipPlain brightCol  -- natural, so not fancy
  , jname    = "gem"
  , jeffect  = NoEffect
  , jcount   = rollZero
  , jfreq    = 30  -- x3, but rare on shallow levels
  , jpower   = rollZero
  }
gem1 = gem
  { jcount   = (0, 5, 0, 0)  -- appears on lvl 2
  }
gem2 = gem
  { jcount   = (0, 2, 0, 0)  -- appears on lvl 5
  }
gem3 = gem
  { jcount   = (0, 1, 0, 0)  -- appears on lvl 10
  }
gem4 = gem
  { jcount   = (0, 1, 0, 0)  -- appears on lvl 10
  }
gold = ItemKind
  { jsymbol  = '$'
  , jflavour = [(BrYellow, False)]
  , jname    = "gold piece"
  , jeffect  = NoEffect
  , jcount   = (0, 31, 0, 101)
  , jfreq    = 80
  , jpower   = rollZero
  }
potion = ItemKind
  { jsymbol  = '!'
  , jflavour = zipFancy stdCol
  , jname    = "potion"
  , jeffect  = NoEffect
  , jcount   = rollOne
  , jfreq    = 10  -- x3
  , jpower   = rollZero
  }
potion_water = potion
  { jeffect  = ApplyPerfume
  }
potion_healing = potion
  { jeffect  = Heal
  , jpower   = (10, 0, 0, 0)
  }
potion_wounding = potion
  { jeffect  = Wound (0, 0)
  , jpower   = (10, 0, 0, 0)
  }
ring = ItemKind
  { jsymbol  = '='
  , jflavour = [(BrWhite, False)]
  , jname    = "ring"
  , jeffect  = NoEffect
  , jcount   = rollOne
  , jfreq    = 20
  , jpower   = rollZero
  }
scroll = ItemKind
  { jsymbol  = '?'
  , jflavour = zipFancy darkCol  -- arcane and old
  , jname    = "scroll"
  , jeffect  = NoEffect
  , jcount   = rollOne
  , jfreq    = 15  -- x2
  , jpower   = rollZero
  }
scroll1 = scroll
  { jeffect  = SummonFriend
  }
scroll2 = scroll
  { jeffect  = SummonEnemy
  }
sword = ItemKind
  { jsymbol  = ')'
  , jflavour = [(BrCyan, False)]
  , jname    = "sword"
  , jeffect  = Wound (1, 3)
  , jcount   = rollOne
  , jfreq    = 60
  , jpower   = (0, 3, 2, 5)
  }
wand = ItemKind
  { jsymbol  = '/'
  , jflavour = [(BrRed, True)]
  , jname    = "wand"
  , jeffect  = NoEffect
  , jcount   = rollOne
  , jfreq    = 20
  , jpower   = rollZero
  }
wand_domination = wand
  { jeffect  = Dominate
  }
