module ItemKind where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM

import Color

data ItemKind = ItemKind
  { jsymbol  :: !Char
  , jflavour :: [Flavour]
  , jname    :: String
  , jsecret  :: String
  , jeffect  :: Effect
  , jquant   :: Roll
  , jfreq    :: !Int
  }
  deriving (Show, Eq, Ord)

-- a + b * lvl + roll(c + d * lvl)
type Roll = (Word8, Word8, Word8, Word8)

type Flavour = Color  -- the simplest possible; add "speckled", etc. later

data Effect =
    NoEffect
  | AffectHP Int  -- base damage, to-dam bonus in Item
  | Dominate
  | SummonFriend
  | SummonEnemy
  deriving (Show, Eq, Ord)

rollOne = (1, 0, 0, 0)

darkFlav   = [Red .. Cyan]
brightFlav = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdFlav    = darkFlav ++ brightFlav

flavourToName :: Flavour -> String
flavourToName = colorToName

effectToName :: Effect -> String
effectToName NoEffect = ""
effectToName (AffectHP n)
  | n > 0 = "of healing (" ++ show n ++ ")"
  | n < 0 = "" -- "(base dmg " ++ show (-n) ++ ")"
  | otherwise = "of life"
effectToName Dominate = "of domination"
effectToName SummonFriend = "of aid calling"
effectToName SummonEnemy = "of summoning"

dungeonLoot :: IM.IntMap ItemKind
dungeonLoot = IM.fromDistinctAscList (L.zip [0..] loot)

getIK ik = dungeonLoot IM.! ik

loot :: [ItemKind]
loot =
  [amulet, dart, gem, gem1, gem2, gem3, gold,
   potion_water, potion_healing,
   ring, scroll, sword,
   wand_domination]

amulet, dart, gem, gem1, gem2, gem3, gold :: ItemKind
potion, potion_water, potion_healing :: ItemKind
ring, scroll, sword :: ItemKind
wand, wand_domination :: ItemKind
amulet = ItemKind
  { jsymbol  = '"'
  , jflavour = [BrWhite]
  , jname    = "amulet"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
dart = ItemKind
  { jsymbol  = ')'
  , jflavour = [Yellow]
  , jname    = "dart"
  , jsecret  = ""
  , jeffect  = AffectHP (-1)
  , jquant   = (3, 0, 6, 0)
  , jfreq    = 40
  }
gem = ItemKind
  { jsymbol  = '*'
  , jflavour = brightFlav
  , jname    = "gem"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 5  -- x4, below
  }
gem1 = gem
gem2 = gem
gem3 = gem
gold = ItemKind
  { jsymbol  = '$'
  , jflavour = [BrYellow]
  , jname    = "gold piece"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = (0, 3, 0, 10)
  , jfreq    = 80
  }
potion = ItemKind
  { jsymbol  = '!'
  , jflavour = stdFlav
  , jname    = "potion"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 20
  }
potion_water = potion
  { jsecret  = "of water"
  }
potion_healing = potion
  { jeffect  = AffectHP 20
  }
ring = ItemKind
  { jsymbol  = '='
  , jflavour = [BrWhite]
  , jname    = "ring"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
scroll = ItemKind
  { jsymbol  = '?'
  , jflavour = darkFlav
  , jname    = "scroll"
  , jsecret  = ""
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
sword = ItemKind
  { jsymbol  = ')'
  , jflavour = [BrCyan]
  , jname    = "sword"
  , jsecret  = ""
  , jeffect  = AffectHP (-3)
  , jquant   = rollOne
  , jfreq    = 70
  }
wand = ItemKind
  { jsymbol  = '/'
  , jflavour = [BrRed]
  , jname    = "wand"
  , jeffect  = NoEffect
  , jsecret  = ""
  , jquant   = rollOne
  , jfreq    = 30
  }
wand_domination = wand
  { jeffect  = Dominate
  }
