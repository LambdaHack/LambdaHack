module ItemKind
  (ItemKind(..), swordKindId)
  where

import Color
import Effect
import Random
import Flavour
import qualified Kind
import qualified Content

-- TODO: jpower is out of place here. It doesn't make sense for all items,
-- and will mean different things for different items. Perhaps it should
-- be part of the Effect, but then we have to be careful to distinguish
-- parts of the Effect that are rolled on item creation and those rolled
-- at each use (e.g., sword magical +damage vs. sword damage dice).
-- Another thing to keep in minds is that jpower will heavily determine
-- the value of the item for shops, treasure chests, artifact set rebalancing,
-- etc., so if we make jpower complex, the value computation gets complex too.
data ItemKind = ItemKind
  { jsymbol  :: !Char       -- ^ map symbol
  , jflavour :: ![Flavour]  -- ^ possible flavours
  , jname    :: !String     -- ^ group name
  , jeffect  :: !Effect     -- ^ the effect when activated
  , jcount   :: !RollQuad   -- ^ created in that quantify
  , jfreq    :: !Int        -- ^ created that often
  , jpower   :: !RollQuad   -- ^ created with that power
  }
  deriving (Show, Eq, Ord)

instance Content.Content ItemKind where
  getFreq = jfreq
  content =
    [amulet, dart, gem1, gem2, gem3, gem4, gold, potion1, potion2, potion3, ring, scroll1, scroll2, sword, wand]

amulet,      dart, gem1, gem2, gem3, gem4, gold, potion1, potion2, potion3, ring, scroll1, scroll2, sword, wand :: ItemKind

gem, potion, scroll :: ItemKind  -- generic templates

-- rollQuad (a, b, x, y) = a * roll b + (lvl * x * roll y) / 10

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


swordKindId :: Kind.Id ItemKind
swordKindId = Kind.getId sword
