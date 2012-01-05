module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), ivalidate
  ) where

import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import Game.LambdaHack.Random

-- TODO: jpower is out of place here. It doesn't make sense for all items,
-- and will mean different things for different items. Perhaps it should
-- be part of the Effect, but then we have to be careful to distinguish
-- parts of the Effect that are rolled on item creation and those rolled
-- at each use (e.g., sword magical +damage vs. sword damage dice).
-- Another thing to keep in minds is that jpower will heavily determine
-- the value of the item for shops, treasure chests, artifact set rebalancing,
-- etc., so if we make jpower complex, the value computation gets complex too.
data ItemKind = ItemKind
  { isymbol  :: !Char       -- ^ map symbol
  , iname    :: !String     -- ^ group name
  , ifreq    :: ![(String, Int)]  -- ^ frequency within groups
  , iflavour :: ![Flavour]  -- ^ possible flavours
  , ieffect  :: !Effect     -- ^ the effect when activated
  , icount   :: !RollQuad   -- ^ created in that quantify
  , ipower   :: !RollQuad   -- ^ created with that power
  , iverbApply   :: !String
  , iverbProject :: !String
  }
  deriving Show

ivalidate :: [ItemKind] -> [ItemKind]
ivalidate _ = [] -- TODO
