-- | The type of kinds of weapons and treasure.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), validateItemKind
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import Game.LambdaHack.Common.Flavour
import qualified Game.LambdaHack.Common.ItemFeature as IF
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol      :: !Char          -- ^ map symbol
  , iname        :: !Text          -- ^ generic name
  , ifreq        :: !Freqs         -- ^ frequency within groups
  , iflavour     :: ![Flavour]     -- ^ possible flavours
  , icount       :: !RollDeep      -- ^ created in that quantify
  , iverbApply   :: !MU.Part       -- ^ the verb for applying and combat
  , iverbProject :: !MU.Part       -- ^ the verb for projecting
  , iweight      :: !Int           -- ^ weight in grams
  , itoThrow     :: !Int           -- ^ percentage bonus to throw speed
  , ifeature     :: ![IF.Feature]  -- ^ properties
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | No specific possible problems for the content of this kind, so far,
-- so the validation function always returns the empty list of offending kinds.
validateItemKind :: [ItemKind] -> [ItemKind]
validateItemKind l = filter (\ik -> T.length (iname ik) > 25) l
