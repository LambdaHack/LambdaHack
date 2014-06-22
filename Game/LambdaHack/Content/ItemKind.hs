-- | The type of kinds of weapons and treasure.
module Game.LambdaHack.Content.ItemKind
  ( ItemKind(..), validateItemKind
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified NLP.Miniutter.English as MU

import qualified Game.LambdaHack.Common.Dice as Dice
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc

-- | Item properties that are fixed for a given kind of items.
data ItemKind = ItemKind
  { isymbol      :: !Char          -- ^ map symbol
  , iname        :: !Text          -- ^ generic name
  , ifreq        :: !Freqs         -- ^ frequency within groups
  , iflavour     :: ![Flavour]     -- ^ possible flavours
  , icount       :: !Dice.Dice     -- ^ created in that quantity
  , iverbApply   :: !MU.Part       -- ^ the verb for applying and melee
  , iverbProject :: !MU.Part       -- ^ the verb for projecting
  , iweight      :: !Int           -- ^ weight in grams
  , iaspects     :: ![Effect.Aspect Dice.Dice]
                                   -- ^ keep the aspect continuously
  , ieffects     :: ![Effect.Effect Dice.Dice]
                                   -- ^ cause the effect when triggered
  , ifeature     :: ![Effect.Feature]
                                   -- ^ public properties
  , idesc        :: !Text          -- ^ description
  , ikit         :: ![(Text, CStore)]  -- ^ accompanying body parts and items
  }
  deriving Show  -- No Eq and Ord to make extending it logically sound, see #53

-- | Filter a list of kinds, passing through only the incorrect ones, if any.
validateItemKind :: [ItemKind] -> [ItemKind]
validateItemKind l = filter (\ik -> T.length (iname ik) > 23) l
