-- | A game requires the engine provided by the library, perhaps customized,
-- and game content, defined completely afresh for the particular game.
-- The general type of the content is @CDefs@ and it has instances
-- for all content kinds, such as items kinds
-- ('Game.LambdaHack.Content.ItemKind').
-- The possible kinds are fixed in the library and defined in the directory
-- @Content/@. On the other hand, game content, that is all elements
-- of @CDefs@ instances, are defined exclusively in the game code proper,
-- with names corresponding to the their kinds.
module Game.LambdaHack.Content.Content (CDefs(..)) where

import Game.LambdaHack.Misc

-- | The general type of a particular game content, e.g., item kinds.
data CDefs a = CDefs
  { getSymbol :: a -> Char    -- ^ symbol, e.g., to print on the map
  , getName   :: a -> String  -- ^ name, e.g., to show to the player
  , getFreq   :: a -> Freqs   -- ^ frequency within groups
  , validate  :: [a] -> [a]   -- ^ validate and catch some offenders, if any
  , content   :: [a]          -- ^ all the defined content of this type
  }
