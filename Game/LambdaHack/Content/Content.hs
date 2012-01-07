-- | The general from of game content.
module Game.LambdaHack.Content.Content ( CDefs(..), Freqs ) where

-- | For each group that the kind belongs to, denoted by a @String@ name
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs = [(String, Int)]

-- | The general type of a particular game content, e.g., monster kinds.
data CDefs a = CDefs
  { getSymbol :: a -> Char    -- ^ symbol, e.g., to print on the map
  , getName   :: a -> String  -- ^ name, e.g., to show to the player
  , getFreq   :: a -> Freqs   -- ^ frequency within groups
  , validate  :: [a] -> [a]   -- ^ validate, find some of the offenders, if any
  , content   :: [a]          -- ^ all the defined content of this type
  }
