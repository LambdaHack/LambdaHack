module Game.LambdaHack.Content.Content ( CDefs(..), Freqs ) where

type Freqs = [(String, Int)]

data CDefs a = CDefs
  { getSymbol :: a -> Char    -- ^ symbol, e.g., to print on the map
  , getName   :: a -> String  -- ^ name, e.g., to show to the player
  , getFreq   :: a -> Freqs   -- ^ frequency within groups
  , validate  :: [a] -> [a]   -- ^ validate, find some of the offenders, if any
  , content   :: [a]          -- ^ all the defined content of this type
  }
