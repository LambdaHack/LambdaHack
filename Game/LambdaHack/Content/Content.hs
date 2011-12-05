module Game.LambdaHack.Content.Content ( CDefs(..) ) where

data CDefs a = CDefs
  { getSymbol :: a -> Char
  , getName :: a -> String
  , getFreq :: a -> Int
  , valid   :: a -> Bool
  , content :: [a]
  }
