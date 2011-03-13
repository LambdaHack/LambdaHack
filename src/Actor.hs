module Actor where

import Control.Monad
import Data.Binary

data Actor = AHero Int     -- ^ hero index (on the lheroes intmap)
           | AMonster Int  -- ^ monster index (on the lmonsters intmap)
  deriving (Show, Eq)

instance Binary Actor where
  put (AHero n)    = putWord8 0 >> put n
  put (AMonster n) = putWord8 1 >> put n
  get = do
          tag <- getWord8
          case tag of
            0 -> liftM AHero get
            1 -> liftM AMonster get
            _ -> fail "no parse (Actor)"
