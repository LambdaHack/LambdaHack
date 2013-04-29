{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( normalLevelBound, maxLevelDim, divUp, Freqs, breturn
  , FactionId, LevelId
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Text (Text)
import Data.Typeable

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 21)

-- | Maximal supported level X and Y dimension (32768). Not checked anywhere.
-- The value is chosen to support architectures with 32-bit ints.
maxLevelDim :: Int
maxLevelDim = 2 ^ (15 :: Int)

-- | Integer division, rounding up.
divUp :: Int -> Int -> Int
divUp n k = (n + k - 1) `div` k

-- | For each group that the kind belongs to, denoted by a @Text@ name
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs = [(Text, Int)]

-- | @breturn b a = [a | b]@
breturn :: MonadPlus m => Bool -> a -> m a
breturn True a  = return a
breturn False _ = mzero

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  get = liftM EM.fromDistinctAscList get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  get = liftM ES.fromDistinctAscList get

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary FactionId where
  put (FactionId n) = put n
  get = fmap FactionId get

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord,  Enum, Typeable)

instance Binary LevelId where
  put (LevelId n) = put n
  get = fmap LevelId get
