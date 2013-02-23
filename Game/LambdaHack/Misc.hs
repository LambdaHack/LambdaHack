{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Misc
  ( normalLevelBound, maxLevelDim, divUp, Freqs, breturn
  , DiffEM, applyDiffEM
  , FactionId, LevelId
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import Data.List
import Data.Text (Text)
import Data.Typeable

import Game.LambdaHack.Utils.Assert

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

type DiffEM k v = [(k, (Maybe v, Maybe v))]

applyDiffEM :: (Enum k, Eq v, Show k, Show v)
            => DiffEM k v -> EM.EnumMap k v -> EM.EnumMap k v
applyDiffEM diffL em =
  let f m (k, (ov, nv)) = assert (ov /= nv) $
        let g v = assert (v == ov `blame` (v, ov, nv, em, diffL)) nv
        in EM.alter g k m
  in foldl' f em diffL

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
