{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( normalLevelBound, divUp, Freqs, breturn
  , FactionId, LevelId, ActorId
    -- * Item containers
  , Container(..), CStore(..)
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Functor
import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HM
import Data.Key
import Data.Text (Text)
import Data.Traversable (traverse)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Point

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 20)

infixl 7 `divUp`
-- | Integer division, rounding up.
divUp :: Integral a => a -> a -> a
divUp n k = (n + k - 1) `div` k

-- | For each group that the kind belongs to, denoted by a @Text@ name
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs = [(Text, Int)]

-- | @breturn b a = [a | b]@
breturn :: MonadPlus m => Bool -> a -> m a
breturn True a  = return a
breturn False _ = mzero

-- | Item container type.
data Container =
    CFloor !LevelId !Point
  | CActor !ActorId !CStore
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

data CStore =
    CGround
  | CEqp
  | CInv
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary CStore

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary FactionId where
  put (FactionId n) = put n
  get = fmap FactionId get

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary LevelId where
  put (LevelId n) = put n
  get = fmap LevelId get

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum)

instance Binary ActorId where
  put (ActorId n) = put n
  get = fmap ActorId get

-- Data.Binary

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  get = liftM EM.fromDistinctAscList get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  get = liftM ES.fromDistinctAscList get

instance (Binary k, Binary v, Eq k, Hashable.Hashable k)
  => Binary (HM.HashMap k v) where
  put ir = put $ HM.toList ir
  get = fmap HM.fromList get

-- Data.Key

type instance Key (EM.EnumMap k) = k

instance Zip (EM.EnumMap k) where
  zipWith = EM.intersectionWith

instance Enum k => ZipWithKey (EM.EnumMap k) where
  zipWithKey = EM.intersectionWithKey

instance Enum k => Keyed (EM.EnumMap k) where
  mapWithKey = EM.mapWithKey

instance Enum k => FoldableWithKey (EM.EnumMap k) where
  foldrWithKey = EM.foldrWithKey

instance Enum k => TraversableWithKey (EM.EnumMap k) where
  traverseWithKey f = fmap EM.fromDistinctAscList
                      . traverse (\(k, v) -> (,) k <$> f k v) . EM.toAscList

instance Enum k => Indexable (EM.EnumMap k) where
  index = (EM.!)

instance Enum k => Lookup (EM.EnumMap k) where
  lookup = EM.lookup

instance Enum k => Adjustable (EM.EnumMap k) where
  adjust = EM.adjust
