{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( -- * Game object identifiers
    FactionId, LevelId, ActorId
    -- * Item containers
  , Container(..), CStore(..)
    -- * Assorted
  , normalLevelBound, divUp, Freqs, breturn
  , serverSaveName, nearby
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Functor
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Key
import Data.Text (Text)
import Data.Traversable (traverse)
import GHC.Generics (Generic)

import Game.LambdaHack.Common.Point

-- | What distance signifies that two actors are "nearby".
nearby :: Int
nearby = 10

serverSaveName :: String
serverSaveName = "server.sav"

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
  | CTrunk !FactionId !LevelId !Point   -- ^ for bootstrapping actor bodies
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

data CStore =
    CGround
  | CBody
  | CEqp
  | CInv
  | CSha
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary CStore

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- Data.Binary

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  {-# INLINEABLE put #-}
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  {-# INLINEABLE get #-}
  get = liftM EM.fromDistinctAscList get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  {-# INLINEABLE put #-}
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  {-# INLINEABLE get #-}
  get = liftM ES.fromDistinctAscList get

instance (Binary k, Binary v, Eq k, Hashable k) => Binary (HM.HashMap k v) where
  {-# INLINEABLE put #-}
  put ir = put $ HM.toList ir
  {-# INLINEABLE get #-}
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

-- Data.Hashable

instance (Enum k, Hashable k, Hashable e) => Hashable (EM.EnumMap k e) where
  {-# INLINEABLE hashWithSalt #-}
  hashWithSalt s x = hashWithSalt s (EM.toAscList x)
