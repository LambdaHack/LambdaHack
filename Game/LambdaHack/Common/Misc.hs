{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( -- * Game object identifiers
    FactionId, LevelId, AbsDepth(..), ActorId
    -- * Item containers
  , Container(..), CStore(..), ItemDialogMode(..)
    -- * Assorted
  , normalLevelBound, divUp, GroupName, toGroupName, Freqs, breturn
  , serverSaveName, Rarity, validateRarity, Tactic(..)
  ) where

import Control.Monad
import Data.Binary
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.Functor
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Key
import Data.List
import Data.Ord
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (traverse)
import GHC.Generics (Generic)
import NLP.Miniutter.English ()

import Game.LambdaHack.Common.Point

serverSaveName :: String
serverSaveName = "server.sav"

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 20)

infixl 7 `divUp`
-- | Integer division, rounding up.
divUp :: Integral a => a -> a -> a
{-# INLINE divUp #-}
divUp n k = (n + k - 1) `div` k

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName a = GroupName Text
  deriving (Eq, Ord, Read, Hashable, Binary)

instance IsString (GroupName a) where
  fromString = GroupName . T.pack

instance Show (GroupName a) where
  show (GroupName gn) = T.unpack gn

toGroupName :: Text -> GroupName a
toGroupName = GroupName

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs a = [(GroupName a, Int)]

-- | Rarity on given depths.
type Rarity = [(Double, Int)]

validateRarity :: Rarity -> [Text]
validateRarity rarity =
  let sortedRarity = sortBy (comparing fst) rarity
  in [ "rarity not sorted" | sortedRarity /= rarity ]
     ++ [ "rarity depth thresholds not unique"
        | nubBy ((==) `on` fst) sortedRarity /= sortedRarity ]
     ++ [ "rarity depth not between 0 and 10"
        | case (sortedRarity, reverse sortedRarity) of
            ((lowest, _) : _, (highest, _) : _) ->
              lowest <= 0 || highest > 10
            _ -> False ]

-- | @breturn b a = [a | b]@
breturn :: MonadPlus m => Bool -> a -> m a
breturn True a  = return a
breturn False _ = mzero

-- | Item container type.
data Container =
    CFloor !LevelId !Point
  | CEmbed !LevelId !Point
  | CActor !ActorId !CStore
  | CTrunk !FactionId !LevelId !Point   -- ^ for bootstrapping actor bodies
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

data CStore =
    CGround
  | COrgan
  | CEqp
  | CInv
  | CSha
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance Binary CStore

instance Hashable CStore

data ItemDialogMode = MStore CStore | MOwned | MStats
  deriving (Show, Read, Eq, Ord)

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | Absolute depth in the dungeon. When used for the maximum depth
-- of the whole dungeon, this can be different than dungeon size,
-- e.g., when the dungeon is branched, and it can even be different
-- than the length of the longest branch, if levels at some depths are missing.
newtype AbsDepth = AbsDepth Int
  deriving (Show, Eq, Ord, Hashable, Binary)

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- TODO: alway shoot, never shoot, etc., but there is too many and this is best
-- expressed via skills, and also we risk micromanagement, so let's stop
-- and think first; perhaps only have as many tactics as needed for realistic
-- AI behaviour in our game modes; perhaps even expose only some of them to UI
data Tactic =
    TBlock    -- ^ always only wait, even if enemy in melee range
  | TFollow   -- ^ always follow leader's target or his position if no target
  | TExplore  -- ^ if enemy nearby, attack, if no items, etc., explore unknown
  | TRoam     -- ^ if enemy nearby, attack, if no items, etc., roam randomly
  | TPatrol   -- ^ find an open and uncrowded area, patrol it according
              --   to sight radius and fallback temporarily to @TRoam@
              --   when enemy is seen by the faction and is within
              --   the actor's sight radius
              --   TODO (currently the same as TExplore; should it chase
              --   targets too (TRoam) and only switch to TPatrol when none?)
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Tactic where
  show TBlock = "block and wait"
  show TFollow = "follow leader's target or position"
  show TExplore = "explore unknown, chase targets"
  show TRoam = "roam freely, chase targets"
  show TPatrol = "find and patrol an area (TODO)"

instance Binary Tactic

instance Hashable Tactic

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
