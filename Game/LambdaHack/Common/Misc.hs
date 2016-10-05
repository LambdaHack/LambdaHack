{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( -- * Game object identifiers
    FactionId, LevelId, AbsDepth(..), ActorId
    -- * Item containers
  , Container(..), CStore(..), ItemDialogMode(..)
    -- * Assorted
  , makePhrase, makeSentence
  , normalLevelBound, GroupName, toGroupName, Freqs, breturn
  , serverSaveName, Rarity, validateRarity, Tactic(..), appDataDir
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.DeepSeq
import Data.Binary
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import Data.Function
import Data.Hashable
import Data.Key
import Data.Ord
import Data.String (IsString (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import System.Directory (getAppUserDataDirectory)
import System.Environment (getProgName)

import Game.LambdaHack.Common.Point

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrregular
makeSentence = MU.makeSentence MU.defIrregular

serverSaveName :: String
serverSaveName = "server.sav"

-- | Level bounds. TODO: query terminal size instead and scroll view.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 20)

-- If ever needed, we can use a symbol table here, since content
-- is never serialized. But we'd need to cover the few cases
-- (e.g., @litemFreq@) where @GroupName@ goes into savegame.
newtype GroupName a = GroupName Text
  deriving (Read, Eq, Ord, Hashable, Binary, Generic)

instance IsString (GroupName a) where
  fromString = GroupName . T.pack

instance Show (GroupName a) where
  show (GroupName gn) = T.unpack gn

instance NFData (GroupName a)

toGroupName :: Text -> GroupName a
{-# INLINE toGroupName #-}
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

instance NFData CStore

data ItemDialogMode = MStore CStore | MOwned | MStats
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData ItemDialogMode

instance Binary ItemDialogMode

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

-- TODO: there is already too many; express this somehow via skills;
-- also, we risk micromanagement; perhaps only have as many tactics
-- as needed for realistic AI behaviour in our game modes;
-- perhaps even expose only some of them to UI; perhaps define tactics
-- in rules content or in game mode defs; perhaps have skills corresponding
-- to exploration. following, etc.
-- | Tactic of non-leader actors. Apart of determining AI operation,
-- each tactic implies a skill modifier, that is added to the non-leader skills
-- defined in 'fskillsOther' field of 'Player'.
data Tactic =
    TExplore  -- ^ if enemy nearby, attack, if no items, etc., explore unknown
  | TFollow   -- ^ always follow leader's target or his position if no target
  | TFollowNoItems   -- ^ follow but don't do any item management nor use
  | TMeleeAndRanged  -- ^ only melee and do ranged combat
  | TMeleeAdjacent   -- ^ only melee (or wait)
  | TBlock    -- ^ always only wait, even if enemy in melee range
  | TRoam     -- ^ if enemy nearby, attack, if no items, etc., roam randomly
  | TPatrol   -- ^ find an open and uncrowded area, patrol it according
              --   to sight radius and fallback temporarily to @TRoam@
              --   when enemy is seen by the faction and is within
              --   the actor's sight radius
              --   TODO (currently the same as TExplore; should it chase
              --   targets too (TRoam) and only switch to TPatrol when none?)
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Tactic where
  show TExplore = "explore unknown, chase targets"
  show TFollow = "follow leader's target or position"
  show TFollowNoItems = "follow leader's target or position, ignore items"
  show TMeleeAndRanged = "only melee and perform ranged combat"
  show TMeleeAdjacent = "only melee"
  show TBlock = "only block and wait"
  show TRoam = "roam freely, chase targets"
  show TPatrol = "find and patrol an area (TODO)"

instance Binary Tactic

instance Hashable Tactic

-- Data.Binary

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  {-# INLINABLE put #-}
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  {-# INLINABLE get #-}
  get = liftM EM.fromDistinctAscList get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  {-# INLINABLE put #-}
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  {-# INLINABLE get #-}
  get = liftM ES.fromDistinctAscList get

-- Data.Key

type instance Key (EM.EnumMap k) = k

instance Zip (EM.EnumMap k) where
  {-# INLINE zipWith #-}
  zipWith = EM.intersectionWith

instance Enum k => ZipWithKey (EM.EnumMap k) where
  {-# INLINE zipWithKey #-}
  zipWithKey = EM.intersectionWithKey

instance Enum k => Keyed (EM.EnumMap k) where
  {-# INLINE mapWithKey #-}
  mapWithKey = EM.mapWithKey

instance Enum k => FoldableWithKey (EM.EnumMap k) where
  {-# INLINE foldrWithKey #-}
  foldrWithKey = EM.foldrWithKey

instance Enum k => TraversableWithKey (EM.EnumMap k) where
  {-# INLINABLE traverseWithKey #-}
  traverseWithKey f = fmap EM.fromDistinctAscList
                      . traverse (\(k, v) -> (,) k <$> f k v) . EM.toAscList

instance Enum k => Indexable (EM.EnumMap k) where
  {-# INLINE index #-}
  index = (EM.!)

instance Enum k => Lookup (EM.EnumMap k) where
  {-# INLINE lookup #-}
  lookup = EM.lookup

instance Enum k => Adjustable (EM.EnumMap k) where
  {-# INLINE adjust #-}
  adjust = EM.adjust

-- Data.Hashable

instance (Enum k, Hashable k, Hashable e) => Hashable (EM.EnumMap k e) where
  {-# INLINABLE hashWithSalt #-}
  hashWithSalt s x = hashWithSalt s (EM.toAscList x)

-- Control.DeepSeq

instance NFData MU.Part

instance NFData MU.Person

instance NFData MU.Polarity

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name
