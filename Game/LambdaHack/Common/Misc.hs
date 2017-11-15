{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Hacks that haven't found their home yet.
module Game.LambdaHack.Common.Misc
  ( -- * Game object identifiers
    FactionId, LevelId, ActorId
    -- * Item containers
  , Container(..), CStore(..), ItemDialogMode(..)
    -- * Assorted
  , GroupName, Freqs, Rarity, AbsDepth(..), Tactic(..)
  , toGroupName, validateRarity, describeTactic
  , makePhrase, makeSentence, normalLevelBound, breturn
  , appDataDir, xM, xD, minusM, minusM1, oneM
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import           Control.DeepSeq
import           Data.Binary
import qualified Data.Char as Char
import qualified Data.EnumMap.Strict as EM
import qualified Data.EnumSet as ES
import qualified Data.Fixed as Fixed
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int64)
import           Data.Key
import           Data.Ord
import           Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Time as Time
import           GHC.Generics (Generic)
import qualified NLP.Miniutter.English as MU
import           System.Directory (getAppUserDataDirectory)
import           System.Environment (getProgName)

import Game.LambdaHack.Common.Point

-- | A unique identifier of a faction in a game.
newtype FactionId = FactionId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | Abstract level identifiers.
newtype LevelId = LevelId Int
  deriving (Show, Eq, Ord, Enum, Hashable, Binary)

-- | A unique identifier of an actor in the dungeon.
newtype ActorId = ActorId Int
  deriving (Show, Eq, Ord, Enum, Binary)

-- | Item container type.
data Container =
    CFloor LevelId Point
  | CEmbed LevelId Point
  | CActor ActorId CStore
  | CTrunk FactionId LevelId Point   -- ^ for bootstrapping actor bodies
  deriving (Show, Eq, Ord, Generic)

instance Binary Container

-- | Actor's item stores.
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

data ItemDialogMode = MStore CStore | MOwned | MStats | MLoreItem | MLoreOrgan
  deriving (Show, Read, Eq, Ord, Generic)

instance NFData ItemDialogMode

instance Binary ItemDialogMode

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

-- | For each group that the kind belongs to, denoted by a @GroupName@
-- in the first component of a pair, the second component of a pair shows
-- how common the kind is within the group.
type Freqs a = [(GroupName a, Int)]

-- | Rarity on given depths.
type Rarity = [(Double, Int)]

-- | Absolute depth in the dungeon. When used for the maximum depth
-- of the whole dungeon, this can be different than dungeon size,
-- e.g., when the dungeon is branched, and it can even be different
-- than the length of the longest branch, if levels at some depths are missing.
newtype AbsDepth = AbsDepth Int
  deriving (Show, Eq, Ord, Hashable, Binary)

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
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Tactic where
  show TExplore        = "explore"
  show TFollow         = "follow freely"
  show TFollowNoItems  = "follow only"
  show TMeleeAndRanged = "fight only"
  show TMeleeAdjacent  = "melee only"
  show TBlock          = "block only"
  show TRoam           = "roam freely"
  show TPatrol         = "patrol area"

instance Binary Tactic

instance Hashable Tactic

toGroupName :: Text -> GroupName a
{-# INLINE toGroupName #-}
toGroupName = GroupName

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

describeTactic :: Tactic -> Text
describeTactic TExplore = "investigate unknown positions, chase targets"
describeTactic TFollow = "follow leader's target or position, grab items"
describeTactic TFollowNoItems =
  "follow leader's target or position, ignore items"
describeTactic TMeleeAndRanged =
  "engage in both melee and ranged combat, don't move"
describeTactic TMeleeAdjacent = "engage exclusively in melee, don't move"
describeTactic TBlock = "block and wait, don't move"
describeTactic TRoam = "move freely, chase targets"
describeTactic TPatrol = "find and patrol an area (WIP)"

-- | Re-exported English phrase creation functions, applied to default
-- irregular word sets.
makePhrase, makeSentence :: [MU.Part] -> Text
makePhrase = MU.makePhrase MU.defIrregular
makeSentence = MU.makeSentence MU.defIrregular

-- | Level bounds.
normalLevelBound :: (Int, Int)
normalLevelBound = (79, 20)

-- | @breturn b a = [a | b]@
breturn :: MonadPlus m => Bool -> a -> m a
breturn True a  = return a
breturn False _ = mzero

-- | Personal data directory for the game. Depends on the OS and the game,
-- e.g., for LambdaHack under Linux it's @~\/.LambdaHack\/@.
appDataDir :: IO FilePath
appDataDir = do
  progName <- getProgName
  let name = takeWhile Char.isAlphaNum progName
  getAppUserDataDirectory name

xM :: Int -> Int64
xM k = fromIntegral k * 1000000

xD :: Double -> Double
xD k = k * 1000000

minusM, minusM1, oneM :: Int64
minusM = xM (-1)
minusM1 = xM (-1) - 1
oneM = xM 1

-- Data.Binary orphan instances

instance (Enum k, Binary k, Binary e) => Binary (EM.EnumMap k e) where
  put m = put (EM.size m) >> mapM_ put (EM.toAscList m)
  get = EM.fromDistinctAscList <$> get

instance (Enum k, Binary k) => Binary (ES.EnumSet k) where
  put m = put (ES.size m) >> mapM_ put (ES.toAscList m)
  get = ES.fromDistinctAscList <$> get

instance Binary Time.NominalDiffTime where
  get = fmap realToFrac (get :: Get Fixed.Pico)
  put = (put :: Fixed.Pico -> Put) . realToFrac

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HM.HashMap k v) where
  get = fmap HM.fromList get
  put = put . HM.toList

-- Data.Key orphan instances

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

-- Data.Hashable orphan instances

instance (Enum k, Hashable k, Hashable e) => Hashable (EM.EnumMap k e) where
  hashWithSalt s x = hashWithSalt s (EM.toAscList x)

-- Control.DeepSeq orphan instances

instance NFData MU.Part

instance NFData MU.Person

instance NFData MU.Polarity
