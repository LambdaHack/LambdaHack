-- | The game arena comprised of levels. No operation in this module
-- involves the 'State', 'COps', 'Config' or 'Action' type.
module Game.LambdaHack.Dungeon
  ( -- * Level identifier
    LevelId, levelNumber, levelDefault
    -- * Dungeon
  , Dungeon, fromList, currentFirst, adjust, mapDungeon, (!), lookup, depth
  ) where

import Prelude hiding (lookup)
import Data.Binary
import qualified Data.Map as M
import qualified Data.List as L

import Game.LambdaHack.Utils.Assert
import Game.LambdaHack.Level

-- | Level ids are, for now, ordered linearly by depth.
newtype LevelId = LambdaCave Int
  deriving (Show, Eq, Ord)

instance Binary LevelId where
  put (LambdaCave n) = put n
  get = fmap LambdaCave get

-- | Depth of a level.
levelNumber :: LevelId -> Int
levelNumber (LambdaCave n) = n

-- | Default level for a given depth.
levelDefault :: Int -> LevelId
levelDefault = LambdaCave

-- | The complete dungeon is a map from level names to levels.
data Dungeon = Dungeon
  { dungeonLevelMap :: M.Map LevelId Level
  , dungeonDepth :: Int  -- can be different than the number of levels
  }
  deriving Show

instance Binary Dungeon where
  put Dungeon{..} = do
    put (M.toAscList dungeonLevelMap)
    put dungeonDepth
  get = do
    lvls <- get
    let dungeonLevelMap = M.fromDistinctAscList lvls
    dungeonDepth <- get
    return Dungeon{..}

-- | Create a dungeon from a list of levels and maximum depth.
-- The depth is only a danger indicator;
-- there may potentially be multiple levels
-- with the same depth.
fromList :: [(LevelId, Level)] -> Int -> Dungeon
fromList lvls d = assert (d <= L.length lvls `blame` (d, L.length lvls)) $
  Dungeon (M.fromList lvls) d

-- | Association list corresponding to the dungeon.
-- Starts at the supplied level id (usually the current level)
-- to try to speed up the searches and keep the dungeon lazy.
currentFirst :: LevelId -> Dungeon -> [(LevelId, Level)]
currentFirst lid (Dungeon m _) =
  (lid, m M.! lid)
  : L.filter ((/= lid) . fst) (M.assocs m)

-- | Adjust the level at a given id.
adjust :: (Level -> Level) -> LevelId -> Dungeon -> Dungeon
adjust f lid (Dungeon m d) = Dungeon (M.adjust f lid m) d

-- | Adjust the level at a given id.
mapDungeon :: (Level -> Level) -> Dungeon -> Dungeon
mapDungeon f (Dungeon m d) = Dungeon (M.map f m) d

-- | Find a level with the given id.
(!) :: Dungeon -> LevelId -> Level
(!) (Dungeon m _) lid = m M.! lid

-- | Try to look up a level with the given id.
lookup :: LevelId -> Dungeon -> Maybe Level
lookup lid (Dungeon m _) = M.lookup lid m

-- | Maximum depth of the dungeon.
depth :: Dungeon -> Int
depth = dungeonDepth
